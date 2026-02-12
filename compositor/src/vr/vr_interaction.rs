//! VR window interaction — head-gaze ray, ray-surface intersection,
//! pointer events, grab/move, scroll, follow mode, and depth adjustment.
//!
//! Implements the "Xray" pattern from the KWin VR MR: the gaze ray does
//! NOT originate from between the eyes (which causes neck strain). Instead
//! the ray origin is offset from the head pose toward the dominant hand.

use std::collections::HashMap;
use tracing::{debug, info};

use super::scene::{Geometry, Mat4, Quat, SceneNode, Transform3D, Vec3, VrScene};

// ── Vec2 ─────────────────────────────────────────────────────

/// 2D vector for UV coordinates and pixel positions.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vec2 {
    pub x: f32,
    pub y: f32,
}

impl Vec2 {
    pub const ZERO: Self = Self { x: 0.0, y: 0.0 };

    pub fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }
}

// ── Ray ──────────────────────────────────────────────────────

/// A ray in 3D space.
#[derive(Debug, Clone, Copy)]
pub struct Ray {
    pub origin: Vec3,
    pub direction: Vec3,
}

impl Ray {
    pub fn new(origin: Vec3, direction: Vec3) -> Self {
        Self {
            origin,
            direction: direction.normalize(),
        }
    }

    /// Evaluate the point at parameter t along the ray.
    pub fn at(&self, t: f32) -> Vec3 {
        Vec3::new(
            self.origin.x + self.direction.x * t,
            self.origin.y + self.direction.y * t,
            self.origin.z + self.direction.z * t,
        )
    }
}

// ── Gaze ray configuration ───────────────────────────────────

/// Configuration for the head-gaze ray (Xray pattern).
#[derive(Debug, Clone, Copy)]
pub struct GazeRayConfig {
    /// Offset from head pose to ray origin (meters, head-local space).
    /// Default: (0.15, -0.10, -0.05) — right-handed user.
    pub offset: Vec3,
    /// Maximum ray distance (meters).
    pub max_distance: f32,
    /// Whether the gaze ray is enabled.
    pub enabled: bool,
}

impl Default for GazeRayConfig {
    fn default() -> Self {
        Self {
            offset: Vec3::new(0.15, -0.10, -0.05),
            max_distance: 10.0,
            enabled: true,
        }
    }
}

impl GazeRayConfig {
    /// Left-handed offset variant.
    pub fn left_handed() -> Self {
        Self {
            offset: Vec3::new(-0.15, -0.10, -0.05),
            ..Default::default()
        }
    }
}

// ── Head pose ────────────────────────────────────────────────

/// Head pose from OpenXR (position + orientation).
#[derive(Debug, Clone, Copy)]
pub struct HeadPose {
    pub position: Vec3,
    pub rotation: Quat,
}

impl Default for HeadPose {
    fn default() -> Self {
        Self {
            position: Vec3::ZERO,
            rotation: Quat::IDENTITY,
        }
    }
}

// ── Ray hit ──────────────────────────────────────────────────

/// Result of a ray-surface intersection.
#[derive(Debug, Clone, Copy)]
pub struct RayHit {
    pub surface_id: u64,
    pub t: f32,
    pub uv: Vec2,
    pub world_point: Vec3,
}

// ── Ray-surface intersection ─────────────────────────────────

/// Compute the gaze ray from head pose and config.
pub fn compute_gaze_ray(head: &HeadPose, config: &GazeRayConfig) -> Ray {
    // Apply offset in head-local space
    let offset = quat_rotate(&head.rotation, &config.offset);
    let origin = Vec3::new(
        head.position.x + offset.x,
        head.position.y + offset.y,
        head.position.z + offset.z,
    );

    // Forward direction in head space is -Z (OpenXR convention)
    let forward = quat_rotate(&head.rotation, &Vec3::new(0.0, 0.0, -1.0));

    Ray::new(origin, forward)
}

/// Rotate a vector by a quaternion.
pub fn quat_rotate(q: &Quat, v: &Vec3) -> Vec3 {
    // q * v * q^-1, using the standard formula:
    // result = v + 2 * (q.w * cross(q.xyz, v) + cross(q.xyz, cross(q.xyz, v)))
    let qv = Vec3::new(q.x, q.y, q.z);
    let uv = cross(&qv, v);
    let uuv = cross(&qv, &uv);
    Vec3::new(
        v.x + (uv.x * q.w + uuv.x) * 2.0,
        v.y + (uv.y * q.w + uuv.y) * 2.0,
        v.z + (uv.z * q.w + uuv.z) * 2.0,
    )
}

fn dot(a: &Vec3, b: &Vec3) -> f32 {
    a.x * b.x + a.y * b.y + a.z * b.z
}

fn cross(a: &Vec3, b: &Vec3) -> Vec3 {
    Vec3::new(
        a.y * b.z - a.z * b.y,
        a.z * b.x - a.x * b.z,
        a.x * b.y - a.y * b.x,
    )
}

/// Ray-quad intersection. Returns (t, uv) if the ray hits the quad.
/// The quad is centered at origin in its local space, spanning
/// [-w/2, w/2] x [-h/2, h/2] in the XY plane (normal along +Z).
pub fn ray_quad_intersection(ray: &Ray, transform: &Transform3D, width: f32, height: f32) -> Option<(f32, Vec2)> {
    // Transform ray into surface-local space
    let model = Mat4::from_transform(transform);
    let inv_model = model.inverse()?;

    let local_origin = transform_point(&inv_model, &ray.origin);
    let local_end = transform_point(&inv_model, &ray.at(1.0));
    let local_dir = Vec3::new(
        local_end.x - local_origin.x,
        local_end.y - local_origin.y,
        local_end.z - local_origin.z,
    )
    .normalize();

    // Ray-plane intersection: plane is Z=0, normal is (0,0,1)
    if local_dir.z.abs() < 1e-8 {
        return None; // Ray parallel to plane
    }

    let t_local = -local_origin.z / local_dir.z;
    if t_local < 0.0 {
        return None; // Intersection behind ray
    }

    // Hit point in local space
    let hit_x = local_origin.x + local_dir.x * t_local;
    let hit_y = local_origin.y + local_dir.y * t_local;

    let hw = width * 0.5;
    let hh = height * 0.5;

    // Check bounds
    if hit_x < -hw || hit_x > hw || hit_y < -hh || hit_y > hh {
        return None;
    }

    // Convert to UV: (0,0) at top-left, (1,1) at bottom-right
    let u = (hit_x + hw) / width;
    let v = (hh - hit_y) / height; // Y-flip for screen coordinates

    // Compute world-space t by projecting hit point back
    let world_hit = transform_point(&model, &Vec3::new(hit_x, hit_y, 0.0));
    let world_t = Vec3::new(
        world_hit.x - ray.origin.x,
        world_hit.y - ray.origin.y,
        world_hit.z - ray.origin.z,
    )
    .length();

    Some((world_t, Vec2::new(u, v)))
}

/// Ray-cylinder intersection. Returns (t, uv) if the ray hits the cylinder.
/// Cylinder is centered at origin, opening toward -Z, arcing around Y axis.
pub fn ray_cylinder_intersection(
    ray: &Ray,
    transform: &Transform3D,
    radius: f32,
    arc_angle: f32,
    height: f32,
) -> Option<(f32, Vec2)> {
    let model = Mat4::from_transform(transform);
    let inv_model = model.inverse()?;

    let local_origin = transform_point(&inv_model, &ray.origin);
    let local_end = transform_point(&inv_model, &ray.at(1.0));
    let local_dir = Vec3::new(
        local_end.x - local_origin.x,
        local_end.y - local_origin.y,
        local_end.z - local_origin.z,
    )
    .normalize();

    // Solve ray-cylinder intersection in XZ plane.
    // Cylinder: x^2 + z^2 = r^2 (centered at origin, along Y axis)
    let a = local_dir.x * local_dir.x + local_dir.z * local_dir.z;
    let b = 2.0 * (local_origin.x * local_dir.x + local_origin.z * local_dir.z);
    let c = local_origin.x * local_origin.x + local_origin.z * local_origin.z - radius * radius;

    let discriminant = b * b - 4.0 * a * c;
    if discriminant < 0.0 {
        return None;
    }

    let sqrt_d = discriminant.sqrt();
    let t1 = (-b - sqrt_d) / (2.0 * a);
    let t2 = (-b + sqrt_d) / (2.0 * a);

    // Check both intersections (inside surface of cylinder = closer to viewer)
    let half_height = height * 0.5;
    let half_arc = arc_angle * 0.5;

    for &t_local in &[t1, t2] {
        if t_local < 0.0 {
            continue;
        }

        let hit = Vec3::new(
            local_origin.x + local_dir.x * t_local,
            local_origin.y + local_dir.y * t_local,
            local_origin.z + local_dir.z * t_local,
        );

        // Check height bounds
        if hit.y < -half_height || hit.y > half_height {
            continue;
        }

        // Check arc angle bounds
        let angle = hit.x.atan2(-hit.z); // angle from -Z axis in XZ plane
        if angle < -half_arc || angle > half_arc {
            continue;
        }

        // Compute UV
        let u = (angle + half_arc) / arc_angle;
        let v = (half_height - hit.y) / height;

        // World-space distance
        let world_hit = transform_point(&model, &hit);
        let world_t = Vec3::new(
            world_hit.x - ray.origin.x,
            world_hit.y - ray.origin.y,
            world_hit.z - ray.origin.z,
        )
        .length();

        return Some((world_t, Vec2::new(u, v)));
    }

    None
}

/// Transform a point by a 4x4 matrix.
fn transform_point(m: &Mat4, p: &Vec3) -> Vec3 {
    let d = &m.data;
    Vec3::new(
        d[0] * p.x + d[4] * p.y + d[8] * p.z + d[12],
        d[1] * p.x + d[5] * p.y + d[9] * p.z + d[13],
        d[2] * p.x + d[6] * p.y + d[10] * p.z + d[14],
    )
}

/// Find the closest ray-surface intersection across all scene nodes.
pub fn raycast_scene(ray: &Ray, scene: &VrScene) -> Option<RayHit> {
    let mut closest: Option<RayHit> = None;

    for (id, node) in &scene.nodes {
        if !node.visible {
            continue;
        }

        let hit = match &node.geometry {
            Geometry::Quad { width, height } => {
                ray_quad_intersection(ray, &node.transform, *width, *height)
            }
            Geometry::Cylinder {
                radius,
                arc_angle,
                height,
                ..
            } => ray_cylinder_intersection(ray, &node.transform, *radius, *arc_angle, *height),
            Geometry::Custom { .. } => None, // Custom meshes not raycasted
        };

        if let Some((t, uv)) = hit {
            let is_closer = closest.as_ref().map_or(true, |c| t < c.t);
            if is_closer {
                closest = Some(RayHit {
                    surface_id: node.surface_id.unwrap_or(*id),
                    t,
                    uv,
                    world_point: ray.at(t),
                });
            }
        }
    }

    closest
}

// ── UV to pixel conversion ───────────────────────────────────

/// Convert UV coordinates to surface pixel coordinates.
pub fn uv_to_pixel(uv: &Vec2, surface_width: u32, surface_height: u32) -> (i32, i32) {
    let px = (uv.x * surface_width as f32).round() as i32;
    let py = (uv.y * surface_height as f32).round() as i32;
    (px.clamp(0, surface_width as i32 - 1), py.clamp(0, surface_height as i32 - 1))
}

// ── Surface pointer state ────────────────────────────────────

/// Tracks pointer state for a single surface.
#[derive(Debug, Clone)]
pub struct SurfacePointerState {
    pub surface_id: u64,
    pub pixel_x: i32,
    pub pixel_y: i32,
    pub entered: bool,
}

// ── Click types ──────────────────────────────────────────────

/// VR click types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClickType {
    Left,
    Right,
    Middle,
    Double,
}

impl ClickType {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Left => "left",
            Self::Right => "right",
            Self::Middle => "middle",
            Self::Double => "double",
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "left" => Some(Self::Left),
            "right" => Some(Self::Right),
            "middle" => Some(Self::Middle),
            "double" => Some(Self::Double),
            _ => None,
        }
    }
}

// ── Head tilt scroll ─────────────────────────────────────────

/// Head-tilt scroll state.
#[derive(Debug, Clone)]
pub struct HeadTiltScroll {
    pub enabled: bool,
    /// Dead zone in degrees before scroll begins.
    pub deadzone_deg: f32,
    /// Scroll speed multiplier.
    pub speed: f32,
    /// Neutral pitch angle (set during calibration or on first use).
    pub neutral_pitch: f32,
}

impl Default for HeadTiltScroll {
    fn default() -> Self {
        Self {
            enabled: true,
            deadzone_deg: 5.0,
            speed: 3.0,
            neutral_pitch: 0.0,
        }
    }
}

impl HeadTiltScroll {
    /// Compute scroll delta from current head pitch.
    /// Returns vertical scroll value: negative = scroll down, positive = scroll up.
    pub fn compute_scroll(&self, current_pitch_deg: f32) -> f32 {
        if !self.enabled {
            return 0.0;
        }

        let delta = current_pitch_deg - self.neutral_pitch;

        if delta.abs() < self.deadzone_deg {
            return 0.0;
        }

        // Linear mapping from dead zone edge to scroll value
        let effective = if delta > 0.0 {
            delta - self.deadzone_deg
        } else {
            delta + self.deadzone_deg
        };

        // Clamp to max ~15 degrees of effective tilt
        let clamped = effective.clamp(-15.0, 15.0);

        // Scale: 1 line per degree at speed=1.0
        clamped * self.speed
    }
}

// ── Grab state ───────────────────────────────────────────────

/// State for an active surface grab (drag/move).
#[derive(Debug, Clone)]
pub struct GrabState {
    pub surface_id: u64,
    pub grab_offset: Vec3,
    pub initial_position: Vec3,
    pub snap_grid: Option<f32>,
}

impl GrabState {
    pub fn new(surface_id: u64, surface_pos: Vec3, hit_point: Vec3) -> Self {
        Self {
            surface_id,
            grab_offset: Vec3::new(
                surface_pos.x - hit_point.x,
                surface_pos.y - hit_point.y,
                surface_pos.z - hit_point.z,
            ),
            initial_position: surface_pos,
            snap_grid: None,
        }
    }

    /// Compute new position from current ray hit, applying constraints.
    pub fn compute_position(&self, ray_hit: &Vec3) -> Vec3 {
        let mut pos = Vec3::new(
            ray_hit.x + self.grab_offset.x,
            ray_hit.y + self.grab_offset.y,
            ray_hit.z + self.grab_offset.z,
        );

        // Constraint: cannot move behind camera (Z > 0 in head space)
        if pos.z > -0.1 {
            pos.z = -0.1;
        }

        // Snap to grid
        if let Some(grid) = self.snap_grid {
            pos.x = (pos.x / grid).round() * grid;
            pos.y = (pos.y / grid).round() * grid;
            pos.z = (pos.z / grid).round() * grid;
        }

        pos
    }
}

// ── Depth adjustment ─────────────────────────────────────────

/// Adjust depth (Z distance) for a surface.
pub fn adjust_depth(current_z: f32, delta: f32, min_distance: f32, max_distance: f32) -> f32 {
    // Surfaces are at negative Z (in front of camera)
    let new_z = current_z + delta;
    new_z.clamp(-max_distance, -min_distance)
}

/// Default depth constraints.
pub const DEPTH_MIN: f32 = 0.3;
pub const DEPTH_MAX: f32 = 5.0;
pub const DEPTH_STEP: f32 = 0.2;

// ── Follow mode ──────────────────────────────────────────────

/// Surface follow behavior.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FollowMode {
    /// No following: surface stays at fixed position.
    None,
    /// Lazy: re-enters FOV after 2 seconds out of view.
    Lazy,
    /// Sticky: always within FOV, immediate follow.
    Sticky,
    /// Locked: HUD-like, locked to head pose.
    Locked,
}

impl FollowMode {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::None => "none",
            Self::Lazy => "lazy",
            Self::Sticky => "sticky",
            Self::Locked => "locked",
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "none" => Some(Self::None),
            "lazy" => Some(Self::Lazy),
            "sticky" => Some(Self::Sticky),
            "locked" => Some(Self::Locked),
            _ => Option::None,
        }
    }
}

impl Default for FollowMode {
    fn default() -> Self {
        Self::None
    }
}

/// Per-surface follow state.
#[derive(Debug, Clone)]
pub struct FollowState {
    pub mode: FollowMode,
    /// How long the surface has been outside FOV (seconds).
    pub out_of_fov_time: f32,
    /// Lerp factor for smooth follow animation.
    pub lerp_rate: f32,
}

impl Default for FollowState {
    fn default() -> Self {
        Self {
            mode: FollowMode::None,
            out_of_fov_time: 0.0,
            lerp_rate: 0.05,
        }
    }
}

impl FollowState {
    /// Check if surface should re-enter FOV.
    pub fn should_follow(&self) -> bool {
        match self.mode {
            FollowMode::None => false,
            FollowMode::Lazy => self.out_of_fov_time > 2.0,
            FollowMode::Sticky => true,
            FollowMode::Locked => true,
        }
    }

    /// Compute target position to bring surface into FOV.
    pub fn compute_follow_target(
        &self,
        head: &HeadPose,
        current_pos: &Vec3,
        distance: f32,
    ) -> Vec3 {
        match self.mode {
            FollowMode::Locked => {
                // Position relative to head
                let offset = quat_rotate(&head.rotation, &Vec3::new(0.0, 0.0, -distance));
                Vec3::new(
                    head.position.x + offset.x,
                    head.position.y + offset.y,
                    head.position.z + offset.z,
                )
            }
            FollowMode::Sticky | FollowMode::Lazy => {
                // Move toward the nearest FOV edge
                let forward = quat_rotate(&head.rotation, &Vec3::new(0.0, 0.0, -1.0));
                let target = Vec3::new(
                    head.position.x + forward.x * distance,
                    head.position.y + forward.y * distance,
                    head.position.z + forward.z * distance,
                );
                // Lerp from current to target
                current_pos.lerp(target, self.lerp_rate)
            }
            FollowMode::None => *current_pos,
        }
    }
}

// ── Pointer visualization ────────────────────────────────────

/// Ray color states for visualization.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RayColorState {
    /// White: idle/hovering, no surface under ray.
    Idle,
    /// Green: surface under ray.
    Hovering,
    /// Blue: grabbing/moving surface.
    Grabbing,
    /// Red: no surface hit (endpoint only).
    NoTarget,
}

impl RayColorState {
    pub fn color(&self) -> [f32; 4] {
        match self {
            Self::Idle => [1.0, 1.0, 1.0, 0.6],
            Self::Hovering => [0.2, 1.0, 0.2, 0.8],
            Self::Grabbing => [0.2, 0.2, 1.0, 0.8],
            Self::NoTarget => [1.0, 0.2, 0.2, 0.4],
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Idle => "idle",
            Self::Hovering => "hovering",
            Self::Grabbing => "grabbing",
            Self::NoTarget => "no-target",
        }
    }
}

// ── Calibration ──────────────────────────────────────────────

/// Calibration point for gaze ray offset optimization.
#[derive(Debug, Clone)]
pub struct CalibrationPoint {
    /// Known target position in world space.
    pub target: Vec3,
    /// Head pose when user confirmed alignment.
    pub head_pose: HeadPose,
}

/// Calibration state for the 5-point gaze calibration.
#[derive(Debug, Clone)]
pub struct CalibrationState {
    pub points: Vec<CalibrationPoint>,
    pub active: bool,
    pub current_target: usize,
    /// Target positions for the 5-point calibration.
    pub targets: Vec<Vec3>,
}

impl Default for CalibrationState {
    fn default() -> Self {
        // 5 targets at 2m distance: center, top-left, top-right, bottom-left, bottom-right
        let d = -2.0;
        let spread = 0.6; // meters
        Self {
            points: Vec::new(),
            active: false,
            current_target: 0,
            targets: vec![
                Vec3::new(0.0, 0.0, d),            // center
                Vec3::new(-spread, spread, d),       // top-left
                Vec3::new(spread, spread, d),        // top-right
                Vec3::new(-spread, -spread, d),      // bottom-left
                Vec3::new(spread, -spread, d),       // bottom-right
            ],
        }
    }
}

impl CalibrationState {
    /// Record a calibration point.
    pub fn record_point(&mut self, head_pose: HeadPose) -> bool {
        if self.current_target >= self.targets.len() {
            return false;
        }

        self.points.push(CalibrationPoint {
            target: self.targets[self.current_target],
            head_pose,
        });
        self.current_target += 1;

        self.current_target >= self.targets.len()
    }

    /// Compute optimal ray offset from calibration data.
    /// Minimizes angular error between ray-target direction and head-forward.
    pub fn compute_offset(&self) -> Option<Vec3> {
        if self.points.len() < 3 {
            return None;
        }

        // For each point, compute the offset that makes the ray from
        // (head_pos + head_rot * offset) in direction (head_rot * -Z)
        // pass closest to the target.
        // Simple approach: average the required offsets.
        let mut sum = Vec3::ZERO;
        let count = self.points.len() as f32;

        for pt in &self.points {
            let forward = quat_rotate(&pt.head_pose.rotation, &Vec3::new(0.0, 0.0, -1.0));
            // Vector from head to target
            let to_target = Vec3::new(
                pt.target.x - pt.head_pose.position.x,
                pt.target.y - pt.head_pose.position.y,
                pt.target.z - pt.head_pose.position.z,
            );

            // Project to_target onto the plane perpendicular to forward
            let along = dot(&to_target, &forward);
            let perp = Vec3::new(
                to_target.x - forward.x * along,
                to_target.y - forward.y * along,
                to_target.z - forward.z * along,
            );

            // Transform back to head-local space
            let inv_rot = Quat {
                x: -pt.head_pose.rotation.x,
                y: -pt.head_pose.rotation.y,
                z: -pt.head_pose.rotation.z,
                w: pt.head_pose.rotation.w,
            };
            let local_offset = quat_rotate(&inv_rot, &perp);
            sum.x += local_offset.x;
            sum.y += local_offset.y;
            sum.z += local_offset.z;
        }

        Some(Vec3::new(sum.x / count, sum.y / count, sum.z / count))
    }

    /// Compute RMS error in degrees for quality assessment.
    pub fn rms_error_deg(&self, offset: &Vec3) -> f32 {
        if self.points.is_empty() {
            return f32::MAX;
        }

        let mut sum_sq = 0.0;
        for pt in &self.points {
            let ray_origin = Vec3::new(
                pt.head_pose.position.x + quat_rotate(&pt.head_pose.rotation, offset).x,
                pt.head_pose.position.y + quat_rotate(&pt.head_pose.rotation, offset).y,
                pt.head_pose.position.z + quat_rotate(&pt.head_pose.rotation, offset).z,
            );
            let forward = quat_rotate(&pt.head_pose.rotation, &Vec3::new(0.0, 0.0, -1.0));
            let to_target = Vec3::new(
                pt.target.x - ray_origin.x,
                pt.target.y - ray_origin.y,
                pt.target.z - ray_origin.z,
            )
            .normalize();

            let cos_angle = dot(&forward, &to_target).clamp(-1.0, 1.0);
            let angle_deg = cos_angle.acos().to_degrees();
            sum_sq += angle_deg * angle_deg;
        }

        (sum_sq / self.points.len() as f32).sqrt()
    }
}

// ── VR Interaction Manager ───────────────────────────────────

/// Central VR interaction state.
pub struct VrInteraction {
    pub gaze_config: GazeRayConfig,
    pub head_pose: HeadPose,
    pub current_ray: Option<Ray>,
    pub current_hit: Option<RayHit>,
    pub active_pointer: Option<SurfacePointerState>,
    pub previous_target: Option<u64>,
    pub grab: Option<GrabState>,
    pub follow_states: HashMap<u64, FollowState>,
    pub head_scroll: HeadTiltScroll,
    pub calibration: CalibrationState,
    pub ray_color: RayColorState,
    pub cursor_scale: f32,
    pub show_ray: bool,
    pub show_hit_point: bool,
}

impl Default for VrInteraction {
    fn default() -> Self {
        Self {
            gaze_config: GazeRayConfig::default(),
            head_pose: HeadPose::default(),
            current_ray: None,
            current_hit: None,
            active_pointer: None,
            previous_target: None,
            grab: None,
            follow_states: HashMap::new(),
            head_scroll: HeadTiltScroll::default(),
            calibration: CalibrationState::default(),
            ray_color: RayColorState::NoTarget,
            cursor_scale: 3.0,
            show_ray: true,
            show_hit_point: true,
        }
    }
}

impl VrInteraction {
    pub fn new() -> Self {
        info!("VR interaction manager initialized");
        Self::default()
    }

    /// Update interaction state for one frame.
    pub fn update(&mut self, head_pose: HeadPose, scene: &VrScene) {
        self.head_pose = head_pose;

        if !self.gaze_config.enabled {
            self.current_ray = None;
            self.current_hit = None;
            return;
        }

        // Compute gaze ray
        let ray = compute_gaze_ray(&head_pose, &self.gaze_config);
        self.current_ray = Some(ray);

        // Raycast scene
        let hit = raycast_scene(&ray, scene);
        let prev_target = self.current_hit.map(|h| h.surface_id);

        // Update pointer state
        match (&hit, &self.active_pointer) {
            (Some(h), Some(ptr)) if ptr.surface_id == h.surface_id => {
                // Motion on same surface
                let node = scene.nodes.get(&h.surface_id);
                let (px, py) = node
                    .map(|n| uv_to_pixel(&h.uv, n.surface_width, n.surface_height))
                    .unwrap_or((0, 0));
                self.active_pointer = Some(SurfacePointerState {
                    surface_id: h.surface_id,
                    pixel_x: px,
                    pixel_y: py,
                    entered: true,
                });
                self.ray_color = if self.grab.is_some() {
                    RayColorState::Grabbing
                } else {
                    RayColorState::Hovering
                };
            }
            (Some(h), _) => {
                // Enter new surface
                let node = scene.nodes.get(&h.surface_id);
                let (px, py) = node
                    .map(|n| uv_to_pixel(&h.uv, n.surface_width, n.surface_height))
                    .unwrap_or((0, 0));
                self.active_pointer = Some(SurfacePointerState {
                    surface_id: h.surface_id,
                    pixel_x: px,
                    pixel_y: py,
                    entered: true,
                });
                self.ray_color = RayColorState::Hovering;
            }
            (None, _) => {
                // Left all surfaces
                self.active_pointer = None;
                self.ray_color = RayColorState::NoTarget;
            }
        }

        self.previous_target = prev_target;
        self.current_hit = hit;
    }

    /// Start grabbing the currently targeted surface.
    pub fn start_grab(&mut self, scene: &VrScene) -> Option<u64> {
        let hit = self.current_hit?;
        let node = scene.nodes.get(&hit.surface_id)?;
        let grab = GrabState::new(
            hit.surface_id,
            node.transform.position,
            hit.world_point,
        );
        let sid = grab.surface_id;
        self.grab = Some(grab);
        self.ray_color = RayColorState::Grabbing;
        info!("VR grab started: surface {}", sid);
        Some(sid)
    }

    /// End the current grab, returning (surface_id, new_position).
    pub fn end_grab(&mut self) -> Option<(u64, Vec3)> {
        let grab = self.grab.take()?;
        let pos = self.current_hit
            .map(|h| grab.compute_position(&h.world_point))
            .unwrap_or(grab.initial_position);
        self.ray_color = if self.current_hit.is_some() {
            RayColorState::Hovering
        } else {
            RayColorState::NoTarget
        };
        info!("VR grab ended: surface {} at {:?}", grab.surface_id, pos);
        Some((grab.surface_id, pos))
    }

    /// Set follow mode for a surface.
    pub fn set_follow_mode(&mut self, surface_id: u64, mode: FollowMode) {
        let state = self.follow_states.entry(surface_id).or_default();
        state.mode = mode;
        info!("VR follow mode: surface {} -> {:?}", surface_id, mode);
    }

    /// Generate IPC s-expression for current pointer state.
    pub fn pointer_sexp(&self) -> String {
        match &self.active_pointer {
            Some(ptr) => {
                let distance = self.current_hit.map(|h| h.t).unwrap_or(0.0);
                format!(
                    "(:surface-id {} :x {} :y {} :distance {:.2} :ray :{})",
                    ptr.surface_id, ptr.pixel_x, ptr.pixel_y, distance,
                    self.ray_color.as_str()
                )
            }
            None => "(:surface-id nil :ray :no-target)".to_string(),
        }
    }

    /// Generate IPC s-expression for a focus change event.
    pub fn focus_change_sexp(&self) -> Option<String> {
        let current = self.current_hit.map(|h| h.surface_id);
        if current != self.previous_target {
            let curr_str = current
                .map(|id| id.to_string())
                .unwrap_or_else(|| "nil".to_string());
            let prev_str = self.previous_target
                .map(|id| id.to_string())
                .unwrap_or_else(|| "nil".to_string());
            Some(format!(
                "(:type :event :event :vr-focus-changed :surface-id {} :prev-surface-id {})",
                curr_str, prev_str
            ))
        } else {
            None
        }
    }
}

// ── Tests ────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quat_rotate_identity() {
        let v = Vec3::new(1.0, 2.0, 3.0);
        let result = quat_rotate(&Quat::IDENTITY, &v);
        assert!((result.x - v.x).abs() < 1e-5);
        assert!((result.y - v.y).abs() < 1e-5);
        assert!((result.z - v.z).abs() < 1e-5);
    }

    #[test]
    fn test_quat_rotate_90_yaw() {
        // 90 degrees around Y axis: +X -> -Z
        let q = Quat::from_euler(std::f32::consts::FRAC_PI_2, 0.0, 0.0);
        let v = Vec3::new(1.0, 0.0, 0.0);
        let result = quat_rotate(&q, &v);
        assert!((result.x).abs() < 1e-4, "x={}", result.x);
        assert!((result.y).abs() < 1e-4, "y={}", result.y);
        assert!((result.z + 1.0).abs() < 1e-4, "z={}", result.z);
    }

    #[test]
    fn test_ray_quad_intersection_direct_hit() {
        // Ray from (0, 0, 2) pointing at -Z, quad at origin 2m x 2m
        let ray = Ray::new(Vec3::new(0.0, 0.0, 2.0), Vec3::new(0.0, 0.0, -1.0));
        let transform = Transform3D::default();
        let result = ray_quad_intersection(&ray, &transform, 2.0, 2.0);

        assert!(result.is_some(), "Should hit the quad");
        let (t, uv) = result.unwrap();
        assert!((t - 2.0).abs() < 0.1, "t={}", t);
        assert!((uv.x - 0.5).abs() < 0.01, "u={}", uv.x);
        assert!((uv.y - 0.5).abs() < 0.01, "v={}", uv.y);
    }

    #[test]
    fn test_ray_quad_intersection_off_center() {
        // Ray from (0.5, 0.5, 2) pointing at -Z, quad at origin 2m x 2m
        let ray = Ray::new(Vec3::new(0.5, 0.5, 2.0), Vec3::new(0.0, 0.0, -1.0));
        let transform = Transform3D::default();
        let result = ray_quad_intersection(&ray, &transform, 2.0, 2.0);

        assert!(result.is_some(), "Should hit the quad");
        let (_t, uv) = result.unwrap();
        assert!((uv.x - 0.75).abs() < 0.01, "u={}", uv.x);
        assert!((uv.y - 0.25).abs() < 0.01, "v={}", uv.y);
    }

    #[test]
    fn test_ray_quad_intersection_miss() {
        // Ray pointing away from quad
        let ray = Ray::new(Vec3::new(0.0, 0.0, 2.0), Vec3::new(0.0, 0.0, 1.0));
        let transform = Transform3D::default();
        let result = ray_quad_intersection(&ray, &transform, 2.0, 2.0);
        assert!(result.is_none(), "Should miss the quad");
    }

    #[test]
    fn test_ray_quad_intersection_miss_outside_bounds() {
        // Ray hits the plane but outside quad bounds
        let ray = Ray::new(Vec3::new(5.0, 0.0, 2.0), Vec3::new(0.0, 0.0, -1.0));
        let transform = Transform3D::default();
        let result = ray_quad_intersection(&ray, &transform, 2.0, 2.0);
        assert!(result.is_none(), "Should miss outside bounds");
    }

    #[test]
    fn test_ray_cylinder_intersection() {
        // Ray from (0, 0, 0) pointing at -Z, cylinder at Z=-2 with radius 2
        let ray = Ray::new(Vec3::new(0.0, 0.0, 0.0), Vec3::new(0.0, 0.0, -1.0));
        let transform = Transform3D::default();
        let result = ray_cylinder_intersection(
            &ray,
            &transform,
            2.0,              // radius
            std::f32::consts::PI, // arc angle (180 degrees)
            2.0,              // height
        );

        assert!(result.is_some(), "Should hit the cylinder");
        let (t, uv) = result.unwrap();
        assert!(t > 0.0, "t should be positive: {}", t);
        // UV.x should be ~0.5 (center of arc)
        assert!((uv.x - 0.5).abs() < 0.1, "u={}", uv.x);
    }

    #[test]
    fn test_closest_intersection() {
        let mut scene = VrScene::new();
        // Near surface at z=-1
        scene.add_surface(1, 1000, 1000);
        scene.nodes.get_mut(&1).unwrap().transform = Transform3D::at(0.0, 0.0, -1.0);
        // Far surface at z=-3
        scene.add_surface(2, 1000, 1000);
        scene.nodes.get_mut(&2).unwrap().transform = Transform3D::at(0.0, 0.0, -3.0);

        let ray = Ray::new(Vec3::new(0.0, 0.0, 0.0), Vec3::new(0.0, 0.0, -1.0));
        let hit = raycast_scene(&ray, &scene);

        assert!(hit.is_some(), "Should hit something");
        assert_eq!(hit.unwrap().surface_id, 1, "Should hit the closer surface");
    }

    #[test]
    fn test_uv_to_pixel_center() {
        let (px, py) = uv_to_pixel(&Vec2::new(0.5, 0.5), 1920, 1080);
        assert_eq!(px, 960);
        assert_eq!(py, 540);
    }

    #[test]
    fn test_uv_to_pixel_corners() {
        let (px, py) = uv_to_pixel(&Vec2::new(0.0, 0.0), 1920, 1080);
        assert_eq!(px, 0);
        assert_eq!(py, 0);

        let (px, py) = uv_to_pixel(&Vec2::new(1.0, 1.0), 1920, 1080);
        assert_eq!(px, 1919);
        assert_eq!(py, 1079);
    }

    #[test]
    fn test_head_tilt_scroll_deadzone() {
        let scroll = HeadTiltScroll::default();
        // Within dead zone
        assert_eq!(scroll.compute_scroll(3.0), 0.0);
        assert_eq!(scroll.compute_scroll(-3.0), 0.0);
    }

    #[test]
    fn test_head_tilt_scroll_active() {
        let scroll = HeadTiltScroll::default();
        // Beyond dead zone (10 degrees down)
        let val = scroll.compute_scroll(-10.0);
        assert!(val < 0.0, "Should scroll down: {}", val);

        // Beyond dead zone (10 degrees up)
        let val = scroll.compute_scroll(10.0);
        assert!(val > 0.0, "Should scroll up: {}", val);
    }

    #[test]
    fn test_head_tilt_scroll_disabled() {
        let mut scroll = HeadTiltScroll::default();
        scroll.enabled = false;
        assert_eq!(scroll.compute_scroll(20.0), 0.0);
    }

    #[test]
    fn test_grab_constraint_no_behind_camera() {
        let grab = GrabState::new(1, Vec3::new(0.0, 0.0, -2.0), Vec3::new(0.0, 0.0, -2.0));
        // Try to move behind camera
        let pos = grab.compute_position(&Vec3::new(0.0, 0.0, 1.0));
        assert!(pos.z <= -0.1, "Should not move behind camera: z={}", pos.z);
    }

    #[test]
    fn test_depth_clamp() {
        // Pull too close
        let z = adjust_depth(-0.5, 0.3, DEPTH_MIN, DEPTH_MAX);
        assert!((z - (-DEPTH_MIN)).abs() < 1e-5, "z={}", z);

        // Push too far
        let z = adjust_depth(-4.8, -0.5, DEPTH_MIN, DEPTH_MAX);
        assert!((z - (-DEPTH_MAX)).abs() < 1e-5, "z={}", z);

        // Normal range
        let z = adjust_depth(-2.0, -0.2, DEPTH_MIN, DEPTH_MAX);
        assert!((z - (-2.2)).abs() < 1e-5, "z={}", z);
    }

    #[test]
    fn test_follow_mode_from_str() {
        assert_eq!(FollowMode::from_str("none"), Some(FollowMode::None));
        assert_eq!(FollowMode::from_str("lazy"), Some(FollowMode::Lazy));
        assert_eq!(FollowMode::from_str("sticky"), Some(FollowMode::Sticky));
        assert_eq!(FollowMode::from_str("locked"), Some(FollowMode::Locked));
        assert_eq!(FollowMode::from_str("invalid"), Option::None);
    }

    #[test]
    fn test_click_type_from_str() {
        assert_eq!(ClickType::from_str("left"), Some(ClickType::Left));
        assert_eq!(ClickType::from_str("right"), Some(ClickType::Right));
        assert_eq!(ClickType::from_str("middle"), Some(ClickType::Middle));
        assert_eq!(ClickType::from_str("double"), Some(ClickType::Double));
        assert_eq!(ClickType::from_str("invalid"), None);
    }

    #[test]
    fn test_gaze_ray_default_offset() {
        let config = GazeRayConfig::default();
        assert!((config.offset.x - 0.15).abs() < 1e-5);
        assert!((config.offset.y - (-0.10)).abs() < 1e-5);
        assert!((config.offset.z - (-0.05)).abs() < 1e-5);
    }

    #[test]
    fn test_gaze_ray_left_handed() {
        let config = GazeRayConfig::left_handed();
        assert!((config.offset.x - (-0.15)).abs() < 1e-5);
    }

    #[test]
    fn test_compute_gaze_ray() {
        let head = HeadPose::default();
        let config = GazeRayConfig::default();
        let ray = compute_gaze_ray(&head, &config);

        // With identity rotation, ray should point along -Z
        assert!((ray.direction.z + 1.0).abs() < 1e-4);
        // Origin offset should be applied
        assert!((ray.origin.x - 0.15).abs() < 1e-4);
    }

    #[test]
    fn test_follow_state_lazy_timing() {
        let mut state = FollowState::default();
        state.mode = FollowMode::Lazy;

        state.out_of_fov_time = 1.0;
        assert!(!state.should_follow(), "Should not follow at 1s");

        state.out_of_fov_time = 2.5;
        assert!(state.should_follow(), "Should follow at 2.5s");
    }

    #[test]
    fn test_calibration_needs_points() {
        let cal = CalibrationState::default();
        assert!(cal.compute_offset().is_none());
    }
}
