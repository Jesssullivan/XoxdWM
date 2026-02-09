//! VR scene graph — 3D scene management for Wayland surfaces in VR.
//!
//! Core types:
//! - `VrScene`: root container holding all scene nodes
//! - `SceneNode`: a positioned element in 3D space (surface quad, cylinder, etc.)
//! - `Transform3D`: position + rotation + scale
//! - `Geometry`: mesh type (Quad, Cylinder, Custom)
//! - `VrLayout`: automatic surface arrangement (Arc, Grid, Stack, Freeform)

use std::collections::HashMap;
use std::f32::consts::PI;
use tracing::{debug, info};

// ── Math types ───────────────────────────────────────────────

/// 3D vector.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vec3 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

impl Vec3 {
    pub const ZERO: Self = Self {
        x: 0.0,
        y: 0.0,
        z: 0.0,
    };
    pub const ONE: Self = Self {
        x: 1.0,
        y: 1.0,
        z: 1.0,
    };

    pub fn new(x: f32, y: f32, z: f32) -> Self {
        Self { x, y, z }
    }

    pub fn lerp(self, other: Self, t: f32) -> Self {
        Self {
            x: self.x + (other.x - self.x) * t,
            y: self.y + (other.y - self.y) * t,
            z: self.z + (other.z - self.z) * t,
        }
    }

    pub fn length(self) -> f32 {
        (self.x * self.x + self.y * self.y + self.z * self.z).sqrt()
    }

    pub fn normalize(self) -> Self {
        let len = self.length();
        if len < 1e-10 {
            return Self::ZERO;
        }
        Self {
            x: self.x / len,
            y: self.y / len,
            z: self.z / len,
        }
    }
}

/// Quaternion for rotations.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Quat {
    pub x: f32,
    pub y: f32,
    pub z: f32,
    pub w: f32,
}

impl Quat {
    pub const IDENTITY: Self = Self {
        x: 0.0,
        y: 0.0,
        z: 0.0,
        w: 1.0,
    };

    /// Create quaternion from Euler angles (yaw, pitch, roll) in radians.
    pub fn from_euler(yaw: f32, pitch: f32, roll: f32) -> Self {
        let (sy, cy) = (yaw * 0.5).sin_cos();
        let (sp, cp) = (pitch * 0.5).sin_cos();
        let (sr, cr) = (roll * 0.5).sin_cos();

        Self {
            x: cr * sp * cy + sr * cp * sy,
            y: cr * cp * sy - sr * sp * cy,
            z: sr * cp * cy - cr * sp * sy,
            w: cr * cp * cy + sr * sp * sy,
        }
    }

    /// Spherical linear interpolation.
    pub fn slerp(self, other: Self, t: f32) -> Self {
        let mut dot = self.x * other.x + self.y * other.y + self.z * other.z + self.w * other.w;

        let other = if dot < 0.0 {
            dot = -dot;
            Self {
                x: -other.x,
                y: -other.y,
                z: -other.z,
                w: -other.w,
            }
        } else {
            other
        };

        if dot > 0.9995 {
            // Linear interpolation for very close quaternions
            return Self {
                x: self.x + (other.x - self.x) * t,
                y: self.y + (other.y - self.y) * t,
                z: self.z + (other.z - self.z) * t,
                w: self.w + (other.w - self.w) * t,
            };
        }

        let theta = dot.acos();
        let sin_theta = theta.sin();
        let a = ((1.0 - t) * theta).sin() / sin_theta;
        let b = (t * theta).sin() / sin_theta;

        Self {
            x: self.x * a + other.x * b,
            y: self.y * a + other.y * b,
            z: self.z * a + other.z * b,
            w: self.w * a + other.w * b,
        }
    }
}

/// 4x4 matrix (column-major, OpenGL convention).
#[derive(Debug, Clone, Copy)]
pub struct Mat4 {
    pub data: [f32; 16],
}

impl Mat4 {
    pub const IDENTITY: Self = Self {
        data: [
            1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
        ],
    };

    /// Create translation matrix.
    pub fn translation(x: f32, y: f32, z: f32) -> Self {
        let mut m = Self::IDENTITY;
        m.data[12] = x;
        m.data[13] = y;
        m.data[14] = z;
        m
    }

    /// Create scale matrix.
    pub fn scale(x: f32, y: f32, z: f32) -> Self {
        let mut m = Self::IDENTITY;
        m.data[0] = x;
        m.data[5] = y;
        m.data[10] = z;
        m
    }

    /// Create rotation matrix from quaternion.
    pub fn from_quat(q: &Quat) -> Self {
        let (x, y, z, w) = (q.x, q.y, q.z, q.w);
        let x2 = x + x;
        let y2 = y + y;
        let z2 = z + z;
        let xx = x * x2;
        let xy = x * y2;
        let xz = x * z2;
        let yy = y * y2;
        let yz = y * z2;
        let zz = z * z2;
        let wx = w * x2;
        let wy = w * y2;
        let wz = w * z2;

        Self {
            data: [
                1.0 - (yy + zz),
                xy + wz,
                xz - wy,
                0.0,
                xy - wz,
                1.0 - (xx + zz),
                yz + wx,
                0.0,
                xz + wy,
                yz - wx,
                1.0 - (xx + yy),
                0.0,
                0.0,
                0.0,
                0.0,
                1.0,
            ],
        }
    }

    /// Multiply two matrices.
    pub fn mul(&self, other: &Self) -> Self {
        let mut out = [0.0f32; 16];
        for col in 0..4 {
            for row in 0..4 {
                let mut sum = 0.0;
                for k in 0..4 {
                    sum += self.data[k * 4 + row] * other.data[col * 4 + k];
                }
                out[col * 4 + row] = sum;
            }
        }
        Self { data: out }
    }

    /// Create asymmetric perspective projection from FOV tangent angles.
    /// Used for OpenXR per-eye projection matrices.
    pub fn perspective_fov(
        left_tan: f32,
        right_tan: f32,
        up_tan: f32,
        down_tan: f32,
        near: f32,
        far: f32,
    ) -> Self {
        let left = -left_tan * near;
        let right = right_tan * near;
        let bottom = -down_tan * near;
        let top = up_tan * near;

        let w = right - left;
        let h = top - bottom;
        let d = far - near;

        Self {
            data: [
                2.0 * near / w,
                0.0,
                0.0,
                0.0,
                0.0,
                2.0 * near / h,
                0.0,
                0.0,
                (right + left) / w,
                (top + bottom) / h,
                -(far + near) / d,
                -1.0,
                0.0,
                0.0,
                -2.0 * far * near / d,
                0.0,
            ],
        }
    }

    /// Compute inverse (for view matrix from pose).
    pub fn inverse(&self) -> Option<Self> {
        // For rigid transforms (rotation + translation), use transpose of rotation
        // block and negate the translation. Full inverse is complex; use
        // simplified form for view matrices.
        let mut inv = [0.0f32; 16];

        // Transpose 3x3 rotation block
        inv[0] = self.data[0];
        inv[1] = self.data[4];
        inv[2] = self.data[8];
        inv[4] = self.data[1];
        inv[5] = self.data[5];
        inv[6] = self.data[9];
        inv[8] = self.data[2];
        inv[9] = self.data[6];
        inv[10] = self.data[10];

        // Negate translation, rotated by transposed rotation
        let tx = self.data[12];
        let ty = self.data[13];
        let tz = self.data[14];
        inv[12] = -(inv[0] * tx + inv[4] * ty + inv[8] * tz);
        inv[13] = -(inv[1] * tx + inv[5] * ty + inv[9] * tz);
        inv[14] = -(inv[2] * tx + inv[6] * ty + inv[10] * tz);

        inv[3] = 0.0;
        inv[7] = 0.0;
        inv[11] = 0.0;
        inv[15] = 1.0;

        Some(Self { data: inv })
    }

    /// Build model matrix from Transform3D.
    pub fn from_transform(t: &Transform3D) -> Self {
        let trans = Self::translation(t.position.x, t.position.y, t.position.z);
        let rot = Self::from_quat(&t.rotation);
        let scl = Self::scale(t.scale.x, t.scale.y, t.scale.z);
        trans.mul(&rot).mul(&scl)
    }
}

// ── Transform ────────────────────────────────────────────────

/// 3D transform: position, rotation, scale.
#[derive(Debug, Clone, Copy)]
pub struct Transform3D {
    pub position: Vec3,
    pub rotation: Quat,
    pub scale: Vec3,
}

impl Default for Transform3D {
    fn default() -> Self {
        Self {
            position: Vec3::ZERO,
            rotation: Quat::IDENTITY,
            scale: Vec3::ONE,
        }
    }
}

impl Transform3D {
    pub fn at(x: f32, y: f32, z: f32) -> Self {
        Self {
            position: Vec3::new(x, y, z),
            ..Default::default()
        }
    }

    /// Interpolate between two transforms.
    pub fn lerp(&self, other: &Self, t: f32) -> Self {
        Self {
            position: self.position.lerp(other.position, t),
            rotation: self.rotation.slerp(other.rotation, t),
            scale: self.scale.lerp(other.scale, t),
        }
    }
}

// ── Vertex and Geometry ──────────────────────────────────────

/// Vertex for VR mesh rendering.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Vertex {
    pub position: [f32; 3],
    pub texcoord: [f32; 2],
    pub normal: [f32; 3],
}

/// Mesh geometry type.
#[derive(Debug, Clone)]
pub enum Geometry {
    /// Flat rectangular quad.
    Quad { width: f32, height: f32 },
    /// Cylindrical projection for curved monitors.
    Cylinder {
        radius: f32,
        arc_angle: f32, // radians
        height: f32,
        segments: u32,
    },
    /// Custom mesh (pre-generated vertices and indices).
    Custom {
        vertices: Vec<Vertex>,
        indices: Vec<u16>,
    },
}

impl Geometry {
    /// Generate vertices and indices for this geometry.
    pub fn generate_mesh(&self) -> (Vec<Vertex>, Vec<u16>) {
        match self {
            Geometry::Quad { width, height } => generate_quad_mesh(*width, *height),
            Geometry::Cylinder {
                radius,
                arc_angle,
                height,
                segments,
            } => generate_cylinder_mesh(*radius, *arc_angle, *height, *segments),
            Geometry::Custom { vertices, indices } => (vertices.clone(), indices.clone()),
        }
    }
}

/// Generate a flat quad mesh centered at origin.
pub fn generate_quad_mesh(width: f32, height: f32) -> (Vec<Vertex>, Vec<u16>) {
    let hw = width * 0.5;
    let hh = height * 0.5;

    let vertices = vec![
        Vertex {
            position: [-hw, hh, 0.0],
            texcoord: [0.0, 0.0],
            normal: [0.0, 0.0, 1.0],
        }, // top-left
        Vertex {
            position: [hw, hh, 0.0],
            texcoord: [1.0, 0.0],
            normal: [0.0, 0.0, 1.0],
        }, // top-right
        Vertex {
            position: [hw, -hh, 0.0],
            texcoord: [1.0, 1.0],
            normal: [0.0, 0.0, 1.0],
        }, // bottom-right
        Vertex {
            position: [-hw, -hh, 0.0],
            texcoord: [0.0, 1.0],
            normal: [0.0, 0.0, 1.0],
        }, // bottom-left
    ];

    let indices = vec![0, 1, 2, 0, 2, 3];

    (vertices, indices)
}

/// Generate a cylindrical mesh for curved virtual monitors.
/// The cylinder is centered at origin, opening toward -Z (viewer faces -Z).
/// Arc goes from -arc_angle/2 to +arc_angle/2 around Y axis.
pub fn generate_cylinder_mesh(
    radius: f32,
    arc_angle: f32,
    height: f32,
    segments: u32,
) -> (Vec<Vertex>, Vec<u16>) {
    let segments = segments.max(2);
    let half_height = height * 0.5;
    let half_arc = arc_angle * 0.5;

    let mut vertices = Vec::with_capacity(((segments + 1) * 2) as usize);
    let mut indices = Vec::with_capacity((segments * 6) as usize);

    for i in 0..=segments {
        let t = i as f32 / segments as f32;
        let angle = -half_arc + t * arc_angle;

        let x = radius * angle.sin();
        let z = -radius * angle.cos();
        // Normal points inward (toward viewer at origin)
        let nx = -angle.sin();
        let nz = angle.cos();

        // Top vertex
        vertices.push(Vertex {
            position: [x, half_height, z],
            texcoord: [t, 0.0],
            normal: [nx, 0.0, nz],
        });

        // Bottom vertex
        vertices.push(Vertex {
            position: [x, -half_height, z],
            texcoord: [t, 1.0],
            normal: [nx, 0.0, nz],
        });
    }

    // Generate triangle indices
    for i in 0..segments {
        let tl = i * 2; // top-left
        let bl = i * 2 + 1; // bottom-left
        let tr = (i + 1) * 2; // top-right
        let br = (i + 1) * 2 + 1; // bottom-right

        indices.push(tl as u16);
        indices.push(tr as u16);
        indices.push(bl as u16);

        indices.push(bl as u16);
        indices.push(tr as u16);
        indices.push(br as u16);
    }

    (vertices, indices)
}

// ── Projection type ──────────────────────────────────────────

/// Surface projection type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProjectionType {
    Flat,
    Cylinder,
}

// ── Scene Node ───────────────────────────────────────────────

/// A node in the VR scene graph.
#[derive(Debug)]
pub struct SceneNode {
    pub surface_id: Option<u64>,
    pub transform: Transform3D,
    pub target_transform: Option<Transform3D>,
    pub geometry: Geometry,
    pub projection: ProjectionType,
    pub texture_id: Option<u32>, // GL texture ID
    pub visible: bool,
    pub focused: bool,
    pub alpha: f32,
    pub ppu: f32,              // pixels-per-unit override (0 = use global)
    pub surface_width: u32,    // pixel width of source surface
    pub surface_height: u32,   // pixel height of source surface
    pub dirty: bool,           // needs texture re-import
}

impl SceneNode {
    pub fn new_surface(surface_id: u64, width: u32, height: u32, ppu: f32) -> Self {
        let w_m = width as f32 / ppu;
        let h_m = height as f32 / ppu;

        Self {
            surface_id: Some(surface_id),
            transform: Transform3D::default(),
            target_transform: None,
            geometry: Geometry::Quad {
                width: w_m,
                height: h_m,
            },
            projection: ProjectionType::Flat,
            texture_id: None,
            visible: true,
            focused: false,
            alpha: 1.0,
            ppu,
            surface_width: width,
            surface_height: height,
            dirty: true,
        }
    }

    /// Update geometry based on current PPU and surface dimensions.
    pub fn update_geometry(&mut self) {
        let w_m = self.surface_width as f32 / self.ppu;
        let h_m = self.surface_height as f32 / self.ppu;

        self.geometry = match self.projection {
            ProjectionType::Flat => Geometry::Quad {
                width: w_m,
                height: h_m,
            },
            ProjectionType::Cylinder => {
                // Arc angle proportional to width
                let arc_angle = w_m / 2.0; // radians, at radius=2.0m
                Geometry::Cylinder {
                    radius: 2.0,
                    arc_angle,
                    height: h_m,
                    segments: 64,
                }
            }
        };
    }
}

// ── Layout modes ─────────────────────────────────────────────

/// VR surface layout mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VrLayoutMode {
    /// Horizontal arc at uniform distance.
    Arc,
    /// 2D grid at fixed distance.
    Grid { columns: u32 },
    /// Stacked at same position with Z offsets.
    Stack,
    /// User-positioned (no automatic layout).
    Freeform,
}

impl Default for VrLayoutMode {
    fn default() -> Self {
        Self::Arc
    }
}

// ── Background ───────────────────────────────────────────────

/// VR background type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VrBackground {
    Dark,
    Gradient,
    Grid,
    Passthrough,
}

impl Default for VrBackground {
    fn default() -> Self {
        Self::Dark
    }
}

// ── Scene ────────────────────────────────────────────────────

/// Focus indication configuration.
#[derive(Debug, Clone)]
pub struct FocusConfig {
    pub pull_forward: f32,    // meters to pull focused surface toward viewer
    pub unfocus_dim: f32,     // alpha multiplier for unfocused surfaces (0.0-1.0)
    pub border_color: [f32; 4], // RGBA
}

impl Default for FocusConfig {
    fn default() -> Self {
        Self {
            pull_forward: 0.1,
            unfocus_dim: 0.8,
            border_color: [0.267, 0.267, 1.0, 1.0], // #4444ff
        }
    }
}

/// The VR scene: root container for all 3D content.
pub struct VrScene {
    pub nodes: HashMap<u64, SceneNode>,
    pub layout_mode: VrLayoutMode,
    pub background: VrBackground,
    pub background_color: [f32; 4],
    pub global_ppu: f32,
    pub focus_config: FocusConfig,
    pub focused_surface: Option<u64>,
    pub arc_distance: f32,     // default distance for arc layout (meters)
    pub arc_spacing_deg: f32,  // gap between surfaces in arc (degrees)
}

impl Default for VrScene {
    fn default() -> Self {
        Self {
            nodes: HashMap::new(),
            layout_mode: VrLayoutMode::default(),
            background: VrBackground::default(),
            background_color: [0.102, 0.102, 0.180, 1.0], // #1a1a2e
            global_ppu: 1000.0,
            focus_config: FocusConfig::default(),
            focused_surface: None,
            arc_distance: 2.0,
            arc_spacing_deg: 5.0,
        }
    }
}

impl VrScene {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a surface to the scene.
    pub fn add_surface(&mut self, surface_id: u64, width: u32, height: u32) {
        let ppu = self.global_ppu;
        let node = SceneNode::new_surface(surface_id, width, height, ppu);
        self.nodes.insert(surface_id, node);
        self.recompute_layout();
        info!("VR scene: added surface {} ({}x{})", surface_id, width, height);
    }

    /// Remove a surface from the scene.
    pub fn remove_surface(&mut self, surface_id: u64) {
        self.nodes.remove(&surface_id);
        if self.focused_surface == Some(surface_id) {
            self.focused_surface = None;
        }
        self.recompute_layout();
        info!("VR scene: removed surface {}", surface_id);
    }

    /// Update the texture for a surface.
    pub fn update_texture(&mut self, surface_id: u64, texture_id: u32) {
        if let Some(node) = self.nodes.get_mut(&surface_id) {
            node.texture_id = Some(texture_id);
            node.dirty = false;
        }
    }

    /// Set the transform for a surface.
    pub fn set_transform(&mut self, surface_id: u64, transform: Transform3D) {
        if let Some(node) = self.nodes.get_mut(&surface_id) {
            node.transform = transform;
        }
    }

    /// Set the projection type for a surface.
    pub fn set_projection(&mut self, surface_id: u64, projection: ProjectionType) {
        if let Some(node) = self.nodes.get_mut(&surface_id) {
            node.projection = projection;
            node.update_geometry();
        }
    }

    /// Set per-surface PPU.
    pub fn set_surface_ppu(&mut self, surface_id: u64, ppu: f32) {
        if let Some(node) = self.nodes.get_mut(&surface_id) {
            node.ppu = ppu;
            node.update_geometry();
        }
    }

    /// Set global PPU and update all surfaces.
    pub fn set_global_ppu(&mut self, ppu: f32) {
        self.global_ppu = ppu;
        for node in self.nodes.values_mut() {
            if node.ppu == self.global_ppu || node.ppu == 0.0 {
                node.ppu = ppu;
                node.update_geometry();
            }
        }
        self.recompute_layout();
    }

    /// Set the focused surface.
    pub fn set_focus(&mut self, surface_id: Option<u64>) {
        // Un-focus previous
        if let Some(old_id) = self.focused_surface {
            if let Some(node) = self.nodes.get_mut(&old_id) {
                node.focused = false;
                node.alpha = self.focus_config.unfocus_dim;
            }
        }
        // Focus new
        if let Some(new_id) = surface_id {
            if let Some(node) = self.nodes.get_mut(&new_id) {
                node.focused = true;
                node.alpha = 1.0;
            }
        }
        self.focused_surface = surface_id;
    }

    /// Set the layout mode and recompute.
    pub fn set_layout(&mut self, mode: VrLayoutMode) {
        self.layout_mode = mode;
        self.recompute_layout();
    }

    /// Recompute surface positions based on current layout mode.
    pub fn recompute_layout(&mut self) {
        let surface_ids: Vec<u64> = self.nodes.keys().copied().collect();
        let count = surface_ids.len();
        if count == 0 {
            return;
        }

        match self.layout_mode {
            VrLayoutMode::Arc => self.layout_arc(&surface_ids),
            VrLayoutMode::Grid { columns } => self.layout_grid(&surface_ids, columns),
            VrLayoutMode::Stack => self.layout_stack(&surface_ids),
            VrLayoutMode::Freeform => {} // no automatic layout
        }
    }

    fn layout_arc(&mut self, surface_ids: &[u64]) {
        let count = surface_ids.len();
        let spacing_rad = self.arc_spacing_deg * PI / 180.0;
        let distance = self.arc_distance;

        // Compute total angular width of all surfaces plus gaps
        let mut total_angle = 0.0f32;
        for id in surface_ids {
            if let Some(node) = self.nodes.get(id) {
                let w_m = node.surface_width as f32 / node.ppu;
                let surface_angle = (w_m / distance).atan() * 2.0;
                total_angle += surface_angle;
            }
        }
        total_angle += spacing_rad * (count as f32 - 1.0).max(0.0);

        let mut current_angle = -total_angle * 0.5;

        for id in surface_ids {
            if let Some(node) = self.nodes.get_mut(id) {
                let w_m = node.surface_width as f32 / node.ppu;
                let h_m = node.surface_height as f32 / node.ppu;
                let surface_angle = (w_m / distance).atan() * 2.0;
                let center_angle = current_angle + surface_angle * 0.5;

                let x = distance * center_angle.sin();
                let z = -distance * center_angle.cos();
                let y = h_m * 0.5; // center vertically at eye height

                node.target_transform = Some(Transform3D {
                    position: Vec3::new(x, y, z),
                    rotation: Quat::from_euler(center_angle, 0.0, 0.0),
                    scale: Vec3::ONE,
                });

                // Apply immediately (animation handled in tick)
                node.transform = node.target_transform.unwrap();

                current_angle += surface_angle + spacing_rad;
            }
        }
    }

    fn layout_grid(&mut self, surface_ids: &[u64], columns: u32) {
        let columns = columns.max(1) as usize;
        let rows = (surface_ids.len() + columns - 1) / columns;
        let distance = self.arc_distance;

        for (i, id) in surface_ids.iter().enumerate() {
            let col = i % columns;
            let row = i / columns;

            if let Some(node) = self.nodes.get_mut(id) {
                let w_m = node.surface_width as f32 / node.ppu;
                let h_m = node.surface_height as f32 / node.ppu;
                let gap = 0.05; // 5cm gap

                let total_w = columns as f32 * (w_m + gap) - gap;
                let total_h = rows as f32 * (h_m + gap) - gap;

                let x = (col as f32 * (w_m + gap)) - total_w * 0.5 + w_m * 0.5;
                let y = total_h * 0.5 - (row as f32 * (h_m + gap)) - h_m * 0.5 + h_m;
                let z = -distance;

                node.transform = Transform3D::at(x, y, z);
                node.target_transform = Some(node.transform);
            }
        }
    }

    fn layout_stack(&mut self, surface_ids: &[u64]) {
        let distance = self.arc_distance;

        for (i, id) in surface_ids.iter().enumerate() {
            if let Some(node) = self.nodes.get_mut(id) {
                let h_m = node.surface_height as f32 / node.ppu;
                let z_offset = i as f32 * 0.01; // 1cm per layer

                node.transform = Transform3D::at(0.0, h_m * 0.5, -distance + z_offset);
                node.target_transform = Some(node.transform);

                // Only top surface is visible in stack
                node.visible = i == surface_ids.len() - 1;
            }
        }
    }

    /// Get ordered list of surface IDs for rendering (back-to-front).
    pub fn render_order(&self) -> Vec<u64> {
        let mut ids: Vec<(u64, f32)> = self
            .nodes
            .iter()
            .filter(|(_, node)| node.visible)
            .map(|(id, node)| {
                let z = node.transform.position.z;
                (*id, z)
            })
            .collect();

        // Sort back-to-front (most negative Z first)
        ids.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));

        // Focused surface renders last (on top)
        if let Some(focused_id) = self.focused_surface {
            if let Some(pos) = ids.iter().position(|(id, _)| *id == focused_id) {
                let item = ids.remove(pos);
                ids.push(item);
            }
        }

        ids.into_iter().map(|(id, _)| id).collect()
    }

    /// Get scene state as an IPC s-expression.
    pub fn scene_sexp(&self) -> String {
        let mut surfaces = String::from("(");
        for (id, node) in &self.nodes {
            let proj = match node.projection {
                ProjectionType::Flat => "flat",
                ProjectionType::Cylinder => "cylinder",
            };
            surfaces.push_str(&format!(
                "(:id {} :position (:x {:.2} :y {:.2} :z {:.2}) :projection :{} :ppu {:.0} :visible {} :focused {})",
                id,
                node.transform.position.x,
                node.transform.position.y,
                node.transform.position.z,
                proj,
                node.ppu,
                if node.visible { "t" } else { "nil" },
                if node.focused { "t" } else { "nil" },
            ));
        }
        surfaces.push(')');

        let layout = match self.layout_mode {
            VrLayoutMode::Arc => "arc".to_string(),
            VrLayoutMode::Grid { columns } => format!("grid-{}", columns),
            VrLayoutMode::Stack => "stack".to_string(),
            VrLayoutMode::Freeform => "freeform".to_string(),
        };

        let bg = match self.background {
            VrBackground::Dark => "dark",
            VrBackground::Gradient => "gradient",
            VrBackground::Grid => "grid",
            VrBackground::Passthrough => "passthrough",
        };

        format!(
            "(:layout :{} :background :{} :ppu {:.0} :surfaces {})",
            layout, bg, self.global_ppu, surfaces
        )
    }

    /// Return surface count.
    pub fn surface_count(&self) -> usize {
        self.nodes.len()
    }
}

// ── Tests ────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quad_mesh_vertices() {
        let (verts, indices) = generate_quad_mesh(2.0, 1.0);
        assert_eq!(verts.len(), 4);
        assert_eq!(indices.len(), 6);
        // Top-left at (-1, 0.5, 0)
        assert!((verts[0].position[0] - (-1.0)).abs() < 1e-5);
        assert!((verts[0].position[1] - 0.5).abs() < 1e-5);
        // UV: top-left = (0, 0)
        assert!((verts[0].texcoord[0]).abs() < 1e-5);
        assert!((verts[0].texcoord[1]).abs() < 1e-5);
        // Normal: +Z
        assert!((verts[0].normal[2] - 1.0).abs() < 1e-5);
    }

    #[test]
    fn test_cylinder_mesh_segments() {
        let segments = 32;
        let (verts, indices) = generate_cylinder_mesh(2.0, PI / 2.0, 1.0, segments);
        // 2 vertices per segment column, (segments+1) columns
        assert_eq!(verts.len(), ((segments + 1) * 2) as usize);
        // 6 indices per segment
        assert_eq!(indices.len(), (segments * 6) as usize);
    }

    #[test]
    fn test_cylinder_normals_point_inward() {
        let (verts, _) = generate_cylinder_mesh(2.0, PI / 2.0, 1.0, 4);
        for v in &verts {
            // Normal should point roughly inward (toward origin in XZ plane)
            let pos_len = (v.position[0] * v.position[0] + v.position[2] * v.position[2]).sqrt();
            let norm_dot_pos = v.normal[0] * v.position[0] + v.normal[2] * v.position[2];
            // Dot product of normal and position should be negative (pointing inward)
            if pos_len > 1e-5 {
                assert!(norm_dot_pos < 0.0, "Normal should point inward");
            }
        }
    }

    #[test]
    fn test_scene_add_remove() {
        let mut scene = VrScene::new();
        scene.add_surface(1, 1920, 1080);
        scene.add_surface(2, 1920, 1080);
        scene.add_surface(3, 1280, 720);
        assert_eq!(scene.surface_count(), 3);

        scene.remove_surface(2);
        assert_eq!(scene.surface_count(), 2);
        assert!(scene.nodes.contains_key(&1));
        assert!(!scene.nodes.contains_key(&2));
        assert!(scene.nodes.contains_key(&3));
    }

    #[test]
    fn test_scene_focus() {
        let mut scene = VrScene::new();
        scene.add_surface(1, 1920, 1080);
        scene.add_surface(2, 1920, 1080);

        scene.set_focus(Some(1));
        assert_eq!(scene.focused_surface, Some(1));
        assert!(scene.nodes[&1].focused);
        assert!(!scene.nodes[&2].focused);

        scene.set_focus(Some(2));
        assert_eq!(scene.focused_surface, Some(2));
        assert!(!scene.nodes[&1].focused);
        assert!(scene.nodes[&2].focused);
    }

    #[test]
    fn test_render_order_focused_last() {
        let mut scene = VrScene::new();
        scene.add_surface(1, 1920, 1080);
        scene.add_surface(2, 1920, 1080);
        scene.add_surface(3, 1920, 1080);

        scene.set_focus(Some(1));
        let order = scene.render_order();
        assert_eq!(*order.last().unwrap(), 1);
    }

    #[test]
    fn test_ppu_surface_sizing() {
        let mut scene = VrScene::new();
        scene.global_ppu = 1000.0;
        scene.add_surface(1, 1920, 1080);

        let node = &scene.nodes[&1];
        match &node.geometry {
            Geometry::Quad { width, height } => {
                assert!((width - 1.92).abs() < 0.01);
                assert!((height - 1.08).abs() < 0.01);
            }
            _ => panic!("Expected quad geometry"),
        }
    }

    #[test]
    fn test_layout_arc_positions() {
        let mut scene = VrScene::new();
        scene.set_layout(VrLayoutMode::Arc);
        scene.add_surface(1, 1920, 1080);
        scene.add_surface(2, 1920, 1080);
        scene.add_surface(3, 1920, 1080);

        // All surfaces should be at roughly arc_distance from origin
        for node in scene.nodes.values() {
            let dist = (node.transform.position.x.powi(2) + node.transform.position.z.powi(2)).sqrt();
            assert!((dist - scene.arc_distance).abs() < 0.5);
        }
    }

    #[test]
    fn test_layout_grid() {
        let mut scene = VrScene::new();
        scene.set_layout(VrLayoutMode::Grid { columns: 2 });
        scene.add_surface(1, 1920, 1080);
        scene.add_surface(2, 1920, 1080);
        scene.add_surface(3, 1920, 1080);

        // All surfaces at same Z distance
        let z_values: Vec<f32> = scene.nodes.values().map(|n| n.transform.position.z).collect();
        for z in &z_values {
            assert!((z - z_values[0]).abs() < 0.01);
        }
    }

    #[test]
    fn test_scene_sexp() {
        let mut scene = VrScene::new();
        scene.add_surface(1, 1920, 1080);
        let sexp = scene.scene_sexp();
        assert!(sexp.contains(":layout :arc"));
        assert!(sexp.contains(":background :dark"));
        assert!(sexp.contains(":ppu 1000"));
    }

    #[test]
    fn test_mat4_identity() {
        let m = Mat4::IDENTITY;
        assert!((m.data[0] - 1.0).abs() < 1e-6);
        assert!((m.data[5] - 1.0).abs() < 1e-6);
        assert!((m.data[10] - 1.0).abs() < 1e-6);
        assert!((m.data[15] - 1.0).abs() < 1e-6);
    }

    #[test]
    fn test_mat4_mul_identity() {
        let a = Mat4::translation(1.0, 2.0, 3.0);
        let result = a.mul(&Mat4::IDENTITY);
        assert!((result.data[12] - 1.0).abs() < 1e-6);
        assert!((result.data[13] - 2.0).abs() < 1e-6);
        assert!((result.data[14] - 3.0).abs() < 1e-6);
    }

    #[test]
    fn test_vec3_lerp() {
        let a = Vec3::new(0.0, 0.0, 0.0);
        let b = Vec3::new(10.0, 20.0, 30.0);
        let mid = a.lerp(b, 0.5);
        assert!((mid.x - 5.0).abs() < 1e-5);
        assert!((mid.y - 10.0).abs() < 1e-5);
        assert!((mid.z - 15.0).abs() < 1e-5);
    }

    #[test]
    fn test_projection_switch() {
        let mut scene = VrScene::new();
        scene.add_surface(1, 1920, 1080);

        scene.set_projection(1, ProjectionType::Cylinder);
        assert_eq!(scene.nodes[&1].projection, ProjectionType::Cylinder);
        match &scene.nodes[&1].geometry {
            Geometry::Cylinder { .. } => {}
            _ => panic!("Expected cylinder geometry after projection switch"),
        }
    }
}
