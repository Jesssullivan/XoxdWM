//! Stereo VR renderer — renders the 3D scene to OpenXR swapchains.
//!
//! Renders each eye's view by:
//! 1. Getting eye poses from OpenXR
//! 2. Computing per-eye view/projection matrices
//! 3. Rendering scene nodes (surface quads/cylinders) with their textures
//! 4. Submitting frames to OpenXR
//!
//! Uses the scene graph from `super::scene` for layout and the texture
//! manager from `super::texture` for surface content.

use tracing::{debug, info};

use super::scene::{Mat4, Quat, Transform3D, Vec3, VrScene};
use super::texture::TextureManager;

/// Per-eye view configuration.
#[derive(Debug, Clone)]
pub struct EyeView {
    pub pose: Transform3D,
    pub projection: Mat4,
    pub viewport_x: u32,
    pub viewport_y: u32,
    pub viewport_w: u32,
    pub viewport_h: u32,
}

impl Default for EyeView {
    fn default() -> Self {
        Self {
            pose: Transform3D::default(),
            projection: Mat4::IDENTITY,
            viewport_x: 0,
            viewport_y: 0,
            viewport_w: 1920,
            viewport_h: 1080,
        }
    }
}

/// Stereo render target configuration.
#[derive(Debug, Clone)]
pub struct StereoConfig {
    pub left_eye: EyeView,
    pub right_eye: EyeView,
    pub ipd: f32, // inter-pupillary distance in meters
}

impl Default for StereoConfig {
    fn default() -> Self {
        let ipd = 0.063; // 63mm average IPD
        Self {
            left_eye: EyeView {
                pose: Transform3D {
                    position: Vec3::new(-ipd / 2.0, 0.0, 0.0),
                    rotation: Quat::IDENTITY,
                    scale: Vec3::ONE,
                },
                ..Default::default()
            },
            right_eye: EyeView {
                pose: Transform3D {
                    position: Vec3::new(ipd / 2.0, 0.0, 0.0),
                    rotation: Quat::IDENTITY,
                    scale: Vec3::ONE,
                },
                ..Default::default()
            },
            ipd,
        }
    }
}

/// VR renderer state.
pub struct VrRenderer {
    pub stereo: StereoConfig,
    pub texture_manager: TextureManager,
    pub frame_count: u64,
    pub clear_color: [f32; 4],
}

impl VrRenderer {
    pub fn new() -> Self {
        info!("VR renderer initialized");
        Self {
            stereo: StereoConfig::default(),
            texture_manager: TextureManager::new(),
            frame_count: 0,
            clear_color: [0.102, 0.102, 0.180, 1.0],
        }
    }

    /// Update eye poses from OpenXR view state.
    /// In a full implementation, this takes xr::View data from locate_views().
    pub fn update_eye_poses(&mut self, left_pose: Transform3D, right_pose: Transform3D) {
        self.stereo.left_eye.pose = left_pose;
        self.stereo.right_eye.pose = right_pose;
    }

    /// Update projection matrices from OpenXR FOV.
    pub fn update_projections(&mut self, left_proj: Mat4, right_proj: Mat4) {
        self.stereo.left_eye.projection = left_proj;
        self.stereo.right_eye.projection = right_proj;
    }

    /// Render one stereo frame.
    /// In a full implementation, this would:
    /// 1. Acquire swapchain images for both eyes
    /// 2. For each eye: set viewport, compute view matrix, render all scene nodes
    /// 3. Release swapchain images
    /// 4. Submit composition layers to OpenXR
    pub fn render_frame(&mut self, scene: &VrScene) {
        // Import any pending textures
        let imported = self.texture_manager.import_pending();
        for (surface_id, _tex_id) in &imported {
            debug!("VR render: imported texture for surface {}", surface_id);
        }

        let render_order = scene.render_order();

        // Left eye
        self.render_eye(&scene, &self.stereo.left_eye.clone(), &render_order);

        // Right eye
        self.render_eye(&scene, &self.stereo.right_eye.clone(), &render_order);

        self.frame_count += 1;
    }

    /// Render one eye's view of the scene.
    fn render_eye(&self, scene: &VrScene, eye: &EyeView, render_order: &[u64]) {
        // Compute view matrix from eye pose
        let model = Mat4::from_transform(&eye.pose);
        let _view = model.inverse().unwrap_or(Mat4::IDENTITY);

        // For each surface in render order:
        for surface_id in render_order {
            if let Some(node) = scene.nodes.get(surface_id) {
                if !node.visible {
                    continue;
                }

                let _model_matrix = Mat4::from_transform(&node.transform);

                // In a full implementation:
                // 1. Bind the node's GL texture
                // 2. Set uniforms: MVP = projection * view * model, alpha
                // 3. Draw the geometry (quad or cylinder mesh)
                // 4. If focused, draw focus border

                debug!(
                    "VR render: eye draw surface {} at ({:.2}, {:.2}, {:.2}) alpha={:.1}",
                    surface_id,
                    node.transform.position.x,
                    node.transform.position.y,
                    node.transform.position.z,
                    node.alpha,
                );
            }
        }
    }

    /// Register a new surface with the texture manager.
    pub fn register_surface(&mut self, surface_id: u64, width: u32, height: u32) {
        self.texture_manager.register_surface(surface_id, width, height);
    }

    /// Unregister a surface from the texture manager.
    pub fn unregister_surface(&mut self, surface_id: u64) {
        self.texture_manager.unregister_surface(surface_id);
    }

    /// Mark a surface's texture as dirty (content changed).
    pub fn mark_surface_dirty(&mut self, surface_id: u64) {
        self.texture_manager.mark_dirty(surface_id);
    }
}

impl Default for VrRenderer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stereo_config_default_ipd() {
        let config = StereoConfig::default();
        assert!((config.ipd - 0.063).abs() < 0.001);
        assert!(config.left_eye.pose.position.x < 0.0);
        assert!(config.right_eye.pose.position.x > 0.0);
    }

    #[test]
    fn test_renderer_frame_count() {
        let mut renderer = VrRenderer::new();
        let scene = VrScene::new();
        assert_eq!(renderer.frame_count, 0);
        renderer.render_frame(&scene);
        assert_eq!(renderer.frame_count, 1);
        renderer.render_frame(&scene);
        assert_eq!(renderer.frame_count, 2);
    }

    #[test]
    fn test_renderer_surface_lifecycle() {
        let mut renderer = VrRenderer::new();
        renderer.register_surface(1, 1920, 1080);
        renderer.register_surface(2, 1280, 720);
        assert_eq!(renderer.texture_manager.texture_count(), 2);

        renderer.unregister_surface(1);
        assert_eq!(renderer.texture_manager.texture_count(), 1);
    }

    #[test]
    fn test_eye_view_default() {
        let eye = EyeView::default();
        assert_eq!(eye.viewport_w, 1920);
        assert_eq!(eye.viewport_h, 1080);
    }
}
