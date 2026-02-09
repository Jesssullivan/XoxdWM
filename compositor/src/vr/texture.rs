//! DMA-BUF texture import pipeline for VR rendering.
//!
//! Imports Wayland surface textures into the OpenXR/OpenGL rendering context
//! using DMA-BUF file descriptors for zero-copy GPU texture sharing.
//!
//! Pipeline: Wayland client -> wl_buffer -> DMA-BUF fd -> GL texture -> OpenXR swapchain

use tracing::{debug, info, warn};

/// Texture format for VR surface rendering.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VrTextureFormat {
    Rgba8,
    Bgra8,
    Rgba16f,
}

impl VrTextureFormat {
    /// GL internal format constant.
    pub fn gl_internal_format(&self) -> u32 {
        match self {
            Self::Rgba8 => 0x8058,   // GL_RGBA8
            Self::Bgra8 => 0x8058,   // GL_RGBA8 (swizzled)
            Self::Rgba16f => 0x881A,  // GL_RGBA16F
        }
    }

    /// GL format constant.
    pub fn gl_format(&self) -> u32 {
        match self {
            Self::Rgba8 => 0x1908,  // GL_RGBA
            Self::Bgra8 => 0x80E1,  // GL_BGRA
            Self::Rgba16f => 0x1908, // GL_RGBA
        }
    }
}

/// Imported texture handle for a Wayland surface.
#[derive(Debug)]
pub struct VrTexture {
    pub surface_id: u64,
    pub gl_texture: u32,
    pub width: u32,
    pub height: u32,
    pub format: VrTextureFormat,
    pub dirty: bool,
}

impl VrTexture {
    /// Create a placeholder texture (actual import happens in render context).
    pub fn placeholder(surface_id: u64, width: u32, height: u32) -> Self {
        Self {
            surface_id,
            gl_texture: 0,
            width,
            height,
            format: VrTextureFormat::Rgba8,
            dirty: true,
        }
    }

    /// Mark texture as needing re-import (surface content changed).
    pub fn mark_dirty(&mut self) {
        self.dirty = true;
    }
}

/// Manages texture import for all VR surfaces.
pub struct TextureManager {
    textures: std::collections::HashMap<u64, VrTexture>,
}

impl TextureManager {
    pub fn new() -> Self {
        info!("VR texture manager initialized");
        Self {
            textures: std::collections::HashMap::new(),
        }
    }

    /// Register a surface for texture tracking.
    pub fn register_surface(&mut self, surface_id: u64, width: u32, height: u32) {
        let tex = VrTexture::placeholder(surface_id, width, height);
        self.textures.insert(surface_id, tex);
        debug!("VR texture: registered surface {} ({}x{})", surface_id, width, height);
    }

    /// Unregister a surface and clean up its texture.
    pub fn unregister_surface(&mut self, surface_id: u64) {
        if let Some(tex) = self.textures.remove(&surface_id) {
            if tex.gl_texture != 0 {
                // GL texture cleanup would happen here in a real implementation
                debug!("VR texture: cleaned up GL texture {} for surface {}", tex.gl_texture, surface_id);
            }
        }
    }

    /// Mark a surface texture as dirty (needs re-import).
    pub fn mark_dirty(&mut self, surface_id: u64) {
        if let Some(tex) = self.textures.get_mut(&surface_id) {
            tex.mark_dirty();
        }
    }

    /// Import pending dirty textures from DMA-BUF.
    /// In a full implementation, this would:
    /// 1. Get the wl_buffer's DMA-BUF fd from Smithay
    /// 2. Use EGL_EXT_image_dma_buf_import to create an EGLImage
    /// 3. Bind to a GL texture via glEGLImageTargetTexture2DOES
    pub fn import_pending(&mut self) -> Vec<(u64, u32)> {
        let mut imported = Vec::new();

        for (id, tex) in &mut self.textures {
            if tex.dirty && tex.gl_texture == 0 {
                // Placeholder: actual DMA-BUF import requires GL context
                debug!("VR texture: would import DMA-BUF for surface {}", id);
                tex.dirty = false;
                imported.push((*id, tex.gl_texture));
            }
        }

        imported
    }

    /// Get the GL texture ID for a surface.
    pub fn get_texture(&self, surface_id: u64) -> Option<u32> {
        self.textures.get(&surface_id).map(|t| t.gl_texture)
    }

    /// Get number of tracked textures.
    pub fn texture_count(&self) -> usize {
        self.textures.len()
    }
}

impl Default for TextureManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_register_unregister() {
        let mut mgr = TextureManager::new();
        mgr.register_surface(1, 1920, 1080);
        mgr.register_surface(2, 1280, 720);
        assert_eq!(mgr.texture_count(), 2);

        mgr.unregister_surface(1);
        assert_eq!(mgr.texture_count(), 1);
        assert!(mgr.get_texture(2).is_some());
    }

    #[test]
    fn test_mark_dirty() {
        let mut mgr = TextureManager::new();
        mgr.register_surface(1, 1920, 1080);

        // Import clears dirty
        mgr.import_pending();
        let tex = mgr.textures.get(&1).unwrap();
        assert!(!tex.dirty);

        // Re-mark dirty
        mgr.mark_dirty(1);
        let tex = mgr.textures.get(&1).unwrap();
        assert!(tex.dirty);
    }

    #[test]
    fn test_texture_format() {
        assert_eq!(VrTextureFormat::Rgba8.gl_internal_format(), 0x8058);
        assert_eq!(VrTextureFormat::Bgra8.gl_format(), 0x80E1);
    }
}
