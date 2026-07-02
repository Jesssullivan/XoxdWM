//! Rendering pipeline -- surface compositing and frame submission.
//!
//! Provides damage-tracked rendering for both winit (development) and
//! DRM (production) backends.  Each function collects render elements
//! from the compositor [`Space`], feeds them through an
//! [`OutputDamageTracker`] for efficient partial redraws, submits the
//! frame, and sends Wayland frame callbacks to clients.

#[cfg(feature = "full-backend")]
use smithay::backend::winit::WinitGraphicsBackend;
use smithay::{
    backend::allocator::Fourcc,
    backend::renderer::{
        damage::OutputDamageTracker,
        element::surface::WaylandSurfaceRenderElement,
        gles::{GlesRenderer, GlesTarget},
        Color32F, ExportMem, ImportAll, Renderer, Texture,
    },
    desktop::{
        space::SpaceRenderElements,
        utils::{surface_primary_scanout_output, update_surface_primary_scanout_output},
        Space, Window,
    },
    output::Output,
    utils::{Physical, Rectangle},
};
use std::{
    env,
    sync::atomic::{AtomicUsize, Ordering},
    time::Duration,
};
use tracing::{info, trace, warn};

use crate::state::EwwmState;

/// Background color (Catppuccin Mocha base: #1e1e2e).
pub const BG_COLOR: Color32F = Color32F::new(0.118, 0.118, 0.180, 1.0);

static DRM_READBACK_COUNT: AtomicUsize = AtomicUsize::new(0);

// ---------------------------------------------------------------------------
// Winit backend
// ---------------------------------------------------------------------------

/// Render a complete frame using the winit development backend.
///
/// The caller (winit event loop) owns the [`OutputDamageTracker`] and
/// passes it in so that damage state persists across frames.
///
/// Steps:
/// 1. Bind the winit back-buffer (also handles window resizes).
/// 2. Collect render elements from the [`Space`] (windows + layer
///    surfaces).
/// 3. Run the damage-tracked render pass via
///    [`OutputDamageTracker::render_output`].
/// 4. Submit the frame (buffer swap) with damage hints.
/// 5. Send Wayland `wl_surface::frame` callbacks so clients know
///    their content was presented.
#[cfg(feature = "full-backend")]
pub fn render_winit(
    backend: &mut WinitGraphicsBackend<GlesRenderer>,
    damage_tracker: &mut OutputDamageTracker,
    state: &mut EwwmState,
    output: &Output,
) {
    // Buffer age tells the damage tracker how many frames old the
    // current back-buffer is, enabling partial redraws.  Must be
    // read before bind() borrows the backend mutably.
    let age = backend.buffer_age().unwrap_or(0);

    // 1. Bind -- gives us the renderer and a framebuffer handle.
    let (renderer, mut framebuffer) = match backend.bind() {
        Ok(pair) => pair,
        Err(e) => {
            warn!("winit bind failed: {}", e);
            return;
        }
    };

    // 2. Collect render elements from the space.
    //    render_elements_for_output includes layer-shell surfaces.
    let elements: Vec<
        SpaceRenderElements<GlesRenderer, WaylandSurfaceRenderElement<GlesRenderer>>,
    > = match state
        .space
        .render_elements_for_output(renderer, output, 1.0 /* alpha */)
    {
        Ok(elems) => elems,
        Err(e) => {
            warn!("failed to collect render elements: {:?}", e);
            Vec::new()
        }
    };

    // 3. Damage-tracked render pass.
    let render_result =
        damage_tracker.render_output(renderer, &mut framebuffer, age, &elements, BG_COLOR);

    // Extract damage rectangles and render element states from the
    // result so we can pass them to submit() and frame callbacks.
    let (damage, render_states) = match render_result {
        Ok(result) => {
            let damage: Option<Vec<Rectangle<i32, Physical>>> = result.damage.map(|d| d.clone());
            let states = result.states;
            trace!(
                "rendered frame: {} damage rect(s)",
                damage.as_ref().map_or(0, |d| d.len())
            );
            (damage, Some(states))
        }
        Err(err) => {
            warn!("render_output failed: {:?}", err);
            (None, None)
        }
    };

    // Drop the renderer/framebuffer borrow before calling submit.
    drop(framebuffer);

    // 4. Submit (buffer swap) with damage hints.
    let submit_damage = damage.as_ref().map(|rects| rects.as_slice());
    if let Err(e) = backend.submit(submit_damage) {
        warn!("winit submit failed: {}", e);
    }

    // 5. Send frame callbacks and update scan-out bookkeeping.
    send_frame_callbacks(&state.space, output, render_states.as_ref());
}

// ---------------------------------------------------------------------------
// DRM backend (production / direct hardware rendering)
// ---------------------------------------------------------------------------

/// Render a frame for the DRM backend.
///
/// Operates on a raw [`GlesRenderer`] and a caller-provided
/// framebuffer, which the DRM surface management code obtains by
/// binding the next scanout buffer.
///
/// The pattern mirrors [`render_winit`] but decouples buffer
/// acquisition (handled by the DRM surface/connector code) from the
/// actual compositing pass.
///
/// # Arguments
///
/// * `renderer`       - The GL ES renderer attached to the DRM device.
/// * `framebuffer`    - Framebuffer for the current scanout buffer.
/// * `damage_tracker` - Per-output damage tracker (stored alongside
///                      the DRM surface).
/// * `space`          - The compositor [`Space`] containing windows.
/// * `output`         - The [`Output`] being rendered.
/// * `age`            - Back-buffer age from the DRM surface (0 if
///                      unknown).
///
/// Returns the damage rectangles that were rendered, or `None` when
/// a full redraw was performed.  The caller uses this to feed
/// damage into `drm_surface.queue_buffer(damage)`.
pub fn render_drm(
    renderer: &mut GlesRenderer,
    framebuffer: &mut GlesTarget<'_>,
    damage_tracker: &mut OutputDamageTracker,
    space: &mut Space<Window>,
    output: &Output,
    age: usize,
) -> Option<Vec<Rectangle<i32, Physical>>> {
    let test_pattern = drm_test_pattern_color();

    // 1. Collect render elements.
    let elements: Vec<
        SpaceRenderElements<GlesRenderer, WaylandSurfaceRenderElement<GlesRenderer>>,
    > = if test_pattern.is_some() {
        Vec::new()
    } else {
        match space.render_elements_for_output(renderer, output, 1.0) {
            Ok(elems) => elems,
            Err(e) => {
                warn!("drm: failed to collect render elements: {:?}", e);
                Vec::new()
            }
        }
    };

    let (background, pattern_name) = match test_pattern {
        Some((name, color)) => (color, Some(name)),
        None => (BG_COLOR, None),
    };

    // A test pattern should repaint the full buffer instead of relying on
    // previous damage state, otherwise a single dark back-buffer can survive.
    let render_age = if pattern_name.is_some() { 0 } else { age };

    // 2. Damage-tracked render pass.
    let render_result =
        damage_tracker.render_output(renderer, framebuffer, render_age, &elements, background);

    let (damage, render_states) = match render_result {
        Ok(result) => {
            let damage: Option<Vec<Rectangle<i32, Physical>>> = result.damage.map(|d| d.clone());
            let states = result.states;
            trace!(
                "drm: rendered frame: {} damage rect(s)",
                damage.as_ref().map_or(0, |d| d.len())
            );
            (damage, Some(states))
        }
        Err(err) => {
            warn!("drm: render_output failed: {:?}", err);
            (None, None)
        }
    };

    if should_readback_drm_frame() {
        log_drm_frame_readback(
            renderer,
            framebuffer,
            output,
            elements.len(),
            damage.as_ref().map_or(0, |rects| rects.len()),
            pattern_name.as_deref(),
        );
    }

    // 3. Send frame callbacks.
    send_frame_callbacks(space, output, render_states.as_ref());

    damage
}

fn drm_test_pattern_color() -> Option<(String, Color32F)> {
    let pattern = env::var("EWWM_DRM_TEST_PATTERN").ok()?;
    let color = match pattern.as_str() {
        "solid-red" | "red" => Color32F::new(1.0, 0.0, 0.0, 1.0),
        "solid-green" | "green" => Color32F::new(0.0, 1.0, 0.0, 1.0),
        "solid-blue" | "blue" => Color32F::new(0.0, 0.0, 1.0, 1.0),
        "solid-white" | "white" => Color32F::new(1.0, 1.0, 1.0, 1.0),
        "solid-gray" | "gray" | "grey" => Color32F::new(0.5, 0.5, 0.5, 1.0),
        other => {
            warn!(
                pattern = other,
                "ignoring unsupported EWWM_DRM_TEST_PATTERN"
            );
            return None;
        }
    };
    Some((pattern, color))
}

fn should_readback_drm_frame() -> bool {
    let enabled = env::var("EWWM_DRM_READBACK")
        .map(|value| !matches!(value.as_str(), "" | "0" | "false" | "FALSE" | "no" | "NO"))
        .unwrap_or(false);
    if !enabled {
        return false;
    }

    let max_frames = env::var("EWWM_DRM_READBACK_FRAMES")
        .ok()
        .and_then(|value| value.parse::<usize>().ok())
        .unwrap_or(4);
    DRM_READBACK_COUNT.fetch_add(1, Ordering::Relaxed) < max_frames
}

fn log_drm_frame_readback(
    renderer: &mut GlesRenderer,
    framebuffer: &GlesTarget<'_>,
    output: &Output,
    element_count: usize,
    damage_rects: usize,
    test_pattern: Option<&str>,
) {
    let size = framebuffer.size();
    if size.w <= 0 || size.h <= 0 {
        warn!(
            output = output.name(),
            ?size,
            "drm framebuffer readback skipped"
        );
        return;
    }

    let mapping = match renderer.copy_framebuffer(
        framebuffer,
        Rectangle::from_size(size),
        Fourcc::Abgr8888,
    ) {
        Ok(mapping) => mapping,
        Err(err) => {
            warn!(
                output = output.name(),
                error = ?err,
                "drm framebuffer readback copy failed"
            );
            return;
        }
    };

    let bytes = match renderer.map_texture(&mapping) {
        Ok(bytes) => bytes,
        Err(err) => {
            warn!(
                output = output.name(),
                error = ?err,
                "drm framebuffer readback map failed"
            );
            return;
        }
    };

    let summary = PixelSummary::new(bytes, size.w as usize, size.h as usize);
    info!(
        output = output.name(),
        width = size.w,
        height = size.h,
        element_count,
        damage_rects,
        test_pattern,
        bytes = bytes.len(),
        nonzero_bytes = summary.nonzero_bytes,
        hash = format_args!("{:016x}", summary.hash),
        first_pixel = ?summary.first_pixel,
        center_pixel = ?summary.center_pixel,
        mean_abgr = ?summary.mean_abgr,
        "drm framebuffer readback"
    );
}

struct PixelSummary {
    hash: u64,
    nonzero_bytes: usize,
    first_pixel: [u8; 4],
    center_pixel: [u8; 4],
    mean_abgr: [u8; 4],
}

impl PixelSummary {
    fn new(bytes: &[u8], width: usize, height: usize) -> Self {
        let mut hash = 0xcbf29ce484222325u64;
        let mut nonzero_bytes = 0usize;
        for byte in bytes {
            if *byte != 0 {
                nonzero_bytes += 1;
            }
            hash ^= u64::from(*byte);
            hash = hash.wrapping_mul(0x100000001b3);
        }

        let first_pixel = pixel_at(bytes, 0);
        let center = height.saturating_div(2).saturating_mul(width) + width.saturating_div(2);
        let center_pixel = pixel_at(bytes, center);

        let pixel_count = bytes.len() / 4;
        let stride = pixel_count.saturating_div(4096).max(1);
        let mut sum = [0u64; 4];
        let mut samples = 0u64;
        for pixel in (0..pixel_count).step_by(stride) {
            let value = pixel_at(bytes, pixel);
            for channel in 0..4 {
                sum[channel] += u64::from(value[channel]);
            }
            samples += 1;
        }
        let mean_abgr = if samples == 0 {
            [0, 0, 0, 0]
        } else {
            [
                (sum[0] / samples) as u8,
                (sum[1] / samples) as u8,
                (sum[2] / samples) as u8,
                (sum[3] / samples) as u8,
            ]
        };

        Self {
            hash,
            nonzero_bytes,
            first_pixel,
            center_pixel,
            mean_abgr,
        }
    }
}

fn pixel_at(bytes: &[u8], pixel: usize) -> [u8; 4] {
    let offset = pixel.saturating_mul(4);
    if offset + 4 <= bytes.len() {
        [
            bytes[offset],
            bytes[offset + 1],
            bytes[offset + 2],
            bytes[offset + 3],
        ]
    } else {
        [0, 0, 0, 0]
    }
}

// ---------------------------------------------------------------------------
// Shared helpers
// ---------------------------------------------------------------------------

/// Send `wl_surface::frame` callbacks to every mapped window and
/// update primary scan-out output bookkeeping.
///
/// If `render_states` is available (from a successful render pass),
/// we update each surface's primary scan-out output so that
/// presentation feedback and DMA-BUF hints work correctly.
fn send_frame_callbacks(
    space: &Space<Window>,
    output: &Output,
    render_states: Option<&smithay::backend::renderer::element::RenderElementStates>,
) {
    // Timestamp for the frame callback (clients use this for
    // animation pacing).  In a real VSync-aware pipeline this would
    // come from the presentation clock; Duration::ZERO tells clients
    // "presented now".
    let frame_time = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or(Duration::ZERO);

    // Throttle: skip sending frame callbacks to surfaces that
    // received one less than ~8 ms ago (half a 60 Hz frame).
    let throttle = Some(Duration::from_millis(8));

    for window in space.elements() {
        // Update primary scan-out tracking when render states are
        // available.  This must happen *before* send_frame so that
        // the surface_primary_scanout_output closure returns the
        // correct output.
        if let Some(states) = render_states {
            window.with_surfaces(|surface, surface_data| {
                update_surface_primary_scanout_output(
                    surface,
                    output,
                    surface_data,
                    states,
                    // compare: pick whichever output has the element
                    // rendered (we only have one output right now, so
                    // just return the first candidate).
                    |current, _current_state, _next, _next_state| current,
                );
            });
        }

        window.send_frame(output, frame_time, throttle, surface_primary_scanout_output);
    }
}
