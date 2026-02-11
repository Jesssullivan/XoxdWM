//! Gaze-triggered scroll events for surfaces.
//!
//! When gaze enters edge zones of a surface, emit scroll events
//! proportional to distance from center.

use tracing::debug;

// ── Config ─────────────────────────────────────────────────

/// Configuration for gaze-based scrolling.
#[derive(Debug, Clone)]
pub struct GazeScrollConfig {
    /// Enable gaze scrolling.
    pub enabled: bool,
    /// Edge zone as fraction of surface (0.0-0.5).
    pub edge_pct: f32,
    /// Speed multiplier.
    pub speed: f32,
    /// Minimum dwell time in edge zone before scroll starts (ms).
    pub activation_delay_ms: f64,
    /// Horizontal scrolling enabled.
    pub horizontal: bool,
}

impl Default for GazeScrollConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            edge_pct: 0.10,
            speed: 5.0,
            activation_delay_ms: 200.0,
            horizontal: true,
        }
    }
}

// ── Scroll event ───────────────────────────────────────────

/// Scroll direction and magnitude.
#[derive(Debug, Clone, PartialEq)]
pub enum ScrollEvent {
    None,
    Vertical { delta: f32 },
    Horizontal { delta: f32 },
    Both { dx: f32, dy: f32 },
}

// ── Edge zone ──────────────────────────────────────────────

/// Which edge zone the gaze is currently in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EdgeZone {
    Top,
    Bottom,
    Left,
    Right,
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}

impl EdgeZone {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Top => "top",
            Self::Bottom => "bottom",
            Self::Left => "left",
            Self::Right => "right",
            Self::TopLeft => "top-left",
            Self::TopRight => "top-right",
            Self::BottomLeft => "bottom-left",
            Self::BottomRight => "bottom-right",
        }
    }
}

// ── State ──────────────────────────────────────────────────

/// State tracking for gaze scroll.
pub struct GazeScrollState {
    pub config: GazeScrollConfig,
    /// Time spent in current edge zone (ms).
    pub dwell_in_edge_ms: f64,
    /// Current edge zone (None if in center).
    pub active_zone: Option<EdgeZone>,
    /// Surface being scrolled.
    pub surface_id: Option<u64>,
}

impl GazeScrollState {
    pub fn new() -> Self {
        Self {
            config: GazeScrollConfig::default(),
            dwell_in_edge_ms: 0.0,
            active_zone: None,
            surface_id: None,
        }
    }

    /// Update scroll state with new gaze position.
    /// Returns scroll event to emit (or None).
    /// gaze_x, gaze_y are in surface-local coordinates [0,1].
    pub fn update(
        &mut self,
        gaze_x: f32,
        gaze_y: f32,
        surface_id: u64,
        dt_ms: f64,
    ) -> ScrollEvent {
        if !self.config.enabled {
            return ScrollEvent::None;
        }

        // Surface changed — reset
        if self.surface_id != Some(surface_id) {
            self.reset();
            self.surface_id = Some(surface_id);
        }

        let edge = self.config.edge_pct;
        let in_top = gaze_y < edge;
        let in_bottom = gaze_y > (1.0 - edge);
        let in_left = self.config.horizontal && gaze_x < edge;
        let in_right = self.config.horizontal && gaze_x > (1.0 - edge);

        let zone = match (in_top, in_bottom, in_left, in_right) {
            (true, _, true, _) => Some(EdgeZone::TopLeft),
            (true, _, _, true) => Some(EdgeZone::TopRight),
            (_, true, true, _) => Some(EdgeZone::BottomLeft),
            (_, true, _, true) => Some(EdgeZone::BottomRight),
            (true, _, _, _) => Some(EdgeZone::Top),
            (_, true, _, _) => Some(EdgeZone::Bottom),
            (_, _, true, _) => Some(EdgeZone::Left),
            (_, _, _, true) => Some(EdgeZone::Right),
            _ => None,
        };

        // Zone changed — reset dwell
        if zone != self.active_zone {
            self.dwell_in_edge_ms = 0.0;
            self.active_zone = zone;
            if zone.is_some() {
                debug!("Gaze scroll: entered {:?}", zone);
            }
            return ScrollEvent::None;
        }

        // Not in any edge zone
        let zone = match zone {
            Some(z) => z,
            None => return ScrollEvent::None,
        };

        // Accumulate dwell
        self.dwell_in_edge_ms += dt_ms;

        // Check activation delay
        if self.dwell_in_edge_ms < self.config.activation_delay_ms {
            return ScrollEvent::None;
        }

        // Calculate proportional speed
        let dy = self.compute_vertical_speed(gaze_y, &zone);
        let dx = self.compute_horizontal_speed(gaze_x, &zone);

        match (dx != 0.0, dy != 0.0) {
            (true, true) => ScrollEvent::Both { dx, dy },
            (false, true) => ScrollEvent::Vertical { delta: dy },
            (true, false) => ScrollEvent::Horizontal { delta: dx },
            (false, false) => ScrollEvent::None,
        }
    }

    /// Compute vertical scroll speed from gaze_y and edge zone.
    fn compute_vertical_speed(&self, gaze_y: f32, zone: &EdgeZone) -> f32 {
        let edge = self.config.edge_pct;
        match zone {
            EdgeZone::Top | EdgeZone::TopLeft | EdgeZone::TopRight => {
                // Distance from center boundary into edge zone
                let dist = (edge - gaze_y).max(0.0);
                let frac = (dist / edge).clamp(0.0, 1.0);
                -self.config.speed * frac // negative = scroll up
            }
            EdgeZone::Bottom | EdgeZone::BottomLeft | EdgeZone::BottomRight => {
                let dist = (gaze_y - (1.0 - edge)).max(0.0);
                let frac = (dist / edge).clamp(0.0, 1.0);
                self.config.speed * frac // positive = scroll down
            }
            _ => 0.0,
        }
    }

    /// Compute horizontal scroll speed from gaze_x and edge zone.
    fn compute_horizontal_speed(&self, gaze_x: f32, zone: &EdgeZone) -> f32 {
        if !self.config.horizontal {
            return 0.0;
        }
        let edge = self.config.edge_pct;
        match zone {
            EdgeZone::Left | EdgeZone::TopLeft | EdgeZone::BottomLeft => {
                let dist = (edge - gaze_x).max(0.0);
                let frac = (dist / edge).clamp(0.0, 1.0);
                -self.config.speed * frac // negative = scroll left
            }
            EdgeZone::Right | EdgeZone::TopRight | EdgeZone::BottomRight => {
                let dist = (gaze_x - (1.0 - edge)).max(0.0);
                let frac = (dist / edge).clamp(0.0, 1.0);
                self.config.speed * frac // positive = scroll right
            }
            _ => 0.0,
        }
    }

    /// Reset scroll state (gaze left surface).
    pub fn reset(&mut self) {
        self.dwell_in_edge_ms = 0.0;
        self.active_zone = None;
        self.surface_id = None;
    }

    /// Generate s-expression for IPC status.
    pub fn status_sexp(&self) -> String {
        let zone_str = self
            .active_zone
            .map(|z| z.as_str().to_string())
            .unwrap_or_else(|| "nil".to_string());
        let surface_str = self
            .surface_id
            .map(|id| id.to_string())
            .unwrap_or_else(|| "nil".to_string());
        format!(
            "(:enabled {} :edge-pct {:.2} :speed {:.1} :activation-delay-ms {:.0} :horizontal {} :zone {} :surface-id {} :dwell-ms {:.0})",
            if self.config.enabled { "t" } else { "nil" },
            self.config.edge_pct,
            self.config.speed,
            self.config.activation_delay_ms,
            if self.config.horizontal { "t" } else { "nil" },
            zone_str,
            surface_str,
            self.dwell_in_edge_ms,
        )
    }
}

// ── Tests ──────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_center_no_scroll() {
        let mut state = GazeScrollState::new();
        // Gaze at center (0.5, 0.5) — should not scroll
        let evt = state.update(0.5, 0.5, 1, 300.0);
        assert_eq!(evt, ScrollEvent::None);
        assert!(state.active_zone.is_none());
    }

    #[test]
    fn test_bottom_edge_scrolls_down() {
        let mut state = GazeScrollState::new();
        state.config.activation_delay_ms = 0.0; // no delay for testing
        // Gaze at bottom edge (0.5, 0.95) — edge_pct=0.10 so 0.90-1.0 is bottom
        let evt = state.update(0.5, 0.95, 1, 16.0);
        // First update enters the zone, second should scroll
        let evt = state.update(0.5, 0.95, 1, 16.0);
        match evt {
            ScrollEvent::Vertical { delta } => {
                assert!(delta > 0.0, "Bottom edge should scroll down (positive delta): {}", delta);
            }
            _ => panic!("Expected Vertical scroll event, got {:?}", evt),
        }
    }

    #[test]
    fn test_top_edge_scrolls_up() {
        let mut state = GazeScrollState::new();
        state.config.activation_delay_ms = 0.0;
        // Gaze at top edge (0.5, 0.05)
        let evt = state.update(0.5, 0.05, 1, 16.0);
        let evt = state.update(0.5, 0.05, 1, 16.0);
        match evt {
            ScrollEvent::Vertical { delta } => {
                assert!(delta < 0.0, "Top edge should scroll up (negative delta): {}", delta);
            }
            _ => panic!("Expected Vertical scroll event, got {:?}", evt),
        }
    }

    #[test]
    fn test_speed_proportional() {
        let mut state = GazeScrollState::new();
        state.config.activation_delay_ms = 0.0;

        // Near the boundary (gaze_y = 0.91, just barely in bottom edge)
        state.update(0.5, 0.91, 1, 16.0);
        let evt_near = state.update(0.5, 0.91, 1, 16.0);

        let mut state2 = GazeScrollState::new();
        state2.config.activation_delay_ms = 0.0;

        // Deep in the edge zone (gaze_y = 0.99)
        state2.update(0.5, 0.99, 1, 16.0);
        let evt_deep = state2.update(0.5, 0.99, 1, 16.0);

        let delta_near = match evt_near {
            ScrollEvent::Vertical { delta } => delta,
            _ => panic!("Expected Vertical for near edge"),
        };
        let delta_deep = match evt_deep {
            ScrollEvent::Vertical { delta } => delta,
            _ => panic!("Expected Vertical for deep edge"),
        };

        assert!(
            delta_deep.abs() > delta_near.abs(),
            "Deeper in edge should scroll faster: near={}, deep={}",
            delta_near, delta_deep
        );
    }

    #[test]
    fn test_activation_delay() {
        let mut state = GazeScrollState::new();
        state.config.activation_delay_ms = 200.0;

        // Enter bottom edge zone
        let evt = state.update(0.5, 0.95, 1, 16.0);
        assert_eq!(evt, ScrollEvent::None, "First frame enters zone");

        // Accumulate time but below threshold (16ms accumulated in edge)
        let evt = state.update(0.5, 0.95, 1, 16.0);
        assert_eq!(evt, ScrollEvent::None, "Should not scroll before activation delay");

        // Accumulate past threshold
        let evt = state.update(0.5, 0.95, 1, 200.0);
        assert_ne!(evt, ScrollEvent::None, "Should scroll after activation delay exceeded");
    }

    #[test]
    fn test_reset() {
        let mut state = GazeScrollState::new();
        state.config.activation_delay_ms = 0.0;

        // Build up some state
        state.update(0.5, 0.95, 1, 16.0);
        state.update(0.5, 0.95, 1, 16.0);
        assert!(state.active_zone.is_some());
        assert!(state.dwell_in_edge_ms > 0.0);

        // Reset
        state.reset();
        assert!(state.active_zone.is_none());
        assert_eq!(state.dwell_in_edge_ms, 0.0);
        assert!(state.surface_id.is_none());
    }

    #[test]
    fn test_surface_change_resets() {
        let mut state = GazeScrollState::new();
        state.config.activation_delay_ms = 0.0;

        // Build dwell on surface 1
        state.update(0.5, 0.95, 1, 16.0);
        state.update(0.5, 0.95, 1, 100.0);
        assert_eq!(state.surface_id, Some(1));

        // Switch to surface 2 — should reset
        let evt = state.update(0.5, 0.95, 2, 16.0);
        assert_eq!(state.surface_id, Some(2));
        // After reset, first frame in new surface just sets zone
        assert_eq!(evt, ScrollEvent::None);
    }

    #[test]
    fn test_disabled_no_scroll() {
        let mut state = GazeScrollState::new();
        state.config.enabled = false;
        state.config.activation_delay_ms = 0.0;

        let evt = state.update(0.5, 0.95, 1, 300.0);
        assert_eq!(evt, ScrollEvent::None);
    }

    #[test]
    fn test_status_sexp() {
        let state = GazeScrollState::new();
        let sexp = state.status_sexp();
        assert!(sexp.contains(":enabled t"));
        assert!(sexp.contains(":edge-pct 0.10"));
        assert!(sexp.contains(":speed 5.0"));
        assert!(sexp.contains(":zone nil"));
    }
}
