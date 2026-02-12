//! Secure input mode — temporarily disables gaze, wink, EEG, and fatigue
//! subsystems while the user is entering sensitive data (e.g. passwords).
//!
//! Provides auto-exit timeout, IPC s-expression serialization for Emacs
//! integration, and visual border indicator support.
//! No openxrs dependency; compiles unconditionally.

use tracing::{debug, info, warn};

// ── SecureInputConfig ──────────────────────────────────────

/// Configuration for secure input mode behavior.
#[derive(Debug, Clone)]
pub struct SecureInputConfig {
    /// Seconds before auto-exit from secure mode.
    pub auto_exit_timeout_secs: u64,
    /// Whether to show a visual border around the secure surface.
    pub show_border: bool,
    /// Border color as [R, G, B, A] floats (0.0–1.0).
    pub border_color: [f32; 4],
    /// Pause gaze tracking while in secure mode.
    pub pause_gaze: bool,
    /// Pause wink detection while in secure mode.
    pub pause_wink: bool,
    /// Pause EEG input while in secure mode.
    pub pause_eeg: bool,
    /// Pause fatigue metric logging while in secure mode.
    pub pause_fatigue_logging: bool,
}

impl Default for SecureInputConfig {
    fn default() -> Self {
        Self {
            auto_exit_timeout_secs: 30,
            show_border: true,
            border_color: [1.0, 0.0, 0.0, 0.8],
            pause_gaze: true,
            pause_wink: true,
            pause_eeg: true,
            pause_fatigue_logging: true,
        }
    }
}

// ── SecureInputEvent ───────────────────────────────────────

/// Events emitted by the secure input subsystem.
#[derive(Debug, Clone)]
pub enum SecureInputEvent {
    /// Secure input mode was activated.
    Entered { reason: String, surface_id: u64 },
    /// Secure input mode was deactivated.
    Exited { reason: String },
    /// Secure input mode timed out automatically.
    AutoExitTimeout,
}

impl SecureInputEvent {
    /// Convert the event to an IPC s-expression.
    pub fn to_sexp(&self) -> String {
        match self {
            Self::Entered { reason, surface_id } => {
                format!(
                    "(:type :event :event :secure-input-entered :reason \"{}\" :surface-id {})",
                    reason, surface_id,
                )
            }
            Self::Exited { reason } => {
                format!(
                    "(:type :event :event :secure-input-exited :reason \"{}\")",
                    reason,
                )
            }
            Self::AutoExitTimeout => {
                "(:type :event :event :secure-input-auto-exit-timeout)".to_string()
            }
        }
    }
}

// ── SecureInputState ───────────────────────────────────────

/// Central state for the secure input mode subsystem.
pub struct SecureInputState {
    /// Whether secure input mode is currently active.
    pub active: bool,
    /// Reason secure mode was entered (e.g. "read-passwd").
    pub reason: Option<String>,
    /// Surface that triggered secure mode, if any.
    pub surface_id: Option<u64>,
    /// Timestamp when secure mode was entered.
    pub entered_at: Option<std::time::Instant>,
    /// Configuration for secure input behavior.
    pub config: SecureInputConfig,
}

impl SecureInputState {
    /// Create a new inactive secure input state with default configuration.
    pub fn new() -> Self {
        info!("Secure input state initialized");
        Self {
            active: false,
            reason: None,
            surface_id: None,
            entered_at: None,
            config: SecureInputConfig::default(),
        }
    }

    /// Activate secure input mode for the given reason and surface.
    /// The `now` parameter should come from the compositor clock
    /// (`state.clock.now()`) so tests can control time.
    pub fn enter(
        &mut self,
        reason: &str,
        surface_id: u64,
        now: std::time::Instant,
    ) {
        if self.active {
            debug!(
                "Secure input mode re-entered: reason=\"{}\" surface_id={}",
                reason, surface_id
            );
        } else {
            info!(
                "Secure input mode entered: reason=\"{}\" surface_id={}",
                reason, surface_id
            );
        }
        self.active = true;
        self.reason = Some(reason.to_string());
        self.surface_id = Some(surface_id);
        self.entered_at = Some(now);
    }

    /// Deactivate secure input mode and clear all state.
    pub fn exit(&mut self) {
        if self.active {
            info!(
                "Secure input mode exited (was reason=\"{}\")",
                self.reason.as_deref().unwrap_or("unknown")
            );
        } else {
            debug!("Secure input exit called while already inactive");
        }
        self.active = false;
        self.reason = None;
        self.surface_id = None;
        self.entered_at = None;
    }

    /// Check whether the auto-exit timeout has been reached.
    /// Pass `clock.now()` as `now` for deterministic testing.
    pub fn is_expired(&self, now: std::time::Instant) -> bool {
        if !self.active {
            return false;
        }
        if let Some(entered) = self.entered_at {
            let elapsed = now.duration_since(entered).as_secs();
            elapsed >= self.config.auto_exit_timeout_secs
        } else {
            false
        }
    }

    /// Tick the secure input state.  If the timeout has expired,
    /// deactivate and return
    /// `Some(SecureInputEvent::AutoExitTimeout)`.
    pub fn tick(&mut self, now: std::time::Instant) -> Option<SecureInputEvent> {
        if self.is_expired(now) {
            warn!(
                "Secure input mode auto-exit: timeout after {}s",
                self.config.auto_exit_timeout_secs
            );
            self.exit();
            Some(SecureInputEvent::AutoExitTimeout)
        } else {
            None
        }
    }

    /// Generate IPC status s-expression.
    /// Pass `clock.now()` as `now` for deterministic testing.
    pub fn status_sexp(&self, now: std::time::Instant) -> String {
        format!(
            "(:active {} :reason {} :surface-id {} :elapsed-s {} :timeout-s {})",
            if self.active { "t" } else { "nil" },
            match &self.reason {
                Some(r) => format!("\"{}\"", r),
                None => "nil".to_string(),
            },
            match self.surface_id {
                Some(id) => id.to_string(),
                None => "nil".to_string(),
            },
            match self.entered_at {
                Some(t) => {
                    format!("{}", now.duration_since(t).as_secs())
                }
                None => "0".to_string(),
            },
            self.config.auto_exit_timeout_secs,
        )
    }

    /// Generate IPC config s-expression.
    pub fn config_sexp(&self) -> String {
        let c = &self.config;
        format!(
            "(:auto-exit-timeout-secs {} :show-border {} :border-color ({:.2} {:.2} {:.2} {:.2}) :pause-gaze {} :pause-wink {} :pause-eeg {} :pause-fatigue-logging {})",
            c.auto_exit_timeout_secs,
            if c.show_border { "t" } else { "nil" },
            c.border_color[0],
            c.border_color[1],
            c.border_color[2],
            c.border_color[3],
            if c.pause_gaze { "t" } else { "nil" },
            if c.pause_wink { "t" } else { "nil" },
            if c.pause_eeg { "t" } else { "nil" },
            if c.pause_fatigue_logging { "t" } else { "nil" },
        )
    }

    /// Generate IPC s-expression for a secure-input-entered event.
    pub fn enter_event_sexp(&self) -> String {
        let reason = self.reason.as_deref().unwrap_or("unknown");
        let sid = self.surface_id.unwrap_or(0);
        SecureInputEvent::Entered {
            reason: reason.to_string(),
            surface_id: sid,
        }
        .to_sexp()
    }

    /// Generate IPC s-expression for a secure-input-exited event.
    pub fn exit_event_sexp(&self) -> String {
        let reason = self.reason.as_deref().unwrap_or("manual");
        SecureInputEvent::Exited {
            reason: reason.to_string(),
        }
        .to_sexp()
    }
}

// ── Tests ───────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Instant;

    #[test]
    fn test_default_inactive() {
        let state = SecureInputState::new();
        assert!(!state.active);
        assert!(state.reason.is_none());
        assert!(state.surface_id.is_none());
        assert!(state.entered_at.is_none());
    }

    #[test]
    fn test_enter_exit() {
        let mut state = SecureInputState::new();
        let now = Instant::now();

        state.enter("read-passwd", 42, now);
        assert!(state.active);
        assert_eq!(state.reason.as_deref(), Some("read-passwd"));
        assert_eq!(state.surface_id, Some(42));
        assert!(state.entered_at.is_some());

        state.exit();
        assert!(!state.active);
        assert!(state.reason.is_none());
        assert!(state.surface_id.is_none());
        assert!(state.entered_at.is_none());
    }

    #[test]
    fn test_auto_exit_timeout() {
        let mut state = SecureInputState::new();
        state.config.auto_exit_timeout_secs = 0; // immediate timeout

        let now = Instant::now();
        state.enter("test", 1, now);
        // With a 0-second timeout, it should be expired immediately
        assert!(state.is_expired(now));

        let event = state.tick(now);
        assert!(matches!(event, Some(SecureInputEvent::AutoExitTimeout)));
        assert!(!state.active);
    }

    #[test]
    fn test_status_sexp() {
        let now = Instant::now();
        let state = SecureInputState::new();
        let sexp = state.status_sexp(now);
        assert!(sexp.contains(":active nil"));
        assert!(sexp.contains(":reason nil"));
        assert!(sexp.contains(":surface-id nil"));
        assert!(sexp.contains(":timeout-s 30"));

        let mut state = SecureInputState::new();
        state.enter("pin-entry", 99, now);
        let sexp = state.status_sexp(now);
        assert!(sexp.contains(":active t"));
        assert!(sexp.contains(":reason \"pin-entry\""));
        assert!(sexp.contains(":surface-id 99"));
    }

    #[test]
    fn test_double_enter_updates() {
        let mut state = SecureInputState::new();
        let now = Instant::now();

        state.enter("first-reason", 10, now);
        assert_eq!(state.reason.as_deref(), Some("first-reason"));
        assert_eq!(state.surface_id, Some(10));

        // Second enter should update reason and surface
        state.enter("second-reason", 20, now);
        assert!(state.active);
        assert_eq!(state.reason.as_deref(), Some("second-reason"));
        assert_eq!(state.surface_id, Some(20));
        // entered_at should be refreshed
        assert!(state.entered_at.is_some());
    }

    #[test]
    fn test_config_defaults() {
        let config = SecureInputConfig::default();
        assert_eq!(config.auto_exit_timeout_secs, 30);
        assert!(config.show_border);
        assert!((config.border_color[0] - 1.0).abs() < f32::EPSILON);
        assert!((config.border_color[1] - 0.0).abs() < f32::EPSILON);
        assert!((config.border_color[2] - 0.0).abs() < f32::EPSILON);
        assert!((config.border_color[3] - 0.8).abs() < f32::EPSILON);
        assert!(config.pause_gaze);
        assert!(config.pause_wink);
        assert!(config.pause_eeg);
        assert!(config.pause_fatigue_logging);
    }

    #[test]
    fn test_config_sexp() {
        let state = SecureInputState::new();
        let sexp = state.config_sexp();
        assert!(sexp.contains(":auto-exit-timeout-secs 30"));
        assert!(sexp.contains(":show-border t"));
        assert!(sexp.contains(":pause-gaze t"));
        assert!(sexp.contains(":pause-wink t"));
        assert!(sexp.contains(":pause-eeg t"));
        assert!(sexp.contains(":pause-fatigue-logging t"));
    }

    #[test]
    fn test_event_sexp_format() {
        let entered = SecureInputEvent::Entered {
            reason: "read-passwd".to_string(),
            surface_id: 42,
        };
        let sexp = entered.to_sexp();
        assert!(sexp.contains(":event :secure-input-entered"));
        assert!(sexp.contains(":reason \"read-passwd\""));
        assert!(sexp.contains(":surface-id 42"));

        let exited = SecureInputEvent::Exited {
            reason: "manual".to_string(),
        };
        let sexp = exited.to_sexp();
        assert!(sexp.contains(":event :secure-input-exited"));
        assert!(sexp.contains(":reason \"manual\""));

        let timeout = SecureInputEvent::AutoExitTimeout;
        let sexp = timeout.to_sexp();
        assert!(sexp.contains(":event :secure-input-auto-exit-timeout"));
    }

    #[test]
    fn test_enter_exit_event_sexp() {
        let mut state = SecureInputState::new();
        let now = Instant::now();
        state.enter("sudo", 55, now);

        let enter_sexp = state.enter_event_sexp();
        assert!(enter_sexp.contains(":secure-input-entered"));
        assert!(enter_sexp.contains(":reason \"sudo\""));
        assert!(enter_sexp.contains(":surface-id 55"));

        let exit_sexp = state.exit_event_sexp();
        assert!(exit_sexp.contains(":secure-input-exited"));
        assert!(exit_sexp.contains(":reason \"sudo\""));
    }

    #[test]
    fn test_tick_no_event_when_inactive() {
        let mut state = SecureInputState::new();
        let now = Instant::now();
        let event = state.tick(now);
        assert!(event.is_none());
    }

    #[test]
    fn test_is_expired_when_inactive() {
        let state = SecureInputState::new();
        let now = Instant::now();
        assert!(!state.is_expired(now));
    }
}
