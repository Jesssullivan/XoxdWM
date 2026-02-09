//! Synthetic wl_keyboard.key event injection via IPC for secure credential entry.
//!
//! Provides an `AutoTypeManager` state machine that splits text into individual
//! characters, yields one `AutoTypeEvent` per tick, and supports pause/resume
//! and abort.  Remaining text is zeroed on drop to limit credential exposure.

use tracing::{debug, info, warn, error};

// ── AutoTypeConfig ──────────────────────────────────────────

/// Configuration for auto-type keystroke injection.
#[derive(Debug, Clone)]
pub struct AutoTypeConfig {
    /// Delay in milliseconds between consecutive keystrokes.
    pub delay_ms: u64,
    /// Whether to verify the target surface before each keystroke.
    pub verify_surface: bool,
    /// Maximum number of characters permitted in a single auto-type request.
    pub max_chars: usize,
}

impl Default for AutoTypeConfig {
    fn default() -> Self {
        Self {
            delay_ms: 10,
            verify_surface: true,
            max_chars: 256,
        }
    }
}

// ── PauseReason ─────────────────────────────────────────────

/// Reason why an auto-type sequence was paused.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PauseReason {
    /// User gaze moved away from the target surface.
    GazeAway,
    /// User explicitly requested a pause.
    UserRequested,
}

impl PauseReason {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::GazeAway => "gaze-away",
            Self::UserRequested => "user-requested",
        }
    }
}

// ── AutoTypePhase ───────────────────────────────────────────

/// State machine phases for an auto-type sequence.
#[derive(Debug, Clone)]
pub enum AutoTypePhase {
    /// No active auto-type sequence.
    Idle,
    /// Actively typing characters.
    Typing {
        remaining: Vec<char>,
        surface_id: u64,
        typed: usize,
    },
    /// Typing paused for a specific reason.
    Paused {
        remaining: Vec<char>,
        surface_id: u64,
        typed: usize,
        reason: PauseReason,
    },
    /// Typing completed successfully.
    Complete {
        surface_id: u64,
        chars_typed: usize,
    },
    /// An error occurred during typing.
    Error {
        message: String,
    },
}

// ── AutoTypeEvent ───────────────────────────────────────────

/// Events emitted by the auto-type state machine.
#[derive(Debug, Clone)]
pub enum AutoTypeEvent {
    /// A single character was typed to the target surface.
    CharTyped { ch: char, surface_id: u64 },
    /// Typing was paused.
    Paused {
        reason: PauseReason,
        surface_id: u64,
        chars_remaining: usize,
    },
    /// Typing was resumed from a paused state.
    Resumed { surface_id: u64 },
    /// All characters were typed successfully.
    Completed { surface_id: u64, chars_typed: usize },
    /// Typing was aborted before completion.
    Aborted { surface_id: u64, chars_typed: usize },
}

impl AutoTypeEvent {
    /// Convert the event to an IPC s-expression.
    pub fn to_sexp(&self) -> String {
        match self {
            Self::CharTyped { ch, surface_id } => {
                format!(
                    "(:type :event :event :autotype-char :surface-id {} :char \"{}\")",
                    surface_id,
                    ch.escape_default()
                )
            }
            Self::Paused {
                reason,
                surface_id,
                chars_remaining,
            } => {
                format!(
                    "(:type :event :event :autotype-paused :surface-id {} :reason :{} :chars-remaining {})",
                    surface_id,
                    reason.as_str(),
                    chars_remaining
                )
            }
            Self::Resumed { surface_id } => {
                format!(
                    "(:type :event :event :autotype-resumed :surface-id {})",
                    surface_id
                )
            }
            Self::Completed {
                surface_id,
                chars_typed,
            } => {
                format!(
                    "(:type :event :event :autotype-complete :surface-id {} :chars-typed {})",
                    surface_id, chars_typed
                )
            }
            Self::Aborted {
                surface_id,
                chars_typed,
            } => {
                format!(
                    "(:type :event :event :autotype-aborted :surface-id {} :chars-typed {})",
                    surface_id, chars_typed
                )
            }
        }
    }
}

// ── AutoTypeManager ─────────────────────────────────────────

/// Manages a synthetic keystroke injection sequence.
///
/// Call `start_typing` to begin, then `tick` once per delay interval.
/// Supports pause/resume via gaze tracking and explicit user control.
/// Remaining text is zeroed on drop for security.
pub struct AutoTypeManager {
    /// Configuration for timing and safety limits.
    pub config: AutoTypeConfig,
    /// Current phase of the auto-type state machine.
    pub phase: AutoTypePhase,
    /// When true, character events are not logged (for credential entry).
    pub secure_mode: bool,
}

impl AutoTypeManager {
    /// Create a new manager with default configuration.
    pub fn new() -> Self {
        info!("AutoType manager initialized");
        Self {
            config: AutoTypeConfig::default(),
            phase: AutoTypePhase::Idle,
            secure_mode: false,
        }
    }

    /// Begin an auto-type sequence, splitting `text` into characters.
    ///
    /// Returns an error string if the text exceeds `max_chars` or if a
    /// sequence is already in progress.
    pub fn start_typing(&mut self, text: &str, surface_id: u64) -> Result<(), String> {
        // Reject if already typing
        if matches!(self.phase, AutoTypePhase::Typing { .. } | AutoTypePhase::Paused { .. }) {
            let msg = "auto-type sequence already in progress".to_string();
            warn!("{}", msg);
            return Err(msg);
        }

        let chars: Vec<char> = text.chars().collect();

        if chars.is_empty() {
            let msg = "empty text provided to auto-type".to_string();
            warn!("{}", msg);
            return Err(msg);
        }

        if chars.len() > self.config.max_chars {
            let msg = format!(
                "text length {} exceeds max_chars limit {}",
                chars.len(),
                self.config.max_chars
            );
            error!("{}", msg);
            self.phase = AutoTypePhase::Error {
                message: msg.clone(),
            };
            return Err(msg);
        }

        info!(
            "AutoType started: {} chars -> surface {}{}",
            chars.len(),
            surface_id,
            if self.secure_mode { " [secure]" } else { "" }
        );

        self.phase = AutoTypePhase::Typing {
            remaining: chars,
            surface_id,
            typed: 0,
        };

        Ok(())
    }

    /// Advance one character in the auto-type sequence.
    ///
    /// Returns `Some(AutoTypeEvent)` when a state transition occurs, or
    /// `None` when idle or paused.
    pub fn tick(&mut self) -> Option<AutoTypeEvent> {
        match &mut self.phase {
            AutoTypePhase::Typing {
                remaining,
                surface_id,
                typed,
            } => {
                if remaining.is_empty() {
                    // All characters consumed — transition to Complete.
                    let sid = *surface_id;
                    let total = *typed;
                    info!("AutoType completed: {} chars on surface {}", total, sid);
                    self.phase = AutoTypePhase::Complete {
                        surface_id: sid,
                        chars_typed: total,
                    };
                    return Some(AutoTypeEvent::Completed {
                        surface_id: sid,
                        chars_typed: total,
                    });
                }

                let ch = remaining.remove(0);
                *typed += 1;
                let sid = *surface_id;

                if !self.secure_mode {
                    debug!("AutoType char: surface {} typed={}", sid, typed);
                }

                Some(AutoTypeEvent::CharTyped {
                    ch,
                    surface_id: sid,
                })
            }
            AutoTypePhase::Idle
            | AutoTypePhase::Paused { .. }
            | AutoTypePhase::Complete { .. }
            | AutoTypePhase::Error { .. } => None,
        }
    }

    /// Pause the current auto-type sequence.
    pub fn pause(&mut self, reason: PauseReason) {
        if let AutoTypePhase::Typing {
            remaining,
            surface_id,
            typed,
        } = &self.phase
        {
            let remaining = remaining.clone();
            let surface_id = *surface_id;
            let typed = *typed;
            let chars_remaining = remaining.len();

            info!(
                "AutoType paused: reason={} surface={} remaining={}",
                reason.as_str(),
                surface_id,
                chars_remaining
            );

            self.phase = AutoTypePhase::Paused {
                remaining,
                surface_id,
                typed,
                reason,
            };
        } else {
            warn!("AutoType pause requested but not currently typing");
        }
    }

    /// Resume a paused auto-type sequence.
    pub fn resume(&mut self) {
        if let AutoTypePhase::Paused {
            remaining,
            surface_id,
            typed,
            ..
        } = &self.phase
        {
            let remaining = remaining.clone();
            let surface_id = *surface_id;
            let typed = *typed;

            info!(
                "AutoType resumed: surface={} remaining={}",
                surface_id,
                remaining.len()
            );

            self.phase = AutoTypePhase::Typing {
                remaining,
                surface_id,
                typed,
            };
        } else {
            warn!("AutoType resume requested but not currently paused");
        }
    }

    /// Abort the current auto-type sequence and clear remaining text.
    pub fn abort(&mut self) {
        match &self.phase {
            AutoTypePhase::Typing {
                surface_id, typed, ..
            }
            | AutoTypePhase::Paused {
                surface_id, typed, ..
            } => {
                let sid = *surface_id;
                let total = *typed;
                warn!("AutoType aborted: surface={} typed={}", sid, total);
                self.clear_sensitive();
                self.phase = AutoTypePhase::Idle;
            }
            _ => {
                debug!("AutoType abort: no active sequence");
                self.phase = AutoTypePhase::Idle;
            }
        }
    }

    /// Generate an IPC status s-expression for the current state.
    pub fn status_sexp(&self) -> String {
        let phase_str = match &self.phase {
            AutoTypePhase::Idle => "idle".to_string(),
            AutoTypePhase::Typing {
                surface_id,
                typed,
                remaining,
            } => {
                format!(
                    "typing :surface-id {} :typed {} :remaining {}",
                    surface_id,
                    typed,
                    remaining.len()
                )
            }
            AutoTypePhase::Paused {
                surface_id,
                typed,
                remaining,
                reason,
            } => {
                format!(
                    "paused :surface-id {} :typed {} :remaining {} :reason :{}",
                    surface_id,
                    typed,
                    remaining.len(),
                    reason.as_str()
                )
            }
            AutoTypePhase::Complete {
                surface_id,
                chars_typed,
            } => {
                format!("complete :surface-id {} :chars-typed {}", surface_id, chars_typed)
            }
            AutoTypePhase::Error { message } => {
                format!("error :message \"{}\"", message)
            }
        };
        format!(
            "(:phase {} :delay-ms {} :verify-surface {} :max-chars {} :secure {})",
            phase_str,
            self.config.delay_ms,
            if self.config.verify_surface { "t" } else { "nil" },
            self.config.max_chars,
            if self.secure_mode { "t" } else { "nil" },
        )
    }

    /// Generate an IPC s-expression for a completed auto-type.
    pub fn complete_sexp(surface_id: u64, chars_typed: usize) -> String {
        format!(
            "(:type :event :event :autotype-complete :surface-id {} :chars-typed {})",
            surface_id, chars_typed
        )
    }

    /// Zero out any remaining text in memory to limit credential exposure.
    pub fn clear_sensitive(&mut self) {
        match &mut self.phase {
            AutoTypePhase::Typing { remaining, .. }
            | AutoTypePhase::Paused { remaining, .. } => {
                for ch in remaining.iter_mut() {
                    *ch = '\0';
                }
                remaining.clear();
                debug!("AutoType: sensitive data cleared");
            }
            _ => {}
        }
    }
}

impl Drop for AutoTypeManager {
    fn drop(&mut self) {
        self.clear_sensitive();
    }
}

// ── Tests ───────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_defaults() {
        let mgr = AutoTypeManager::new();
        assert!(matches!(mgr.phase, AutoTypePhase::Idle));
        assert_eq!(mgr.config.delay_ms, 10);
        assert!(mgr.config.verify_surface);
        assert_eq!(mgr.config.max_chars, 256);
        assert!(!mgr.secure_mode);
    }

    #[test]
    fn test_start_typing() {
        let mut mgr = AutoTypeManager::new();
        let result = mgr.start_typing("hello", 42);
        assert!(result.is_ok());
        match &mgr.phase {
            AutoTypePhase::Typing {
                remaining,
                surface_id,
                typed,
            } => {
                assert_eq!(*surface_id, 42);
                assert_eq!(*typed, 0);
                assert_eq!(remaining.len(), 5);
                assert_eq!(remaining[0], 'h');
            }
            _ => panic!("Expected Typing phase after start_typing"),
        }
    }

    #[test]
    fn test_tick_advances() {
        let mut mgr = AutoTypeManager::new();
        mgr.start_typing("ab", 1).unwrap();

        // First tick: type 'a'
        let evt = mgr.tick();
        assert!(matches!(
            evt,
            Some(AutoTypeEvent::CharTyped { ch: 'a', surface_id: 1 })
        ));

        // Verify state
        match &mgr.phase {
            AutoTypePhase::Typing {
                remaining, typed, ..
            } => {
                assert_eq!(*typed, 1);
                assert_eq!(remaining.len(), 1);
                assert_eq!(remaining[0], 'b');
            }
            _ => panic!("Expected Typing phase"),
        }

        // Second tick: type 'b'
        let evt = mgr.tick();
        assert!(matches!(
            evt,
            Some(AutoTypeEvent::CharTyped { ch: 'b', surface_id: 1 })
        ));
    }

    #[test]
    fn test_pause_resume() {
        let mut mgr = AutoTypeManager::new();
        mgr.start_typing("abc", 10).unwrap();

        // Type one character
        mgr.tick();

        // Pause
        mgr.pause(PauseReason::GazeAway);
        assert!(matches!(
            mgr.phase,
            AutoTypePhase::Paused {
                typed: 1,
                ..
            }
        ));

        // Tick while paused should return None
        assert!(mgr.tick().is_none());

        // Resume
        mgr.resume();
        assert!(matches!(mgr.phase, AutoTypePhase::Typing { typed: 1, .. }));

        // Tick should continue
        let evt = mgr.tick();
        assert!(matches!(
            evt,
            Some(AutoTypeEvent::CharTyped { ch: 'b', .. })
        ));
    }

    #[test]
    fn test_abort_clears() {
        let mut mgr = AutoTypeManager::new();
        mgr.start_typing("secret", 5).unwrap();

        // Type two characters
        mgr.tick();
        mgr.tick();

        // Abort
        mgr.abort();
        assert!(matches!(mgr.phase, AutoTypePhase::Idle));

        // Further ticks should return None
        assert!(mgr.tick().is_none());
    }

    #[test]
    fn test_complete_after_all_chars() {
        let mut mgr = AutoTypeManager::new();
        mgr.start_typing("xy", 99).unwrap();

        // Type all characters
        let evt1 = mgr.tick();
        assert!(matches!(
            evt1,
            Some(AutoTypeEvent::CharTyped { ch: 'x', .. })
        ));

        let evt2 = mgr.tick();
        assert!(matches!(
            evt2,
            Some(AutoTypeEvent::CharTyped { ch: 'y', .. })
        ));

        // Next tick should complete
        let evt3 = mgr.tick();
        assert!(matches!(
            evt3,
            Some(AutoTypeEvent::Completed {
                surface_id: 99,
                chars_typed: 2
            })
        ));

        // Now in Complete phase; further ticks return None
        assert!(mgr.tick().is_none());
    }

    #[test]
    fn test_max_chars_limit() {
        let mut mgr = AutoTypeManager::new();
        mgr.config.max_chars = 5;

        // Text within limit
        assert!(mgr.start_typing("hello", 1).is_ok());
        mgr.abort();

        // Text exceeding limit
        let result = mgr.start_typing("toolong", 1);
        assert!(result.is_err());
        assert!(matches!(mgr.phase, AutoTypePhase::Error { .. }));
    }

    #[test]
    fn test_status_sexp_format() {
        let mgr = AutoTypeManager::new();
        let sexp = mgr.status_sexp();
        assert!(sexp.contains(":phase idle"));
        assert!(sexp.contains(":delay-ms 10"));
        assert!(sexp.contains(":verify-surface t"));
        assert!(sexp.contains(":max-chars 256"));
        assert!(sexp.contains(":secure nil"));

        // Test with active typing
        let mut mgr2 = AutoTypeManager::new();
        mgr2.start_typing("test", 7).unwrap();
        let sexp2 = mgr2.status_sexp();
        assert!(sexp2.contains(":phase typing"));
        assert!(sexp2.contains(":surface-id 7"));
        assert!(sexp2.contains(":remaining 4"));
    }

    #[test]
    fn test_start_typing_rejects_empty() {
        let mut mgr = AutoTypeManager::new();
        let result = mgr.start_typing("", 1);
        assert!(result.is_err());
    }

    #[test]
    fn test_start_typing_rejects_while_active() {
        let mut mgr = AutoTypeManager::new();
        mgr.start_typing("abc", 1).unwrap();
        let result = mgr.start_typing("def", 2);
        assert!(result.is_err());
    }

    #[test]
    fn test_event_to_sexp() {
        let evt = AutoTypeEvent::Completed {
            surface_id: 42,
            chars_typed: 10,
        };
        let sexp = evt.to_sexp();
        assert!(sexp.contains(":event :autotype-complete"));
        assert!(sexp.contains(":surface-id 42"));
        assert!(sexp.contains(":chars-typed 10"));

        let evt2 = AutoTypeEvent::Paused {
            reason: PauseReason::GazeAway,
            surface_id: 5,
            chars_remaining: 3,
        };
        let sexp2 = evt2.to_sexp();
        assert!(sexp2.contains(":event :autotype-paused"));
        assert!(sexp2.contains(":reason :gaze-away"));
        assert!(sexp2.contains(":chars-remaining 3"));

        let evt3 = AutoTypeEvent::Aborted {
            surface_id: 7,
            chars_typed: 2,
        };
        let sexp3 = evt3.to_sexp();
        assert!(sexp3.contains(":event :autotype-aborted"));
        assert!(sexp3.contains(":chars-typed 2"));
    }

    #[test]
    fn test_complete_sexp_static() {
        let sexp = AutoTypeManager::complete_sexp(100, 50);
        assert!(sexp.contains(":event :autotype-complete"));
        assert!(sexp.contains(":surface-id 100"));
        assert!(sexp.contains(":chars-typed 50"));
    }

    #[test]
    fn test_clear_sensitive_zeros_remaining() {
        let mut mgr = AutoTypeManager::new();
        mgr.start_typing("secret", 1).unwrap();
        mgr.tick(); // type 's'

        mgr.clear_sensitive();

        // Remaining should be empty after clear
        match &mgr.phase {
            AutoTypePhase::Typing { remaining, .. } => {
                assert!(remaining.is_empty());
            }
            _ => panic!("Expected Typing phase"),
        }
    }

    #[test]
    fn test_pause_reason_as_str() {
        assert_eq!(PauseReason::GazeAway.as_str(), "gaze-away");
        assert_eq!(PauseReason::UserRequested.as_str(), "user-requested");
    }
}
