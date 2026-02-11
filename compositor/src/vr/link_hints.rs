//! Link hint overlay for gaze-based link following.
//!
//! Renders hint labels on clickable elements.  Gaze dwell on hint
//! highlights it; confirmation action follows the link.

use tracing::{debug, info};

// ── Hint data ──────────────────────────────────────────────

/// A single hint entry from userscript.
#[derive(Debug, Clone)]
pub struct LinkHint {
    pub id: u32,
    pub text: String,
    pub url: String,
    pub rect: HintRect,
    pub label: String,
}

/// Bounding rectangle for a hint (surface-local, normalized 0-1).
#[derive(Debug, Clone)]
pub struct HintRect {
    pub x: f32,
    pub y: f32,
    pub w: f32,
    pub h: f32,
}

impl HintRect {
    /// Return whether the point (px, py) is inside this rectangle.
    pub fn contains(&self, px: f32, py: f32) -> bool {
        px >= self.x && px <= self.x + self.w && py >= self.y && py <= self.y + self.h
    }
}

// ── Events ─────────────────────────────────────────────────

/// Events emitted by the link hint overlay.
#[derive(Debug, Clone, PartialEq)]
pub enum LinkHintEvent {
    /// Hint highlighted by gaze.
    Highlighted {
        hint_id: u32,
        text: String,
        url: String,
    },
    /// Hint confirmed (dwell or explicit).
    Confirmed { hint_id: u32, url: String },
    /// Hints cleared.
    Cleared,
}

// ── State ──────────────────────────────────────────────────

/// State for link hint overlay.
pub struct LinkHintState {
    pub active: bool,
    pub surface_id: Option<u64>,
    pub hints: Vec<LinkHint>,
    pub focused_hint: Option<u32>,
    pub dwell_ms: f64,
    pub confirm_dwell_ms: f64,
    pub highlight_dwell_ms: f64,
}

impl LinkHintState {
    pub fn new() -> Self {
        Self {
            active: false,
            surface_id: None,
            hints: Vec::new(),
            focused_hint: None,
            dwell_ms: 0.0,
            confirm_dwell_ms: 800.0,
            highlight_dwell_ms: 300.0,
        }
    }

    /// Load hints from JSON (from userscript).
    ///
    /// Expected format: `[{"text":"...","url":"...","x":0.1,"y":0.2,"w":0.05,"h":0.03}, ...]`
    pub fn load_hints(&mut self, json: &str, surface_id: u64) -> Result<usize, String> {
        // Minimal JSON array parser (avoids serde dependency for this module)
        let trimmed = json.trim();
        if !trimmed.starts_with('[') || !trimmed.ends_with(']') {
            return Err("expected JSON array".to_string());
        }

        let inner = &trimmed[1..trimmed.len() - 1];
        let mut hints = Vec::new();
        let mut id_counter = 0u32;

        // Split by top-level objects (handle nested braces)
        for obj_str in split_json_objects(inner) {
            let text = extract_json_string(&obj_str, "text").unwrap_or_default();
            let url = extract_json_string(&obj_str, "url").unwrap_or_default();
            let x = extract_json_number(&obj_str, "x").unwrap_or(0.0);
            let y = extract_json_number(&obj_str, "y").unwrap_or(0.0);
            let w = extract_json_number(&obj_str, "w").unwrap_or(0.0);
            let h = extract_json_number(&obj_str, "h").unwrap_or(0.0);

            hints.push(LinkHint {
                id: id_counter,
                text,
                url,
                rect: HintRect { x, y, w, h },
                label: String::new(), // assigned below
            });
            id_counter += 1;
        }

        if hints.is_empty() {
            return Err("no hints parsed from JSON".to_string());
        }

        // Generate labels
        let labels = Self::generate_labels(hints.len());
        for (hint, label) in hints.iter_mut().zip(labels.into_iter()) {
            hint.label = label;
        }

        let count = hints.len();
        self.hints = hints;
        self.surface_id = Some(surface_id);
        self.active = true;
        self.focused_hint = None;
        self.dwell_ms = 0.0;

        info!(count, surface_id, "Link hints loaded");
        Ok(count)
    }

    /// Update with gaze position.  Returns event if any.
    pub fn update_gaze(
        &mut self,
        gaze_x: f32,
        gaze_y: f32,
        dt_ms: f64,
    ) -> Option<LinkHintEvent> {
        if !self.active || self.hints.is_empty() {
            return None;
        }

        // Find hint under gaze
        let hit = self
            .hints
            .iter()
            .find(|h| h.rect.contains(gaze_x, gaze_y));

        match hit {
            Some(hint) => {
                let hint_id = hint.id;

                if self.focused_hint == Some(hint_id) {
                    // Same hint — accumulate dwell
                    self.dwell_ms += dt_ms;

                    // Check auto-confirm threshold
                    if self.dwell_ms >= self.confirm_dwell_ms {
                        let url = hint.url.clone();
                        debug!(hint_id, "Link hint auto-confirmed by dwell");
                        self.clear();
                        return Some(LinkHintEvent::Confirmed { hint_id, url });
                    }

                    // Check highlight threshold (first time)
                    if self.dwell_ms >= self.highlight_dwell_ms
                        && self.dwell_ms - dt_ms < self.highlight_dwell_ms
                    {
                        return Some(LinkHintEvent::Highlighted {
                            hint_id,
                            text: hint.text.clone(),
                            url: hint.url.clone(),
                        });
                    }

                    None
                } else {
                    // New hint — reset dwell
                    self.focused_hint = Some(hint_id);
                    self.dwell_ms = 0.0;
                    None
                }
            }
            None => {
                // Gaze not on any hint — reset
                if self.focused_hint.is_some() {
                    self.focused_hint = None;
                    self.dwell_ms = 0.0;
                }
                None
            }
        }
    }

    /// Explicit confirm of currently highlighted hint.
    pub fn confirm(&mut self) -> Option<LinkHintEvent> {
        if !self.active {
            return None;
        }
        let hint_id = self.focused_hint?;
        let hint = self.hints.iter().find(|h| h.id == hint_id)?;
        let url = hint.url.clone();
        debug!(hint_id, "Link hint explicitly confirmed");
        self.clear();
        Some(LinkHintEvent::Confirmed { hint_id, url })
    }

    /// Clear all hints.
    pub fn clear(&mut self) {
        self.active = false;
        self.hints.clear();
        self.focused_hint = None;
        self.dwell_ms = 0.0;
        self.surface_id = None;
    }

    /// Generate hint labels for N hints ("a".."z", "aa".."az", etc.)
    pub fn generate_labels(count: usize) -> Vec<String> {
        let mut labels = Vec::with_capacity(count);
        let alphabet: Vec<char> = ('a'..='z').collect();
        let base = alphabet.len();

        for i in 0..count {
            if i < base {
                labels.push(String::from(alphabet[i]));
            } else {
                // Multi-character: first char is (i / base - 1), second is (i % base)
                let first = (i / base) - 1;
                let second = i % base;
                if first < base {
                    labels.push(format!("{}{}", alphabet[first], alphabet[second]));
                } else {
                    // For very large counts, just use numeric suffix
                    labels.push(format!("z{}", i));
                }
            }
        }

        labels
    }

    /// S-expression for IPC.
    pub fn status_sexp(&self) -> String {
        let surface_str = self
            .surface_id
            .map(|id| id.to_string())
            .unwrap_or_else(|| "nil".to_string());
        let focused_str = self
            .focused_hint
            .map(|id| id.to_string())
            .unwrap_or_else(|| "nil".to_string());
        format!(
            "(:active {} :surface-id {} :hint-count {} :focused-hint {} :dwell-ms {:.0} :confirm-dwell-ms {:.0} :highlight-dwell-ms {:.0})",
            if self.active { "t" } else { "nil" },
            surface_str,
            self.hints.len(),
            focused_str,
            self.dwell_ms,
            self.confirm_dwell_ms,
            self.highlight_dwell_ms,
        )
    }
}

// ── Minimal JSON helpers (no serde dependency) ─────────────

/// Split a string containing JSON objects `{...},{...}` into individual object strings.
fn split_json_objects(s: &str) -> Vec<String> {
    let mut objects = Vec::new();
    let mut depth = 0i32;
    let mut start = None;

    for (i, ch) in s.char_indices() {
        match ch {
            '{' => {
                if depth == 0 {
                    start = Some(i);
                }
                depth += 1;
            }
            '}' => {
                depth -= 1;
                if depth == 0 {
                    if let Some(s_idx) = start {
                        objects.push(s[s_idx..=i].to_string());
                    }
                    start = None;
                }
            }
            _ => {}
        }
    }
    objects
}

/// Extract a string value for a given key from a JSON object string.
fn extract_json_string(obj: &str, key: &str) -> Option<String> {
    let pattern = format!("\"{}\"", key);
    let idx = obj.find(&pattern)?;
    let after_key = &obj[idx + pattern.len()..];
    // Skip whitespace and colon
    let after_colon = after_key.find(':')?;
    let rest = &after_key[after_colon + 1..].trim_start();
    if rest.starts_with('"') {
        let inner = &rest[1..];
        let end = inner.find('"')?;
        Some(inner[..end].to_string())
    } else {
        None
    }
}

/// Extract a numeric value for a given key from a JSON object string.
fn extract_json_number(obj: &str, key: &str) -> Option<f32> {
    let pattern = format!("\"{}\"", key);
    let idx = obj.find(&pattern)?;
    let after_key = &obj[idx + pattern.len()..];
    let after_colon = after_key.find(':')?;
    let rest = after_key[after_colon + 1..].trim_start();
    // Parse until non-numeric character
    let end = rest
        .find(|c: char| !c.is_ascii_digit() && c != '.' && c != '-')
        .unwrap_or(rest.len());
    rest[..end].parse().ok()
}

// ── Tests ──────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_labels_single() {
        let labels = LinkHintState::generate_labels(5);
        assert_eq!(labels, vec!["a", "b", "c", "d", "e"]);
    }

    #[test]
    fn test_generate_labels_multi() {
        let labels = LinkHintState::generate_labels(28);
        assert_eq!(labels[0], "a");
        assert_eq!(labels[25], "z");
        assert_eq!(labels[26], "aa");
        assert_eq!(labels[27], "ab");
    }

    #[test]
    fn test_load_hints_json() {
        let json = r#"[
            {"text":"Link 1","url":"https://example.com/1","x":0.1,"y":0.2,"w":0.05,"h":0.03},
            {"text":"Link 2","url":"https://example.com/2","x":0.3,"y":0.4,"w":0.05,"h":0.03}
        ]"#;

        let mut state = LinkHintState::new();
        let result = state.load_hints(json, 42);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 2);
        assert!(state.active);
        assert_eq!(state.surface_id, Some(42));
        assert_eq!(state.hints.len(), 2);
        assert_eq!(state.hints[0].text, "Link 1");
        assert_eq!(state.hints[0].url, "https://example.com/1");
        assert_eq!(state.hints[0].label, "a");
        assert_eq!(state.hints[1].label, "b");
    }

    #[test]
    fn test_gaze_highlight() {
        let mut state = LinkHintState::new();
        state.active = true;
        state.hints = vec![LinkHint {
            id: 0,
            text: "Test Link".to_string(),
            url: "https://example.com".to_string(),
            rect: HintRect {
                x: 0.1,
                y: 0.2,
                w: 0.1,
                h: 0.05,
            },
            label: "a".to_string(),
        }];
        state.highlight_dwell_ms = 100.0;
        state.confirm_dwell_ms = 800.0;

        // First gaze enters hint — sets focused, no event
        let evt = state.update_gaze(0.15, 0.22, 16.0);
        assert!(evt.is_none());
        assert_eq!(state.focused_hint, Some(0));

        // Accumulate dwell past highlight threshold
        let evt = state.update_gaze(0.15, 0.22, 110.0);
        assert!(matches!(evt, Some(LinkHintEvent::Highlighted { hint_id: 0, .. })));
    }

    #[test]
    fn test_gaze_confirm_dwell() {
        let mut state = LinkHintState::new();
        state.active = true;
        state.hints = vec![LinkHint {
            id: 0,
            text: "Test".to_string(),
            url: "https://example.com".to_string(),
            rect: HintRect {
                x: 0.1,
                y: 0.2,
                w: 0.1,
                h: 0.05,
            },
            label: "a".to_string(),
        }];
        state.confirm_dwell_ms = 200.0;
        state.highlight_dwell_ms = 50.0;

        // Enter hint
        state.update_gaze(0.15, 0.22, 16.0);

        // Accumulate past confirm threshold
        let evt = state.update_gaze(0.15, 0.22, 300.0);
        assert!(
            matches!(evt, Some(LinkHintEvent::Confirmed { hint_id: 0, .. })),
            "Should auto-confirm after dwell threshold: {:?}",
            evt
        );
        // State should be cleared after confirm
        assert!(!state.active);
    }

    #[test]
    fn test_explicit_confirm() {
        let mut state = LinkHintState::new();
        state.active = true;
        state.hints = vec![LinkHint {
            id: 0,
            text: "Test".to_string(),
            url: "https://example.com".to_string(),
            rect: HintRect {
                x: 0.1,
                y: 0.2,
                w: 0.1,
                h: 0.05,
            },
            label: "a".to_string(),
        }];
        state.focused_hint = Some(0);

        let evt = state.confirm();
        assert!(matches!(evt, Some(LinkHintEvent::Confirmed { hint_id: 0, .. })));
        assert!(!state.active);
    }

    #[test]
    fn test_confirm_no_focus_returns_none() {
        let mut state = LinkHintState::new();
        state.active = true;
        state.focused_hint = None;

        let evt = state.confirm();
        assert!(evt.is_none());
    }

    #[test]
    fn test_load_invalid_json() {
        let mut state = LinkHintState::new();
        let result = state.load_hints("not json", 1);
        assert!(result.is_err());
    }

    #[test]
    fn test_status_sexp() {
        let state = LinkHintState::new();
        let sexp = state.status_sexp();
        assert!(sexp.contains(":active nil"));
        assert!(sexp.contains(":hint-count 0"));
        assert!(sexp.contains(":focused-hint nil"));
    }
}
