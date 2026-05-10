//! Compositor configuration — JSON config file loading for non-Nix users.
//!
//! Nix users configure the compositor through home-manager which generates
//! Elisp; this module provides a parallel path for users who install from
//! RPM or manually.  The config file lives at
//! `$XDG_CONFIG_HOME/exwm-vr/compositor.json` (or `~/.config/exwm-vr/compositor.json`).
//!
//! Because we cannot add `serde` / `serde_json` to Cargo.toml on macOS
//! (Wayland deps won't link), this module uses a hand-rolled flat-JSON
//! parser that handles the simple key-value shape of our config.

use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use tracing::{info, warn};

// ── Types ────────────────────────────────────────────────────

/// Compositor configuration.
#[derive(Debug, Clone)]
pub struct CompositorConfig {
    // General
    pub log_level: String,
    pub ipc_socket_path: Option<String>,

    // Display
    pub default_scale: f64,
    pub cursor_theme: String,
    pub cursor_size: u32,

    // Workspace policy
    pub workspace_count: usize,
    pub active_workspace: usize,
    pub layout_mode: String,
    pub key_actions: HashMap<String, String>,
    pub app_launch_commands: HashMap<String, String>,
    pub autostart_enabled: bool,
    pub autostart_targets: Vec<String>,
    pub session_lock_command: Option<String>,
    pub session_idle_enabled: bool,
    pub session_idle_command: Option<String>,

    // VR
    pub vr_enabled: bool,
    pub vr_runtime: String,
    pub follow_policy: String,
    pub follow_h_fov: f32,
    pub follow_v_fov: f32,
    pub follow_speed: f32,
    pub passthrough_blend_mode: String,

    // GPU
    pub gpu_auto_vr_boost: bool,
    pub gpu_power_profile: String,

    // Overlay defaults
    pub overlay_max_count: usize,
    pub overlay_default_alpha: f32,

    // Extensions — arbitrary key-value pairs for downstream consumers.
    pub extra: HashMap<String, String>,
}

impl Default for CompositorConfig {
    fn default() -> Self {
        Self {
            log_level: "info".to_string(),
            ipc_socket_path: None,
            default_scale: 1.0,
            cursor_theme: "Adwaita".to_string(),
            cursor_size: 24,
            workspace_count: 4,
            active_workspace: 0,
            layout_mode: "tiling".to_string(),
            key_actions: default_key_actions(),
            app_launch_commands: HashMap::new(),
            autostart_enabled: false,
            autostart_targets: Vec::new(),
            session_lock_command: None,
            session_idle_enabled: false,
            session_idle_command: None,
            vr_enabled: false,
            vr_runtime: "monado".to_string(),
            follow_policy: "threshold-only".to_string(),
            follow_h_fov: 80.0,
            follow_v_fov: 60.0,
            follow_speed: 0.15,
            passthrough_blend_mode: "opaque".to_string(),
            gpu_auto_vr_boost: true,
            gpu_power_profile: "auto".to_string(),
            overlay_max_count: 16,
            overlay_default_alpha: 0.9,
            extra: HashMap::new(),
        }
    }
}

impl CompositorConfig {
    /// Return the default config file path.
    pub fn config_path() -> PathBuf {
        let config_home = std::env::var("XDG_CONFIG_HOME")
            .ok()
            .filter(|s| !s.is_empty())
            .unwrap_or_else(|| {
                let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
                format!("{}/.config", home)
            });
        PathBuf::from(config_home).join("exwm-vr/compositor.json")
    }

    /// Return the configured IPC socket as a path, if one was supplied.
    pub fn ipc_socket_pathbuf(&self) -> Option<PathBuf> {
        self.ipc_socket_path.as_ref().map(PathBuf::from)
    }

    /// Return a nonzero workspace count for runtime state.
    pub fn normalized_workspace_count(&self) -> usize {
        self.workspace_count.max(1)
    }

    /// Return an active workspace index bounded by the configured count.
    pub fn normalized_active_workspace(&self) -> usize {
        self.active_workspace
            .min(self.normalized_workspace_count().saturating_sub(1))
    }

    /// Load configuration from a JSON file.
    ///
    /// Uses a simple line-based parser that handles flat JSON objects.
    /// Unknown keys are stored in `extra`.
    pub fn load_from_file(path: &str) -> Result<Self, String> {
        let content =
            fs::read_to_string(path).map_err(|e| format!("cannot read {}: {}", path, e))?;
        Self::parse_json(&content)
    }

    /// Load from the default config path, falling back to defaults if the
    /// file does not exist.
    pub fn load_or_default() -> Self {
        let path = Self::config_path();
        if !path.exists() {
            info!(
                "config: no config file at {}, using defaults",
                path.display()
            );
            return Self::default();
        }
        match Self::load_from_file(&path.to_string_lossy()) {
            Ok(cfg) => {
                info!("config: loaded from {}", path.display());
                cfg
            }
            Err(e) => {
                warn!(
                    "config: failed to parse {}: {} (using defaults)",
                    path.display(),
                    e
                );
                Self::default()
            }
        }
    }

    /// Parse a flat JSON object string into config.
    ///
    /// Expects `{ "key": value, ... }` where value is a string, number,
    /// or boolean.  Nested objects are not supported.
    fn parse_json(json: &str) -> Result<Self, String> {
        let mut cfg = Self::default();
        let pairs = parse_flat_json(json)?;

        for (key, val) in &pairs {
            match key.as_str() {
                "log_level" => cfg.log_level = unquote(val),
                "ipc_socket_path" => cfg.ipc_socket_path = Some(unquote(val)),
                "default_scale" => {
                    cfg.default_scale = val
                        .parse()
                        .map_err(|_| format!("invalid default_scale: {}", val))?;
                }
                "cursor_theme" => cfg.cursor_theme = unquote(val),
                "cursor_size" => {
                    cfg.cursor_size = val
                        .parse()
                        .map_err(|_| format!("invalid cursor_size: {}", val))?;
                }
                "workspace_count" => {
                    cfg.workspace_count = val
                        .parse()
                        .map_err(|_| format!("invalid workspace_count: {}", val))?;
                }
                "active_workspace" => {
                    cfg.active_workspace = val
                        .parse()
                        .map_err(|_| format!("invalid active_workspace: {}", val))?;
                }
                "layout_mode" => cfg.layout_mode = unquote(val),
                "autostart_enabled" => cfg.autostart_enabled = val == "true",
                "autostart_targets" => cfg.autostart_targets = parse_string_list(val),
                "session_lock_command" => cfg.session_lock_command = Some(unquote(val)),
                "session_idle_enabled" => cfg.session_idle_enabled = val == "true",
                "session_idle_command" => cfg.session_idle_command = Some(unquote(val)),
                "vr_enabled" => cfg.vr_enabled = val == "true",
                "vr_runtime" => cfg.vr_runtime = unquote(val),
                "follow_policy" => cfg.follow_policy = unquote(val),
                "follow_h_fov" => {
                    cfg.follow_h_fov = val
                        .parse()
                        .map_err(|_| format!("invalid follow_h_fov: {}", val))?;
                }
                "follow_v_fov" => {
                    cfg.follow_v_fov = val
                        .parse()
                        .map_err(|_| format!("invalid follow_v_fov: {}", val))?;
                }
                "follow_speed" => {
                    cfg.follow_speed = val
                        .parse()
                        .map_err(|_| format!("invalid follow_speed: {}", val))?;
                }
                "passthrough_blend_mode" => cfg.passthrough_blend_mode = unquote(val),
                "gpu_auto_vr_boost" => cfg.gpu_auto_vr_boost = val == "true",
                "gpu_power_profile" => cfg.gpu_power_profile = unquote(val),
                "overlay_max_count" => {
                    cfg.overlay_max_count = val
                        .parse()
                        .map_err(|_| format!("invalid overlay_max_count: {}", val))?;
                }
                "overlay_default_alpha" => {
                    cfg.overlay_default_alpha = val
                        .parse()
                        .map_err(|_| format!("invalid overlay_default_alpha: {}", val))?;
                }
                _ => {
                    if let Some(name) = key.strip_prefix("key_action.") {
                        cfg.key_actions.insert(name.to_string(), unquote(val));
                    } else if let Some(name) = key.strip_prefix("app_launch.") {
                        cfg.app_launch_commands
                            .insert(name.to_string(), unquote(val));
                    } else {
                        cfg.extra.insert(key.clone(), unquote(val));
                    }
                }
            }
        }

        Ok(cfg)
    }

    /// Serialize config to a JSON string.
    pub fn to_json_string(&self) -> String {
        let mut s = String::from("{\n");
        let fields: Vec<String> = vec![
            format!("  \"log_level\": \"{}\"", self.log_level),
            match &self.ipc_socket_path {
                Some(p) => format!("  \"ipc_socket_path\": \"{}\"", p),
                None => "  \"ipc_socket_path\": null".to_string(),
            },
            format!("  \"default_scale\": {}", self.default_scale),
            format!("  \"cursor_theme\": \"{}\"", self.cursor_theme),
            format!("  \"cursor_size\": {}", self.cursor_size),
            format!("  \"workspace_count\": {}", self.workspace_count),
            format!("  \"active_workspace\": {}", self.active_workspace),
            format!("  \"layout_mode\": \"{}\"", self.layout_mode),
            format!("  \"autostart_enabled\": {}", self.autostart_enabled),
            format!(
                "  \"autostart_targets\": \"{}\"",
                self.autostart_targets.join(",")
            ),
            match &self.session_lock_command {
                Some(command) => format!("  \"session_lock_command\": \"{}\"", command),
                None => "  \"session_lock_command\": null".to_string(),
            },
            format!("  \"session_idle_enabled\": {}", self.session_idle_enabled),
            match &self.session_idle_command {
                Some(command) => format!("  \"session_idle_command\": \"{}\"", command),
                None => "  \"session_idle_command\": null".to_string(),
            },
            format!("  \"vr_enabled\": {}", self.vr_enabled),
            format!("  \"vr_runtime\": \"{}\"", self.vr_runtime),
            format!("  \"follow_policy\": \"{}\"", self.follow_policy),
            format!("  \"follow_h_fov\": {}", self.follow_h_fov),
            format!("  \"follow_v_fov\": {}", self.follow_v_fov),
            format!("  \"follow_speed\": {}", self.follow_speed),
            format!(
                "  \"passthrough_blend_mode\": \"{}\"",
                self.passthrough_blend_mode
            ),
            format!("  \"gpu_auto_vr_boost\": {}", self.gpu_auto_vr_boost),
            format!("  \"gpu_power_profile\": \"{}\"", self.gpu_power_profile),
            format!("  \"overlay_max_count\": {}", self.overlay_max_count),
            format!(
                "  \"overlay_default_alpha\": {}",
                self.overlay_default_alpha
            ),
        ];
        s.push_str(&fields.join(",\n"));
        if !self.extra.is_empty() {
            for (k, v) in &self.extra {
                s.push_str(&format!(",\n  \"{}\": \"{}\"", k, v));
            }
        }
        for (key, action) in &self.key_actions {
            s.push_str(&format!(",\n  \"key_action.{}\": \"{}\"", key, action));
        }
        for (target, command) in &self.app_launch_commands {
            s.push_str(&format!(",\n  \"app_launch.{}\": \"{}\"", target, command));
        }
        s.push_str("\n}");
        s
    }

    /// Generate a default configuration file with inline comments.
    /// (JSON does not support comments, so they are prefixed with `//`
    /// which users can strip or ignore.)
    pub fn generate_default_config() -> String {
        r#"{
  // EXWM-VR compositor configuration
  // See docs/user-guide.md for details.

  // General
  "log_level": "info",
  // "ipc_socket_path": "/run/user/1000/ewwm-ipc.sock",

  // Display
  "default_scale": 1.0,
  "cursor_theme": "Adwaita",
  "cursor_size": 24,

  // Workspace policy
  "workspace_count": 4,
  "active_workspace": 0,
  "layout_mode": "tiling",
  "key_action.s-1": "workspace-switch:0",
  "key_action.s-2": "workspace-switch:1",
  "key_action.s-SPC": "layout-cycle",
  "key_action.s-r": "config-reload",
  // "key_action.s-RET": "app-launch:terminal",
  // "app_launch.terminal": "foot",

  // Native startup/session policy
  "autostart_enabled": false,
  "autostart_targets": "",
  // "session_lock_command": "swaylock",
  "session_idle_enabled": false,
  // "session_idle_command": "swayidle -w",

  // VR
  "vr_enabled": false,
  "vr_runtime": "monado",
  "follow_policy": "threshold-only",
  "follow_h_fov": 80.0,
  "follow_v_fov": 60.0,
  "follow_speed": 0.15,
  "passthrough_blend_mode": "opaque",

  // GPU power management
  "gpu_auto_vr_boost": true,
  "gpu_power_profile": "auto",

  // Overlays
  "overlay_max_count": 16,
  "overlay_default_alpha": 0.9
}"#
        .to_string()
    }
}

// ── Flat JSON parser ─────────────────────────────────────────

/// Parse a flat JSON object into key-value pairs.
///
/// Handles: `{ "key": "string", "key2": 123, "key3": true }`.
/// Skips lines that are comments (start with `//` after stripping)
/// or blank.  Does not handle nested objects or arrays.
fn parse_flat_json(json: &str) -> Result<Vec<(String, String)>, String> {
    let mut pairs = Vec::new();

    for line in json.lines() {
        let trimmed = line.trim();

        // Skip blank lines, braces, and comment lines.
        if trimmed.is_empty() || trimmed == "{" || trimmed == "}" || trimmed.starts_with("//") {
            continue;
        }

        // Strip trailing comma.
        let trimmed = trimmed.strip_suffix(',').unwrap_or(trimmed);

        // Find the colon separating key from value.
        let colon = match trimmed.find(':') {
            Some(pos) => pos,
            None => continue,
        };

        let raw_key = trimmed[..colon].trim();
        let raw_val = trimmed[colon + 1..].trim();

        // Key must be a quoted string.
        if !raw_key.starts_with('"') || !raw_key.ends_with('"') {
            continue;
        }
        let key = raw_key[1..raw_key.len() - 1].to_string();

        // Value: keep as-is for parsing (quoted strings, numbers, bools, null).
        // For null, we skip (leave field at default).
        if raw_val == "null" {
            continue;
        }

        pairs.push((key, raw_val.to_string()));
    }

    Ok(pairs)
}

/// Remove surrounding quotes from a JSON string value.
fn unquote(s: &str) -> String {
    let trimmed = s.trim();
    if trimmed.starts_with('"') && trimmed.ends_with('"') && trimmed.len() >= 2 {
        trimmed[1..trimmed.len() - 1].to_string()
    } else {
        trimmed.to_string()
    }
}

fn parse_string_list(s: &str) -> Vec<String> {
    let trimmed = s.trim();
    let body = if trimmed.starts_with('[') && trimmed.ends_with(']') {
        &trimmed[1..trimmed.len() - 1]
    } else {
        trimmed
    };
    body.split(',')
        .map(unquote)
        .map(|item| item.trim().to_string())
        .filter(|item| !item.is_empty())
        .collect()
}

fn default_key_actions() -> HashMap<String, String> {
    let mut actions = HashMap::new();
    for workspace in 0..4 {
        actions.insert(
            format!("s-{}", workspace + 1),
            format!("workspace-switch:{}", workspace),
        );
    }
    actions.insert("s-SPC".to_string(), "layout-cycle".to_string());
    actions.insert("s-r".to_string(), "config-reload".to_string());
    actions.insert("s-q".to_string(), "compositor-exit".to_string());
    actions
}

// ── Tests ────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_values() {
        let cfg = CompositorConfig::default();
        assert_eq!(cfg.log_level, "info");
        assert!(cfg.ipc_socket_path.is_none());
        assert!((cfg.default_scale - 1.0).abs() < f64::EPSILON);
        assert_eq!(cfg.cursor_theme, "Adwaita");
        assert_eq!(cfg.cursor_size, 24);
        assert_eq!(cfg.workspace_count, 4);
        assert_eq!(cfg.active_workspace, 0);
        assert_eq!(cfg.layout_mode, "tiling");
        assert_eq!(
            cfg.key_actions.get("s-1").map(String::as_str),
            Some("workspace-switch:0")
        );
        assert!(!cfg.autostart_enabled);
        assert!(cfg.autostart_targets.is_empty());
        assert!(!cfg.vr_enabled);
        assert_eq!(cfg.vr_runtime, "monado");
        assert_eq!(cfg.follow_policy, "threshold-only");
        assert!((cfg.follow_h_fov - 80.0).abs() < f32::EPSILON);
        assert!(cfg.gpu_auto_vr_boost);
        assert_eq!(cfg.gpu_power_profile, "auto");
        assert_eq!(cfg.overlay_max_count, 16);
    }

    #[test]
    fn test_config_path() {
        let path = CompositorConfig::config_path();
        let path_str = path.to_string_lossy();
        assert!(path_str.ends_with("exwm-vr/compositor.json"));
    }

    #[test]
    fn test_json_generation_roundtrip() {
        let cfg = CompositorConfig::default();
        let json = cfg.to_json_string();
        assert!(json.contains("\"log_level\": \"info\""));
        assert!(json.contains("\"cursor_size\": 24"));
        assert!(json.contains("\"workspace_count\": 4"));
        assert!(json.contains("\"active_workspace\": 0"));
        assert!(json.contains("\"layout_mode\": \"tiling\""));
        assert!(json.contains("\"autostart_enabled\": false"));
        assert!(json.contains("\"vr_enabled\": false"));
        assert!(json.contains("\"gpu_auto_vr_boost\": true"));

        // Parse it back.
        let parsed = CompositorConfig::parse_json(&json).unwrap();
        assert_eq!(parsed.log_level, "info");
        assert_eq!(parsed.cursor_size, 24);
        assert_eq!(parsed.workspace_count, 4);
        assert_eq!(parsed.active_workspace, 0);
        assert_eq!(parsed.layout_mode, "tiling");
        assert!(!parsed.vr_enabled);
        assert!(parsed.gpu_auto_vr_boost);
    }

    #[test]
    fn test_override_single_field() {
        let json = r#"{
            "vr_enabled": true,
            "cursor_size": 48,
            "workspace_count": 6,
            "active_workspace": 2,
            "layout_mode": "monocle",
            "key_action.s-RET": "app-launch:terminal",
            "app_launch.terminal": "foot",
            "autostart_enabled": true,
            "autostart_targets": "terminal,browser",
            "session_lock_command": "swaylock",
            "session_idle_enabled": true,
            "session_idle_command": "swayidle -w",
            "gpu_power_profile": "high"
        }"#;
        let cfg = CompositorConfig::parse_json(json).unwrap();
        assert!(cfg.vr_enabled);
        assert_eq!(cfg.cursor_size, 48);
        assert_eq!(cfg.workspace_count, 6);
        assert_eq!(cfg.active_workspace, 2);
        assert_eq!(cfg.layout_mode, "monocle");
        assert_eq!(
            cfg.key_actions.get("s-RET").map(String::as_str),
            Some("app-launch:terminal")
        );
        assert_eq!(
            cfg.app_launch_commands.get("terminal").map(String::as_str),
            Some("foot")
        );
        assert!(cfg.autostart_enabled);
        assert_eq!(cfg.autostart_targets, vec!["terminal", "browser"]);
        assert_eq!(cfg.session_lock_command.as_deref(), Some("swaylock"));
        assert!(cfg.session_idle_enabled);
        assert_eq!(cfg.session_idle_command.as_deref(), Some("swayidle -w"));
        assert_eq!(cfg.gpu_power_profile, "high");
        // Untouched fields keep defaults.
        assert_eq!(cfg.log_level, "info");
        assert_eq!(cfg.cursor_theme, "Adwaita");
    }

    #[test]
    fn test_unknown_keys_go_to_extra() {
        let json = r#"{
            "my_custom_key": "my_value",
            "another_key": "42"
        }"#;
        let cfg = CompositorConfig::parse_json(json).unwrap();
        assert_eq!(cfg.extra.get("my_custom_key").unwrap(), "my_value");
        assert_eq!(cfg.extra.get("another_key").unwrap(), "42");
    }

    #[test]
    fn test_comments_are_skipped() {
        let json = r#"{
            // This is a comment
            "log_level": "debug"
        }"#;
        let cfg = CompositorConfig::parse_json(json).unwrap();
        assert_eq!(cfg.log_level, "debug");
    }

    #[test]
    fn test_null_values_are_skipped() {
        let json = r#"{
            "ipc_socket_path": null,
            "vr_enabled": true
        }"#;
        let cfg = CompositorConfig::parse_json(json).unwrap();
        assert!(cfg.ipc_socket_path.is_none());
        assert!(cfg.vr_enabled);
    }

    #[test]
    fn test_ipc_socket_pathbuf() {
        let json = r#"{
            "ipc_socket_path": "/tmp/xoxdwm-ipc.sock"
        }"#;
        let cfg = CompositorConfig::parse_json(json).unwrap();
        assert_eq!(
            cfg.ipc_socket_pathbuf().unwrap(),
            PathBuf::from("/tmp/xoxdwm-ipc.sock")
        );
    }

    #[test]
    fn test_workspace_normalization() {
        let json = r#"{
            "workspace_count": 0,
            "active_workspace": 9
        }"#;
        let cfg = CompositorConfig::parse_json(json).unwrap();
        assert_eq!(cfg.normalized_workspace_count(), 1);
        assert_eq!(cfg.normalized_active_workspace(), 0);

        let json = r#"{
            "workspace_count": 3,
            "active_workspace": 9
        }"#;
        let cfg = CompositorConfig::parse_json(json).unwrap();
        assert_eq!(cfg.normalized_workspace_count(), 3);
        assert_eq!(cfg.normalized_active_workspace(), 2);
    }

    #[test]
    fn test_generate_default_config() {
        let text = CompositorConfig::generate_default_config();
        assert!(text.contains("\"log_level\": \"info\""));
        assert!(text.contains("\"workspace_count\": 4"));
        assert!(text.contains("\"key_action.s-1\""));
        assert!(text.contains("\"autostart_enabled\": false"));
        assert!(text.contains("\"gpu_auto_vr_boost\": true"));
        assert!(text.contains("// EXWM-VR compositor configuration"));
    }
}
