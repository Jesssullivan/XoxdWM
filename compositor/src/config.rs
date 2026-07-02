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

use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::path::PathBuf;
use tracing::{info, warn};

// ── Types ────────────────────────────────────────────────────

pub const DEFAULT_LAYOUT_MODE: &str = "tiling";
pub const LAYOUT_CYCLE: [&str; 4] = ["tiling", "monocle", "grid", "floating"];

pub fn is_valid_layout_mode(layout: &str) -> bool {
    LAYOUT_CYCLE.contains(&layout)
}

pub fn is_valid_gaze_zone_layout(layout: &str) -> bool {
    matches!(layout, "default" | "vim-like" | "spacemacs" | "custom")
}

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
    pub layout_default: String,
    pub layout_master_ratio: f32,
    pub workspace_app_rules: BTreeMap<String, usize>,
    pub floating_app_ids: Vec<String>,
    pub key_action_bindings: BTreeMap<String, String>,
    pub app_launch_commands: BTreeMap<String, String>,
    pub autostart_enabled: bool,
    pub autostart_targets: Vec<String>,
    pub session_lock_command: String,
    pub session_idle_enabled: bool,
    pub session_idle_command: String,

    // VR
    pub vr_enabled: bool,
    pub vr_runtime: String,
    pub follow_policy: String,
    pub follow_h_fov: f32,
    pub follow_v_fov: f32,
    pub follow_speed: f32,
    pub passthrough_blend_mode: String,
    pub passthrough_opacity: f32,
    pub gaze_zone_layout: String,
    pub gaze_zone_custom_map: String,

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
            layout_default: DEFAULT_LAYOUT_MODE.to_string(),
            layout_master_ratio: 0.55,
            workspace_app_rules: BTreeMap::new(),
            floating_app_ids: Vec::new(),
            key_action_bindings: default_key_action_bindings(),
            app_launch_commands: default_app_launch_commands(),
            autostart_enabled: false,
            autostart_targets: Vec::new(),
            session_lock_command: "swaylock".to_string(),
            session_idle_enabled: false,
            session_idle_command: String::new(),
            vr_enabled: false,
            vr_runtime: "monado".to_string(),
            follow_policy: "threshold-only".to_string(),
            follow_h_fov: 80.0,
            follow_v_fov: 60.0,
            follow_speed: 0.15,
            passthrough_blend_mode: "opaque".to_string(),
            passthrough_opacity: 1.0,
            gaze_zone_layout: "default".to_string(),
            gaze_zone_custom_map: String::new(),
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

    pub fn normalized_layout_default(&self) -> String {
        if is_valid_layout_mode(&self.layout_default) {
            self.layout_default.clone()
        } else {
            DEFAULT_LAYOUT_MODE.to_string()
        }
    }

    pub fn normalized_layout_master_ratio(&self) -> f32 {
        if self.layout_master_ratio.is_finite() {
            self.layout_master_ratio.clamp(0.05, 0.95)
        } else {
            0.55
        }
    }

    pub fn workspace_for_app_candidates(&self, candidates: &[Option<&str>]) -> Option<usize> {
        self.workspace_app_rules
            .iter()
            .find_map(|(selector, workspace)| {
                selector_matches_candidates(selector, candidates).then_some(*workspace)
            })
    }

    pub fn should_float_app_candidates(&self, candidates: &[Option<&str>]) -> bool {
        self.floating_app_ids
            .iter()
            .any(|selector| selector_matches_candidates(selector, candidates))
    }

    pub fn native_action_for_key(&self, key: &str) -> Option<&str> {
        self.key_action_bindings.get(key).map(String::as_str)
    }

    pub fn app_launch_command(&self, name: &str) -> Option<&str> {
        self.app_launch_commands.get(name).map(String::as_str)
    }

    pub fn configured_autostart_targets(&self) -> &[String] {
        &self.autostart_targets
    }

    pub fn session_lock_command(&self) -> Option<&str> {
        let command = self.session_lock_command.trim();
        (!command.is_empty()).then_some(command)
    }

    pub fn session_idle_command(&self) -> Option<&str> {
        let command = self.session_idle_command.trim();
        (!command.is_empty()).then_some(command)
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

    /// Load from the default config path without swallowing parse/read errors.
    ///
    /// Missing config is still a valid default-config case. Runtime reload
    /// uses this stricter path so an invalid config does not look like a
    /// successful reload.
    pub fn load_default_path_strict() -> Result<(Self, String), String> {
        let path = Self::config_path();
        if !path.exists() {
            return Ok((Self::default(), format!("defaults:{}", path.display())));
        }

        let path_text = path.to_string_lossy();
        let cfg = Self::load_from_file(path_text.as_ref())?;
        Ok((cfg, format!("loaded:{}", path.display())))
    }

    /// Load from the default config path, falling back to defaults if the
    /// file does not exist.
    pub fn load_or_default() -> Self {
        match Self::load_default_path_strict() {
            Ok((cfg, source)) => {
                info!(source, "config loaded");
                cfg
            }
            Err(e) => {
                let path = Self::config_path();
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
                "layout_default" => {
                    let layout = unquote(val);
                    if !is_valid_layout_mode(&layout) {
                        return Err(format!("invalid layout_default: {}", layout));
                    }
                    cfg.layout_default = layout;
                }
                "layout_master_ratio" => {
                    let ratio: f32 = val
                        .parse()
                        .map_err(|_| format!("invalid layout_master_ratio: {}", val))?;
                    if !ratio.is_finite() || !(0.05..=0.95).contains(&ratio) {
                        return Err(format!("invalid layout_master_ratio: {}", val));
                    }
                    cfg.layout_master_ratio = ratio;
                }
                "workspace_app_rules" => {
                    cfg.workspace_app_rules = parse_workspace_app_rules(&unquote(val))?;
                }
                "floating_app_ids" => {
                    cfg.floating_app_ids = parse_csv_list(&unquote(val));
                }
                "key_action_bindings" => {
                    cfg.key_action_bindings = parse_string_map(&unquote(val))?;
                }
                "app_launch_commands" => {
                    cfg.app_launch_commands = parse_string_map(&unquote(val))?;
                }
                "autostart_enabled" => cfg.autostart_enabled = val == "true",
                "autostart_targets" => {
                    cfg.autostart_targets = parse_csv_list(&unquote(val));
                }
                "session_lock_command" => cfg.session_lock_command = unquote(val),
                "session_idle_enabled" => cfg.session_idle_enabled = val == "true",
                "session_idle_command" => cfg.session_idle_command = unquote(val),
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
                "passthrough_opacity" => {
                    let opacity: f32 = val
                        .parse()
                        .map_err(|_| format!("invalid passthrough_opacity: {}", val))?;
                    if !opacity.is_finite() || !(0.0..=1.0).contains(&opacity) {
                        return Err(format!("invalid passthrough_opacity: {}", val));
                    }
                    cfg.passthrough_opacity = opacity;
                }
                "gaze_zone_layout" => {
                    let layout = unquote(val);
                    if !is_valid_gaze_zone_layout(&layout) {
                        return Err(format!("invalid gaze_zone_layout: {}", layout));
                    }
                    cfg.gaze_zone_layout = layout;
                }
                "gaze_zone_custom_map" => cfg.gaze_zone_custom_map = unquote(val),
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
                    cfg.extra.insert(key.clone(), unquote(val));
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
            format!("  \"layout_default\": \"{}\"", self.layout_default),
            format!("  \"layout_master_ratio\": {}", self.layout_master_ratio),
            format!(
                "  \"workspace_app_rules\": \"{}\"",
                workspace_app_rules_to_string(&self.workspace_app_rules)
            ),
            format!(
                "  \"floating_app_ids\": \"{}\"",
                self.floating_app_ids.join(",")
            ),
            format!(
                "  \"key_action_bindings\": \"{}\"",
                string_map_to_string(&self.key_action_bindings)
            ),
            format!(
                "  \"app_launch_commands\": \"{}\"",
                string_map_to_string(&self.app_launch_commands)
            ),
            format!("  \"autostart_enabled\": {}", self.autostart_enabled),
            format!(
                "  \"autostart_targets\": \"{}\"",
                self.autostart_targets.join(",")
            ),
            format!(
                "  \"session_lock_command\": \"{}\"",
                self.session_lock_command
            ),
            format!("  \"session_idle_enabled\": {}", self.session_idle_enabled),
            format!(
                "  \"session_idle_command\": \"{}\"",
                self.session_idle_command
            ),
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
            format!("  \"passthrough_opacity\": {}", self.passthrough_opacity),
            format!("  \"gaze_zone_layout\": \"{}\"", self.gaze_zone_layout),
            format!(
                "  \"gaze_zone_custom_map\": \"{}\"",
                self.gaze_zone_custom_map
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
        s.push_str("\n}");
        s
    }

    /// Generate a default configuration file with inline comments.
    /// (JSON does not support comments, so they are prefixed with `//`
    /// which users can strip or ignore.)
    pub fn generate_default_config() -> String {
        r#"{
  // XoxdWM compositor configuration
  // See docs/user-guide.md for details.

  // General
  "log_level": "info",
  // "ipc_socket_path": "/run/user/1000/ewwm-ipc.sock",

  // Display
  "default_scale": 1.0,
  "cursor_theme": "Adwaita",
  "cursor_size": 24,

  // Workspace and layout policy
  "workspace_count": 4,
  "active_workspace": 0,
  "layout_default": "tiling",
  "layout_master_ratio": 0.55,
  "workspace_app_rules": "",
  "floating_app_ids": "",
  "key_action_bindings": "s-1=workspace:0,s-2=workspace:1,s-3=workspace:2,s-4=workspace:3,s-RET=launch:terminal,s-b=launch:browser,s-SPC=layout:cycle,s-j=focus:next,s-k=focus:previous,s-r=compositor:reload,s-q=compositor:exit",
  "app_launch_commands": "terminal=foot,browser=firefox,launcher=rofi -show drun",
  "autostart_enabled": false,
  "autostart_targets": "",
  "session_lock_command": "swaylock",
  "session_idle_enabled": false,
  "session_idle_command": "",

  // VR
  "vr_enabled": false,
  "vr_runtime": "monado",
  "follow_policy": "threshold-only",
  "follow_h_fov": 80.0,
  "follow_v_fov": 60.0,
  "follow_speed": 0.15,
  "passthrough_blend_mode": "opaque",
  "passthrough_opacity": 1.0,
  "gaze_zone_layout": "default",
  "gaze_zone_custom_map": "",

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

fn parse_csv_list(value: &str) -> Vec<String> {
    value
        .split(',')
        .map(str::trim)
        .filter(|item| !item.is_empty())
        .map(ToString::to_string)
        .collect()
}

fn parse_workspace_app_rules(value: &str) -> Result<BTreeMap<String, usize>, String> {
    let mut rules = BTreeMap::new();
    for entry in parse_csv_list(value) {
        let (selector, workspace) = entry
            .split_once('=')
            .ok_or_else(|| format!("invalid workspace_app_rules entry: {}", entry))?;
        let selector = selector.trim();
        if selector.is_empty() {
            return Err(format!("invalid workspace_app_rules entry: {}", entry));
        }
        let workspace = workspace
            .trim()
            .parse()
            .map_err(|_| format!("invalid workspace_app_rules workspace: {}", entry))?;
        rules.insert(selector.to_string(), workspace);
    }
    Ok(rules)
}

fn parse_string_map(value: &str) -> Result<BTreeMap<String, String>, String> {
    let mut rules = BTreeMap::new();
    for entry in parse_csv_list(value) {
        let (key, val) = entry
            .split_once('=')
            .ok_or_else(|| format!("invalid key/value entry: {}", entry))?;
        let key = key.trim();
        let val = val.trim();
        if key.is_empty() || val.is_empty() {
            return Err(format!("invalid key/value entry: {}", entry));
        }
        rules.insert(key.to_string(), val.to_string());
    }
    Ok(rules)
}

fn workspace_app_rules_to_string(rules: &BTreeMap<String, usize>) -> String {
    rules
        .iter()
        .map(|(selector, workspace)| format!("{}={}", selector, workspace))
        .collect::<Vec<_>>()
        .join(",")
}

fn string_map_to_string(rules: &BTreeMap<String, String>) -> String {
    rules
        .iter()
        .map(|(key, val)| format!("{}={}", key, val))
        .collect::<Vec<_>>()
        .join(",")
}

fn default_key_action_bindings() -> BTreeMap<String, String> {
    [
        ("s-1", "workspace:0"),
        ("s-2", "workspace:1"),
        ("s-3", "workspace:2"),
        ("s-4", "workspace:3"),
        ("s-RET", "launch:terminal"),
        ("s-b", "launch:browser"),
        ("s-SPC", "layout:cycle"),
        ("s-j", "focus:next"),
        ("s-k", "focus:previous"),
        ("s-r", "compositor:reload"),
        ("s-q", "compositor:exit"),
    ]
    .into_iter()
    .map(|(key, action)| (key.to_string(), action.to_string()))
    .collect()
}

fn default_app_launch_commands() -> BTreeMap<String, String> {
    [
        ("terminal", "foot"),
        ("browser", "firefox"),
        ("launcher", "rofi -show drun"),
    ]
    .into_iter()
    .map(|(name, command)| (name.to_string(), command.to_string()))
    .collect()
}

fn selector_matches_candidates(selector: &str, candidates: &[Option<&str>]) -> bool {
    candidates
        .iter()
        .flatten()
        .any(|candidate| *candidate == selector)
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
        assert_eq!(cfg.layout_default, DEFAULT_LAYOUT_MODE);
        assert!((cfg.layout_master_ratio - 0.55).abs() < f32::EPSILON);
        assert!(cfg.workspace_app_rules.is_empty());
        assert!(cfg.floating_app_ids.is_empty());
        assert_eq!(cfg.native_action_for_key("s-1"), Some("workspace:0"));
        assert_eq!(cfg.native_action_for_key("s-RET"), Some("launch:terminal"));
        assert_eq!(cfg.app_launch_command("terminal"), Some("foot"));
        assert!(!cfg.autostart_enabled);
        assert!(cfg.configured_autostart_targets().is_empty());
        assert_eq!(cfg.session_lock_command(), Some("swaylock"));
        assert!(!cfg.session_idle_enabled);
        assert_eq!(cfg.session_idle_command(), None);
        assert!(!cfg.vr_enabled);
        assert_eq!(cfg.vr_runtime, "monado");
        assert_eq!(cfg.follow_policy, "threshold-only");
        assert!((cfg.follow_h_fov - 80.0).abs() < f32::EPSILON);
        assert!((cfg.passthrough_opacity - 1.0).abs() < f32::EPSILON);
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
        assert!(json.contains("\"layout_default\": \"tiling\""));
        assert!(json.contains("\"layout_master_ratio\": 0.55"));
        assert!(json.contains("\"workspace_app_rules\": \"\""));
        assert!(json.contains("\"floating_app_ids\": \"\""));
        assert!(json.contains("s-RET=launch:terminal"));
        assert!(json.contains("terminal=foot"));
        assert!(json.contains("\"autostart_enabled\": false"));
        assert!(json.contains("\"autostart_targets\": \"\""));
        assert!(json.contains("\"session_lock_command\": \"swaylock\""));
        assert!(json.contains("\"session_idle_enabled\": false"));
        assert!(json.contains("\"session_idle_command\": \"\""));
        assert!(json.contains("\"vr_enabled\": false"));
        assert!(json.contains("\"passthrough_opacity\": 1"));
        assert!(json.contains("\"gpu_auto_vr_boost\": true"));

        // Parse it back.
        let parsed = CompositorConfig::parse_json(&json).unwrap();
        assert_eq!(parsed.log_level, "info");
        assert_eq!(parsed.cursor_size, 24);
        assert_eq!(parsed.workspace_count, 4);
        assert_eq!(parsed.active_workspace, 0);
        assert_eq!(parsed.layout_default, DEFAULT_LAYOUT_MODE);
        assert!((parsed.layout_master_ratio - 0.55).abs() < f32::EPSILON);
        assert!(parsed.workspace_app_rules.is_empty());
        assert!(parsed.floating_app_ids.is_empty());
        assert_eq!(parsed.native_action_for_key("s-1"), Some("workspace:0"));
        assert_eq!(parsed.app_launch_command("browser"), Some("firefox"));
        assert!(!parsed.autostart_enabled);
        assert!(parsed.configured_autostart_targets().is_empty());
        assert_eq!(parsed.session_lock_command(), Some("swaylock"));
        assert!(!parsed.session_idle_enabled);
        assert_eq!(parsed.session_idle_command(), None);
        assert!(!parsed.vr_enabled);
        assert!((parsed.passthrough_opacity - 1.0).abs() < f32::EPSILON);
        assert!(parsed.gpu_auto_vr_boost);
    }

    #[test]
    fn test_override_single_field() {
        let json = r#"{
            "vr_enabled": true,
            "cursor_size": 48,
            "workspace_count": 6,
            "active_workspace": 2,
            "layout_default": "grid",
            "layout_master_ratio": 0.6,
            "workspace_app_rules": "firefox=1,foot=2",
            "floating_app_ids": "pavucontrol,org.keepassxc.KeePassXC",
            "key_action_bindings": "s-1=workspace:0,s-RET=launch:terminal",
            "app_launch_commands": "terminal=foot,browser=firefox",
            "autostart_enabled": true,
            "autostart_targets": "terminal,browser",
            "session_lock_command": "swaylock --daemonize",
            "session_idle_enabled": true,
            "session_idle_command": "swayidle -w timeout 300 swaylock",
            "passthrough_opacity": 0.5,
            "gpu_power_profile": "high"
        }"#;
        let cfg = CompositorConfig::parse_json(json).unwrap();
        assert!(cfg.vr_enabled);
        assert_eq!(cfg.cursor_size, 48);
        assert_eq!(cfg.workspace_count, 6);
        assert_eq!(cfg.active_workspace, 2);
        assert_eq!(cfg.layout_default, "grid");
        assert!((cfg.layout_master_ratio - 0.6).abs() < f32::EPSILON);
        assert_eq!(cfg.workspace_app_rules.get("firefox"), Some(&1));
        assert_eq!(cfg.workspace_app_rules.get("foot"), Some(&2));
        assert_eq!(
            cfg.floating_app_ids,
            vec![
                "pavucontrol".to_string(),
                "org.keepassxc.KeePassXC".to_string()
            ]
        );
        assert_eq!(cfg.native_action_for_key("s-1"), Some("workspace:0"));
        assert_eq!(cfg.native_action_for_key("s-RET"), Some("launch:terminal"));
        assert_eq!(cfg.app_launch_command("terminal"), Some("foot"));
        assert_eq!(cfg.app_launch_command("browser"), Some("firefox"));
        assert!(cfg.autostart_enabled);
        assert_eq!(
            cfg.configured_autostart_targets(),
            &["terminal".to_string(), "browser".to_string()]
        );
        assert_eq!(cfg.session_lock_command(), Some("swaylock --daemonize"));
        assert!(cfg.session_idle_enabled);
        assert_eq!(
            cfg.session_idle_command(),
            Some("swayidle -w timeout 300 swaylock")
        );
        assert!((cfg.passthrough_opacity - 0.5).abs() < f32::EPSILON);
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
    fn test_layout_validation() {
        assert!(is_valid_layout_mode("tiling"));
        assert!(is_valid_layout_mode("monocle"));
        assert!(is_valid_layout_mode("grid"));
        assert!(is_valid_layout_mode("floating"));
        assert!(!is_valid_layout_mode("spiral"));

        let bad_layout = r#"{
            "layout_default": "spiral"
        }"#;
        assert!(CompositorConfig::parse_json(bad_layout).is_err());

        let bad_ratio = r#"{
            "layout_master_ratio": 1.2
        }"#;
        assert!(CompositorConfig::parse_json(bad_ratio).is_err());

        let bad_opacity = r#"{
            "passthrough_opacity": 1.2
        }"#;
        assert!(CompositorConfig::parse_json(bad_opacity).is_err());
    }

    #[test]
    fn test_native_manage_rule_matching() {
        let json = r#"{
            "workspace_app_rules": "firefox=1,foot=2",
            "floating_app_ids": "pavucontrol,org.keepassxc.KeePassXC"
        }"#;
        let cfg = CompositorConfig::parse_json(json).unwrap();
        let firefox = [Some("firefox"), None, None];
        let foot = [None, Some("foot"), None];
        let keepass = [Some("org.keepassxc.KeePassXC"), None, None];
        let unknown = [Some("unknown"), None, None];

        assert_eq!(cfg.workspace_for_app_candidates(&firefox), Some(1));
        assert_eq!(cfg.workspace_for_app_candidates(&foot), Some(2));
        assert!(cfg.should_float_app_candidates(&keepass));
        assert!(!cfg.should_float_app_candidates(&unknown));

        let roundtrip = cfg.to_json_string();
        assert!(roundtrip.contains("\"workspace_app_rules\": \"firefox=1,foot=2\""));
        assert!(roundtrip.contains("\"floating_app_ids\": \"pavucontrol,org.keepassxc.KeePassXC\""));
    }

    #[test]
    fn test_native_key_action_bindings() {
        let json = r#"{
            "key_action_bindings": "s-1=workspace:0,s-j=focus:next,s-SPC=layout:cycle,s-RET=launch:terminal,s-q=compositor:exit",
            "app_launch_commands": "terminal=foot,browser=firefox",
            "autostart_enabled": true,
            "autostart_targets": "terminal,browser"
        }"#;
        let cfg = CompositorConfig::parse_json(json).unwrap();
        assert_eq!(cfg.native_action_for_key("s-1"), Some("workspace:0"));
        assert_eq!(cfg.native_action_for_key("s-j"), Some("focus:next"));
        assert_eq!(cfg.native_action_for_key("s-SPC"), Some("layout:cycle"));
        assert_eq!(cfg.native_action_for_key("s-RET"), Some("launch:terminal"));
        assert_eq!(cfg.native_action_for_key("s-q"), Some("compositor:exit"));
        assert_eq!(cfg.app_launch_command("terminal"), Some("foot"));
        assert_eq!(cfg.app_launch_command("browser"), Some("firefox"));
        assert!(cfg.autostart_enabled);
        assert_eq!(cfg.configured_autostart_targets().len(), 2);

        let roundtrip = cfg.to_json_string();
        assert!(roundtrip.contains("s-RET=launch:terminal"));
        assert!(roundtrip.contains("terminal=foot"));
        assert!(roundtrip.contains("\"autostart_enabled\": true"));
        assert!(roundtrip.contains("\"autostart_targets\": \"terminal,browser\""));
    }

    #[test]
    fn test_generate_default_config() {
        let text = CompositorConfig::generate_default_config();
        assert!(text.contains("\"log_level\": \"info\""));
        assert!(text.contains("\"workspace_count\": 4"));
        assert!(text.contains("\"layout_default\": \"tiling\""));
        assert!(text.contains("\"workspace_app_rules\": \"\""));
        assert!(text.contains("\"key_action_bindings\""));
        assert!(text.contains("s-RET=launch:terminal"));
        assert!(text.contains("\"app_launch_commands\""));
        assert!(text.contains("\"passthrough_opacity\": 1"));
        assert!(text.contains("\"gpu_auto_vr_boost\": true"));
        assert!(text.contains("// XoxdWM compositor configuration"));
    }
}
