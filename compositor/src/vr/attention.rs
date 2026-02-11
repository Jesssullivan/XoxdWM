//! EEG attention/focus monitoring — band power analysis for cognitive state.
//!
//! Computes a focus score from EEG frequency band powers (alpha, beta,
//! theta, gamma) using a sigmoid-based engagement index.  Classifies
//! attention into five states from deep focus to drowsy.
//! No openxrs dependency; compiles unconditionally.

use std::collections::VecDeque;

// ── AttentionState ────────────────────────────────────────

/// Classification of cognitive attention level.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttentionState {
    /// Intense concentration, flow state.
    DeepFocus,
    /// Normal engaged attention.
    Focused,
    /// Neutral — neither focused nor relaxed.
    Neutral,
    /// Relaxed, alpha-dominant.
    Relaxed,
    /// Drowsy, theta-dominant.
    Drowsy,
}

impl AttentionState {
    /// String representation for IPC.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::DeepFocus => "deep-focus",
            Self::Focused => "focused",
            Self::Neutral => "neutral",
            Self::Relaxed => "relaxed",
            Self::Drowsy => "drowsy",
        }
    }

    /// Parse from string.
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "deep-focus" => Some(Self::DeepFocus),
            "focused" => Some(Self::Focused),
            "neutral" => Some(Self::Neutral),
            "relaxed" => Some(Self::Relaxed),
            "drowsy" => Some(Self::Drowsy),
            _ => None,
        }
    }
}

// ── BandPowers ────────────────────────────────────────────

/// EEG frequency band power measurements.
#[derive(Debug, Clone)]
pub struct BandPowers {
    /// Alpha band power (8-12 Hz, occipital O1/O2).
    pub alpha: f64,
    /// Beta band power (12-30 Hz, frontal F3/F4).
    pub beta: f64,
    /// Theta band power (4-8 Hz, midline Fz).
    pub theta: f64,
    /// Gamma band power (30-100 Hz).
    pub gamma: f64,
}

impl Default for BandPowers {
    fn default() -> Self {
        Self {
            alpha: 0.0,
            beta: 0.0,
            theta: 0.0,
            gamma: 0.0,
        }
    }
}

// ── AttentionConfig ───────────────────────────────────────

/// Configuration for attention monitoring thresholds.
#[derive(Debug, Clone)]
pub struct AttentionConfig {
    /// Whether attention monitoring is enabled.
    pub enabled: bool,
    /// Rolling window duration (seconds).
    pub window_seconds: f64,
    /// Minimum interval between updates (milliseconds).
    pub update_interval_ms: f64,
    /// Focus score above which state is Focused.
    pub focus_threshold: f64,
    /// Focus score above which state is DeepFocus (do-not-disturb).
    pub dnd_threshold: f64,
    /// Focus score below which state is Drowsy.
    pub drowsy_threshold: f64,
    /// Focus score below which state is Relaxed.
    pub relaxed_threshold: f64,
    /// Whether calibration has been performed.
    pub calibrated: bool,
    /// Sigmoid calibration factor (steepness).
    pub calibration_factor: f64,
}

impl Default for AttentionConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            window_seconds: 2.0,
            update_interval_ms: 250.0,
            focus_threshold: 0.6,
            dnd_threshold: 0.8,
            drowsy_threshold: 0.2,
            relaxed_threshold: 0.35,
            calibrated: false,
            calibration_factor: 1.0,
        }
    }
}

// ── AttentionMonitor ──────────────────────────────────────

/// Central attention monitoring from EEG band powers.
pub struct AttentionMonitor {
    /// Configuration.
    pub config: AttentionConfig,
    /// Current classified attention state.
    pub current_state: AttentionState,
    /// Current focus score (0.0-1.0).
    pub focus_score: f64,
    /// Latest band power measurements.
    pub band_powers: BandPowers,
    /// Duration in current state (milliseconds).
    pub state_duration_ms: f64,
    /// Recent focus score history for smoothing.
    history: VecDeque<f64>,
    /// Maximum history length.
    history_max: usize,
    /// Timestamp of last update (milliseconds).
    last_update_ms: f64,
    /// Calibration samples collected during calibration.
    calibration_samples: Vec<f64>,
    /// Whether currently in calibration mode.
    calibrating: bool,
}

impl AttentionMonitor {
    /// Create a new attention monitor with defaults.
    pub fn new() -> Self {
        Self {
            config: AttentionConfig::default(),
            current_state: AttentionState::Neutral,
            focus_score: 0.5,
            band_powers: BandPowers::default(),
            state_duration_ms: 0.0,
            history: VecDeque::with_capacity(64),
            history_max: 64,
            last_update_ms: 0.0,
            calibration_samples: Vec::with_capacity(128),
            calibrating: false,
        }
    }

    /// Update band power measurements and recompute focus score.
    pub fn update_bands(
        &mut self,
        alpha: f64,
        beta: f64,
        theta: f64,
        gamma: f64,
    ) {
        if !self.config.enabled {
            return;
        }

        self.band_powers = BandPowers {
            alpha,
            beta,
            theta,
            gamma,
        };

        let raw_score = self.compute_focus();

        // Collect calibration data if calibrating
        if self.calibrating {
            self.calibration_samples.push(raw_score);
        }

        // Add to history and compute smoothed score
        if self.history.len() >= self.history_max {
            self.history.pop_front();
        }
        self.history.push_back(raw_score);

        let smoothed = if self.history.is_empty() {
            raw_score
        } else {
            self.history.iter().sum::<f64>() / self.history.len() as f64
        };

        let prev_state = self.current_state;
        self.focus_score = smoothed.clamp(0.0, 1.0);
        self.current_state = Self::classify_state(self.focus_score, &self.config);

        if self.current_state == prev_state {
            self.state_duration_ms += self.config.update_interval_ms;
        } else {
            self.state_duration_ms = 0.0;
        }
    }

    /// Compute raw focus score from band powers using engagement index.
    ///
    /// Engagement index = sigmoid(beta / (alpha + theta) * calibration_factor)
    /// Higher beta relative to alpha+theta indicates engagement.
    fn compute_focus(&self) -> f64 {
        let denominator = self.band_powers.alpha + self.band_powers.theta;
        if denominator <= 0.0 {
            return 0.5;
        }
        let ratio = self.band_powers.beta / denominator;
        let scaled = ratio * self.config.calibration_factor;
        // Sigmoid: 1 / (1 + exp(-x)) centered at x=1
        1.0 / (1.0 + (-2.0 * (scaled - 1.0)).exp())
    }

    /// Classify a focus score into an attention state.
    fn classify_state(score: f64, config: &AttentionConfig) -> AttentionState {
        if score >= config.dnd_threshold {
            AttentionState::DeepFocus
        } else if score >= config.focus_threshold {
            AttentionState::Focused
        } else if score <= config.drowsy_threshold {
            AttentionState::Drowsy
        } else if score <= config.relaxed_threshold {
            AttentionState::Relaxed
        } else {
            AttentionState::Neutral
        }
    }

    /// Start calibration mode — collects baseline samples.
    pub fn start_calibration(&mut self) {
        self.calibrating = true;
        self.calibration_samples.clear();
        self.config.calibrated = false;
    }

    /// Finish calibration and compute calibration factor.
    ///
    /// Adjusts `calibration_factor` so the mean raw score during
    /// calibration maps to the neutral midpoint (0.5).
    pub fn finish_calibration(&mut self) -> Result<(), String> {
        self.calibrating = false;
        if self.calibration_samples.len() < 10 {
            return Err(format!(
                "Need at least 10 calibration samples, got {}",
                self.calibration_samples.len()
            ));
        }
        let mean = self.calibration_samples.iter().sum::<f64>()
            / self.calibration_samples.len() as f64;
        if mean <= 0.01 {
            return Err("Calibration mean too low, check signal".to_string());
        }
        // Adjust factor so mean maps to ~0.5
        self.config.calibration_factor = 0.5 / mean;
        self.config.calibrated = true;
        self.calibration_samples.clear();
        Ok(())
    }

    // ── IPC adapters ──────────────────────────────────────

    /// Generate IPC status s-expression.
    pub fn status_sexp(&self) -> String {
        format!(
            "(:enabled {} :state :{} :focus-score {:.3} :state-duration-ms {:.0} :calibrated {} :alpha {:.3} :beta {:.3} :theta {:.3} :gamma {:.3})",
            if self.config.enabled { "t" } else { "nil" },
            self.current_state.as_str(),
            self.focus_score,
            self.state_duration_ms,
            if self.config.calibrated { "t" } else { "nil" },
            self.band_powers.alpha,
            self.band_powers.beta,
            self.band_powers.theta,
            self.band_powers.gamma,
        )
    }

    /// Generate IPC config s-expression.
    pub fn config_sexp(&self) -> String {
        format!(
            "(:enabled {} :window-seconds {:.1} :update-interval-ms {:.0} :focus-threshold {:.2} :dnd-threshold {:.2} :drowsy-threshold {:.2} :relaxed-threshold {:.2} :calibration-factor {:.3} :calibrated {})",
            if self.config.enabled { "t" } else { "nil" },
            self.config.window_seconds,
            self.config.update_interval_ms,
            self.config.focus_threshold,
            self.config.dnd_threshold,
            self.config.drowsy_threshold,
            self.config.relaxed_threshold,
            self.config.calibration_factor,
            if self.config.calibrated { "t" } else { "nil" },
        )
    }

    /// Set a named threshold value.
    pub fn set_threshold(
        &mut self,
        name: &str,
        value: f64,
    ) -> Result<(), String> {
        match name {
            "focus" => {
                self.config.focus_threshold = value;
                Ok(())
            }
            "dnd" => {
                self.config.dnd_threshold = value;
                Ok(())
            }
            "drowsy" => {
                self.config.drowsy_threshold = value;
                Ok(())
            }
            "relaxed" => {
                self.config.relaxed_threshold = value;
                Ok(())
            }
            _ => Err(format!("Unknown threshold: {}", name)),
        }
    }
}

// ── Tests ───────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_state() {
        let monitor = AttentionMonitor::new();
        assert!(monitor.config.enabled);
        assert_eq!(monitor.current_state, AttentionState::Neutral);
        assert!((monitor.focus_score - 0.5).abs() < f64::EPSILON);
        assert!(monitor.history.is_empty());
    }

    #[test]
    fn test_high_beta_produces_focus() {
        let mut monitor = AttentionMonitor::new();
        // High beta, low alpha/theta => high engagement
        monitor.update_bands(0.2, 2.0, 0.1, 0.0);
        assert!(
            monitor.focus_score > 0.6,
            "High beta should produce high focus, got {:.3}",
            monitor.focus_score,
        );
        assert!(
            monitor.current_state == AttentionState::Focused
                || monitor.current_state == AttentionState::DeepFocus,
            "Should be focused or deep-focus, got {:?}",
            monitor.current_state,
        );
    }

    #[test]
    fn test_high_theta_produces_drowsy() {
        let mut monitor = AttentionMonitor::new();
        // Low beta, high theta => drowsy
        monitor.update_bands(0.1, 0.05, 2.0, 0.0);
        assert!(
            monitor.focus_score < 0.3,
            "High theta should produce low focus, got {:.3}",
            monitor.focus_score,
        );
    }

    #[test]
    fn test_classify_state_thresholds() {
        let config = AttentionConfig::default();
        assert_eq!(
            AttentionMonitor::classify_state(0.9, &config),
            AttentionState::DeepFocus,
        );
        assert_eq!(
            AttentionMonitor::classify_state(0.7, &config),
            AttentionState::Focused,
        );
        assert_eq!(
            AttentionMonitor::classify_state(0.5, &config),
            AttentionState::Neutral,
        );
        assert_eq!(
            AttentionMonitor::classify_state(0.3, &config),
            AttentionState::Relaxed,
        );
        assert_eq!(
            AttentionMonitor::classify_state(0.1, &config),
            AttentionState::Drowsy,
        );
    }

    #[test]
    fn test_state_roundtrip() {
        let states = vec![
            ("deep-focus", AttentionState::DeepFocus),
            ("focused", AttentionState::Focused),
            ("neutral", AttentionState::Neutral),
            ("relaxed", AttentionState::Relaxed),
            ("drowsy", AttentionState::Drowsy),
        ];
        for (s, st) in &states {
            assert_eq!(AttentionState::from_str(s), Some(*st));
            assert_eq!(st.as_str(), *s);
        }
        assert_eq!(AttentionState::from_str("invalid"), None);
    }

    #[test]
    fn test_calibration_flow() {
        let mut monitor = AttentionMonitor::new();
        monitor.start_calibration();
        assert!(monitor.calibrating);

        // Feed 20 samples
        for _ in 0..20 {
            monitor.update_bands(1.0, 1.0, 1.0, 0.0);
        }

        let result = monitor.finish_calibration();
        assert!(result.is_ok(), "Calibration should succeed: {:?}", result);
        assert!(monitor.config.calibrated);
        assert!(!monitor.calibrating);
    }

    #[test]
    fn test_calibration_insufficient_samples() {
        let mut monitor = AttentionMonitor::new();
        monitor.start_calibration();
        for _ in 0..5 {
            monitor.update_bands(1.0, 1.0, 1.0, 0.0);
        }
        let result = monitor.finish_calibration();
        assert!(result.is_err());
    }

    #[test]
    fn test_disabled_ignores_updates() {
        let mut monitor = AttentionMonitor::new();
        monitor.config.enabled = false;
        monitor.update_bands(0.2, 3.0, 0.1, 0.0);
        assert!(
            monitor.history.is_empty(),
            "Disabled monitor should not accumulate history",
        );
    }

    #[test]
    fn test_status_sexp() {
        let monitor = AttentionMonitor::new();
        let sexp = monitor.status_sexp();
        assert!(sexp.contains(":enabled t"));
        assert!(sexp.contains(":state :neutral"));
        assert!(sexp.contains(":focus-score"));
        assert!(sexp.contains(":calibrated nil"));
    }

    #[test]
    fn test_set_threshold() {
        let mut monitor = AttentionMonitor::new();
        assert!(monitor.set_threshold("focus", 0.7).is_ok());
        assert!((monitor.config.focus_threshold - 0.7).abs() < f64::EPSILON);
        assert!(monitor.set_threshold("dnd", 0.85).is_ok());
        assert!((monitor.config.dnd_threshold - 0.85).abs() < f64::EPSILON);
        assert!(monitor.set_threshold("unknown", 0.5).is_err());
    }
}
