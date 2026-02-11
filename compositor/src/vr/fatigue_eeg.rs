//! EEG-based fatigue detection — extends Week 13 eye fatigue with
//! electrophysiological indicators.
//!
//! Computes a composite fatigue index from multiple EEG biomarkers:
//! theta power increase, alpha power increase, beta power decrease,
//! blink rate elevation, and theta/beta ratio.
//! No openxrs dependency; compiles unconditionally.

// ── EegFatigueLevel ─────────────────────────────────────

/// EEG-based fatigue severity level.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EegFatigueLevel {
    /// No detectable fatigue.
    None,
    /// Mild fatigue (theta starting to rise).
    Mild,
    /// Moderate fatigue (alpha increase, beta decrease).
    Moderate,
    /// Severe fatigue (multiple indicators triggered).
    Severe,
}

impl EegFatigueLevel {
    /// String representation for IPC.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::None => "none",
            Self::Mild => "mild",
            Self::Moderate => "moderate",
            Self::Severe => "severe",
        }
    }

    /// Parse from string.
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "none" => Some(Self::None),
            "mild" => Some(Self::Mild),
            "moderate" => Some(Self::Moderate),
            "severe" => Some(Self::Severe),
            _ => Option::None,
        }
    }
}

// ── EegFatigueConfig ────────────────────────────────────

/// Configuration for EEG fatigue detection.
#[derive(Debug, Clone)]
pub struct EegFatigueConfig {
    /// Whether EEG fatigue monitoring is enabled.
    pub enabled: bool,
    /// Weight for theta power increase indicator.
    pub theta_weight: f64,
    /// Weight for alpha power increase indicator.
    pub alpha_weight: f64,
    /// Weight for blink rate elevation indicator.
    pub blink_rate_weight: f64,
    /// Weight for beta power decrease indicator.
    pub beta_decrease_weight: f64,
    /// Weight for theta/beta ratio indicator.
    pub ratio_weight: f64,
    /// Fatigue index threshold for mild fatigue.
    pub mild_threshold: f64,
    /// Fatigue index threshold for moderate fatigue.
    pub moderate_threshold: f64,
    /// Fatigue index threshold for severe fatigue.
    pub severe_threshold: f64,
    /// Whether to auto-save on severe fatigue.
    pub auto_save_on_severe: bool,
}

impl Default for EegFatigueConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            theta_weight: 0.3,
            alpha_weight: 0.25,
            blink_rate_weight: 0.2,
            beta_decrease_weight: 0.15,
            ratio_weight: 0.1,
            mild_threshold: 0.5,
            moderate_threshold: 0.7,
            severe_threshold: 0.9,
            auto_save_on_severe: true,
        }
    }
}

// ── EegFatigueMonitor ───────────────────────────────────

/// EEG-based fatigue monitor using multi-indicator composite index.
pub struct EegFatigueMonitor {
    /// Configuration.
    pub config: EegFatigueConfig,
    /// Current composite fatigue index (0.0-1.0).
    pub fatigue_index: f64,
    /// Current fatigue level.
    pub level: EegFatigueLevel,
    /// Theta power trend relative to baseline.
    pub theta_trend: f64,
    /// Alpha power trend relative to baseline.
    pub alpha_trend: f64,
    /// Beta power trend relative to baseline.
    pub beta_trend: f64,
    /// Baseline theta power (set during calibration).
    baseline_theta: f64,
    /// Baseline alpha power (set during calibration).
    baseline_alpha: f64,
    /// Baseline beta power (set during calibration).
    baseline_beta: f64,
    /// Current blink rate (blinks per minute).
    blink_rate: f64,
    /// Baseline blink rate for comparison.
    baseline_blink_rate: f64,
    /// Whether actively monitoring.
    monitoring: bool,
}

impl EegFatigueMonitor {
    /// Create a new EEG fatigue monitor with defaults.
    pub fn new() -> Self {
        Self {
            config: EegFatigueConfig::default(),
            fatigue_index: 0.0,
            level: EegFatigueLevel::None,
            theta_trend: 0.0,
            alpha_trend: 0.0,
            beta_trend: 0.0,
            baseline_theta: 1.0,
            baseline_alpha: 1.0,
            baseline_beta: 1.0,
            blink_rate: 0.0,
            baseline_blink_rate: 15.0,
            monitoring: false,
        }
    }

    /// Start fatigue monitoring.
    pub fn start_monitoring(&mut self) {
        self.monitoring = true;
    }

    /// Stop fatigue monitoring.
    pub fn stop_monitoring(&mut self) {
        self.monitoring = false;
    }

    /// Set baseline EEG band powers (from a rested state).
    pub fn set_baseline(&mut self, theta: f64, alpha: f64, beta: f64) {
        self.baseline_theta = theta.max(0.001);
        self.baseline_alpha = alpha.max(0.001);
        self.baseline_beta = beta.max(0.001);
    }

    /// Set baseline blink rate for comparison.
    pub fn set_baseline_blink_rate(&mut self, rate: f64) {
        self.baseline_blink_rate = rate.max(1.0);
    }

    /// Update fatigue metrics with current EEG band powers and blink rate.
    pub fn update(
        &mut self,
        theta: f64,
        alpha: f64,
        beta: f64,
        blink_rate: f64,
    ) {
        if !self.monitoring || !self.config.enabled {
            return;
        }

        self.blink_rate = blink_rate;

        // Compute trends relative to baseline
        self.theta_trend = (theta - self.baseline_theta) / self.baseline_theta;
        self.alpha_trend = (alpha - self.baseline_alpha) / self.baseline_alpha;
        self.beta_trend = (beta - self.baseline_beta) / self.baseline_beta;

        // Compute composite fatigue index
        self.fatigue_index = self.compute_index();
        self.level = Self::classify(self.fatigue_index, &self.config);
    }

    /// Compute the composite fatigue index from individual indicators.
    ///
    /// Each indicator is normalized to 0.0-1.0:
    /// - Theta increase: positive trend is fatiguing
    /// - Alpha increase: positive trend is fatiguing
    /// - Beta decrease: negative trend is fatiguing
    /// - Blink rate elevation: above baseline is fatiguing
    /// - Theta/beta ratio: high ratio is fatiguing
    fn compute_index(&self) -> f64 {
        // Theta increase indicator (saturates at +100% increase)
        let theta_indicator = (self.theta_trend / 1.0).clamp(0.0, 1.0);

        // Alpha increase indicator
        let alpha_indicator = (self.alpha_trend / 1.0).clamp(0.0, 1.0);

        // Beta decrease indicator (negative trend = fatigue)
        let beta_indicator = (-self.beta_trend / 0.5).clamp(0.0, 1.0);

        // Blink rate elevation indicator
        let blink_elevation = if self.baseline_blink_rate > 0.0 {
            (self.blink_rate - self.baseline_blink_rate)
                / self.baseline_blink_rate
        } else {
            0.0
        };
        let blink_indicator = (blink_elevation / 0.5).clamp(0.0, 1.0);

        // Theta/beta ratio indicator
        let current_ratio = if self.baseline_beta > 0.001 {
            let current_theta =
                self.baseline_theta * (1.0 + self.theta_trend);
            let current_beta = self.baseline_beta * (1.0 + self.beta_trend);
            if current_beta > 0.001 {
                current_theta / current_beta
            } else {
                0.0
            }
        } else {
            0.0
        };
        let baseline_ratio = self.baseline_theta / self.baseline_beta;
        let ratio_increase = if baseline_ratio > 0.0 {
            (current_ratio - baseline_ratio) / baseline_ratio
        } else {
            0.0
        };
        let ratio_indicator = (ratio_increase / 1.0).clamp(0.0, 1.0);

        // Weighted sum
        let index = self.config.theta_weight * theta_indicator
            + self.config.alpha_weight * alpha_indicator
            + self.config.beta_decrease_weight * beta_indicator
            + self.config.blink_rate_weight * blink_indicator
            + self.config.ratio_weight * ratio_indicator;

        index.clamp(0.0, 1.0)
    }

    /// Classify fatigue level from composite index.
    fn classify(
        index: f64,
        config: &EegFatigueConfig,
    ) -> EegFatigueLevel {
        if index >= config.severe_threshold {
            EegFatigueLevel::Severe
        } else if index >= config.moderate_threshold {
            EegFatigueLevel::Moderate
        } else if index >= config.mild_threshold {
            EegFatigueLevel::Mild
        } else {
            EegFatigueLevel::None
        }
    }

    // ── IPC adapters ──────────────────────────────────────

    /// Generate IPC status s-expression.
    pub fn status_sexp(&self) -> String {
        format!(
            "(:enabled {} :monitoring {} :level :{} :fatigue-index {:.3} :theta-trend {:.3} :alpha-trend {:.3} :beta-trend {:.3} :blink-rate {:.1})",
            if self.config.enabled { "t" } else { "nil" },
            if self.monitoring { "t" } else { "nil" },
            self.level.as_str(),
            self.fatigue_index,
            self.theta_trend,
            self.alpha_trend,
            self.beta_trend,
            self.blink_rate,
        )
    }

    /// Generate IPC config s-expression.
    pub fn config_sexp(&self) -> String {
        format!(
            "(:enabled {} :theta-weight {:.2} :alpha-weight {:.2} :blink-rate-weight {:.2} :beta-decrease-weight {:.2} :ratio-weight {:.2} :mild-threshold {:.2} :moderate-threshold {:.2} :severe-threshold {:.2} :auto-save-on-severe {})",
            if self.config.enabled { "t" } else { "nil" },
            self.config.theta_weight,
            self.config.alpha_weight,
            self.config.blink_rate_weight,
            self.config.beta_decrease_weight,
            self.config.ratio_weight,
            self.config.mild_threshold,
            self.config.moderate_threshold,
            self.config.severe_threshold,
            if self.config.auto_save_on_severe { "t" } else { "nil" },
        )
    }
}

// ── Tests ───────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_monitor() {
        let m = EegFatigueMonitor::new();
        assert!(m.config.enabled);
        assert!(!m.monitoring);
        assert_eq!(m.level, EegFatigueLevel::None);
        assert!(m.fatigue_index.abs() < f64::EPSILON);
    }

    #[test]
    fn test_level_roundtrip() {
        let levels = vec![
            ("none", EegFatigueLevel::None),
            ("mild", EegFatigueLevel::Mild),
            ("moderate", EegFatigueLevel::Moderate),
            ("severe", EegFatigueLevel::Severe),
        ];
        for (s, l) in &levels {
            assert_eq!(EegFatigueLevel::from_str(s), Some(*l));
            assert_eq!(l.as_str(), *s);
        }
        assert_eq!(EegFatigueLevel::from_str("invalid"), None);
    }

    #[test]
    fn test_classify_thresholds() {
        let config = EegFatigueConfig::default();
        assert_eq!(
            EegFatigueMonitor::classify(0.0, &config),
            EegFatigueLevel::None,
        );
        assert_eq!(
            EegFatigueMonitor::classify(0.3, &config),
            EegFatigueLevel::None,
        );
        assert_eq!(
            EegFatigueMonitor::classify(0.5, &config),
            EegFatigueLevel::Mild,
        );
        assert_eq!(
            EegFatigueMonitor::classify(0.7, &config),
            EegFatigueLevel::Moderate,
        );
        assert_eq!(
            EegFatigueMonitor::classify(0.95, &config),
            EegFatigueLevel::Severe,
        );
    }

    #[test]
    fn test_set_baseline() {
        let mut m = EegFatigueMonitor::new();
        m.set_baseline(2.0, 3.0, 4.0);
        assert!((m.baseline_theta - 2.0).abs() < f64::EPSILON);
        assert!((m.baseline_alpha - 3.0).abs() < f64::EPSILON);
        assert!((m.baseline_beta - 4.0).abs() < f64::EPSILON);

        // Zero baseline should clamp to 0.001
        m.set_baseline(0.0, 0.0, 0.0);
        assert!(m.baseline_theta >= 0.001);
    }

    #[test]
    fn test_update_produces_fatigue() {
        let mut m = EegFatigueMonitor::new();
        m.start_monitoring();
        m.set_baseline(1.0, 1.0, 1.0);

        // Theta doubled, alpha up 50%, beta halved, high blink rate
        m.update(2.0, 1.5, 0.5, 25.0);
        assert!(
            m.fatigue_index > 0.3,
            "Fatigue index should be elevated, got {:.3}",
            m.fatigue_index,
        );
        assert!(m.theta_trend > 0.0);
        assert!(m.beta_trend < 0.0);
    }

    #[test]
    fn test_monitoring_disabled_ignores_update() {
        let mut m = EegFatigueMonitor::new();
        // Not started
        m.update(5.0, 5.0, 0.1, 50.0);
        assert!(
            m.fatigue_index.abs() < f64::EPSILON,
            "Should not update when not monitoring",
        );
    }

    #[test]
    fn test_no_fatigue_at_baseline() {
        let mut m = EegFatigueMonitor::new();
        m.start_monitoring();
        m.set_baseline(1.0, 1.0, 1.0);
        m.set_baseline_blink_rate(15.0);

        // Values at baseline
        m.update(1.0, 1.0, 1.0, 15.0);
        assert!(
            m.fatigue_index < 0.1,
            "No fatigue expected at baseline, got {:.3}",
            m.fatigue_index,
        );
        assert_eq!(m.level, EegFatigueLevel::None);
    }

    #[test]
    fn test_status_sexp() {
        let m = EegFatigueMonitor::new();
        let sexp = m.status_sexp();
        assert!(sexp.contains(":enabled t"));
        assert!(sexp.contains(":monitoring nil"));
        assert!(sexp.contains(":level :none"));
        assert!(sexp.contains(":fatigue-index"));
    }

    #[test]
    fn test_config_sexp() {
        let m = EegFatigueMonitor::new();
        let sexp = m.config_sexp();
        assert!(sexp.contains(":theta-weight 0.30"));
        assert!(sexp.contains(":alpha-weight 0.25"));
        assert!(sexp.contains(":mild-threshold 0.50"));
        assert!(sexp.contains(":auto-save-on-severe t"));
    }
}
