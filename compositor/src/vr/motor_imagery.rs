//! Motor imagery classification — left/right hand and foot movement.
//!
//! Detects event-related desynchronization (ERD) in the mu (8-12 Hz)
//! and beta (12-30 Hz) bands over sensorimotor cortex (C3, C4, Cz).
//! Uses a simple CSP-like feature extraction with threshold-based
//! classification (no external ML dependencies).
//! No openxrs dependency; compiles unconditionally.

// ── MiClass ─────────────────────────────────────────────

/// Motor imagery classification result.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MiClass {
    /// Left hand imagery (contralateral ERD at C4).
    LeftHand,
    /// Right hand imagery (contralateral ERD at C3).
    RightHand,
    /// Foot imagery (ERD at Cz).
    Foot,
    /// Rest / no imagery detected.
    Rest,
}

impl MiClass {
    /// String representation for IPC.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::LeftHand => "left-hand",
            Self::RightHand => "right-hand",
            Self::Foot => "foot",
            Self::Rest => "rest",
        }
    }

    /// Parse from string.
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "left-hand" => Some(Self::LeftHand),
            "right-hand" => Some(Self::RightHand),
            "foot" => Some(Self::Foot),
            "rest" => Some(Self::Rest),
            _ => None,
        }
    }
}

// ── MiConfig ────────────────────────────────────────────

/// Configuration for motor imagery classification.
#[derive(Debug, Clone)]
pub struct MiConfig {
    /// Whether motor imagery classification is enabled.
    pub enabled: bool,
    /// Analysis window duration (seconds).
    pub window_seconds: f64,
    /// Mu band frequency range (Hz).
    pub mu_band: (f64, f64),
    /// Beta band frequency range (Hz).
    pub beta_band: (f64, f64),
    /// Minimum confidence to accept a classification (0.0-1.0).
    pub min_confidence: f64,
    /// Whether calibration has been performed.
    pub calibrated: bool,
}

impl Default for MiConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            window_seconds: 2.0,
            mu_band: (8.0, 12.0),
            beta_band: (12.0, 30.0),
            min_confidence: 0.6,
            calibrated: false,
        }
    }
}

// ── MiResult ────────────────────────────────────────────

/// Result of a motor imagery classification.
#[derive(Debug, Clone)]
pub struct MiResult {
    /// Classified motor imagery type.
    pub class: MiClass,
    /// Classification confidence (0.0-1.0).
    pub confidence: f64,
    /// ERD at C3 (left motor cortex).
    pub erd_c3: f64,
    /// ERD at C4 (right motor cortex).
    pub erd_c4: f64,
    /// ERD at Cz (midline, vertex).
    pub erd_cz: f64,
    /// Timestamp of the classification.
    pub timestamp: f64,
}

// ── MiClassifier ────────────────────────────────────────

/// Motor imagery classifier using ERD-based feature extraction.
pub struct MiClassifier {
    /// Configuration.
    pub config: MiConfig,
    /// Last classification result.
    pub last_result: Option<MiResult>,
    /// Total number of classifications performed.
    pub classifications: u64,
    /// Baseline mu+beta power at C3 (from calibration).
    baseline_c3: f64,
    /// Baseline mu+beta power at C4 (from calibration).
    baseline_c4: f64,
    /// Baseline mu+beta power at Cz (from calibration).
    baseline_cz: f64,
    /// Per-class ERD thresholds [left, right, foot].
    thresholds: [f64; 3],
    /// Calibration trial data: (class, c3, c4, cz).
    calibration_trials: Vec<(String, f64, f64, f64)>,
    /// Whether currently in calibration mode.
    calibrating: bool,
}

impl MiClassifier {
    /// Create a new motor imagery classifier with defaults.
    pub fn new() -> Self {
        Self {
            config: MiConfig::default(),
            last_result: None,
            classifications: 0,
            baseline_c3: 1.0,
            baseline_c4: 1.0,
            baseline_cz: 1.0,
            thresholds: [-0.3, -0.3, -0.3],
            calibration_trials: Vec::with_capacity(64),
            calibrating: false,
        }
    }

    /// Start calibration mode.
    pub fn start_calibration(&mut self) {
        self.calibrating = true;
        self.calibration_trials.clear();
        self.config.calibrated = false;
    }

    /// Record a calibration trial with a known class label.
    pub fn record_calibration_trial(
        &mut self,
        class: &str,
        c3_power: f64,
        c4_power: f64,
        cz_power: f64,
    ) {
        if !self.calibrating {
            return;
        }
        self.calibration_trials
            .push((class.to_string(), c3_power, c4_power, cz_power));
    }

    /// Finish calibration and compute baselines and thresholds.
    pub fn finish_calibration(&mut self) -> Result<(), String> {
        self.calibrating = false;

        // Separate rest trials for baseline
        let rest_trials: Vec<_> = self
            .calibration_trials
            .iter()
            .filter(|(c, _, _, _)| c == "rest")
            .collect();

        if rest_trials.len() < 5 {
            return Err(format!(
                "Need at least 5 rest trials, got {}",
                rest_trials.len(),
            ));
        }

        // Compute baseline from rest trials
        let n = rest_trials.len() as f64;
        self.baseline_c3 =
            rest_trials.iter().map(|(_, c3, _, _)| c3).sum::<f64>() / n;
        self.baseline_c4 =
            rest_trials.iter().map(|(_, _, c4, _)| c4).sum::<f64>() / n;
        self.baseline_cz =
            rest_trials.iter().map(|(_, _, _, cz)| cz).sum::<f64>() / n;

        // Ensure baselines are positive
        if self.baseline_c3 <= 0.0
            || self.baseline_c4 <= 0.0
            || self.baseline_cz <= 0.0
        {
            return Err("Baseline power too low, check signal".to_string());
        }

        // Compute per-class ERD from labeled trials
        for (idx, class_name) in
            ["left-hand", "right-hand", "foot"].iter().enumerate()
        {
            let class_trials: Vec<_> = self
                .calibration_trials
                .iter()
                .filter(|(c, _, _, _)| c == class_name)
                .collect();
            if class_trials.len() >= 3 {
                let mean_erd: f64 = class_trials
                    .iter()
                    .map(|(_, c3, c4, cz)| {
                        let erd_c3 = Self::compute_erd(*c3, self.baseline_c3);
                        let erd_c4 = Self::compute_erd(*c4, self.baseline_c4);
                        let erd_cz = Self::compute_erd(*cz, self.baseline_cz);
                        match idx {
                            0 => erd_c4, // left hand -> C4
                            1 => erd_c3, // right hand -> C3
                            2 => erd_cz, // foot -> Cz
                            _ => 0.0,
                        }
                    })
                    .sum::<f64>()
                    / class_trials.len() as f64;
                self.thresholds[idx] = mean_erd * 0.5; // 50% of mean ERD
            }
        }

        self.config.calibrated = true;
        self.calibration_trials.clear();
        Ok(())
    }

    /// Classify motor imagery from current mu+beta band powers.
    ///
    /// Powers should be computed from the mu and beta bands at
    /// C3, C4, and Cz electrode positions.
    pub fn classify(
        &mut self,
        c3_power: f64,
        c4_power: f64,
        cz_power: f64,
    ) -> Option<MiResult> {
        if !self.config.enabled || !self.config.calibrated {
            return None;
        }

        let erd_c3 = Self::compute_erd(c3_power, self.baseline_c3);
        let erd_c4 = Self::compute_erd(c4_power, self.baseline_c4);
        let erd_cz = Self::compute_erd(cz_power, self.baseline_cz);

        // Classification: find the most desynchronized channel
        // Left hand -> contralateral C4 ERD
        // Right hand -> contralateral C3 ERD
        // Foot -> Cz ERD
        let candidates = [
            (MiClass::LeftHand, erd_c4, self.thresholds[0]),
            (MiClass::RightHand, erd_c3, self.thresholds[1]),
            (MiClass::Foot, erd_cz, self.thresholds[2]),
        ];

        let mut best_class = MiClass::Rest;
        let mut best_erd = 0.0f64;
        let mut best_threshold = 0.0f64;

        for &(class, erd, threshold) in &candidates {
            // ERD is negative (desynchronization); more negative = stronger
            if erd < threshold && erd < best_erd {
                best_erd = erd;
                best_class = class;
                best_threshold = threshold;
            }
        }

        // Compute confidence from ERD magnitude relative to threshold
        let confidence = if best_threshold < 0.0 {
            (best_erd / best_threshold).clamp(0.0, 1.0)
        } else {
            0.0
        };

        if best_class == MiClass::Rest
            || confidence < self.config.min_confidence
        {
            let result = MiResult {
                class: MiClass::Rest,
                confidence: 0.0,
                erd_c3,
                erd_c4,
                erd_cz,
                timestamp: 0.0,
            };
            self.last_result = Some(result.clone());
            return Some(result);
        }

        let result = MiResult {
            class: best_class,
            confidence,
            erd_c3,
            erd_c4,
            erd_cz,
            timestamp: 0.0,
        };

        self.last_result = Some(result.clone());
        self.classifications += 1;
        Some(result)
    }

    /// Compute event-related desynchronization.
    ///
    /// ERD = (current - baseline) / baseline
    /// Negative values indicate desynchronization (motor imagery).
    fn compute_erd(current: f64, baseline: f64) -> f64 {
        if baseline <= 0.0 {
            return 0.0;
        }
        (current - baseline) / baseline
    }

    // ── IPC adapters ──────────────────────────────────────

    /// Generate IPC status s-expression.
    pub fn status_sexp(&self) -> String {
        let last_str = match &self.last_result {
            Some(r) => format!(
                "(:class :{} :confidence {:.2} :erd-c3 {:.3} :erd-c4 {:.3} :erd-cz {:.3})",
                r.class.as_str(), r.confidence, r.erd_c3, r.erd_c4, r.erd_cz,
            ),
            None => "nil".to_string(),
        };
        format!(
            "(:enabled {} :calibrated {} :classifications {} :last-result {})",
            if self.config.enabled { "t" } else { "nil" },
            if self.config.calibrated { "t" } else { "nil" },
            self.classifications,
            last_str,
        )
    }

    /// Generate IPC config s-expression.
    pub fn config_sexp(&self) -> String {
        format!(
            "(:enabled {} :window-seconds {:.1} :mu-band ({:.1} {:.1}) :beta-band ({:.1} {:.1}) :min-confidence {:.2} :calibrated {} :thresholds ({:.3} {:.3} {:.3}))",
            if self.config.enabled { "t" } else { "nil" },
            self.config.window_seconds,
            self.config.mu_band.0,
            self.config.mu_band.1,
            self.config.beta_band.0,
            self.config.beta_band.1,
            self.config.min_confidence,
            if self.config.calibrated { "t" } else { "nil" },
            self.thresholds[0],
            self.thresholds[1],
            self.thresholds[2],
        )
    }
}

// ── Tests ───────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_classifier() {
        let c = MiClassifier::new();
        assert!(c.config.enabled);
        assert!(!c.config.calibrated);
        assert!(c.last_result.is_none());
        assert_eq!(c.classifications, 0);
    }

    #[test]
    fn test_class_roundtrip() {
        let classes = vec![
            ("left-hand", MiClass::LeftHand),
            ("right-hand", MiClass::RightHand),
            ("foot", MiClass::Foot),
            ("rest", MiClass::Rest),
        ];
        for (s, cls) in &classes {
            assert_eq!(MiClass::from_str(s), Some(*cls));
            assert_eq!(cls.as_str(), *s);
        }
        assert_eq!(MiClass::from_str("invalid"), None);
    }

    #[test]
    fn test_compute_erd() {
        // Current = 0.5 * baseline -> ERD = -0.5 (50% desynchronization)
        let erd = MiClassifier::compute_erd(0.5, 1.0);
        assert!((erd - (-0.5)).abs() < f64::EPSILON);

        // Current = baseline -> ERD = 0
        let erd = MiClassifier::compute_erd(1.0, 1.0);
        assert!(erd.abs() < f64::EPSILON);

        // Current > baseline -> positive (synchronization)
        let erd = MiClassifier::compute_erd(1.5, 1.0);
        assert!((erd - 0.5).abs() < f64::EPSILON);

        // Zero baseline
        let erd = MiClassifier::compute_erd(1.0, 0.0);
        assert!(erd.abs() < f64::EPSILON);
    }

    #[test]
    fn test_calibration_flow() {
        let mut c = MiClassifier::new();
        c.start_calibration();
        assert!(c.calibrating);

        // Record rest trials (baseline)
        for _ in 0..10 {
            c.record_calibration_trial("rest", 1.0, 1.0, 1.0);
        }
        // Record left-hand trials (C4 desynchronized)
        for _ in 0..5 {
            c.record_calibration_trial("left-hand", 1.0, 0.5, 1.0);
        }
        // Record right-hand trials (C3 desynchronized)
        for _ in 0..5 {
            c.record_calibration_trial("right-hand", 0.5, 1.0, 1.0);
        }
        // Record foot trials (Cz desynchronized)
        for _ in 0..5 {
            c.record_calibration_trial("foot", 1.0, 1.0, 0.5);
        }

        let result = c.finish_calibration();
        assert!(result.is_ok(), "Calibration failed: {:?}", result);
        assert!(c.config.calibrated);
    }

    #[test]
    fn test_calibration_insufficient_rest() {
        let mut c = MiClassifier::new();
        c.start_calibration();
        for _ in 0..3 {
            c.record_calibration_trial("rest", 1.0, 1.0, 1.0);
        }
        let result = c.finish_calibration();
        assert!(result.is_err());
    }

    #[test]
    fn test_classify_left_hand() {
        let mut c = MiClassifier::new();
        // Manually set calibration
        c.config.calibrated = true;
        c.baseline_c3 = 1.0;
        c.baseline_c4 = 1.0;
        c.baseline_cz = 1.0;
        c.thresholds = [-0.25, -0.25, -0.25];
        c.config.min_confidence = 0.3;

        // Left hand: C4 desynchronized
        let result = c.classify(1.0, 0.4, 1.0);
        assert!(result.is_some());
        let r = result.unwrap();
        assert_eq!(r.class, MiClass::LeftHand);
        assert!(r.erd_c4 < -0.25);
    }

    #[test]
    fn test_classify_rest_when_no_erd() {
        let mut c = MiClassifier::new();
        c.config.calibrated = true;
        c.baseline_c3 = 1.0;
        c.baseline_c4 = 1.0;
        c.baseline_cz = 1.0;
        c.thresholds = [-0.25, -0.25, -0.25];

        // No desynchronization
        let result = c.classify(1.0, 1.0, 1.0);
        assert!(result.is_some());
        assert_eq!(result.unwrap().class, MiClass::Rest);
    }

    #[test]
    fn test_status_and_config_sexp() {
        let c = MiClassifier::new();
        let status = c.status_sexp();
        assert!(status.contains(":enabled t"));
        assert!(status.contains(":calibrated nil"));
        assert!(status.contains(":last-result nil"));

        let config = c.config_sexp();
        assert!(config.contains(":window-seconds 2.0"));
        assert!(config.contains(":mu-band (8.0 12.0)"));
        assert!(config.contains(":beta-band (12.0 30.0)"));
    }
}
