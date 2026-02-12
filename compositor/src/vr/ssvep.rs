//! SSVEP (Steady-State Visual Evoked Potential) classifier.
//!
//! Detects frequency-domain responses to flickering stimuli for
//! workspace selection.  Each workspace is associated with a unique
//! flicker frequency; the classifier measures signal power at each
//! target frequency and selects the strongest above SNR threshold.
//! No openxrs dependency; compiles unconditionally.

use std::f64::consts::PI;

// ── SsvepConfig ──────────────────────────────────────────

/// Configuration for SSVEP classification.
#[derive(Debug, Clone)]
pub struct SsvepConfig {
    /// Whether SSVEP classification is enabled.
    pub enabled: bool,
    /// Analysis window duration (seconds).
    pub window_seconds: f64,
    /// Minimum signal-to-noise ratio (dB) to accept a classification.
    pub min_snr_db: f64,
    /// Minimum confidence (0.0-1.0) to accept a classification.
    pub min_confidence: f64,
    /// Target frequencies: (workspace_id, frequency_hz).
    pub frequencies: Vec<(usize, f64)>,
}

impl Default for SsvepConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            window_seconds: 3.0,
            min_snr_db: 3.0,
            min_confidence: 0.7,
            frequencies: vec![
                (1, 12.0),
                (2, 15.0),
                (3, 20.0),
                (4, 24.0),
            ],
        }
    }
}

// ── SsvepResult ─────────────────────────────────────────

/// Result of a successful SSVEP classification.
#[derive(Debug, Clone)]
pub struct SsvepResult {
    /// Workspace ID associated with the detected frequency.
    pub workspace_id: usize,
    /// Detected frequency in Hz.
    pub frequency: f64,
    /// Signal-to-noise ratio in dB.
    pub snr_db: f64,
    /// Classification confidence (0.0-1.0).
    pub confidence: f64,
    /// Timestamp of the classification.
    pub timestamp: f64,
}

// ── SsvepClassifier ─────────────────────────────────────

/// SSVEP frequency-domain classifier for workspace selection.
pub struct SsvepClassifier {
    /// Configuration.
    pub config: SsvepConfig,
    /// Whether classification is currently active.
    pub active: bool,
    /// Last successful classification result.
    pub last_result: Option<SsvepResult>,
    /// Total number of classifications performed.
    pub classifications: u64,
    /// Computed power at each target frequency (internal buffer).
    frequency_powers: Vec<f64>,
}

impl SsvepClassifier {
    /// Create a new SSVEP classifier with default configuration.
    pub fn new() -> Self {
        let num_freqs = SsvepConfig::default().frequencies.len();
        Self {
            config: SsvepConfig::default(),
            active: false,
            last_result: None,
            classifications: 0,
            frequency_powers: vec![0.0; num_freqs],
        }
    }

    /// Start classification.
    pub fn start(&mut self) {
        self.active = true;
    }

    /// Stop classification.
    pub fn stop(&mut self) {
        self.active = false;
    }

    /// Classify an EEG data window against target frequencies.
    ///
    /// Uses the Goertzel algorithm to compute power at each target
    /// frequency, then selects the strongest response above the
    /// SNR and confidence thresholds.
    pub fn classify(
        &mut self,
        eeg_data: &[f64],
        sample_rate: u32,
    ) -> Option<SsvepResult> {
        if !self.active || !self.config.enabled || eeg_data.is_empty() {
            return None;
        }

        // Copy config values before any mutation
        let min_snr_db = self.config.min_snr_db;
        let min_confidence = self.config.min_confidence;
        let frequencies: Vec<(usize, f64)> = self.config.frequencies.clone();

        // Compute power at each target frequency
        let mut powers = Vec::with_capacity(frequencies.len());
        for &(_ws_id, freq) in &frequencies {
            let power = Self::compute_power_at_freq(eeg_data, freq, sample_rate);
            powers.push(power);
        }

        // Compute mean power for noise estimate (excluding target bins)
        let total_power: f64 = powers.iter().sum();
        let n = powers.len() as f64;

        // Find the strongest frequency
        let mut best_idx = 0;
        let mut best_power = 0.0f64;
        for (i, &p) in powers.iter().enumerate() {
            if p > best_power {
                best_power = p;
                best_idx = i;
            }
        }

        // Compute SNR in dB
        let noise_power = if n > 1.0 {
            (total_power - best_power) / (n - 1.0)
        } else {
            0.001
        };
        let noise_floor = noise_power.max(1e-10);
        let snr_db = 10.0 * (best_power / noise_floor).log10();

        // Compute confidence from SNR
        let confidence = (snr_db / 10.0).clamp(0.0, 1.0);

        // Store computed powers
        self.frequency_powers = powers;

        // Check thresholds
        if snr_db < min_snr_db
            || confidence < min_confidence
        {
            return None;
        }

        let (ws_id, freq) = frequencies[best_idx];
        let result = SsvepResult {
            workspace_id: ws_id,
            frequency: freq,
            snr_db,
            confidence,
            timestamp: 0.0, // caller should set actual timestamp
        };

        self.last_result = Some(result.clone());
        self.classifications += 1;

        Some(result)
    }

    /// Compute spectral power at a specific frequency using the
    /// Goertzel algorithm (O(N) single-frequency DFT).
    fn compute_power_at_freq(
        data: &[f64],
        freq: f64,
        sample_rate: u32,
    ) -> f64 {
        let n = data.len();
        if n == 0 || sample_rate == 0 {
            return 0.0;
        }
        let k = (freq * n as f64 / sample_rate as f64).round();
        let omega = 2.0 * PI * k / n as f64;
        let coeff = 2.0 * omega.cos();

        let mut s_prev = 0.0;
        let mut s_prev2 = 0.0;

        for &sample in data {
            let s = sample + coeff * s_prev - s_prev2;
            s_prev2 = s_prev;
            s_prev = s;
        }

        // Power = |X(k)|^2 / N^2
        let real = s_prev - s_prev2 * omega.cos();
        let imag = s_prev2 * omega.sin();
        let power = (real * real + imag * imag) / (n * n) as f64;
        power
    }

    /// Set the target frequency mapping.
    pub fn set_frequencies(&mut self, freqs: Vec<(usize, f64)>) {
        self.frequency_powers = vec![0.0; freqs.len()];
        self.config.frequencies = freqs;
    }

    // ── IPC adapters ──────────────────────────────────────

    /// Generate IPC status s-expression.
    pub fn status_sexp(&self) -> String {
        let last_str = match &self.last_result {
            Some(r) => format!(
                "(:workspace {} :frequency {:.1} :snr-db {:.1} :confidence {:.2})",
                r.workspace_id, r.frequency, r.snr_db, r.confidence,
            ),
            None => "nil".to_string(),
        };
        format!(
            "(:enabled {} :active {} :classifications {} :last-result {})",
            if self.config.enabled { "t" } else { "nil" },
            if self.active { "t" } else { "nil" },
            self.classifications,
            last_str,
        )
    }

    /// Generate IPC config s-expression.
    pub fn config_sexp(&self) -> String {
        let freqs_str: Vec<String> = self
            .config
            .frequencies
            .iter()
            .map(|(ws, f)| format!("({} {:.1})", ws, f))
            .collect();
        format!(
            "(:enabled {} :window-seconds {:.1} :min-snr-db {:.1} :min-confidence {:.2} :frequencies ({}))",
            if self.config.enabled { "t" } else { "nil" },
            self.config.window_seconds,
            self.config.min_snr_db,
            self.config.min_confidence,
            freqs_str.join(" "),
        )
    }
}

// ── Tests ───────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::f64::consts::PI;

    /// Generate a sine wave at a given frequency.
    fn sine_wave(freq: f64, sample_rate: u32, n_samples: usize) -> Vec<f64> {
        (0..n_samples)
            .map(|i| {
                let t = i as f64 / sample_rate as f64;
                (2.0 * PI * freq * t).sin()
            })
            .collect()
    }

    #[test]
    fn test_new_classifier() {
        let c = SsvepClassifier::new();
        assert!(c.config.enabled);
        assert!(!c.active);
        assert!(c.last_result.is_none());
        assert_eq!(c.classifications, 0);
        assert_eq!(c.config.frequencies.len(), 4);
    }

    #[test]
    fn test_start_stop() {
        let mut c = SsvepClassifier::new();
        assert!(!c.active);
        c.start();
        assert!(c.active);
        c.stop();
        assert!(!c.active);
    }

    #[test]
    fn test_classify_detects_dominant_frequency() {
        let mut c = SsvepClassifier::new();
        c.start();
        c.config.min_snr_db = 1.0;
        c.config.min_confidence = 0.1;

        // Generate a strong 15 Hz signal (workspace 2)
        let data = sine_wave(15.0, 250, 750);
        let result = c.classify(&data, 250);

        assert!(
            result.is_some(),
            "Should detect 15 Hz signal",
        );
        let r = result.unwrap();
        assert_eq!(r.workspace_id, 2);
        assert!((r.frequency - 15.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_classify_inactive_returns_none() {
        let mut c = SsvepClassifier::new();
        // Not started
        let data = sine_wave(15.0, 250, 750);
        let result = c.classify(&data, 250);
        assert!(result.is_none());
    }

    #[test]
    fn test_classify_empty_data() {
        let mut c = SsvepClassifier::new();
        c.start();
        let result = c.classify(&[], 250);
        assert!(result.is_none());
    }

    #[test]
    fn test_set_frequencies() {
        let mut c = SsvepClassifier::new();
        c.set_frequencies(vec![(10, 8.0), (20, 16.0)]);
        assert_eq!(c.config.frequencies.len(), 2);
        assert_eq!(c.frequency_powers.len(), 2);
    }

    #[test]
    fn test_goertzel_power() {
        // A pure sine at 10 Hz should have most power at 10 Hz
        let data = sine_wave(10.0, 250, 250);
        let p10 = SsvepClassifier::compute_power_at_freq(&data, 10.0, 250);
        let p20 = SsvepClassifier::compute_power_at_freq(&data, 20.0, 250);
        assert!(
            p10 > p20 * 10.0,
            "Power at 10Hz ({:.6}) should dominate 20Hz ({:.6})",
            p10,
            p20,
        );
    }

    #[test]
    fn test_status_and_config_sexp() {
        let c = SsvepClassifier::new();
        let status = c.status_sexp();
        assert!(status.contains(":enabled t"));
        assert!(status.contains(":active nil"));
        assert!(status.contains(":classifications 0"));
        assert!(status.contains(":last-result nil"));

        let config = c.config_sexp();
        assert!(config.contains(":window-seconds 3.0"));
        assert!(config.contains(":min-snr-db 3.0"));
        assert!(config.contains("(1 12.0)"));
    }
}
