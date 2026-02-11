//! P300 event-related potential detector.
//!
//! Detects the P300 component — a positive voltage deflection occurring
//! ~300ms after a rare or task-relevant stimulus (oddball paradigm).
//! Used for BCI selection via rapid serial visual presentation.
//! No openxrs dependency; compiles unconditionally.

// ── P300Config ──────────────────────────────────────────

/// Configuration for P300 detection.
#[derive(Debug, Clone)]
pub struct P300Config {
    /// Whether P300 detection is enabled.
    pub enabled: bool,
    /// Number of stimulus repetitions to average before detection.
    pub repetitions: u32,
    /// Stimulus onset asynchrony (milliseconds).
    pub soa_ms: f64,
    /// Stimulus presentation duration (milliseconds).
    pub stimulus_duration_ms: f64,
    /// Detection window post-stimulus (min_ms, max_ms).
    pub detection_window_ms: (f64, f64),
    /// Minimum amplitude threshold (microvolts).
    pub amplitude_threshold_uv: f64,
    /// Minimum confidence to accept a detection (0.0-1.0).
    pub min_confidence: f64,
}

impl Default for P300Config {
    fn default() -> Self {
        Self {
            enabled: true,
            repetitions: 5,
            soa_ms: 200.0,
            stimulus_duration_ms: 100.0,
            detection_window_ms: (250.0, 500.0),
            amplitude_threshold_uv: 5.0,
            min_confidence: 0.7,
        }
    }
}

// ── P300Result ──────────────────────────────────────────

/// Result of a successful P300 detection.
#[derive(Debug, Clone)]
pub struct P300Result {
    /// ID of the target that elicited the P300.
    pub target_id: usize,
    /// Classification confidence (0.0-1.0).
    pub confidence: f64,
    /// Latency of the P300 peak (milliseconds post-stimulus).
    pub latency_ms: f64,
    /// Amplitude of the P300 peak (microvolts).
    pub amplitude_uv: f64,
    /// Number of repetitions used for averaging.
    pub repetitions_used: u32,
    /// Timestamp of the detection.
    pub timestamp: f64,
}

// ── P300Detector ────────────────────────────────────────

/// P300 event-related potential detector with oddball averaging.
pub struct P300Detector {
    /// Configuration.
    pub config: P300Config,
    /// Whether detection is currently active.
    pub active: bool,
    /// Last successful detection result.
    pub last_result: Option<P300Result>,
    /// Total number of detections performed.
    pub detections: u64,
    /// Number of targets in the current paradigm.
    target_count: usize,
    /// Accumulated ERP buffers per target: repetition_buffers[target][repetition].
    repetition_buffers: Vec<Vec<Vec<f64>>>,
    /// Current repetition index.
    current_repetition: u32,
}

impl P300Detector {
    /// Create a new P300 detector with default configuration.
    pub fn new() -> Self {
        Self {
            config: P300Config::default(),
            active: false,
            last_result: None,
            detections: 0,
            target_count: 0,
            repetition_buffers: Vec::new(),
            current_repetition: 0,
        }
    }

    /// Start a P300 detection session with a given number of targets.
    pub fn start(&mut self, num_targets: usize) {
        self.target_count = num_targets;
        self.repetition_buffers = vec![Vec::new(); num_targets];
        self.current_repetition = 0;
        self.active = true;
    }

    /// Stop the current detection session.
    pub fn stop(&mut self) {
        self.active = false;
        self.current_repetition = 0;
    }

    /// Record a stimulus-locked EEG window for a given target.
    ///
    /// `eeg_window` should contain the post-stimulus epoch covering
    /// at least the detection window.
    pub fn record_stimulus(
        &mut self,
        target_id: usize,
        eeg_window: &[f64],
    ) {
        if !self.active || !self.config.enabled {
            return;
        }
        if target_id >= self.target_count {
            return;
        }
        self.repetition_buffers[target_id].push(eeg_window.to_vec());
    }

    /// Attempt to detect a P300 from accumulated stimulus epochs.
    ///
    /// Averages across repetitions for each target, finds the peak
    /// in the detection window, and selects the target with the
    /// strongest average P300 component.
    pub fn detect(&mut self) -> Option<P300Result> {
        if !self.active || !self.config.enabled {
            return None;
        }

        // Check that all targets have enough repetitions
        let min_reps = self
            .repetition_buffers
            .iter()
            .map(|b| b.len() as u32)
            .min()
            .unwrap_or(0);

        if min_reps < self.config.repetitions {
            return None;
        }

        // Compute averaged ERP per target and find peak
        let mut best_target = 0;
        let mut best_amplitude = 0.0f64;
        let mut best_latency_idx = 0usize;
        let mut best_erp_len = 0usize;

        for (tid, buffers) in self.repetition_buffers.iter().enumerate() {
            let avg = Self::average_erp(buffers);
            if avg.is_empty() {
                continue;
            }

            // Detection window in samples (assume 250 Hz for now)
            let sample_rate = 250.0; // samples per second
            let ms_per_sample = 1000.0 / sample_rate;
            let win_start =
                (self.config.detection_window_ms.0 / ms_per_sample) as usize;
            let win_end = (self.config.detection_window_ms.1 / ms_per_sample)
                .min(avg.len() as f64) as usize;

            let (amplitude, latency_idx) =
                Self::find_peak(&avg, (win_start, win_end));

            if amplitude > best_amplitude {
                best_amplitude = amplitude;
                best_latency_idx = latency_idx;
                best_target = tid;
                best_erp_len = avg.len();
            }
        }

        // Compute latency in milliseconds
        let ms_per_sample = 1000.0 / 250.0;
        let latency_ms = best_latency_idx as f64 * ms_per_sample;

        // Compute confidence based on amplitude relative to threshold
        let confidence = if self.config.amplitude_threshold_uv > 0.0 {
            (best_amplitude / (self.config.amplitude_threshold_uv * 2.0))
                .clamp(0.0, 1.0)
        } else {
            0.0
        };

        // Check thresholds
        if best_amplitude < self.config.amplitude_threshold_uv
            || confidence < self.config.min_confidence
        {
            return None;
        }

        let result = P300Result {
            target_id: best_target,
            confidence,
            latency_ms,
            amplitude_uv: best_amplitude,
            repetitions_used: min_reps,
            timestamp: 0.0, // caller sets actual timestamp
        };

        self.last_result = Some(result.clone());
        self.detections += 1;
        self.active = false; // detection complete

        Some(result)
    }

    /// Compute the average ERP waveform across repetition buffers.
    fn average_erp(buffers: &[Vec<f64>]) -> Vec<f64> {
        if buffers.is_empty() {
            return Vec::new();
        }
        let min_len = buffers.iter().map(|b| b.len()).min().unwrap_or(0);
        if min_len == 0 {
            return Vec::new();
        }
        let n = buffers.len() as f64;
        let mut avg = vec![0.0; min_len];
        for buf in buffers {
            for (i, &val) in buf.iter().take(min_len).enumerate() {
                avg[i] += val / n;
            }
        }
        avg
    }

    /// Find the peak (maximum) value and its index within a window.
    fn find_peak(erp: &[f64], window: (usize, usize)) -> (f64, usize) {
        let start = window.0.min(erp.len());
        let end = window.1.min(erp.len());
        if start >= end {
            return (0.0, 0);
        }
        let mut max_val = f64::NEG_INFINITY;
        let mut max_idx = start;
        for i in start..end {
            if erp[i] > max_val {
                max_val = erp[i];
                max_idx = i;
            }
        }
        (max_val.max(0.0), max_idx)
    }

    // ── IPC adapters ──────────────────────────────────────

    /// Generate IPC status s-expression.
    pub fn status_sexp(&self) -> String {
        let last_str = match &self.last_result {
            Some(r) => format!(
                "(:target {} :confidence {:.2} :latency-ms {:.1} :amplitude-uv {:.1} :reps {})",
                r.target_id, r.confidence, r.latency_ms, r.amplitude_uv, r.repetitions_used,
            ),
            None => "nil".to_string(),
        };
        format!(
            "(:enabled {} :active {} :targets {} :detections {} :last-result {})",
            if self.config.enabled { "t" } else { "nil" },
            if self.active { "t" } else { "nil" },
            self.target_count,
            self.detections,
            last_str,
        )
    }

    /// Generate IPC config s-expression.
    pub fn config_sexp(&self) -> String {
        format!(
            "(:enabled {} :repetitions {} :soa-ms {:.0} :stimulus-duration-ms {:.0} :detection-window ({:.0} {:.0}) :amplitude-threshold-uv {:.1} :min-confidence {:.2})",
            if self.config.enabled { "t" } else { "nil" },
            self.config.repetitions,
            self.config.soa_ms,
            self.config.stimulus_duration_ms,
            self.config.detection_window_ms.0,
            self.config.detection_window_ms.1,
            self.config.amplitude_threshold_uv,
            self.config.min_confidence,
        )
    }
}

// ── Tests ───────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_detector() {
        let d = P300Detector::new();
        assert!(d.config.enabled);
        assert!(!d.active);
        assert!(d.last_result.is_none());
        assert_eq!(d.detections, 0);
        assert_eq!(d.target_count, 0);
    }

    #[test]
    fn test_start_stop() {
        let mut d = P300Detector::new();
        d.start(6);
        assert!(d.active);
        assert_eq!(d.target_count, 6);
        assert_eq!(d.repetition_buffers.len(), 6);

        d.stop();
        assert!(!d.active);
    }

    #[test]
    fn test_record_stimulus() {
        let mut d = P300Detector::new();
        d.start(3);
        d.record_stimulus(0, &[1.0, 2.0, 3.0]);
        d.record_stimulus(0, &[1.5, 2.5, 3.5]);
        assert_eq!(d.repetition_buffers[0].len(), 2);
        assert_eq!(d.repetition_buffers[1].len(), 0);
    }

    #[test]
    fn test_record_invalid_target() {
        let mut d = P300Detector::new();
        d.start(2);
        d.record_stimulus(5, &[1.0, 2.0]); // out of range
        assert_eq!(d.repetition_buffers[0].len(), 0);
        assert_eq!(d.repetition_buffers[1].len(), 0);
    }

    #[test]
    fn test_detect_with_clear_p300() {
        let mut d = P300Detector::new();
        d.config.repetitions = 3;
        d.config.amplitude_threshold_uv = 2.0;
        d.config.min_confidence = 0.3;
        d.start(2);

        // Create epochs: target 0 has a strong P300 peak at ~300ms
        // At 250 Hz, 300ms = sample 75
        let epoch_len = 200; // 800ms at 250 Hz
        for _ in 0..3 {
            let mut epoch0 = vec![0.0; epoch_len];
            epoch0[75] = 10.0; // Strong P300 peak
            d.record_stimulus(0, &epoch0);

            let epoch1 = vec![0.5; epoch_len]; // No P300
            d.record_stimulus(1, &epoch1);
        }

        let result = d.detect();
        assert!(result.is_some(), "Should detect P300 in target 0");
        let r = result.unwrap();
        assert_eq!(r.target_id, 0);
        assert!(r.amplitude_uv > 5.0);
        assert_eq!(r.repetitions_used, 3);
    }

    #[test]
    fn test_detect_insufficient_repetitions() {
        let mut d = P300Detector::new();
        d.config.repetitions = 5;
        d.start(2);

        // Only 2 repetitions for each target
        for _ in 0..2 {
            d.record_stimulus(0, &[1.0; 200]);
            d.record_stimulus(1, &[0.5; 200]);
        }

        let result = d.detect();
        assert!(
            result.is_none(),
            "Should not detect with insufficient reps",
        );
    }

    #[test]
    fn test_average_erp() {
        let buffers = vec![
            vec![1.0, 2.0, 3.0],
            vec![3.0, 4.0, 5.0],
        ];
        let avg = P300Detector::average_erp(&buffers);
        assert_eq!(avg.len(), 3);
        assert!((avg[0] - 2.0).abs() < f64::EPSILON);
        assert!((avg[1] - 3.0).abs() < f64::EPSILON);
        assert!((avg[2] - 4.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_find_peak() {
        let data = vec![0.0, 1.0, 5.0, 3.0, 2.0, 8.0, 1.0];
        let (val, idx) = P300Detector::find_peak(&data, (0, 7));
        assert!((val - 8.0).abs() < f64::EPSILON);
        assert_eq!(idx, 5);

        // Windowed peak
        let (val, idx) = P300Detector::find_peak(&data, (0, 4));
        assert!((val - 5.0).abs() < f64::EPSILON);
        assert_eq!(idx, 2);
    }

    #[test]
    fn test_status_and_config_sexp() {
        let d = P300Detector::new();
        let status = d.status_sexp();
        assert!(status.contains(":enabled t"));
        assert!(status.contains(":active nil"));
        assert!(status.contains(":targets 0"));
        assert!(status.contains(":last-result nil"));

        let config = d.config_sexp();
        assert!(config.contains(":repetitions 5"));
        assert!(config.contains(":soa-ms 200"));
        assert!(config.contains(":detection-window (250 500)"));
    }
}
