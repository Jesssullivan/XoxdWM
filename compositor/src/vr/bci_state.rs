//! Central BCI state aggregator — OpenBCI Cyton/Daisy EEG integration.
//!
//! Manages the BCI pipeline including board connection, signal quality
//! monitoring, and coordination of all BCI subsystems (attention, SSVEP,
//! P300, motor imagery, EEG fatigue).
//! No openxrs dependency; compiles unconditionally.

use super::attention::AttentionMonitor;
use super::fatigue_eeg::EegFatigueMonitor;
use super::motor_imagery::MiClassifier;
use super::p300::P300Detector;
use super::ssvep::SsvepClassifier;

// ── Board types ─────────────────────────────────────────

/// Supported BCI board hardware.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BciBoard {
    /// OpenBCI Cyton (8 channels).
    Cyton8,
    /// OpenBCI Cyton + Daisy (16 channels).
    CytonDaisy16,
    /// Synthetic data for testing.
    Synthetic,
}

impl BciBoard {
    /// String representation for IPC.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Cyton8 => "cyton",
            Self::CytonDaisy16 => "daisy",
            Self::Synthetic => "synthetic",
        }
    }

    /// Parse from string.
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "cyton" => Some(Self::Cyton8),
            "daisy" => Some(Self::CytonDaisy16),
            "synthetic" => Some(Self::Synthetic),
            _ => None,
        }
    }

    /// Board ID for brainflow-compatible numbering.
    pub fn board_id(&self) -> i32 {
        match self {
            Self::Cyton8 => 0,
            Self::CytonDaisy16 => 2,
            Self::Synthetic => -1,
        }
    }

    /// Number of EEG channels.
    pub fn channel_count(&self) -> usize {
        match self {
            Self::Cyton8 => 8,
            Self::CytonDaisy16 => 16,
            Self::Synthetic => 8,
        }
    }
}

// ── Connection state ────────────────────────────────────

/// BCI connection state machine.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BciConnectionState {
    /// No board connected.
    Disconnected,
    /// Attempting to connect.
    Connecting,
    /// Connected and ready for streaming.
    Connected,
    /// Connection error.
    Error(String),
}

impl BciConnectionState {
    /// String representation for IPC.
    pub fn as_str(&self) -> &str {
        match self {
            Self::Disconnected => "disconnected",
            Self::Connecting => "connecting",
            Self::Connected => "connected",
            Self::Error(_) => "error",
        }
    }
}

// ── BciConfig ───────────────────────────────────────────

/// Configuration for the BCI subsystem.
#[derive(Debug, Clone)]
pub struct BciConfig {
    /// Board ID (0=Cyton, 2=Daisy, -1=Synthetic).
    pub board_id: i32,
    /// Serial port path.
    pub serial_port: String,
    /// Sample rate in Hz (250 for Cyton, 125 for Daisy).
    pub sample_rate: u32,
    /// Notch filter frequency (50.0 or 60.0 Hz).
    pub notch_frequency: f64,
    /// Whether artifact rejection is enabled.
    pub artifact_rejection: bool,
    /// Data retention period in days.
    pub data_retention_days: u32,
}

impl Default for BciConfig {
    fn default() -> Self {
        Self {
            board_id: -1, // synthetic by default
            serial_port: "/dev/ttyUSB0".to_string(),
            sample_rate: 250,
            notch_frequency: 60.0,
            artifact_rejection: true,
            data_retention_days: 30,
        }
    }
}

// ── Channel quality ─────────────────────────────────────

/// Signal quality metrics for a single EEG channel.
#[derive(Debug, Clone)]
pub struct ChannelQuality {
    /// Channel index (0-based).
    pub channel: usize,
    /// 10-20 system label (e.g., "Fp1", "C3").
    pub label: String,
    /// Impedance in kilohms.
    pub impedance_kohm: f64,
    /// Signal quality (0.0-1.0).
    pub signal_quality: f64,
}

/// Standard 10-20 labels for 8-channel Cyton placement.
const CYTON_8_LABELS: [&str; 8] =
    ["Fp1", "Fp2", "C3", "C4", "T5", "T6", "O1", "O2"];

/// Standard 10-20 labels for 16-channel Daisy placement.
const DAISY_16_LABELS: [&str; 16] = [
    "Fp1", "Fp2", "F3", "F4", "C3", "C4", "P3", "P4", "O1", "O2", "F7",
    "F8", "T3", "T4", "T5", "T6",
];

// ── Session data ────────────────────────────────────────

/// Metadata for a BCI recording session.
#[derive(Debug, Clone)]
pub struct BciSession {
    /// Unique session identifier.
    pub session_id: String,
    /// Session start timestamp (seconds since epoch).
    pub start_time: f64,
    /// Duration in seconds.
    pub duration_s: f64,
    /// Number of frames recorded.
    pub frame_count: u64,
}

// ── BciState ────────────────────────────────────────────

/// Central BCI state aggregating all subsystems.
pub struct BciState {
    /// Configuration.
    pub config: BciConfig,
    /// Connection state.
    pub connection: BciConnectionState,
    /// Per-channel signal quality.
    pub channels: Vec<ChannelQuality>,
    /// Attention/focus monitor.
    pub attention: AttentionMonitor,
    /// SSVEP frequency classifier.
    pub ssvep: SsvepClassifier,
    /// P300 event-related potential detector.
    pub p300: P300Detector,
    /// Motor imagery classifier.
    pub motor_imagery: MiClassifier,
    /// EEG-based fatigue monitor.
    pub fatigue_eeg: EegFatigueMonitor,
    /// Whether data streaming is active.
    pub streaming: bool,
    /// Total frames received since last start.
    pub frames_received: u64,
    /// Timestamp of last frame (seconds).
    pub last_frame_time: f64,
    /// Whether running in synthetic data mode.
    pub synthetic_mode: bool,
    /// Board hardware type.
    board: BciBoard,
    /// Recorded session history.
    sessions: Vec<BciSession>,
}

impl BciState {
    /// Create a new BCI state with defaults (synthetic board).
    pub fn new() -> Self {
        let board = BciBoard::Synthetic;
        let channels = Self::init_channels(board);
        Self {
            config: BciConfig::default(),
            connection: BciConnectionState::Disconnected,
            channels,
            attention: AttentionMonitor::new(),
            ssvep: SsvepClassifier::new(),
            p300: P300Detector::new(),
            motor_imagery: MiClassifier::new(),
            fatigue_eeg: EegFatigueMonitor::new(),
            streaming: false,
            frames_received: 0,
            last_frame_time: 0.0,
            synthetic_mode: true,
            board,
            sessions: Vec::new(),
        }
    }

    /// Initialize channel quality entries for a board type.
    fn init_channels(board: BciBoard) -> Vec<ChannelQuality> {
        let count = board.channel_count();
        let labels = match board {
            BciBoard::CytonDaisy16 => &DAISY_16_LABELS[..count],
            _ => &CYTON_8_LABELS[..count.min(8)],
        };
        labels
            .iter()
            .enumerate()
            .map(|(i, &label)| ChannelQuality {
                channel: i,
                label: label.to_string(),
                impedance_kohm: 0.0,
                signal_quality: 0.0,
            })
            .collect()
    }

    // ── IPC adapter methods ─────────────────────────────

    /// Generate full status as s-expression.
    pub fn status_sexp(&self) -> String {
        let conn_str = match &self.connection {
            BciConnectionState::Error(msg) => {
                format!("(:state :error :message \"{}\")", msg)
            }
            other => format!("(:state :{})", other.as_str()),
        };
        format!(
            "(:board :{} :connection {} :streaming {} :frames {} :last-frame-time {:.3} :synthetic {} :sample-rate {} :channels {})",
            self.board.as_str(),
            conn_str,
            if self.streaming { "t" } else { "nil" },
            self.frames_received,
            self.last_frame_time,
            if self.synthetic_mode { "t" } else { "nil" },
            self.config.sample_rate,
            self.channels.len(),
        )
    }

    /// Start the BCI session (connect and begin streaming).
    pub fn start(&mut self) -> Result<(), String> {
        if self.streaming {
            return Err("Already streaming".to_string());
        }
        self.connection = BciConnectionState::Connecting;
        // In production this would init the board via serial/USB.
        // For now, transition directly to Connected.
        self.connection = BciConnectionState::Connected;
        self.streaming = true;
        self.frames_received = 0;
        self.last_frame_time = 0.0;

        let session = BciSession {
            session_id: format!("session-{}", self.sessions.len() + 1),
            start_time: 0.0,
            duration_s: 0.0,
            frame_count: 0,
        };
        self.sessions.push(session);
        Ok(())
    }

    /// Stop the BCI session.
    pub fn stop(&mut self) -> Result<(), String> {
        if !self.streaming {
            return Err("Not streaming".to_string());
        }
        self.streaming = false;
        self.connection = BciConnectionState::Disconnected;

        // Update last session with final frame count
        if let Some(session) = self.sessions.last_mut() {
            session.frame_count = self.frames_received;
        }
        Ok(())
    }

    /// Restart the BCI session (stop then start).
    pub fn restart(&mut self) -> Result<(), String> {
        if self.streaming {
            self.stop()?;
        }
        self.start()
    }

    /// Generate signal quality as s-expression.
    pub fn signal_quality_sexp(&self) -> String {
        let channels_str: Vec<String> = self
            .channels
            .iter()
            .map(|ch| {
                format!(
                    "(:channel {} :label \"{}\" :impedance {:.1} :quality {:.2})",
                    ch.channel, ch.label, ch.impedance_kohm, ch.signal_quality,
                )
            })
            .collect();
        format!("({})", channels_str.join(" "))
    }

    /// Set the board type by name.
    pub fn set_board_by_name(
        &mut self,
        name: &str,
    ) -> Result<(), String> {
        if self.streaming {
            return Err("Cannot change board while streaming".to_string());
        }
        let board = BciBoard::from_str(name)
            .ok_or_else(|| format!("Unknown board: {}", name))?;
        self.board = board;
        self.config.board_id = board.board_id();
        self.synthetic_mode = board == BciBoard::Synthetic;
        self.config.sample_rate = match board {
            BciBoard::CytonDaisy16 => 125,
            _ => 250,
        };
        self.channels = Self::init_channels(board);
        Ok(())
    }

    /// Set the serial port path.
    pub fn set_serial_port(&mut self, port: &str) {
        self.config.serial_port = port.to_string();
    }

    /// Set the notch filter frequency.
    pub fn set_notch(&mut self, freq: f64) -> Result<(), String> {
        if freq != 50.0 && freq != 60.0 {
            return Err(format!(
                "Notch frequency must be 50.0 or 60.0, got {}",
                freq,
            ));
        }
        self.config.notch_frequency = freq;
        Ok(())
    }

    /// Inject a synthetic event for testing.
    pub fn inject_synthetic_event(
        &mut self,
        event_type: &str,
        params: &[(String, String)],
    ) -> Result<(), String> {
        if !self.synthetic_mode {
            return Err(
                "Synthetic events only available in synthetic mode"
                    .to_string(),
            );
        }
        match event_type {
            "attention" => {
                let alpha = Self::param_f64(params, "alpha").unwrap_or(1.0);
                let beta = Self::param_f64(params, "beta").unwrap_or(1.0);
                let theta = Self::param_f64(params, "theta").unwrap_or(1.0);
                let gamma = Self::param_f64(params, "gamma").unwrap_or(0.0);
                self.attention.update_bands(alpha, beta, theta, gamma);
                Ok(())
            }
            "fatigue" => {
                let theta = Self::param_f64(params, "theta").unwrap_or(1.0);
                let alpha = Self::param_f64(params, "alpha").unwrap_or(1.0);
                let beta = Self::param_f64(params, "beta").unwrap_or(1.0);
                let blink_rate =
                    Self::param_f64(params, "blink-rate").unwrap_or(15.0);
                self.fatigue_eeg.update(theta, alpha, beta, blink_rate);
                Ok(())
            }
            _ => Err(format!("Unknown synthetic event type: {}", event_type)),
        }
    }

    /// List recorded sessions as s-expression.
    pub fn data_list(&self) -> String {
        let sessions_str: Vec<String> = self
            .sessions
            .iter()
            .map(|s| {
                format!(
                    "(:id \"{}\" :start {:.0} :duration {:.1} :frames {})",
                    s.session_id, s.start_time, s.duration_s, s.frame_count,
                )
            })
            .collect();
        format!("({})", sessions_str.join(" "))
    }

    /// Delete a recorded session by ID.
    pub fn data_delete(
        &mut self,
        session_id: &str,
    ) -> Result<(), String> {
        let initial_len = self.sessions.len();
        self.sessions.retain(|s| s.session_id != session_id);
        if self.sessions.len() == initial_len {
            Err(format!("Session not found: {}", session_id))
        } else {
            Ok(())
        }
    }

    /// Extract a named f64 parameter from key-value pairs.
    fn param_f64(params: &[(String, String)], key: &str) -> Option<f64> {
        params
            .iter()
            .find(|(k, _)| k == key)
            .and_then(|(_, v)| v.parse::<f64>().ok())
    }
}

// ── Tests ───────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_state() {
        let state = BciState::new();
        assert_eq!(state.board, BciBoard::Synthetic);
        assert!(state.synthetic_mode);
        assert!(!state.streaming);
        assert_eq!(state.frames_received, 0);
        assert_eq!(
            state.connection,
            BciConnectionState::Disconnected,
        );
        assert_eq!(state.channels.len(), 8);
    }

    #[test]
    fn test_board_roundtrip() {
        let boards = vec![
            ("cyton", BciBoard::Cyton8),
            ("daisy", BciBoard::CytonDaisy16),
            ("synthetic", BciBoard::Synthetic),
        ];
        for (s, b) in &boards {
            assert_eq!(BciBoard::from_str(s), Some(*b));
            assert_eq!(b.as_str(), *s);
        }
        assert_eq!(BciBoard::from_str("unknown"), None);
    }

    #[test]
    fn test_board_ids() {
        assert_eq!(BciBoard::Cyton8.board_id(), 0);
        assert_eq!(BciBoard::CytonDaisy16.board_id(), 2);
        assert_eq!(BciBoard::Synthetic.board_id(), -1);
    }

    #[test]
    fn test_start_stop() {
        let mut state = BciState::new();
        assert!(state.start().is_ok());
        assert!(state.streaming);
        assert_eq!(state.connection, BciConnectionState::Connected);
        assert_eq!(state.sessions.len(), 1);

        assert!(state.stop().is_ok());
        assert!(!state.streaming);
        assert_eq!(
            state.connection,
            BciConnectionState::Disconnected,
        );

        // Double stop should error
        assert!(state.stop().is_err());
    }

    #[test]
    fn test_restart() {
        let mut state = BciState::new();
        state.start().unwrap();
        assert!(state.restart().is_ok());
        assert!(state.streaming);
        assert_eq!(state.sessions.len(), 2);
    }

    #[test]
    fn test_set_board() {
        let mut state = BciState::new();
        assert!(state.set_board_by_name("daisy").is_ok());
        assert_eq!(state.board, BciBoard::CytonDaisy16);
        assert_eq!(state.channels.len(), 16);
        assert_eq!(state.config.sample_rate, 125);
        assert!(!state.synthetic_mode);

        assert!(state.set_board_by_name("cyton").is_ok());
        assert_eq!(state.channels.len(), 8);
        assert_eq!(state.config.sample_rate, 250);

        assert!(state.set_board_by_name("invalid").is_err());
    }

    #[test]
    fn test_set_notch() {
        let mut state = BciState::new();
        assert!(state.set_notch(50.0).is_ok());
        assert!((state.config.notch_frequency - 50.0).abs() < f64::EPSILON);
        assert!(state.set_notch(60.0).is_ok());
        assert!(state.set_notch(45.0).is_err());
    }

    #[test]
    fn test_data_list_and_delete() {
        let mut state = BciState::new();
        state.start().unwrap();
        state.stop().unwrap();
        state.start().unwrap();
        state.stop().unwrap();
        assert_eq!(state.sessions.len(), 2);

        let list = state.data_list();
        assert!(list.contains("session-1"));
        assert!(list.contains("session-2"));

        assert!(state.data_delete("session-1").is_ok());
        assert_eq!(state.sessions.len(), 1);
        assert!(state.data_delete("nonexistent").is_err());
    }

    #[test]
    fn test_status_sexp() {
        let state = BciState::new();
        let sexp = state.status_sexp();
        assert!(sexp.contains(":board :synthetic"));
        assert!(sexp.contains(":streaming nil"));
        assert!(sexp.contains(":synthetic t"));
        assert!(sexp.contains(":sample-rate 250"));
    }
}
