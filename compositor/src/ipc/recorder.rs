//! IPC session recording and replay for regression testing.
//!
//! Records timestamped IPC message exchanges (request -> response pairs)
//! and can replay them for validation.

use std::collections::VecDeque;
use std::time::{SystemTime, UNIX_EPOCH};

/// Direction of an IPC message.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MessageDirection {
    /// Client -> compositor request.
    Request,
    /// Compositor -> client response.
    Response,
    /// Compositor -> clients broadcast event.
    Event,
}

impl MessageDirection {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Request => "request",
            Self::Response => "response",
            Self::Event => "event",
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "request" => Some(Self::Request),
            "response" => Some(Self::Response),
            "event" => Some(Self::Event),
            _ => None,
        }
    }
}

/// A single recorded IPC message.
#[derive(Debug, Clone)]
pub struct RecordedMessage {
    /// Timestamp in milliseconds since UNIX epoch.
    pub timestamp_ms: i64,
    /// Client ID that sent/received the message.
    pub client_id: u64,
    /// Direction of the message.
    pub direction: MessageDirection,
    /// Raw s-expression payload.
    pub payload: String,
}

impl RecordedMessage {
    /// Serialize to a simple s-expression format.
    pub fn to_sexp(&self) -> String {
        let escaped = self
            .payload
            .replace('\\', "\\\\")
            .replace('"', "\\\"");
        format!(
            "(:timestamp {} :client {} :direction :{} :payload \"{}\")",
            self.timestamp_ms,
            self.client_id,
            self.direction.as_str(),
            escaped,
        )
    }
}

/// IPC session recorder.
pub struct IpcRecorder {
    /// Whether recording is active.
    pub active: bool,
    /// Recorded messages in order.
    pub messages: Vec<RecordedMessage>,
    /// Session name/label.
    pub session_name: Option<String>,
    /// Recording start timestamp.
    pub started_at: Option<i64>,
}

impl IpcRecorder {
    /// Create a new inactive recorder.
    pub fn new() -> Self {
        Self {
            active: false,
            messages: Vec::new(),
            session_name: None,
            started_at: None,
        }
    }

    /// Start recording.
    pub fn start(&mut self, session_name: Option<String>) {
        self.active = true;
        self.messages.clear();
        self.session_name = session_name;
        self.started_at = Some(unix_millis_now());
    }

    /// Stop recording.
    pub fn stop(&mut self) {
        self.active = false;
    }

    /// Record a message if recording is active.
    pub fn record(
        &mut self,
        client_id: u64,
        direction: MessageDirection,
        payload: &str,
    ) {
        if !self.active {
            return;
        }
        self.messages.push(RecordedMessage {
            timestamp_ms: unix_millis_now(),
            client_id,
            direction,
            payload: payload.to_string(),
        });
    }

    /// Serialize the entire recording to an s-expression list.
    pub fn to_sexp(&self) -> String {
        let name = match &self.session_name {
            Some(n) => format!("\"{}\"", n),
            None => "nil".to_string(),
        };
        let started = self.started_at.unwrap_or(0);
        let msg_count = self.messages.len();

        let mut out = format!(
            "(:session-name {} :started-at {} :message-count {} :messages (",
            name, started, msg_count,
        );

        for msg in &self.messages {
            out.push_str(&msg.to_sexp());
            out.push(' ');
        }
        out.push_str("))");
        out
    }

    /// Get recording status as s-expression.
    pub fn status_sexp(&self) -> String {
        format!(
            "(:active {} :session-name {} :message-count {} :started-at {})",
            if self.active { "t" } else { "nil" },
            match &self.session_name {
                Some(n) => format!("\"{}\"", n),
                None => "nil".to_string(),
            },
            self.messages.len(),
            self.started_at.unwrap_or(0),
        )
    }
}

/// IPC session replayer.
pub struct IpcReplayer {
    /// Messages to replay.
    messages: VecDeque<RecordedMessage>,
    /// Total messages in the recording.
    pub total: usize,
    /// Messages consumed so far.
    pub consumed: usize,
}

impl IpcReplayer {
    /// Create from recorded messages.
    pub fn new(messages: Vec<RecordedMessage>) -> Self {
        let total = messages.len();
        Self {
            messages: VecDeque::from(messages),
            total,
            consumed: 0,
        }
    }

    /// Get the next request message (skipping responses/events).
    pub fn next_request(&mut self) -> Option<RecordedMessage> {
        while let Some(msg) = self.messages.pop_front() {
            self.consumed += 1;
            if msg.direction == MessageDirection::Request {
                return Some(msg);
            }
        }
        None
    }

    /// Get all remaining messages.
    pub fn remaining(&self) -> usize {
        self.messages.len()
    }
}

/// Get current UNIX time in milliseconds.
fn unix_millis_now() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as i64)
        .unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_recorder_lifecycle() {
        let mut rec = IpcRecorder::new();
        assert!(!rec.active);

        rec.start(Some("test-session".to_string()));
        assert!(rec.active);
        assert!(rec.started_at.is_some());

        rec.record(
            1,
            MessageDirection::Request,
            "(:type :hello :id 1 :version 1)",
        );
        rec.record(
            1,
            MessageDirection::Response,
            "(:type :hello :id 1 :version 1)",
        );
        assert_eq!(rec.messages.len(), 2);

        rec.stop();
        assert!(!rec.active);
        assert_eq!(rec.messages.len(), 2);
    }

    #[test]
    fn test_recorder_inactive_no_record() {
        let mut rec = IpcRecorder::new();
        rec.record(1, MessageDirection::Request, "test");
        assert_eq!(rec.messages.len(), 0);
    }

    #[test]
    fn test_recorder_to_sexp() {
        let mut rec = IpcRecorder::new();
        rec.start(Some("demo".to_string()));
        rec.record(
            1,
            MessageDirection::Request,
            "(:type :ping :id 1)",
        );
        let sexp = rec.to_sexp();
        assert!(sexp.contains(":session-name \"demo\""));
        assert!(sexp.contains(":message-count 1"));
        assert!(sexp.contains(":direction :request"));
    }

    #[test]
    fn test_recorder_status_sexp() {
        let mut rec = IpcRecorder::new();
        let sexp = rec.status_sexp();
        assert!(sexp.contains(":active nil"));
        assert!(sexp.contains(":message-count 0"));

        rec.start(None);
        rec.record(1, MessageDirection::Request, "test");
        let sexp = rec.status_sexp();
        assert!(sexp.contains(":active t"));
        assert!(sexp.contains(":message-count 1"));
    }

    #[test]
    fn test_replayer() {
        let messages = vec![
            RecordedMessage {
                timestamp_ms: 1000,
                client_id: 1,
                direction: MessageDirection::Request,
                payload: "(:type :hello)".to_string(),
            },
            RecordedMessage {
                timestamp_ms: 1001,
                client_id: 1,
                direction: MessageDirection::Response,
                payload: "(:type :hello :status :ok)".to_string(),
            },
            RecordedMessage {
                timestamp_ms: 1002,
                client_id: 1,
                direction: MessageDirection::Request,
                payload: "(:type :ping)".to_string(),
            },
        ];

        let mut replayer = IpcReplayer::new(messages);
        assert_eq!(replayer.total, 3);
        assert_eq!(replayer.remaining(), 3);

        let req1 = replayer.next_request().unwrap();
        assert_eq!(req1.payload, "(:type :hello)");

        // next_request skips the response
        let req2 = replayer.next_request().unwrap();
        assert_eq!(req2.payload, "(:type :ping)");

        assert!(replayer.next_request().is_none());
        assert_eq!(replayer.consumed, 3);
    }

    #[test]
    fn test_message_direction_roundtrip() {
        for dir in [
            MessageDirection::Request,
            MessageDirection::Response,
            MessageDirection::Event,
        ] {
            let s = dir.as_str();
            assert_eq!(MessageDirection::from_str(s), Some(dir));
        }
        assert_eq!(MessageDirection::from_str("invalid"), None);
    }

    #[test]
    fn test_recorded_message_sexp_escaping() {
        let msg = RecordedMessage {
            timestamp_ms: 0,
            client_id: 1,
            direction: MessageDirection::Request,
            payload: r#"(:type :hello :client "test\"app")"#.to_string(),
        };
        let sexp = msg.to_sexp();
        // Should have escaped the inner quotes
        assert!(sexp.contains(r#"\\\"app"#));
    }
}
