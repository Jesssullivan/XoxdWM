//! Unified input source abstraction for integration testing.
//!
//! Provides `InputEvent` enum and `InputProvider` trait so integration tests
//! can drive multiple subsystems (gaze, hand, BCI, IPC) through a single
//! scripted sequence.  Production code does NOT use this — each subsystem
//! has its own well-tested state machines.

use std::collections::VecDeque;
use std::time::Duration;

/// A unified input event that can be injected into the compositor.
#[derive(Debug, Clone)]
pub enum InputEvent {
    /// Gaze direction (yaw, pitch in radians).
    Gaze { yaw: f64, pitch: f64, confidence: f64 },
    /// Hand joint update (left=true/false, joint index 0-25, x/y/z).
    Hand { left: bool, joint: u8, x: f64, y: f64, z: f64 },
    /// Blink event (left_closed, right_closed, duration_ms).
    Blink { left_closed: bool, right_closed: bool, duration_ms: u32 },
    /// Gesture detected.
    Gesture { name: String, hand: String, confidence: f64 },
    /// BCI sample (channel, value).
    BciSample { channel: u8, value: f64 },
    /// Raw IPC message string.
    IpcMessage { payload: String },
    /// Wait for a duration (used in scripted sequences).
    Wait { duration: Duration },
}

/// Trait for providing input events to the compositor.
pub trait InputProvider: Send {
    /// Get the next input event, if any.
    fn next_event(&mut self) -> Option<InputEvent>;
    /// Whether there are more events to deliver.
    fn has_events(&self) -> bool;
}

/// A scripted input provider that delivers events from a pre-defined queue.
pub struct ScriptedInputProvider {
    events: VecDeque<InputEvent>,
}

impl ScriptedInputProvider {
    /// Create from a vector of events.
    pub fn new(events: Vec<InputEvent>) -> Self {
        Self {
            events: VecDeque::from(events),
        }
    }

    /// Number of remaining events.
    pub fn remaining(&self) -> usize {
        self.events.len()
    }
}

impl InputProvider for ScriptedInputProvider {
    fn next_event(&mut self) -> Option<InputEvent> {
        self.events.pop_front()
    }

    fn has_events(&self) -> bool {
        !self.events.is_empty()
    }
}

/// A recording wrapper that logs all events from an inner provider.
pub struct RecordingProvider<P: InputProvider> {
    inner: P,
    recorded: Vec<InputEvent>,
}

impl<P: InputProvider> RecordingProvider<P> {
    /// Wrap an existing provider with recording.
    pub fn new(inner: P) -> Self {
        Self {
            inner,
            recorded: Vec::new(),
        }
    }

    /// Get all recorded events.
    pub fn recorded(&self) -> &[InputEvent] {
        &self.recorded
    }

    /// Consume and return recorded events.
    pub fn into_recorded(self) -> Vec<InputEvent> {
        self.recorded
    }
}

impl<P: InputProvider> InputProvider for RecordingProvider<P> {
    fn next_event(&mut self) -> Option<InputEvent> {
        let event = self.inner.next_event()?;
        self.recorded.push(event.clone());
        Some(event)
    }

    fn has_events(&self) -> bool {
        self.inner.has_events()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scripted_provider() {
        let events = vec![
            InputEvent::Gaze { yaw: 0.0, pitch: 0.0, confidence: 1.0 },
            InputEvent::Wait { duration: Duration::from_millis(100) },
            InputEvent::IpcMessage { payload: "(:type :ping :id 1)".to_string() },
        ];

        let mut provider = ScriptedInputProvider::new(events);
        assert!(provider.has_events());
        assert_eq!(provider.remaining(), 3);

        let e1 = provider.next_event().unwrap();
        assert!(matches!(e1, InputEvent::Gaze { .. }));

        let e2 = provider.next_event().unwrap();
        assert!(matches!(e2, InputEvent::Wait { .. }));

        let e3 = provider.next_event().unwrap();
        assert!(matches!(e3, InputEvent::IpcMessage { .. }));

        assert!(!provider.has_events());
        assert!(provider.next_event().is_none());
    }

    #[test]
    fn test_recording_provider() {
        let events = vec![
            InputEvent::BciSample { channel: 0, value: 0.5 },
            InputEvent::Gesture { name: "pinch".to_string(), hand: "left".to_string(), confidence: 0.9 },
        ];

        let scripted = ScriptedInputProvider::new(events);
        let mut recording = RecordingProvider::new(scripted);

        assert!(recording.has_events());

        let _ = recording.next_event().unwrap();
        let _ = recording.next_event().unwrap();

        assert!(!recording.has_events());
        assert_eq!(recording.recorded().len(), 2);

        let recorded = recording.into_recorded();
        assert_eq!(recorded.len(), 2);
    }

    #[test]
    fn test_empty_provider() {
        let mut provider = ScriptedInputProvider::new(vec![]);
        assert!(!provider.has_events());
        assert_eq!(provider.remaining(), 0);
        assert!(provider.next_event().is_none());
    }

    #[test]
    fn test_hand_event() {
        let events = vec![
            InputEvent::Hand { left: true, joint: 0, x: 0.1, y: 0.2, z: 0.3 },
            InputEvent::Blink { left_closed: true, right_closed: false, duration_ms: 150 },
        ];
        let mut provider = ScriptedInputProvider::new(events);
        let e = provider.next_event().unwrap();
        assert!(matches!(e, InputEvent::Hand { left: true, joint: 0, .. }));
    }
}
