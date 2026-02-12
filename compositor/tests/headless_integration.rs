//! Headless integration tests for the EWWM compositor.
//!
//! These tests validate IPC protocol compliance, handler dispatch correctness,
//! and module integration without requiring a running Wayland display server.
//! They can run in CI, on macOS (dev), or inside containers.
//!
//! NOTE: Full end-to-end tests that boot the compositor require Linux with
//! wayland-server support. These tests focus on the logic layer.

use ewwm_compositor::clock::{Clock, TestClock};
use ewwm_compositor::ipc::recorder::{
    IpcRecorder, IpcReplayer, MessageDirection, RecordedMessage,
};
use ewwm_compositor::input_source::{
    InputEvent, InputProvider, ScriptedInputProvider,
};

use std::sync::Arc;
use std::time::Duration;

// ── Clock integration tests ─────────────────────────────────

#[test]
fn test_clock_advance_deterministic() {
    let clock = TestClock::new();
    let t0 = clock.now();
    let ms0 = clock.unix_millis();

    clock.advance(Duration::from_secs(10));

    let t1 = clock.now();
    let ms1 = clock.unix_millis();

    assert_eq!(t1 - t0, Duration::from_secs(10));
    assert_eq!(ms1 - ms0, 10_000);
}

#[test]
fn test_clock_trait_object_arc() {
    let clock: Arc<dyn Clock> = Arc::new(TestClock::new());
    let _now = clock.now();
    let ms = clock.unix_millis();
    assert!(ms > 0);
}

// ── IPC recorder integration tests ──────────────────────────

#[test]
fn test_recorder_full_session() {
    let mut rec = IpcRecorder::new();

    // Start recording
    rec.start(Some("integration-test".to_string()));
    assert!(rec.active);

    // Simulate a hello handshake
    rec.record(
        1,
        MessageDirection::Request,
        "(:type :hello :id 1 :version 1 :client \"test\")",
    );
    rec.record(
        1,
        MessageDirection::Response,
        "(:type :hello :id 1 :version 1 :server \"ewwm-compositor\" \
         :features (:xwayland t :vr nil))",
    );

    // Simulate a ping
    rec.record(
        1,
        MessageDirection::Request,
        "(:type :ping :id 2 :timestamp 1000)",
    );
    rec.record(
        1,
        MessageDirection::Response,
        "(:type :response :id 2 :status :ok \
         :client-timestamp 1000 :server-timestamp 1001)",
    );

    // Simulate a surface-list
    rec.record(
        1,
        MessageDirection::Request,
        "(:type :surface-list :id 3)",
    );
    rec.record(
        1,
        MessageDirection::Response,
        "(:type :response :id 3 :status :ok :surfaces ())",
    );

    rec.stop();
    assert!(!rec.active);
    assert_eq!(rec.messages.len(), 6);

    // Verify s-expression serialization
    let sexp = rec.to_sexp();
    assert!(sexp.contains(":session-name \"integration-test\""));
    assert!(sexp.contains(":message-count 6"));
}

#[test]
fn test_replayer_extracts_requests() {
    let messages = vec![
        RecordedMessage {
            timestamp_ms: 100,
            client_id: 1,
            direction: MessageDirection::Request,
            payload: "(:type :hello :id 1 :version 1)".to_string(),
        },
        RecordedMessage {
            timestamp_ms: 101,
            client_id: 1,
            direction: MessageDirection::Response,
            payload: "(:type :hello :id 1 :status :ok)".to_string(),
        },
        RecordedMessage {
            timestamp_ms: 200,
            client_id: 1,
            direction: MessageDirection::Request,
            payload: "(:type :ping :id 2)".to_string(),
        },
        RecordedMessage {
            timestamp_ms: 201,
            client_id: 1,
            direction: MessageDirection::Response,
            payload: "(:type :response :id 2 :status :ok)".to_string(),
        },
        RecordedMessage {
            timestamp_ms: 250,
            client_id: 0,
            direction: MessageDirection::Event,
            payload: "(:type :event :event :surface-added)".to_string(),
        },
    ];

    let mut replayer = IpcReplayer::new(messages);
    assert_eq!(replayer.total, 5);
    assert_eq!(replayer.remaining(), 5);

    // First request
    let req1 = replayer.next_request().unwrap();
    assert!(req1.payload.contains(":hello"));

    // Second request (skips response)
    let req2 = replayer.next_request().unwrap();
    assert!(req2.payload.contains(":ping"));

    // No more requests (remaining are response + event)
    assert!(replayer.next_request().is_none());
    assert_eq!(replayer.consumed, 5);
}

// ── Input source integration tests ──────────────────────────

#[test]
fn test_scripted_input_multi_modal_sequence() {
    let events = vec![
        // Simulate looking at center
        InputEvent::Gaze {
            yaw: 0.0,
            pitch: 0.0,
            confidence: 1.0,
        },
        // Wait for dwell
        InputEvent::Wait {
            duration: Duration::from_millis(500),
        },
        // Gaze still at center (dwell should trigger)
        InputEvent::Gaze {
            yaw: 0.01,
            pitch: -0.01,
            confidence: 0.95,
        },
        // Pinch to confirm
        InputEvent::Gesture {
            name: "pinch".to_string(),
            hand: "right".to_string(),
            confidence: 0.9,
        },
        // BCI attention high
        InputEvent::BciSample {
            channel: 0,
            value: 0.8,
        },
        // Send IPC command
        InputEvent::IpcMessage {
            payload: "(:type :surface-focus :id 1 :surface-id 42)"
                .to_string(),
        },
    ];

    let mut provider = ScriptedInputProvider::new(events);
    assert_eq!(provider.remaining(), 6);

    let mut consumed = 0;
    while provider.has_events() {
        let event = provider.next_event().unwrap();
        consumed += 1;
        match consumed {
            1 => assert!(matches!(event, InputEvent::Gaze { .. })),
            2 => assert!(matches!(event, InputEvent::Wait { .. })),
            3 => assert!(matches!(event, InputEvent::Gaze { .. })),
            4 => assert!(matches!(event, InputEvent::Gesture { .. })),
            5 => assert!(matches!(event, InputEvent::BciSample { .. })),
            6 => assert!(matches!(event, InputEvent::IpcMessage { .. })),
            _ => panic!("unexpected event count"),
        }
    }
    assert_eq!(consumed, 6);
}

// ── Module compilation tests ─────────────────────────────────

#[test]
fn test_secure_input_state_with_clock() {
    let clock = TestClock::new();
    let mut state =
        ewwm_compositor::secure_input::SecureInputState::new();

    let now = clock.now();
    state.enter("test-password", 42, now);
    assert!(state.active);
    assert_eq!(state.reason.as_deref(), Some("test-password"));

    // Not expired yet (0 seconds elapsed)
    assert!(!state.is_expired(now));

    // Advance past timeout (default 30s)
    clock.advance(Duration::from_secs(31));
    let later = clock.now();
    assert!(state.is_expired(later));

    // Tick should auto-exit
    let event = state.tick(later);
    assert!(event.is_some());
    assert!(!state.active);
}

#[test]
fn test_secure_input_status_sexp_with_clock() {
    let clock = TestClock::new();
    let mut state =
        ewwm_compositor::secure_input::SecureInputState::new();

    let now = clock.now();
    state.enter("sudo", 7, now);

    // Advance 5 seconds
    clock.advance(Duration::from_secs(5));
    let later = clock.now();

    let sexp = state.status_sexp(later);
    assert!(sexp.contains(":active t"));
    assert!(sexp.contains(":elapsed-s 5"));
    assert!(sexp.contains(":timeout-s 30"));
}

#[test]
fn test_ipc_recorder_status() {
    let rec = IpcRecorder::new();
    let status = rec.status_sexp();
    assert!(status.contains(":active nil"));
    assert!(status.contains(":message-count 0"));
}

// ── VR module compilation tests (no openxrs needed) ──────────

#[test]
fn test_drm_lease_connector_types() {
    use ewwm_compositor::vr::drm_lease::{ConnectorType, DisplayMode};

    assert_eq!(ConnectorType::DisplayPort.as_str(), "DP");
    assert_eq!(ConnectorType::Hdmi.as_str(), "HDMI");
    assert_eq!(ConnectorType::UsbC.as_str(), "USB-C");

    let mode = DisplayMode {
        width: 2560,
        height: 1440,
        refresh_hz: 90,
        preferred: true,
    };
    assert_eq!(mode.resolution_str(), "2560x1440@90Hz");
}

#[test]
fn test_scene_graph_types() {
    use ewwm_compositor::vr::scene::{Quat, Vec3};

    let v = Vec3::new(1.0, 2.0, 3.0);
    assert!((v.length() - 3.741_657_5).abs() < 0.001);

    let q = Quat::IDENTITY;
    assert!((q.w - 1.0).abs() < f32::EPSILON);
}

#[test]
fn test_autotype_manager() {
    let mgr = ewwm_compositor::autotype::AutoTypeManager::new();
    let sexp = mgr.status_sexp();
    assert!(sexp.contains(":phase idle"));
}
