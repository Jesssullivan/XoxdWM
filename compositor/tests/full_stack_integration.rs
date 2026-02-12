//! Full-stack multi-modal integration tests.
//!
//! Uses `TestClock` and `ScriptedInputProvider` to drive deterministic
//! sequences through multiple subsystems: gaze focus, gesture recognition,
//! BCI attention, secure input, and IPC recording.
//!
//! These tests validate that subsystems compose correctly when driven
//! by the unified input abstraction.

use ewwm_compositor::clock::{Clock, TestClock};
use ewwm_compositor::input_source::{InputEvent, InputProvider, RecordingProvider, ScriptedInputProvider};
use ewwm_compositor::ipc::recorder::{IpcRecorder, MessageDirection};

use std::sync::Arc;
use std::time::Duration;

// ── Gaze → dwell → focus deterministic sequence ─────────────

#[test]
fn test_gaze_dwell_focus_with_clock() {
    use ewwm_compositor::vr::eye_tracking::{GazeData, GazeSource, GazeSurfaceHit};
    use ewwm_compositor::vr::gaze_focus::{GazeFocusEvent, GazeFocusManager};
    use ewwm_compositor::vr::scene::Vec3;
    use ewwm_compositor::vr::vr_interaction::Ray;

    let clock = TestClock::new();
    let mut gaze_focus = GazeFocusManager::new();

    let surface_id = 42u64;
    let hit = GazeSurfaceHit {
        surface_id,
        pixel_x: 500.0,
        pixel_y: 300.0,
        distance: 1.5,
        confidence: 0.95,
    };
    let gaze = GazeData {
        source: GazeSource::Simulated,
        ray: Ray::new(Vec3::new(0.0, 0.0, 0.0), Vec3::new(0.0, 0.0, -1.0)),
        confidence: 0.95,
        timestamp_s: 0.0,
        per_eye: None,
    };

    // Frame 1: dwell starts
    let dt_s = 0.016; // ~60fps
    let event = gaze_focus.update(Some(&hit), Some(&gaze), dt_s);
    assert!(
        matches!(event, Some(GazeFocusEvent::DwellStarted { .. })),
        "expected DwellStarted, got {:?}",
        event
    );

    // Advance clock and accumulate dwell (200ms default threshold)
    // Feed steady gaze for ~15 frames (240ms total at 16ms/frame)
    let mut focus_requested = false;
    for frame in 1..=15 {
        clock.advance(Duration::from_millis(16));
        let t = frame as f64 * 0.016;
        let gaze_t = GazeData {
            source: GazeSource::Simulated,
            ray: Ray::new(Vec3::new(0.0, 0.0, 0.0), Vec3::new(0.0, 0.0, -1.0)),
            confidence: 0.95,
            timestamp_s: t,
            per_eye: None,
        };

        let event = gaze_focus.update(Some(&hit), Some(&gaze_t), dt_s);
        if let Some(GazeFocusEvent::FocusRequested {
            surface_id: sid,
            dwell_ms,
            ..
        }) = &event
        {
            assert_eq!(*sid, surface_id);
            assert!(*dwell_ms >= 200.0, "dwell_ms should be >= 200, got {}", dwell_ms);
            focus_requested = true;
            break;
        }
    }

    assert!(focus_requested, "FocusRequested should fire after dwell threshold");
    assert_eq!(gaze_focus.focused_surface, Some(surface_id));
}

// ── Gaze → saccade suppresses dwell ────────────────────────

#[test]
fn test_saccade_suppresses_dwell() {
    use ewwm_compositor::vr::eye_tracking::{GazeData, GazeSource, GazeSurfaceHit};
    use ewwm_compositor::vr::gaze_focus::{GazeFocusEvent, GazeFocusManager};
    use ewwm_compositor::vr::scene::Vec3;
    use ewwm_compositor::vr::vr_interaction::Ray;

    let mut gaze_focus = GazeFocusManager::new();

    let hit = GazeSurfaceHit {
        surface_id: 10,
        pixel_x: 100.0,
        pixel_y: 100.0,
        distance: 1.0,
        confidence: 0.9,
    };

    // Start dwell
    let gaze = GazeData {
        source: GazeSource::Simulated,
        ray: Ray::new(Vec3::new(0.0, 0.0, 0.0), Vec3::new(0.0, 0.0, -1.0)),
        confidence: 0.9,
        timestamp_s: 0.0,
        per_eye: None,
    };
    let event = gaze_focus.update(Some(&hit), Some(&gaze), 0.016);
    assert!(matches!(event, Some(GazeFocusEvent::DwellStarted { .. })));

    // Rapid gaze jump (saccade) — direction changes dramatically
    let saccade_gaze = GazeData {
        source: GazeSource::Simulated,
        ray: Ray::new(Vec3::new(0.0, 0.0, 0.0), Vec3::new(1.0, 0.0, 0.0)),
        confidence: 0.9,
        timestamp_s: 0.016,
        per_eye: None,
    };
    let event = gaze_focus.update(Some(&hit), Some(&saccade_gaze), 0.016);

    // Should get saccade detected (which resets dwell)
    assert!(
        matches!(event, Some(GazeFocusEvent::SaccadeDetected { .. })),
        "expected SaccadeDetected, got {:?}",
        event
    );
}

// ── Gesture recognition: pinch lifecycle ────────────────────

#[test]
fn test_pinch_gesture_lifecycle() {
    use ewwm_compositor::vr::gesture::{GestureEvent, GestureState, GestureType};
    use ewwm_compositor::vr::hand_tracking::{Hand, HandJoint, HandSkeleton, JointPose};

    let mut gesture = GestureState::new();

    // Build a skeleton where thumb tip and index tip are close (pinch)
    let mut left = HandSkeleton::new(Hand::Left);
    left.tracking_active = true;
    left.confidence = 0.9;

    // Set thumb tip at (0.0, 0.0, 0.0) and index tip very close
    left.joints[HandJoint::ThumbTip.index()] = JointPose {
        position: [0.05, 0.05, 0.0],
        orientation: [0.0, 0.0, 0.0, 1.0],
        radius: 0.01,
        valid: true,
    };
    left.joints[HandJoint::IndexTip.index()] = JointPose {
        position: [0.052, 0.052, 0.0], // ~3mm away — within pinch threshold
        orientation: [0.0, 0.0, 0.0, 1.0],
        radius: 0.01,
        valid: true,
    };
    // Palm position for distance calculations
    left.joints[HandJoint::Palm.index()] = JointPose {
        position: [0.0, 0.0, 0.0],
        orientation: [0.0, 0.0, 0.0, 1.0],
        radius: 0.03,
        valid: true,
    };

    // Other fingertips need to be away from palm (not grab)
    for joint in &[HandJoint::MiddleTip, HandJoint::RingTip, HandJoint::LittleTip] {
        left.joints[joint.index()] = JointPose {
            position: [0.0, 0.15, 0.0], // extended away
            orientation: [0.0, 0.0, 0.0, 1.0],
            radius: 0.01,
            valid: true,
        };
    }

    let right = HandSkeleton::new(Hand::Right); // inactive

    // First update may be debounced; keep feeding
    let mut pinch_started = false;
    for _frame in 0..10 {
        let events = gesture.update(&left, &right, 16.0);
        for event in &events {
            if matches!(event, GestureEvent::Started { gesture: GestureType::Pinch, .. }) {
                pinch_started = true;
            }
        }
        if pinch_started {
            break;
        }
    }
    assert!(pinch_started, "Pinch gesture should be detected");

    // Release: move fingertips apart
    left.joints[HandJoint::IndexTip.index()].position = [0.15, 0.15, 0.0];

    let events = gesture.update(&left, &right, 16.0);
    let released = events
        .iter()
        .any(|e| matches!(e, GestureEvent::Released { gesture: GestureType::Pinch, .. }));
    assert!(released, "Pinch release should be detected");
}

// ── BCI attention state transitions ────────────────────────

#[test]
fn test_attention_monitor_focus_transitions() {
    use ewwm_compositor::vr::attention::{AttentionMonitor, AttentionState};

    let mut monitor = AttentionMonitor::new();
    assert_eq!(monitor.current_state, AttentionState::Neutral);

    // Feed high beta/low theta (focused)
    for _ in 0..20 {
        monitor.update_bands(0.3, 0.8, 0.1, 0.4);
    }
    // After enough high-beta samples, should shift toward focused
    assert!(
        monitor.focus_score > 0.5,
        "High beta should increase focus score, got {}",
        monitor.focus_score
    );

    // Feed high theta/low beta (drowsy/relaxed)
    for _ in 0..50 {
        monitor.update_bands(0.8, 0.1, 0.9, 0.1);
    }
    assert!(
        monitor.focus_score < 0.5,
        "High theta/alpha should decrease focus, got {}",
        monitor.focus_score
    );
}

// ── Secure input + clock: expiry ────────────────────────────

#[test]
fn test_secure_input_deterministic_expiry() {
    use ewwm_compositor::secure_input::SecureInputState;

    let clock = TestClock::new();
    let mut state = SecureInputState::new();

    let now = clock.now();
    state.enter("password-field", 99, now);
    assert!(state.active);

    // 10 seconds in: not expired
    clock.advance(Duration::from_secs(10));
    assert!(!state.is_expired(clock.now()));

    // 29 seconds: still not expired
    clock.advance(Duration::from_secs(19));
    assert!(!state.is_expired(clock.now()));

    // 31 seconds: expired
    clock.advance(Duration::from_secs(2));
    let expired_now = clock.now();
    assert!(state.is_expired(expired_now));

    // Tick should auto-exit
    let event = state.tick(expired_now);
    assert!(event.is_some(), "tick should emit exit event");
    assert!(!state.active);

    // Status sexp after exit
    let sexp = state.status_sexp(expired_now);
    assert!(sexp.contains(":active nil"));
}

// ── Full multi-modal sequence with recording ─────────────────

#[test]
fn test_full_multi_modal_sequence() {
    use ewwm_compositor::vr::eye_tracking::{GazeData, GazeSource, GazeSurfaceHit};
    use ewwm_compositor::vr::gaze_focus::{GazeFocusEvent, GazeFocusManager};
    use ewwm_compositor::vr::scene::Vec3;
    use ewwm_compositor::vr::vr_interaction::Ray;

    let clock = TestClock::new();

    // Build scripted input sequence
    let events = vec![
        // 1. Look at surface
        InputEvent::Gaze {
            yaw: 0.0,
            pitch: 0.0,
            confidence: 0.95,
        },
        // 2. Wait for dwell
        InputEvent::Wait {
            duration: Duration::from_millis(250),
        },
        // 3. Gaze still steady
        InputEvent::Gaze {
            yaw: 0.01,
            pitch: -0.01,
            confidence: 0.9,
        },
        // 4. Pinch to confirm
        InputEvent::Gesture {
            name: "pinch".to_string(),
            hand: "right".to_string(),
            confidence: 0.9,
        },
        // 5. BCI shows focus
        InputEvent::BciSample {
            channel: 0,
            value: 0.85,
        },
        // 6. IPC command
        InputEvent::IpcMessage {
            payload: "(:type :surface-focus :id 10 :surface-id 42)".to_string(),
        },
    ];

    let scripted = ScriptedInputProvider::new(events);
    let mut recording = RecordingProvider::new(scripted);

    // Also set up IPC recorder
    let mut ipc_recorder = IpcRecorder::new();
    ipc_recorder.start(Some("multi-modal-test".to_string()));

    // Set up subsystems
    let mut gaze_focus = GazeFocusManager::new();
    let surface_id = 42u64;

    let mut total_events = 0;
    let mut gaze_events = 0;
    let mut ipc_events = 0;
    let mut wait_total = Duration::ZERO;
    let mut frame_counter = 0u64;

    // Process the input stream
    while recording.has_events() {
        let event = recording.next_event().unwrap();
        total_events += 1;

        match event {
            InputEvent::Gaze { yaw, pitch, confidence } => {
                gaze_events += 1;
                frame_counter += 1;
                // Feed to gaze focus manager
                let hit = GazeSurfaceHit {
                    surface_id,
                    pixel_x: 500.0 + (yaw as f32 * 100.0),
                    pixel_y: 300.0 + (pitch as f32 * 100.0),
                    distance: 1.5,
                    confidence: confidence as f32,
                };
                let gaze = GazeData {
                    source: GazeSource::Simulated,
                    ray: Ray::new(
                        Vec3::new(0.0, 0.0, 0.0),
                        Vec3::new(yaw as f32, pitch as f32, -1.0),
                    ),
                    confidence: confidence as f32,
                    timestamp_s: frame_counter as f64 * 0.016,
                    per_eye: None,
                };
                let _focus_event = gaze_focus.update(Some(&hit), Some(&gaze), 0.016);
            }
            InputEvent::Wait { duration } => {
                clock.advance(duration);
                wait_total += duration;
            }
            InputEvent::Gesture { name, hand, confidence } => {
                // Record as IPC event
                ipc_recorder.record(
                    0,
                    MessageDirection::Event,
                    &format!(
                        "(:type :event :event :gesture-started :gesture \"{}\" :hand \"{}\" :confidence {:.2})",
                        name, hand, confidence
                    ),
                );
            }
            InputEvent::BciSample { channel, value } => {
                ipc_recorder.record(
                    0,
                    MessageDirection::Event,
                    &format!(
                        "(:type :event :event :bci-sample :channel {} :value {:.2})",
                        channel, value
                    ),
                );
            }
            InputEvent::IpcMessage { payload } => {
                ipc_events += 1;
                ipc_recorder.record(1, MessageDirection::Request, &payload);
            }
            _ => {}
        }
    }

    // Validate the recording captured everything
    let recorded = recording.into_recorded();
    assert_eq!(recorded.len(), 6, "should have recorded 6 events");
    assert_eq!(total_events, 6);
    assert_eq!(gaze_events, 2);
    assert_eq!(ipc_events, 1);
    assert_eq!(wait_total, Duration::from_millis(250));

    // IPC recorder should have gesture + BCI + IPC message
    ipc_recorder.stop();
    assert_eq!(ipc_recorder.messages.len(), 3);

    let sexp = ipc_recorder.to_sexp();
    assert!(sexp.contains(":session-name \"multi-modal-test\""));
    assert!(sexp.contains(":message-count 3"));
}

// ── VrState stub creation ────────────────────────────────────

#[test]
fn test_vr_state_stub_subsystem_access() {
    use ewwm_compositor::vr::VrState;

    let vr = VrState::new();
    assert_eq!(vr.session_state_str(), "disabled");
    assert_eq!(vr.hmd_name(), "none");
    assert!(!vr.is_headless());

    // All subsystems accessible through VrState
    assert!(vr.gaze_focus.focused_surface.is_none());
    assert!(vr.gesture.binding_count() == 0);
    assert!(matches!(vr.bci.connection, ewwm_compositor::vr::bci_state::BciConnectionState::Disconnected));
    assert_eq!(vr.frame_stats_sexp(), "(:fps 0 :missed 0 :frame-time-ms 0.0)");
}

// ── Deterministic dwell with TestClock driving timing ────────

#[test]
fn test_deterministic_dwell_with_test_clock() {
    use ewwm_compositor::vr::eye_tracking::{GazeData, GazeSource, GazeSurfaceHit};
    use ewwm_compositor::vr::gaze_focus::{DwellConfig, GazeFocusEvent, GazeFocusManager};
    use ewwm_compositor::vr::scene::Vec3;
    use ewwm_compositor::vr::vr_interaction::Ray;

    let clock = TestClock::new();
    let mut gaze_focus = GazeFocusManager::new();

    // Set a precise dwell threshold
    gaze_focus.config = DwellConfig {
        threshold_ms: 100.0,
        max_jitter_px: 50.0,
        require_confidence: 0.5,
    };

    let hit = GazeSurfaceHit {
        surface_id: 7,
        pixel_x: 200.0,
        pixel_y: 200.0,
        distance: 1.0,
        confidence: 0.8,
    };

    // Frame 0: start dwell
    let gaze = GazeData {
        source: GazeSource::Simulated,
        ray: Ray::new(Vec3::new(0.0, 0.0, 0.0), Vec3::new(0.0, 0.0, -1.0)),
        confidence: 0.8,
        timestamp_s: 0.0,
        per_eye: None,
    };
    let event = gaze_focus.update(Some(&hit), Some(&gaze), 0.020);
    assert!(matches!(event, Some(GazeFocusEvent::DwellStarted { .. })));

    // Advance exactly 50ms (4 frames at 50Hz) — should be progress, not focus
    for i in 1..=4 {
        clock.advance(Duration::from_millis(20));
        let t = i as f64 * 0.020;
        let gaze_t = GazeData {
            source: GazeSource::Simulated,
            ray: Ray::new(Vec3::new(0.0, 0.0, 0.0), Vec3::new(0.0, 0.0, -1.0)),
            confidence: 0.8,
            timestamp_s: t,
            per_eye: None,
        };
        let event = gaze_focus.update(Some(&hit), Some(&gaze_t), 0.020);
        match &event {
            Some(GazeFocusEvent::DwellProgress { elapsed_ms, .. }) => {
                assert!(*elapsed_ms < 100.0, "should not reach threshold yet at {}ms", elapsed_ms);
            }
            other => {
                // May get other events (cooldown, etc.) — skip
                assert!(
                    !matches!(other, Some(GazeFocusEvent::FocusRequested { .. })),
                    "focus should not be requested at only {}ms",
                    i * 20
                );
            }
        }
    }

    // Advance to 120ms total (frame 6) — should trigger focus
    let mut focus_triggered = false;
    for i in 5..=6 {
        clock.advance(Duration::from_millis(20));
        let t = i as f64 * 0.020;
        let gaze_t = GazeData {
            source: GazeSource::Simulated,
            ray: Ray::new(Vec3::new(0.0, 0.0, 0.0), Vec3::new(0.0, 0.0, -1.0)),
            confidence: 0.8,
            timestamp_s: t,
            per_eye: None,
        };
        let event = gaze_focus.update(Some(&hit), Some(&gaze_t), 0.020);
        if matches!(event, Some(GazeFocusEvent::FocusRequested { .. })) {
            focus_triggered = true;
            break;
        }
    }
    assert!(focus_triggered, "focus should be requested after 100ms dwell threshold");
    assert_eq!(gaze_focus.focused_surface, Some(7));
}

// ── IPC recording + replay round-trip ────────────────────────

#[test]
fn test_ipc_record_replay_round_trip() {
    use ewwm_compositor::ipc::recorder::{IpcReplayer, RecordedMessage};

    let mut recorder = IpcRecorder::new();
    recorder.start(Some("round-trip".to_string()));

    // Simulate a session
    let commands = vec![
        "(:type :hello :id 1 :version 1 :client \"test\")",
        "(:type :ping :id 2 :timestamp 1000)",
        "(:type :surface-list :id 3)",
        "(:type :vr-status :id 4)",
        "(:type :gaze-focus-status :id 5)",
    ];

    for (i, cmd) in commands.iter().enumerate() {
        recorder.record(1, MessageDirection::Request, cmd);
        recorder.record(
            1,
            MessageDirection::Response,
            &format!("(:type :response :id {} :status :ok)", i + 1),
        );
    }

    recorder.stop();
    assert_eq!(recorder.messages.len(), 10);

    // Build replayer from recorded messages
    let replayer_messages: Vec<RecordedMessage> = recorder
        .messages
        .iter()
        .map(|m| RecordedMessage {
            timestamp_ms: m.timestamp_ms,
            client_id: m.client_id,
            direction: m.direction.clone(),
            payload: m.payload.clone(),
        })
        .collect();

    let mut replayer = IpcReplayer::new(replayer_messages);
    assert_eq!(replayer.total, 10);

    // Extract only requests
    let mut request_count = 0;
    while let Some(req) = replayer.next_request() {
        request_count += 1;
        assert!(
            req.payload.contains(":type :"),
            "request should contain type"
        );
    }
    assert_eq!(request_count, 5, "should extract 5 requests from 10 messages");
    assert_eq!(replayer.consumed, 10);
}

// ── Scene graph + multi-surface dwell switching ──────────────

#[test]
fn test_multi_surface_focus_switching() {
    use ewwm_compositor::vr::eye_tracking::{GazeData, GazeSource, GazeSurfaceHit};
    use ewwm_compositor::vr::gaze_focus::{DwellConfig, GazeFocusEvent, GazeFocusManager};
    use ewwm_compositor::vr::scene::Vec3;
    use ewwm_compositor::vr::vr_interaction::Ray;

    let clock = TestClock::new();
    let mut gaze_focus = GazeFocusManager::new();
    gaze_focus.config = DwellConfig {
        threshold_ms: 50.0, // fast dwell for test
        max_jitter_px: 50.0,
        require_confidence: 0.5,
    };

    let dir = Vec3::new(0.0, 0.0, -1.0);

    // Focus surface 1
    let hit1 = GazeSurfaceHit {
        surface_id: 1,
        pixel_x: 100.0,
        pixel_y: 100.0,
        distance: 1.0,
        confidence: 0.9,
    };

    // Start + dwell on surface 1
    for frame in 0..=5 {
        let t = frame as f64 * 0.016;
        let gaze = GazeData {
            source: GazeSource::Simulated,
            ray: Ray::new(Vec3::new(0.0, 0.0, 0.0), dir),
            confidence: 0.9,
            timestamp_s: t,
            per_eye: None,
        };
        let _event = gaze_focus.update(Some(&hit1), Some(&gaze), 0.016);
        clock.advance(Duration::from_millis(16));
    }
    assert_eq!(gaze_focus.focused_surface, Some(1));

    // Wait for cooldown to expire
    clock.advance(Duration::from_millis(500));
    let gaze_cooldown = GazeData {
        source: GazeSource::Simulated,
        ray: Ray::new(Vec3::new(0.0, 0.0, 0.0), dir),
        confidence: 0.9,
        timestamp_s: 1.0,
        per_eye: None,
    };
    // Tick the cooldown timer
    for _ in 0..5 {
        gaze_focus.update(None, Some(&gaze_cooldown), 0.100);
    }

    // Switch to surface 2
    let hit2 = GazeSurfaceHit {
        surface_id: 2,
        pixel_x: 800.0,
        pixel_y: 400.0,
        distance: 1.0,
        confidence: 0.9,
    };

    for frame in 0..=5 {
        let t = 1.5 + frame as f64 * 0.016;
        let gaze = GazeData {
            source: GazeSource::Simulated,
            ray: Ray::new(Vec3::new(0.0, 0.0, 0.0), dir),
            confidence: 0.9,
            timestamp_s: t,
            per_eye: None,
        };
        let _event = gaze_focus.update(Some(&hit2), Some(&gaze), 0.016);
        clock.advance(Duration::from_millis(16));
    }
    assert_eq!(gaze_focus.focused_surface, Some(2));

    // History should contain both surfaces
    assert!(gaze_focus.focus_history.len() >= 2);
}

// ── InputProvider recording validates event ordering ─────────

#[test]
fn test_recording_provider_preserves_order() {
    let events = vec![
        InputEvent::BciSample { channel: 0, value: 0.1 },
        InputEvent::BciSample { channel: 1, value: 0.2 },
        InputEvent::Gaze { yaw: 0.5, pitch: 0.3, confidence: 0.9 },
        InputEvent::Gesture {
            name: "grab".to_string(),
            hand: "left".to_string(),
            confidence: 0.8,
        },
        InputEvent::Blink {
            left_closed: true,
            right_closed: false,
            duration_ms: 120,
        },
        InputEvent::Hand {
            left: true,
            joint: 4,
            x: 0.1,
            y: 0.2,
            z: 0.3,
        },
        InputEvent::IpcMessage {
            payload: "(:type :ping :id 1)".to_string(),
        },
        InputEvent::Wait {
            duration: Duration::from_millis(100),
        },
    ];

    let scripted = ScriptedInputProvider::new(events);
    let mut recording = RecordingProvider::new(scripted);

    // Drain all events
    let mut count = 0;
    while recording.has_events() {
        let _ = recording.next_event().unwrap();
        count += 1;
    }
    assert_eq!(count, 8);

    let recorded = recording.into_recorded();
    assert_eq!(recorded.len(), 8);

    // Verify ordering
    assert!(matches!(recorded[0], InputEvent::BciSample { channel: 0, .. }));
    assert!(matches!(recorded[1], InputEvent::BciSample { channel: 1, .. }));
    assert!(matches!(recorded[2], InputEvent::Gaze { .. }));
    assert!(matches!(recorded[3], InputEvent::Gesture { .. }));
    assert!(matches!(recorded[4], InputEvent::Blink { .. }));
    assert!(matches!(recorded[5], InputEvent::Hand { .. }));
    assert!(matches!(recorded[6], InputEvent::IpcMessage { .. }));
    assert!(matches!(recorded[7], InputEvent::Wait { .. }));
}

// ── Clock Arc<dyn Clock> shared across subsystems ────────────

#[test]
fn test_shared_clock_across_subsystems() {
    use ewwm_compositor::secure_input::SecureInputState;

    let test_clock = TestClock::new();

    // Test that Clock trait works through Arc<dyn Clock>
    let clock: Arc<dyn Clock> = Arc::new(TestClock::new());
    let now1 = clock.now();
    let now2 = clock.now();
    assert_eq!(now1, now2, "same clock should return same time without advance");

    let ms1 = clock.unix_millis();
    let ms2 = clock.unix_millis();
    assert_eq!(ms1, ms2);

    // Use a concrete TestClock for deterministic testing of two subsystems
    let mut state1 = SecureInputState::new();
    let mut state2 = SecureInputState::new();

    let now = test_clock.now();
    state1.enter("field-1", 1, now);
    state2.enter("field-2", 2, now);

    assert!(state1.active);
    assert!(state2.active);

    // Both see the same time
    assert!(!state1.is_expired(now));
    assert!(!state2.is_expired(now));

    // Advance and verify both expire at the same point
    test_clock.advance(Duration::from_secs(31));
    let later = test_clock.now();
    assert!(state1.is_expired(later));
    assert!(state2.is_expired(later));
}
