//! Input event handling — keyboard, pointer, focus management.

use crate::config::LAYOUT_CYCLE;
use crate::ipc::{dispatch::format_event, server::IpcServer};
use crate::state::{EwwmState, FocusDirection};
use smithay::{
    backend::input::{
        AbsolutePositionEvent, Axis, AxisSource, ButtonState, Event, InputBackend, InputEvent,
        KeyState, KeyboardKeyEvent, PointerAxisEvent, PointerButtonEvent,
        PointerMotionAbsoluteEvent,
    },
    input::{
        keyboard::{FilterResult, KeyboardHandle, ModifiersState},
        pointer::{AxisFrame, ButtonEvent, MotionEvent},
    },
    utils::SERIAL_COUNTER,
};
use tracing::{debug, trace, warn};

/// Handle an input event from any backend.
pub fn handle_input<B: InputBackend>(state: &mut EwwmState, event: InputEvent<B>) {
    match event {
        InputEvent::Keyboard { event } => handle_keyboard::<B>(state, event),
        InputEvent::PointerMotionAbsolute { event } => {
            handle_pointer_motion_absolute::<B>(state, event)
        }
        InputEvent::PointerButton { event } => handle_pointer_button::<B>(state, event),
        InputEvent::PointerAxis { event } => handle_pointer_axis::<B>(state, event),
        _ => {}
    }
}

/// Convert xkbcommon modifiers + keysym to an Emacs-style key description.
fn format_key_description(keysym: xkbcommon::xkb::Keysym, mods: &ModifiersState) -> Option<String> {
    let sym_name = xkbcommon::xkb::keysym_get_name(keysym);

    // Map common keysym names to Emacs names
    let key_name = match sym_name.as_str() {
        "Return" => "RET".to_string(),
        "Escape" => "ESC".to_string(),
        "BackSpace" => "DEL".to_string(),
        "Tab" => "TAB".to_string(),
        "space" => "SPC".to_string(),
        "Delete" => "delete".to_string(),
        name if name.len() == 1 => name.to_lowercase(),
        name => name.to_string(),
    };

    let mut desc = String::new();
    if mods.ctrl {
        desc.push_str("C-");
    }
    if mods.alt {
        desc.push_str("M-");
    }
    if mods.logo {
        desc.push_str("s-");
    }
    if mods.shift && key_name.len() > 1 {
        desc.push_str("S-");
    }
    desc.push_str(&key_name);

    Some(desc)
}

fn handle_keyboard<B: InputBackend>(state: &mut EwwmState, event: B::KeyboardKeyEvent) {
    let serial = SERIAL_COUNTER.next_serial();
    let time = Event::time_msec(&event);
    let keycode = event.key_code();
    let key_state = event.state();

    let keyboard = state.seat.get_keyboard().unwrap();

    // Check for grabbed keys
    let _grab_result = keyboard.input::<bool, _>(
        state,
        keycode,
        key_state,
        serial,
        time,
        |state, mods, handle| {
            if key_state == KeyState::Pressed {
                let keysym = handle.modified_sym();
                if let Some(key_desc) = format_key_description(keysym, mods) {
                    if handle_native_key_action(state, &key_desc, time) {
                        return FilterResult::Intercept(true);
                    }
                    if state.grabbed_keys.contains(&key_desc) {
                        debug!(key = %key_desc, "grabbed key intercepted");
                        // Emit key-pressed event to IPC clients
                        let event = format_event(
                            "key-pressed",
                            &[
                                ("key", &format!("\"{}\"", key_desc)),
                                (
                                    "modifiers",
                                    &format!(
                                        "(:super {} :ctrl {} :alt {} :shift {})",
                                        if mods.logo { "t" } else { "nil" },
                                        if mods.ctrl { "t" } else { "nil" },
                                        if mods.alt { "t" } else { "nil" },
                                        if mods.shift { "t" } else { "nil" },
                                    ),
                                ),
                                ("timestamp", &time.to_string()),
                            ],
                        );
                        IpcServer::broadcast_event(state, &event);
                        return FilterResult::Intercept(true);
                    }
                }
            }
            FilterResult::Forward
        },
    );
}

fn handle_native_key_action(state: &mut EwwmState, key: &str, time: u32) -> bool {
    let Some(action) = state
        .config
        .native_action_for_key(key)
        .map(ToString::to_string)
    else {
        return false;
    };

    debug!(key, action, "native key action");
    let result = execute_native_key_action(state, &action);
    let (status, detail) = match result {
        Ok(detail) => ("ok", detail),
        Err(reason) => {
            warn!(key, action, reason, "native key action failed");
            ("error", reason)
        }
    };

    let key_value = quoted(key);
    let action_value = quoted(&action);
    let status_value = format!(":{status}");
    let detail_value = quoted(&detail);
    let event = format_event(
        "native-key-action",
        &[
            ("key", &key_value),
            ("action", &action_value),
            ("status", &status_value),
            ("detail", &detail_value),
            ("timestamp", &time.to_string()),
        ],
    );
    IpcServer::broadcast_event(state, &event);
    true
}

fn execute_native_key_action(state: &mut EwwmState, action: &str) -> Result<String, String> {
    if let Some(workspace) = action.strip_prefix("workspace:") {
        let workspace = workspace
            .parse::<usize>()
            .map_err(|_| format!("invalid workspace action: {action}"))?;
        return switch_workspace(state, workspace);
    }

    if let Some(name) = action.strip_prefix("launch:") {
        return state.launch_configured_app(name);
    }

    match action {
        "focus:next" => Ok(focus_adjacent(state, FocusDirection::Next)),
        "focus:previous" => Ok(focus_adjacent(state, FocusDirection::Previous)),
        "layout:cycle" => Ok(cycle_layout(state)),
        "compositor:exit" => {
            state.running = false;
            Ok("compositor-exit".to_string())
        }
        "compositor:reload" => {
            let source = state.reload_native_config()?;
            Ok(format!("config-reloaded:{source}"))
        }
        _ => Err(format!("unknown native key action: {action}")),
    }
}

fn switch_workspace(state: &mut EwwmState, workspace: usize) -> Result<String, String> {
    if workspace >= state.workspace_count {
        return Err(format!(
            "workspace {} out of range (count {})",
            workspace, state.workspace_count
        ));
    }

    let previous = state.active_workspace;
    state.active_workspace = workspace;
    state.apply_native_layout();
    if previous != workspace {
        let event = format_event(
            "workspace-changed",
            &[
                ("workspace", &workspace.to_string()),
                ("previous", &previous.to_string()),
            ],
        );
        IpcServer::broadcast_event(state, &event);
    }
    Ok(format!("workspace:{workspace}"))
}

fn focus_adjacent(state: &mut EwwmState, direction: FocusDirection) -> String {
    match state.focus_adjacent_surface(direction) {
        Some(surface_id) => format!("surface:{surface_id}"),
        None => "surface:none".to_string(),
    }
}

fn cycle_layout(state: &mut EwwmState) -> String {
    let previous = state.current_layout.clone();
    let current_index = LAYOUT_CYCLE
        .iter()
        .position(|layout| *layout == state.current_layout)
        .unwrap_or(0);
    let next = LAYOUT_CYCLE[(current_index + 1) % LAYOUT_CYCLE.len()];
    state.current_layout = next.to_string();
    state.apply_native_layout();

    let layout_kw = format!(":{next}");
    let previous_kw = format!(":{previous}");
    let event = format_event(
        "layout-changed",
        &[
            ("layout", layout_kw.as_str()),
            ("previous", previous_kw.as_str()),
        ],
    );
    IpcServer::broadcast_event(state, &event);
    format!("layout:{next}")
}

fn quoted(value: &str) -> String {
    format!("\"{}\"", value.replace('\\', "\\\\").replace('"', "\\\""))
}

fn handle_pointer_motion_absolute<B: InputBackend>(
    state: &mut EwwmState,
    event: B::PointerMotionAbsoluteEvent,
) {
    let output = state.space.outputs().next().cloned();
    if let Some(output) = output {
        let output_geo = state.space.output_geometry(&output).unwrap();
        let pos = event.position_transformed(output_geo.size);

        let serial = SERIAL_COUNTER.next_serial();
        let pointer = state.seat.get_pointer().unwrap();

        // Find surface under pointer for focus
        let surface_under = state.space.element_under(pos).map(|(w, loc)| {
            let surface = w
                .toplevel()
                .expect("window has toplevel")
                .wl_surface()
                .clone();
            (surface, loc.to_f64())
        });

        pointer.motion(
            state,
            surface_under,
            &MotionEvent {
                location: pos,
                serial,
                time: Event::time_msec(&event) as u32,
            },
        );
    }
}

fn handle_pointer_button<B: InputBackend>(state: &mut EwwmState, event: B::PointerButtonEvent) {
    let serial = SERIAL_COUNTER.next_serial();
    let button = event.button_code();
    let button_state = event.state();

    let pointer = state.seat.get_pointer().unwrap();
    pointer.button(
        state,
        &ButtonEvent {
            button,
            state: button_state,
            serial,
            time: Event::time_msec(&event) as u32,
        },
    );

    // Focus follows click: set keyboard focus to surface under pointer
    if button_state == ButtonState::Pressed {
        if let Some(surface) = pointer.current_focus() {
            let serial = SERIAL_COUNTER.next_serial();
            if let Some(keyboard) = state.seat.get_keyboard() {
                keyboard.set_focus(state, Some(surface), serial);
            }
        }
    }
}

fn handle_pointer_axis<B: InputBackend>(state: &mut EwwmState, event: B::PointerAxisEvent) {
    let source = event.source();
    let horizontal = event.amount(Axis::Horizontal).unwrap_or(0.0);
    let vertical = event.amount(Axis::Vertical).unwrap_or(0.0);

    let pointer = state.seat.get_pointer().unwrap();
    let mut frame = AxisFrame::new(Event::time_msec(&event) as u32).source(source);
    if horizontal != 0.0 {
        frame = frame.value(Axis::Horizontal, horizontal);
    }
    if vertical != 0.0 {
        frame = frame.value(Axis::Vertical, vertical);
    }
    pointer.axis(state, frame);
    pointer.frame(state);
}
