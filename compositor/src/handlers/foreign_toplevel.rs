//! Foreign toplevel list handler — expose managed surfaces to external tools.
//!
//! Implements ext-foreign-toplevel-list-v1 so taskbars (waybar) and
//! app switchers can query the window list.

use crate::state::EwwmState;
use smithay::{
    delegate_foreign_toplevel_list,
    wayland::foreign_toplevel_list::{ForeignToplevelListHandler, ForeignToplevelListState},
};

impl ForeignToplevelListHandler for EwwmState {
    fn foreign_toplevel_list_state(&mut self) -> &mut ForeignToplevelListState {
        &mut self.foreign_toplevel_state
    }
}

delegate_foreign_toplevel_list!(EwwmState);
