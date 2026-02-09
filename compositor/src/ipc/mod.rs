//! IPC server — s-expression protocol over Unix domain socket.
//!
//! Provides bidirectional communication between the compositor and Emacs.
//! Wire format: 4-byte big-endian length prefix + UTF-8 s-expression payload.

pub mod dispatch;
pub mod server;

pub use server::IpcServer;
