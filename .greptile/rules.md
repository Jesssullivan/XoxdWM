# XoxdWM: Repo-Specific Review Rules

Inherits all rules from `_org-enforced-rules.md`.

## Systems Programming

- All system call return values must be checked. No ignoring `errno`.
- Memory allocations must have corresponding cleanup on all code paths, including error paths.
- No unbounded stack allocations (e.g., VLAs or large arrays on stack).

## If C

- No implicit fallthrough in `switch` without a `/* fallthrough */` comment.
- Use `const` on pointers that should not be modified.
- Header files must have include guards (`#ifndef`/`#define`/`#endif`).
- Prefer `size_t` for sizes and indices.

## If Rust

- `unsafe` blocks require `// SAFETY:` comments.
- No `unwrap()` in event handling code paths -- use proper error handling.
- Use `#[deny(unsafe_op_in_unsafe_fn)]` at the crate level.

## Window Management

- Event loop must not block indefinitely without a timeout or signal mechanism.
- Resource handles (windows, display connections) must be released on shutdown.
- Configuration parsing must handle malformed input gracefully, not panic.
