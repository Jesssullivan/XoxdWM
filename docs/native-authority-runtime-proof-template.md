# XoxdWM Native Authority Runtime Proof

Use this template for the Linux/Honey/Yoga proof gate attached to
`Native XoxdWM Authority`. This is not a headless smoke replacement and is not
Honey P4 headset first-frame proof. It records whether a real Linux session can
perform basic WM behavior without Emacs in the authority path.

## Scope

- No reboot is required.
- Do not stop or modify `rke2-server` on `honey`.
- Emacs/eGreg may run as applications or IPC clients, but
  `exwm-vr-emacs.service` must not be required for WM authority.
- Headless IPC success is supporting evidence only; product proof requires
  human-visible desktop behavior.

## Command

```bash
just native-authority-proof
```

Installed Rocky package command:

```bash
xoxdwm-native-authority-proof
```

Remote repo wrapper:

```bash
# Conservative read-only IPC proof over SSH.
just native-authority-proof-remote honey 0

# Mutating workspace/layout proof when an operator is watching the session.
just native-authority-proof-remote yoga 1

# Mutating proof that also requests a configured native app launch target.
just native-authority-proof-remote yoga 1 terminal
```

Optional evidence file:

```bash
XOXDWM_PROOF_EVIDENCE=docs/proofs/native-authority-$(hostname)-$(date +%Y%m%dT%H%M%S).md \
  just native-authority-proof
```

Set `XOXDWM_PROOF_MUTATE=0` for read-only IPC checks. The default proof mutates
layout/workspace state so the operator can observe native visibility and reflow.

## Machine Evidence

- host:
- date:
- branch:
- commit:
- package version:
- compositor binary:
- IPC socket:
- `exwm-vr-compositor.service` state:
- `exwm-vr-emacs.service` state:
- `rke2-server` state on Honey:
- `just native-authority-proof` result:
- command output link or paste:

## Human Observation

- visual_observed: TODO_yes_or_no
- app_launch_observed: TODO_yes_or_no
- focus_change_observed: TODO_yes_or_no
- workspace_visibility_observed: TODO_yes_or_no
- layout_reflow_observed: TODO_yes_or_no
- Emacs/eGreg role during proof: TODO_app_only_or_absent
- notes:

## Completion Interpretation

Pass only when:

- the compositor IPC proof passes
- Emacs service is absent or explicitly app-layer only
- a human observes visible workspace switching and layout reflow
- at least one normal developer app launches under the XoxdWM session
- focus can move between visible app surfaces

Fail or keep open when:

- only headless IPC passes
- Emacs/EXWM owns workspace, focus, launch, or layout behavior
- workspace switching changes IPC state but not visible surfaces
- layout changes update state but do not visibly reflow surfaces
