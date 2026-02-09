;;; week8-integration-test.el --- Week 8 integration tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-vr-scene)

;; ── Rust module structure ─────────────────────────────────────

(ert-deftest week8-integration/scene-rs-exists ()
  "compositor/src/vr/scene.rs exists."
  (let ((f (expand-file-name "compositor/src/vr/scene.rs"
                              (locate-dominating-file default-directory ".git"))))
    (should (file-exists-p f))))

(ert-deftest week8-integration/texture-rs-exists ()
  "compositor/src/vr/texture.rs exists."
  (let ((f (expand-file-name "compositor/src/vr/texture.rs"
                              (locate-dominating-file default-directory ".git"))))
    (should (file-exists-p f))))

(ert-deftest week8-integration/vr-renderer-rs-exists ()
  "compositor/src/vr/vr_renderer.rs exists."
  (let ((f (expand-file-name "compositor/src/vr/vr_renderer.rs"
                              (locate-dominating-file default-directory ".git"))))
    (should (file-exists-p f))))

;; ── Scene module contents ─────────────────────────────────────

(ert-deftest week8-integration/scene-has-vr-scene ()
  "scene.rs defines VrScene struct."
  (let ((f (expand-file-name "compositor/src/vr/scene.rs"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "pub struct VrScene" nil t)))))

(ert-deftest week8-integration/scene-has-scene-node ()
  "scene.rs defines SceneNode struct."
  (let ((f (expand-file-name "compositor/src/vr/scene.rs"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "pub struct SceneNode" nil t)))))

(ert-deftest week8-integration/scene-has-geometry ()
  "scene.rs defines Geometry enum."
  (let ((f (expand-file-name "compositor/src/vr/scene.rs"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "pub enum Geometry" nil t)))))

(ert-deftest week8-integration/scene-has-layout-modes ()
  "scene.rs defines VrLayoutMode enum."
  (let ((f (expand-file-name "compositor/src/vr/scene.rs"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "pub enum VrLayoutMode" nil t)))))

(ert-deftest week8-integration/scene-has-mat4 ()
  "scene.rs defines Mat4 struct."
  (let ((f (expand-file-name "compositor/src/vr/scene.rs"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "pub struct Mat4" nil t)))))

(ert-deftest week8-integration/scene-has-projection-types ()
  "scene.rs defines ProjectionType enum."
  (let ((f (expand-file-name "compositor/src/vr/scene.rs"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "pub enum ProjectionType" nil t)))))

;; ── mod.rs exports scene ──────────────────────────────────────

(ert-deftest week8-integration/mod-rs-has-scene ()
  "vr/mod.rs declares scene module."
  (let ((f (expand-file-name "compositor/src/vr/mod.rs"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "pub mod scene;" nil t)))))

(ert-deftest week8-integration/mod-rs-has-texture ()
  "vr/mod.rs declares texture module (behind vr feature)."
  (let ((f (expand-file-name "compositor/src/vr/mod.rs"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "pub mod texture;" nil t)))))

(ert-deftest week8-integration/mod-rs-has-vr-renderer ()
  "vr/mod.rs declares vr_renderer module (behind vr feature)."
  (let ((f (expand-file-name "compositor/src/vr/mod.rs"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "pub mod vr_renderer;" nil t)))))

;; ── IPC dispatch has scene commands ───────────────────────────

(ert-deftest week8-integration/ipc-has-scene-status ()
  "IPC dispatch handles vr-scene-status."
  (let ((f (expand-file-name "compositor/src/ipc/dispatch.rs"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "\"vr-scene-status\"" nil t)))))

(ert-deftest week8-integration/ipc-has-scene-set-layout ()
  "IPC dispatch handles vr-scene-set-layout."
  (let ((f (expand-file-name "compositor/src/ipc/dispatch.rs"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "\"vr-scene-set-layout\"" nil t)))))

(ert-deftest week8-integration/ipc-has-scene-set-ppu ()
  "IPC dispatch handles vr-scene-set-ppu."
  (let ((f (expand-file-name "compositor/src/ipc/dispatch.rs"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "\"vr-scene-set-ppu\"" nil t)))))

(ert-deftest week8-integration/ipc-has-scene-set-background ()
  "IPC dispatch handles vr-scene-set-background."
  (let ((f (expand-file-name "compositor/src/ipc/dispatch.rs"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "\"vr-scene-set-background\"" nil t)))))

(ert-deftest week8-integration/ipc-has-scene-focus ()
  "IPC dispatch handles vr-scene-focus."
  (let ((f (expand-file-name "compositor/src/ipc/dispatch.rs"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "\"vr-scene-focus\"" nil t)))))

(ert-deftest week8-integration/ipc-has-scene-move ()
  "IPC dispatch handles vr-scene-move."
  (let ((f (expand-file-name "compositor/src/ipc/dispatch.rs"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "\"vr-scene-move\"" nil t)))))

;; ── Emacs module ──────────────────────────────────────────────

(ert-deftest week8-integration/ewwm-vr-scene-provides ()
  "ewwm-vr-scene provides its feature."
  (should (featurep 'ewwm-vr-scene)))

(ert-deftest week8-integration/ewwm-vr-scene-commands ()
  "All VR scene interactive commands exist."
  (should (commandp 'ewwm-vr-scene-status))
  (should (commandp 'ewwm-vr-scene-set-layout))
  (should (commandp 'ewwm-vr-scene-set-ppu))
  (should (commandp 'ewwm-vr-scene-set-background))
  (should (commandp 'ewwm-vr-scene-set-projection))
  (should (commandp 'ewwm-vr-scene-focus))
  (should (commandp 'ewwm-vr-scene-move)))

;; ── VrState has scene field ───────────────────────────────────

(ert-deftest week8-integration/state-has-scene-field ()
  "Compositor VrState (stub) has scene field."
  (let ((f (expand-file-name "compositor/src/vr/stub.rs"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "pub scene: VrScene" nil t)))))

(ert-deftest week8-integration/openxr-state-has-scene-field ()
  "Compositor VrState (openxr) has scene field."
  (let ((f (expand-file-name "compositor/src/vr/openxr_state.rs"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "pub scene: VrScene" nil t)))))

;; ── ewwm.el integration ──────────────────────────────────────

(ert-deftest week8-integration/ewwm-requires-scene ()
  "ewwm.el requires ewwm-vr-scene."
  (let ((f (expand-file-name "lisp/vr/ewwm.el"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "(require 'ewwm-vr-scene)" nil t)))))

(ert-deftest week8-integration/ewwm-init-calls-scene ()
  "ewwm.el init calls ewwm-vr-scene-init."
  (let ((f (expand-file-name "lisp/vr/ewwm.el"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "(ewwm-vr-scene-init)" nil t)))))

(ert-deftest week8-integration/ewwm-exit-calls-scene-teardown ()
  "ewwm.el exit calls ewwm-vr-scene-teardown."
  (let ((f (expand-file-name "lisp/vr/ewwm.el"
                              (locate-dominating-file default-directory ".git"))))
    (with-temp-buffer
      (insert-file-contents f)
      (should (search-forward "(ewwm-vr-scene-teardown)" nil t)))))

;;; week8-integration-test.el ends here
