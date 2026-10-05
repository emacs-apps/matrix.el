EMACS ?= emacs
CARGO ?= cargo
APPKIT_DIR ?= ../appkit.el
VIDEO_DIR ?= ../video.el
ELISP := matrix-rpc.el matrix-state.el matrix-runtime.el matrix.el matrix-root.el matrix-room.el matrix-verify.el matrix-transient.el matrix-evil.el

.PHONY: backend release check check-backend check-elisp

backend:
	$(CARGO) build --locked --manifest-path backend/Cargo.toml

release:
	$(CARGO) build --locked --release --manifest-path backend/Cargo.toml

check: check-backend check-elisp

check-backend:
	$(CARGO) test --locked --manifest-path backend/Cargo.toml

check-elisp:
	$(EMACS) -Q --batch -L "$(APPKIT_DIR)" -L "$(VIDEO_DIR)" -L . --eval '(setq load-prefer-newer t byte-compile-error-on-warn t)' -f batch-byte-compile $(ELISP)
	$(EMACS) -Q --batch -L "$(APPKIT_DIR)" -L "$(VIDEO_DIR)" -L . -L test --eval '(setq load-prefer-newer t)' -l matrix-state-test -l matrix-ui-test -f ert-run-tests-batch-and-exit
