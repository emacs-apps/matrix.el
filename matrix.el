;;; matrix.el --- Persistent Matrix accounts in AppKit -*- lexical-binding: t; -*-

;;; Commentary:
;; Matrix text client using this checkout's AppKit and Matrix Rust SDK 0.19.1.
;;
;; Build from appkit-apps/matrix with `make backend' (Rust 1.96+ and native
;; build tools), or `make release'.  `make check' runs Rust and Emacs checks.
;; Add appkit-apps/appkit.el and appkit-apps/matrix to `load-path', then
;; (require 'matrix).  The bundled release/debug executable is discovered
;; automatically; `matrix-backend-program' can select an installed binary.
;;
;; M-x matrix-connect chooses a local profile: existing profiles restore
;; their device; new profiles prompt for MXID, optional homeserver URL and
;; password.  Native password login is supported, not OAuth/SSO.  Use
;; M-x matrix-restore after restarting, not another login under a new name.
;; Corrupt, incomplete and revoked sessions never silently create a device.
;;
;; The account directory lists joined rooms and invitations.  RET opens or
;; joins a room; j joins by ID/alias, c creates a private encrypted room,
;; g refreshes, and v opens devices/SAS verification.  C-c C-c sends a room
;; draft; C-c C-r returns to rooms and C-c C-v opens devices.  SDK queue
;; acceptance is not delivery: pending/sent/failed state stays in the timeline.
;; Async results cannot clear intervening edits.  An uncertain send retains
;; the draft and blocks resubmission until C-c C-u explicitly reviews it.
;;
;; Closing any/all Matrix buffers does not disconnect the account.
;; M-x matrix-disconnect stops sync locally but retains session and keys.
;; An App owns account lifecycle and a presentation mirror; its Source owns
;; private framed JSON-RPC transport.  Room/verification Surfaces do not own
;; sync, SDK send queues or protocol state.  The Rust SDK owns encryption,
;; device trust, transactions and persistent SQLite stores.
;;
;; Compare the displayed SAS on both devices before confirming.  Room key
;; export/import use an encrypted file and a separately prompted passphrase;
;; export never overwrites a file and must be outside the account store.
;; `matrix-keys-recover' uses an existing server recovery setup/key, without
;; resetting trust.  This client does not create/reset server key backups.
;;
;; Profiles live under `matrix-data-directory' (default: matrix/ beneath
;; `user-emacs-directory').  Keep this directory OUT of version control and
;; unencrypted backups: tokens, device keys and decrypted history are local
;; private data, not a separately encrypted vault.  Storage is owner-only and
;; process-locked; use disk encryption against offline access.
;; Passwords are not saved; RPC event logging/hooks are disabled.  Do not
;; delete store markers to retry an interrupted login: first review/revoke
;; any uncertain device in an existing trusted client.
;;
;; The current interface covers text, joined/invited rooms, SAS and key
;; recovery.  It does not expose media, QR verification or history pagination.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-app)
(require 'appkit-surface)
(require 'appkit-projection)
(require 'matrix-runtime)

(declare-function matrix-root-open "matrix-root" (app &optional select))
(declare-function matrix-root--result "matrix-root" (model method result error context))
(declare-function matrix-room--result "matrix-room" (model method result error context))
(declare-function matrix-verify-open "matrix-verify" (app &optional select no-refresh))
(declare-function matrix-verify--result "matrix-verify" (model method result error context))

(autoload 'matrix-join-room "matrix-root" nil t)
(autoload 'matrix-create-room "matrix-root" nil t)
(autoload 'matrix-verification-request "matrix-verify" nil t)
(autoload 'matrix-keys-export "matrix-verify" nil t)
(autoload 'matrix-keys-import "matrix-verify" nil t)
(autoload 'matrix-keys-recover "matrix-verify" nil t)

(defvar matrix--request-token 0
  "Monotonic UI operation identity, independent of transport request IDs.")
(defvar-local matrix--registered-surface nil
  "Exact Surface registered by this host, retained through AppKit detachment.")

(defun matrix--one-line (text)
  "Return plain TEXT on one physical directory row."
  (replace-regexp-in-string "[\n\r\t]+" " "
                            (substring-no-properties (or text ""))))

(defun matrix--error-text (error)
  "Return the safe user-visible message in a protocol ERROR."
  (or (plist-get error :message) "The operation failed"))

(defun matrix--current-surface (&optional kind)
  "Return this host's live Matrix Surface, optionally requiring KIND."
  (let ((surface (appkit-current-surface)))
    (unless (and (appkit-surface-live-p surface)
                 (eq surface matrix--registered-surface)
                 (or (not kind)
                     (eq kind (car (appkit-surface-identity surface)))))
      (user-error "This command requires a live Matrix %s buffer"
                  (or kind "account")))
    surface))

(defun matrix--current-app ()
  "Return the current Matrix account, or prompt for a live account."
  (if (and (appkit-surface-live-p matrix--registered-surface)
           (eq matrix--registered-surface (appkit-current-surface)))
      (appkit-surface-app matrix--registered-surface)
    (let* ((accounts (matrix-runtime-accounts))
           (choices (mapcar (lambda (app)
                              (cons (plist-get (matrix-runtime-state app) :account)
                                    app))
                            accounts)))
      (unless choices (user-error "No live Matrix account; use M-x matrix-connect"))
      (if (= (length choices) 1)
          (cdar choices)
        (cdr (assoc (completing-read "Matrix account: " choices nil t)
                    choices))))))

(defun matrix--close-host ()
  "Unregister this host's exact Surface without stopping its account."
  (when matrix--registered-surface
    (let ((surface matrix--registered-surface))
      (setq matrix--registered-surface nil)
      (matrix-runtime-unregister-surface (appkit-surface-app surface) surface))))

(defun matrix--register (app surface &optional room-id)
  "Register newly opened SURFACE of APP and optional ROOM-ID externally."
  (with-current-buffer (appkit-surface-buffer surface)
    (setq-local matrix--registered-surface surface)
    (add-hook 'kill-buffer-hook #'matrix--close-host nil t)
    (add-hook 'change-major-mode-hook #'matrix--close-host nil t))
  (matrix-runtime-register-surface app surface room-id)
  surface)

(defun matrix--surface-init (context input)
  "Capture exact generation address and presentation identity INPUT."
  (appkit-next
   :model (list :identity input
                :address (appkit-transition-context-owner-address context))
   :render (appkit-projection-change-create :full-p t)))

(defun matrix--matching-result-p (model method context)
  "Whether METHOD's result CONTEXT owns MODEL's outstanding operation."
  (let ((operation (plist-get model :operation)))
    (and operation
         (equal (plist-get model :address) (plist-get context :target))
         (equal method (plist-get operation :method))
         (equal (plist-get operation :token) (plist-get context :token)))))

(defun matrix--surface-update (_context model message)
  "Reduce directed UI MESSAGE without issuing protocol work in a reducer."
  (pcase message
    (`(matrix-update ,_kind ,room-id)
     (appkit-next
      :model model
      :render (if (or (not room-id)
                      (not (eq (car (plist-get model :identity)) 'room))
                      (equal room-id (cadr (plist-get model :identity))))
                  (appkit-projection-change-create :full-p t :frame-p t)
                appkit-render-none)))
    ('(matrix-send-reviewed)
     (appkit-next
      :model (plist-put (plist-put (copy-sequence model) :uncertain-send nil)
                        :notice "Previous submission reviewed; no message was resent.")
      :render (appkit-projection-change-create :frame-p t)))
    (`(matrix-operation ,operation)
     (appkit-next
      :model (plist-put (plist-put (copy-sequence model) :operation operation)
                        :notice (format "%s: waiting for response…"
                                        (plist-get operation :method)))
      :render (appkit-projection-change-create :frame-p t)))
    (`(matrix-result ,_id ,method ,result ,error ,context)
     (if (not (matrix--matching-result-p model method context))
         (appkit-next :model model :render appkit-render-none)
       (let ((next (plist-put (copy-sequence model) :operation nil)))
         (setq next (plist-put next :notice
                               (if error (matrix--error-text error)
                                 (format "%s completed" method))))
         (setq next
               (pcase (car (plist-get model :identity))
                 ('room (matrix-room--result next method result error context))
                 ('root (matrix-root--result next method result error context))
                 ('verify (matrix-verify--result next method result error context))
                 (_ next)))
         (appkit-next :model next
                      :render (appkit-projection-change-create :full-p t :frame-p t)))))
    (_ (appkit-next :model model :render appkit-render-none))))

(defun matrix--surface-type (name mode render &optional mount)
  "Create a Generated Surface NAME using MODE, RENDER and optional MOUNT.
RENDER receives Surface, the pass-scoped account model, and Surface model."
  (appkit-surface-type-create
   :name name :mode mode :init #'matrix--surface-init
   :update #'matrix--surface-update
   :renderer-factory
   (lambda (_surface)
     (appkit-generated-renderer-create
      :mount (or mount #'ignore)
      :unmount #'ignore
      :merge #'appkit-projection-change-merge
      :render (lambda (surface app-view model _request)
                (funcall render surface (appkit-app-read-view-model app-view) model)
                nil)))))

(defun matrix--request (surface method params &optional context)
  "Issue METHOD with PARAMS for SURFACE using an owned non-secret CONTEXT.
Only the exact Surface generation and operation token may accept the result."
  (let* ((model (appkit-surface-model surface))
         (owned (append (list :target (plist-get model :address)
                              :token (cl-incf matrix--request-token)
                              :method method)
                        context)))
    (when (plist-get model :operation)
      (user-error "Wait for the current operation before starting another"))
    (appkit-surface-send surface (list 'matrix-operation owned))
    (condition-case nil
        (matrix-runtime-request (appkit-surface-app surface) method params owned)
      (user-error
       (appkit-surface-send
        surface (list 'matrix-result nil method nil
                      '(:message "Operation was not sent; check that the account is ready."
                        :data (:code "transport_not_sent"))
                      owned)))
      (error
       ;; Never echo request parameters or transport exceptions (possibly secrets).
       (appkit-surface-send
        surface (list 'matrix-result nil method nil
                      '(:message "Transport failed; acceptance is unknown. Check account status."
                        :data (:code "transport_uncertain"))
                      owned))))))

(defun matrix--read-profile (&optional existing)
  "Read a local profile ID, requiring an EXISTING profile when non-nil."
  (let* ((profiles (delete-dups
                    (append (matrix-runtime-profiles)
                            (mapcar (lambda (app)
                                      (plist-get (matrix-runtime-state app) :account))
                                    (matrix-runtime-accounts)))))
         (account (completing-read "Local Matrix profile: " profiles nil existing)))
    (when (string-empty-p (string-trim account))
      (user-error "A nonempty local profile ID is required"))
    account))

;;;###autoload
(defun matrix-connect (&optional account)
  "Connect local ACCOUNT, restoring its identity or logging in a new profile.
Existing stores, including corrupt ones, are never silently replaced."
  (interactive)
  (setq account (or account (matrix--read-profile)))
  (if (or (matrix-runtime-profile-exists-p account)
          (seq-find (lambda (app)
                      (equal account (plist-get (matrix-runtime-state app) :account)))
                    (matrix-runtime-accounts)))
      (matrix-restore account)
    (matrix-login account)))

;;;###autoload
(defun matrix-login (&optional account)
  "Log in a new local ACCOUNT with a password prompted without echo.
To retain an existing device use `matrix-restore', never a second login."
  (interactive)
  (setq account (or account (matrix--read-profile)))
  (when (or (matrix-runtime-profile-exists-p account)
            (seq-find (lambda (app)
                        (equal account (plist-get (matrix-runtime-state app) :account)))
                      (matrix-runtime-accounts)))
    (user-error "Profile already exists; use matrix-restore to keep its device identity"))
  (let* ((user (read-string "Matrix user ID (@user:server): "))
         (homeserver (read-string "Homeserver URL (empty for discovery): "))
         (password (read-passwd "Matrix password: "))
         (login (list :user_id user :password password
                      :device_name "Emacs Matrix")))
    (unless (string-empty-p homeserver)
      (setq login (plist-put login :homeserver homeserver)))
    (require 'matrix-root)
    (matrix-root-open (matrix-runtime-connect account login) t)))

;;;###autoload
(defun matrix-restore (&optional account)
  "Restore ACCOUNT's saved device and encryption state without logging in."
  (interactive)
  (require 'matrix-root)
  (matrix-root-open
   (matrix-runtime-connect (or account (matrix--read-profile t))) t))

;;;###autoload
(defun matrix-disconnect (&optional app)
  "Stop APP locally, keeping its saved session, encryption keys and device.
This does not remotely log out.  Closing a room is not a disconnect."
  (interactive)
  (matrix-runtime-disconnect (or app (matrix--current-app))))

;;;###autoload
(defun matrix-accounts ()
  "Open the account directory for a live Matrix account."
  (interactive)
  (require 'matrix-root)
  (matrix-root-open (matrix--current-app) t))

;;;###autoload
(defun matrix-devices ()
  "Show this account's devices and interactive SAS verification."
  (interactive)
  (require 'matrix-verify)
  (matrix-verify-open (matrix--current-app) t))

(provide 'matrix)
;;; matrix.el ends here
