;;; matrix.el --- Persistent Matrix accounts in AppKit -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-app)
(require 'appkit-surface)
(require 'appkit-projection)
(require 'appkit-directory)
(require 'matrix-runtime)

(declare-function matrix-root-open "matrix-root" (app &optional select))
(declare-function matrix-root--result "matrix-root" (model method result error context))
(declare-function matrix-room--result "matrix-room" (model method result error context))
(declare-function matrix-verify-open "matrix-verify" (app &optional select no-refresh))
(declare-function matrix-verify--result "matrix-verify" (model method result error context))
(declare-function matrix-room--sync-history "matrix-room" (model state))
(declare-function matrix-room--history-complete "matrix-room" (model state result error context))
(declare-function matrix-room--observe-command "matrix-room" ())

(autoload 'matrix-join-room "matrix-root" nil t)
(autoload 'matrix-create-room "matrix-root" nil t)
(autoload 'matrix-verification-request "matrix-verify" nil t)
(autoload 'matrix-keys-export "matrix-verify" nil t)
(autoload 'matrix-keys-import "matrix-verify" nil t)
(autoload 'matrix-keys-recover "matrix-verify" nil t)
(autoload 'matrix-switch-room "matrix-root" nil t)
(autoload 'matrix-root-transient "matrix-transient" nil t)
(autoload 'matrix-account-transient "matrix-transient" nil t)
(autoload 'matrix-room-transient "matrix-transient" nil t)
(autoload 'matrix-message-transient "matrix-transient" nil t)
(autoload 'matrix-composer-transient "matrix-transient" nil t)
(autoload 'matrix-verify-transient "matrix-transient" nil t)

(defvar matrix-navigation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'matrix-accounts)
    (define-key map (kbd "s") #'matrix-switch-room)
    (define-key map (kbd "v") #'matrix-devices)
    map)
  "Common native navigation prefix for Matrix destinations.")

(defvar matrix--request-token 0
  "Monotonic UI operation identity, independent of transport request IDs.")
(defvar-local matrix--registered-surface nil
  "Exact Surface registered by this host, retained through AppKit detachment.")

(defun matrix--one-line (text)
  "Return plain TEXT on one physical directory row."
  (replace-regexp-in-string "[\n\r\t]+" " "
                            (substring-no-properties (or text ""))))

(defvar-local matrix--header-line nil
  "Presentation-only header from the last committed render pass.")

(defun matrix--frame (state title &optional detail badge)
  "Present STATE and TITLE, with optional DETAIL and leading safety BADGE."
  (setq-local
   matrix--header-line
   (concat " Matrix" (when badge (concat " " badge))
           " · " (matrix--one-line (plist-get state :account))
           " · " (matrix--one-line title)
           (when (and detail (not (string-empty-p detail)))
             (concat "  " (matrix--one-line detail)))))
  (setq-local header-line-format '(:eval matrix--header-line))
  (setq-local mode-line-process
              (format " [%s]" (plist-get state :connection)))
  (force-mode-line-update))

(defun matrix--width (surface)
  "Return SURFACE's canonical usable presentation width."
  (or (appkit-surface-responsive-width surface 1) (max 1 fill-column)))

(defun matrix--geometry (surface _width)
  "Request a coalesced geometry pass for exact SURFACE."
  (appkit-surface-post surface '(matrix-geometry)))

(defun matrix--directory-note (key label &optional face section)
  "Make a stable KEY note with LABEL, FACE and optional owning SECTION."
  (appkit-directory-entry-create
   :key key :role 'note :section-key (or section '(account))
   :label (matrix--one-line label) :face face :stamp (list label face)))

(defun matrix--directory-action (key label payload &optional section)
  "Make a KEY action with LABEL and PAYLOAD in SECTION."
  (appkit-directory-entry-create
   :key key :role 'item :section-key (or section '(account))
   :label (matrix--one-line label) :item-p t
   :payload payload :stamp (list label payload)
   :help-echo "Open this item"))

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
  (appkit-surface-enable-responsive-geometry surface #'matrix--geometry)
  surface)

(defun matrix--open (app type identity name &optional select room-id)
  "Open or select exact APP/IDENTITY using TYPE and NAME.
Register ROOM-ID's timeline when provided; retain the reading position."
  (let ((existing (appkit-app-surface app identity)))
    (when (and existing (not (appkit-surface-live-p existing)))
      (user-error "Matrix view is unavailable; close its buffer before reopening"))
    (let ((surface
           (or existing
               (matrix--register
                app (appkit-open-generated-surface
                     type :app app :identity identity :input identity :buffer-name name)
                room-id))))
      (when select
        (pop-to-buffer (appkit-surface-buffer surface))
        (appkit-surface-refresh-responsive-geometry surface))
      surface)))

(defun matrix--surface-init (context input)
  "Capture exact generation address and presentation identity INPUT."
  (let ((model (list :identity input
                     :address (appkit-transition-context-owner-address context))))
    (when (eq (car input) 'room)
      (setq model
            (matrix-room--sync-history
             model (appkit-app-read-view-model
                    (appkit-transition-context-app-read-view context)))))
    (appkit-next :model model
                 :render (appkit-projection-change-create :full-p t))))

(defun matrix--matching-result-p (model method context)
  "Whether METHOD's result CONTEXT owns MODEL's outstanding operation."
  (let ((operation (plist-get model :operation)))
    (and operation
         (equal (plist-get model :address) (plist-get context :target))
         (equal method (plist-get operation :method))
         (equal (plist-get operation :token) (plist-get context :token)))))

(defun matrix--surface-update (context model message)
  "Reduce directed UI MESSAGE without issuing protocol work in a reducer."
  (let* ((room-p (eq (car (plist-get model :identity)) 'room))
         (state (appkit-app-read-view-model
                 (appkit-transition-context-app-read-view context))))
    (pcase message
      (`(matrix-update ,kind ,room-id)
       (if (and room-p room-id
                (not (equal room-id (cadr (plist-get model :identity)))))
           (appkit-next :model model :render appkit-render-none)
         (appkit-next
          :model (if room-p (matrix-room--sync-history model state) model)
          :render (appkit-projection-change-create :full-p t :frame-p t)
          :commands (when (and room-p (eq kind 'timeline))
                      (list (matrix-room--observe-command))))))
      ('(matrix-geometry)
       (appkit-next :model model
                    :render (appkit-projection-change-create :geometry-p t :frame-p t)))
      (`(matrix-filter ,filter)
       (appkit-next :model (plist-put (copy-sequence model) :filter filter)
                    :render (appkit-projection-change-create :full-p t :frame-p t)))
      ('(matrix-history-started)
       (appkit-next :model (plist-put (copy-sequence model) :history-error nil)
                    :render (appkit-projection-change-create :frame-p t)))
      ('(matrix-history-observer-failed)
       (appkit-next
        :model (plist-put (copy-sequence model) :history-error
                          "Automatic history observation failed; load older manually.")
        :render (appkit-projection-change-create :frame-p t)))
      ('(matrix-send-reviewed)
       (appkit-next
        :model (plist-put (plist-put (copy-sequence model) :uncertain-send nil)
                          :notice "Previous submission reviewed; no message was resent.")
        :render (appkit-projection-change-create :frame-p t)))
      (`(matrix-operation ,operation)
       (appkit-next
        :model (plist-put (plist-put (copy-sequence model) :operation operation)
                          :notice nil)
        :render (appkit-projection-change-create :frame-p t)))
      (`(matrix-result ,_id "timeline.paginate" ,result ,error ,owned)
       (if-let* ((next (and room-p
                           (matrix-room--history-complete model state result error owned))))
           (appkit-next :model next
                        :render (appkit-projection-change-create :frame-p t)
                        :commands (list (matrix-room--observe-command)))
         (appkit-next :model model :render appkit-render-none)))
      (`(matrix-result ,_id ,method ,result ,error ,owned)
       (if (not (matrix--matching-result-p model method owned))
           (appkit-next :model model :render appkit-render-none)
         (let ((next (plist-put (copy-sequence model) :operation nil)))
           (setq next (plist-put next :notice
                                 (if error (matrix--error-text error)
                                   (format "%s completed" method))))
           (setq next
                 (pcase (car (plist-get model :identity))
                   ('room (matrix-room--result next method result error owned))
                   ('root (matrix-root--result next method result error owned))
                   ('verify (matrix-verify--result next method result error owned))
                   (_ next)))
           (appkit-next :model next
                        :render (appkit-projection-change-create :full-p t :frame-p t)))))
      (_ (appkit-next :model model :render appkit-render-none)))))

(defun matrix--surface-type (name mode render &optional mount)
  "Create a Generated Surface NAME using MODE, RENDER and optional MOUNT.
RENDER receives Surface, account read view's model, Surface model and change."
  (appkit-surface-type-create
   :name name :mode mode :init #'matrix--surface-init
   :update #'matrix--surface-update
   :renderer-factory
   (lambda (_surface)
     (appkit-generated-renderer-create
      :mount (or mount #'ignore)
      :unmount #'ignore
      :merge #'appkit-projection-change-merge
      :render (lambda (surface app-view model request)
                (funcall render surface (appkit-app-read-view-model app-view) model request)
                nil)))))

(defun matrix--request (surface method params &optional context)
  "Issue METHOD with PARAMS for SURFACE using an owned non-secret CONTEXT.
Only the exact Surface generation and operation token may accept the result."
  (unless (and (appkit-surface-live-p surface)
               (eq surface (appkit-app-surface
                            (appkit-surface-app surface) (appkit-surface-identity surface))))
    (user-error "The Matrix view that initiated this request is no longer live"))
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

(require 'matrix-evil)

(provide 'matrix)
;;; matrix.el ends here
