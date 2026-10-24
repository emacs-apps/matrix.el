;;; matrix-verify.el --- Matrix device trust and encrypted key recovery -*- lexical-binding: t; -*-

;;; Commentary:
;; SAS is displayed as native text and is never confirmed automatically.
;; Secret prompts feed the transport only; no passphrase or recovery key is
;; retained in a Surface model, rendered row, or completion context.

;;; Code:

(require 'matrix)
(require 'appkit-directory)

(defvar-local matrix-verify--sas-shown nil
  "Render-owned SAS fingerprints actually presented by this exact host.")

(defvar matrix-verify-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map appkit-directory-mode-map)
    (define-key map (kbd "v") #'matrix-verification-request)
    map)
  "Keymap for the device and SAS directory.")

(appkit-bindings-define 'matrix-verify-mode-map 'read-only
  'app-menu #'matrix-verify-transient
  'refresh #'matrix-verify-refresh)

(define-derived-mode matrix-verify-mode appkit-directory-mode "Matrix-Verify"
  "Matrix devices and explicit short authentication string verification."
  (setq-local matrix-verify--sas-shown (make-hash-table :test #'equal)))

(defun matrix-verify--sas (flow)
  "Return an owned, valid SAS fingerprint from FLOW, or nil."
  (let ((decimals (append (plist-get flow :decimals) nil))
        (emojis (append (plist-get flow :emojis) nil)))
    (when (or (and (= (length decimals) 3) (seq-every-p #'integerp decimals))
              (and (= (length emojis) 7) (seq-every-p #'stringp emojis)))
      (list :decimals (and (= (length decimals) 3)
                          (seq-every-p #'integerp decimals) decimals)
            :emojis (and (= (length emojis) 7)
                        (seq-every-p #'stringp emojis) emojis)))))

(defun matrix-verify--sas-text (sas)
  "Format SAS for explicit comparison with the other device."
  (string-join
   (delq nil
         (list
          (when-let* ((decimals (plist-get sas :decimals)))
            (concat "Numbers: " (mapconcat #'number-to-string decimals "  ")))
          (when-let* ((emojis (plist-get sas :emojis)))
            (concat "Symbols: " (mapconcat #'matrix--one-line emojis "  ")))))
   "   |   "))

(defun matrix-verify--projection (state model)
  "Return (ENTRIES . SHOWN-SAS) from pass-scoped STATE and Surface MODEL."
  (let* ((session (plist-get state :session))
         (user-id (plist-get session :user_id))
         (shown (make-hash-table :test #'equal))
         (flows (plist-get state :verifications))
         (entries
          (list
           (appkit-directory-entry-create
            :key '(account) :role 'section :label "Device trust and key recovery" :face 'bold)
           (matrix--directory-note '(identity)
                              (format "%s / %s — %s"
                                      (or user-id "Not authenticated")
                                      (or (plist-get session :device_id) "—")
                                      (plist-get state :connection)))
           (matrix--directory-note '(security)
                              "Compare SAS on both devices. Confirm only an exact match; otherwise cancel."
                              'warning))))
    (when-let* ((error (plist-get state :last-error)))
      (setq entries (append entries (list (matrix--directory-note '(error) error 'error)))))
    (when-let* ((notice (plist-get model :notice)))
      (setq entries (append entries (list (matrix--directory-note '(operation) notice)))))
    (setq entries
          (append entries
                  (list (appkit-directory-entry-create
                         :key '(devices) :role 'section :label "Your devices" :face 'bold))))
    (unless (plist-get model :devices-loaded)
      (setq entries (append entries (list (matrix--directory-note '(devices-loading)
                                                             "Devices not yet loaded; press g." nil '(devices))))))
    (dolist (device (plist-get model :devices))
      (let* ((id (plist-get device :device_id))
             (self (eq (plist-get device :is_self) t))
             (label (format "%s  %s  [%s]%s"
                            id (or (plist-get device :display_name) "")
                            (if (eq (plist-get device :verified) t) "verified" "unverified")
                            (if self "  (this device)" " — request SAS verification"))))
        (setq entries
              (append entries
                      (list (if self
                                (matrix--directory-note (list 'device id) label nil '(devices))
                              (matrix--directory-action (list 'device id) label
                                                   (list 'request user-id id) '(devices))))))))
    (setq entries
          (append entries
                  (list (appkit-directory-entry-create
                         :key '(flows) :role 'section :label "Verification flows" :face 'bold))))
    (when (hash-table-p flows)
      (let (ordered)
        (maphash (lambda (_key flow) (push flow ordered)) flows)
        (dolist (flow (sort ordered
                            (lambda (a b)
                              (string-lessp
                               (concat (plist-get a :user_id) "/" (plist-get a :flow_id))
                               (concat (plist-get b :user_id) "/" (plist-get b :flow_id))))))
          (let* ((user (plist-get flow :user_id))
                 (id (plist-get flow :flow_id))
                 (status (plist-get flow :state))
                 (key (cons user id))
                 (sas (and (equal status "sas") (matrix-verify--sas flow))))
            (setq entries
                  (append entries
                          (list (matrix--directory-note
                                 (list 'flow user id)
                                 (format "%s / %s   Device: %s   [%s]"
                                         user id (or (plist-get flow :device_id) "unknown") status)
                                 nil '(flows)))))
            (when-let* ((message (plist-get flow :message)))
              (setq entries (append entries
                                    (list (matrix--directory-note (list 'flow-message user id) message nil '(flows))))))
            (when (equal status "requested")
              (setq entries
                    (append entries
                            (list (matrix--directory-action
                                   (list 'accept user id) "  Accept SAS verification request"
                                   (list 'accept user id) '(flows))))))
            (when sas
              (puthash key sas shown)
              (setq entries
                    (append entries
                            (list
                             (matrix--directory-note (list 'sas user id) (matrix-verify--sas-text sas) 'bold '(flows))
                             (matrix--directory-action
                              (list 'confirm user id) "  Compare both devices and confirm exact match"
                              (list 'confirm user id) '(flows))))))
            (when (and (equal status "sas") (not sas))
              (setq entries
                    (append entries
                            (list (matrix--directory-note
                                   (list 'invalid-sas user id)
                                   "No complete SAS available — confirmation is disabled." 'error '(flows))))))
            (unless (member status '("done" "cancelled"))
              (setq entries
                    (append entries
                            (list (matrix--directory-action
                                   (list 'cancel user id) "  Cancel / SAS does not match"
                                   (list 'cancel user id) '(flows))))))))))
    (cons entries shown)))

(defun matrix-verify--render (_surface state model _request)
  "Present STATE and MODEL, recording only SAS successfully rendered here."
  (matrix--frame state "Devices and trust")
  (let ((projection (matrix-verify--projection state model)))
    (appkit-directory-reconcile (appkit-directory-surface) (car projection)
                                :preserve-position-p t)
    (setq matrix-verify--sas-shown (cdr projection))))

(defun matrix-verify--flow (surface user flow-id)
  "Read current verification FLOW-ID for USER on SURFACE's account."
  (let ((flows (plist-get (matrix-runtime-state (appkit-surface-app surface))
                          :verifications)))
    (and (hash-table-p flows) (gethash (cons user flow-id) flows))))

(defun matrix-verify--confirm (user flow-id)
  "Confirm USER's FLOW-ID only after displaying and explicitly comparing SAS."
  (let* ((surface (matrix--current-surface 'verify))
         (flow (matrix-verify--flow surface user flow-id))
         (sas (matrix-verify--sas flow))
         (shown (gethash (cons user flow-id) matrix-verify--sas-shown)))
    (unless (and (equal (plist-get flow :state) "sas") sas (equal sas shown))
      (user-error "SAS is not currently displayed for this flow; wait for an update"))
    (when (yes-or-no-p
           (format "Compared %s on %s / %s with the other device, and EVERYTHING matches? "
                   (matrix-verify--sas-text shown) user
                   (or (plist-get flow :device_id) "unknown device")))
      ;; Minibuffer input permits process delivery; compare again afterward.
      (let ((current (matrix-verify--flow surface user flow-id)))
        (unless (and (appkit-surface-live-p surface)
                     (equal (plist-get current :state) "sas")
                     (equal shown (matrix-verify--sas current)))
          (user-error "Verification changed during comparison; compare the current SAS again")))
      (matrix--request surface "verification.confirm"
                       (list :user_id user :flow_id flow-id)))))

(defun matrix-verify--activate (_directory entry)
  "Activate device or verification ENTRY through an explicit user action."
  (pcase (appkit-directory-entry-payload entry)
    (`(command ,command) (call-interactively command))
    (`(request ,user ,device)
     (matrix-verification-request user device))
    (`(confirm ,user ,id) (matrix-verify--confirm user id))
    (`(,action ,user ,id)
     (when (memq action '(accept cancel))
       (matrix--request (matrix--current-surface 'verify)
                        (if (eq action 'accept) "verification.accept" "verification.cancel")
                        (list :user_id user :flow_id id))))))

(defun matrix-verify--mount (_surface _app-view _model)
  "Install native verification directory actions."
  (appkit-directory-configure (appkit-directory-surface)
                              :activate-function #'matrix-verify--activate
                              :action-rows-p t))

(defun matrix-verify--result (model method result error _context)
  "Commit non-secret METHOD RESULT or ERROR to presentation MODEL."
  (if error model
    (pcase method
      ("devices.list"
       (plist-put (plist-put model :devices (append (plist-get result :devices) nil))
                  :devices-loaded t))
      ("verification.request"
       (plist-put model :notice
                  (format "Verification requested: %s — accept on the other device."
                          (plist-get result :flow_id))))
      ("verification.confirm"
       (plist-put model :notice "Local comparison confirmed; waiting for the other device and SDK completion."))
      ("keys.export"
       (plist-put model :notice
                  (format "Encrypted key export completed%s. Keep its passphrase separate."
                          (if (numberp (plist-get result :exported))
                              (format " (%d keys)" (plist-get result :exported)) ""))))
      ("keys.import"
       (plist-put model :notice
                  (format "Imported %s of %s room keys; SDK will retry decryption."
                          (plist-get result :imported) (plist-get result :total))))
      ("keys.recover"
       (plist-put model :notice "Key recovery completed without resetting cross-signing or backup."))
      (_ model))))

(defvar matrix-verify--type
  (matrix--surface-type 'matrix-verify #'matrix-verify-mode
                        #'matrix-verify--render '(verification) #'matrix-verify--mount)
  "Generated Matrix device and verification Surface type.")

(defun matrix-verify-open (app &optional select no-refresh)
  "Open APP's trust directory, optionally SELECT it.
NO-REFRESH leaves the initial operation slot free for a user key/SAS command."
  (let* ((existing (appkit-app-surface app '(verify)))
         (surface (matrix--open
                   app matrix-verify--type '(verify)
                   (format "*Matrix:%s:devices*"
                           (plist-get (matrix-runtime-state app) :account))
                   select)))
    (unless (or existing no-refresh) (matrix--request surface "devices.list" nil))
    surface))

(defun matrix-verify-refresh ()
  "Refresh device verification state without confirming any verification."
  (interactive)
  (matrix--request (matrix--current-surface 'verify) "devices.list" nil))

;;;###autoload
(defun matrix-verification-request (&optional user-id device-id)
  "Request SAS verification of USER-ID's DEVICE-ID; never confirm implicitly."
  (interactive)
  (let* ((surface (matrix-verify-open (matrix--current-app) t t))
         (session (plist-get (matrix-runtime-state (appkit-surface-app surface)) :session))
         (user (or user-id (read-string "Verify Matrix user: " (plist-get session :user_id))))
         (device (or device-id (read-string "Verify device ID: "))))
    (when (or (string-empty-p user) (string-empty-p device))
      (user-error "A user ID and device ID are required"))
    (matrix--request surface "verification.request" (list :user_id user :device_id device))))

(defun matrix-verify--secret-surface ()
  "Open the current account's trust directory for a key operation."
  (let ((surface (matrix-verify-open (matrix--current-app) t t)))
    (when (plist-get (appkit-surface-model surface) :operation)
      (user-error "Wait for the current device operation before entering a secret"))
    surface))

;;;###autoload
(defun matrix-keys-export ()
  "Export room keys to an encrypted file using a separately prompted passphrase."
  (interactive)
  (let* ((surface (matrix-verify--secret-surface))
         (path (expand-file-name (read-file-name "Encrypted key export file: " nil nil nil)))
         (passphrase nil))
    (when (file-remote-p path) (user-error "The Matrix backend requires a local export path"))
    (when (or (file-exists-p path) (file-symlink-p path))
      (user-error "Choose a new export path; existing files are never overwritten"))
    (setq passphrase (read-passwd "Key export passphrase: " t))
    (when (string-empty-p passphrase) (user-error "An encryption passphrase is required"))
    (matrix--request surface "keys.export" (list :path path :passphrase passphrase))))

;;;###autoload
(defun matrix-keys-import ()
  "Import encrypted room keys, prompting for their passphrase without echo."
  (interactive)
  (let* ((surface (matrix-verify--secret-surface))
         (path (expand-file-name (read-file-name "Encrypted key file to import: " nil nil t))))
    (when (file-remote-p path) (user-error "The Matrix backend requires a local import path"))
    (matrix--request surface "keys.import"
                     (list :path path :passphrase (read-passwd "Key import passphrase: ")))))

;;;###autoload
(defun matrix-keys-recover ()
  "Recover encryption keys with a secret recovery key, without resetting trust."
  (interactive)
  (let ((surface (matrix-verify--secret-surface)))
    (matrix--request surface "keys.recover"
                     (list :recovery_key (read-passwd "Matrix recovery key: ")))))

(provide 'matrix-verify)
;;; matrix-verify.el ends here
