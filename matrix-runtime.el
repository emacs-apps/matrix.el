;;; matrix-runtime.el --- Matrix account ownership -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Matrix contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; One App owns a private backend and committed presentation mirror.  Room
;; Surfaces subscribe through exact runtime addresses, never transport handles.
;; Sending belongs to the account even after the initiating room is closed.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-app)
(require 'appkit-surface)
(require 'appkit-command)
(require 'appkit-context)
(require 'matrix-rpc)
(require 'matrix-state)

(declare-function matrix-avatar-clear-cache "matrix-avatar" (app))
(declare-function matrix-compose-complete-command "matrix-compose" (context &optional id))
(declare-function matrix-compose-release-capture "matrix-compose" (context))

(cl-defstruct (matrix-runtime-resource-call
               (:constructor matrix-runtime-resource-call-create))
  "A cancellable presentation read, never authority for a durable operation."
  app resolve reject active timer)

(defcustom matrix-data-directory (expand-file-name "matrix/" user-emacs-directory)
  "Private directory containing local Matrix account profiles.
Sessions and SDK databases contain credentials and decrypted data.  This
client requires owner-only permissions; use encrypted storage for protection
against disk theft.  Account passwords are never saved."
  :type 'directory
  :group 'matrix)

(defvar matrix-runtime--accounts (make-hash-table :test #'equal)
  "Exact private store path to live account App.")

(defvar matrix-runtime--retiring (make-hash-table :test #'equal)
  "Store paths whose previous backend may still be flushing its databases.")

(defun matrix-runtime--profile-directory (account)
  "Return the private profile path for local ACCOUNT identity."
  (unless (and (stringp account) (<= (length account) 128)
               (string-match-p "\\`[[:alnum:]][[:alnum:]_.-]*\\'" account))
    (user-error "Use a local profile name containing letters, numbers, dots, _ or -"))
  (expand-file-name (concat account "/") matrix-data-directory))

(defun matrix-runtime-profile-exists-p (account)
  "Whether ACCOUNT has saved or uncertain device authority.
Corrupt and incomplete profiles still require restore, not an implicit login."
  (let ((directory (matrix-runtime--profile-directory account)))
    (cl-some (lambda (name)
               (let ((path (expand-file-name name directory)))
                 (or (file-exists-p path) (file-symlink-p path))))
             '("session.json" "account.initialized" "login.pending"))))

(defun matrix-runtime-profiles ()
  "Return sorted local profile names having device authority."
  (when (file-directory-p matrix-data-directory)
    (cl-loop for name in (directory-files matrix-data-directory nil
                                          "\\`[[:alnum:]][[:alnum:]_.-]*\\'")
             when (matrix-runtime-profile-exists-p name) collect name)))

(defun matrix-runtime--private-directory (directory)
  "Create DIRECTORY privately or reject an unsafe existing directory."
  (when (file-symlink-p (directory-file-name directory))
    (user-error "Matrix profile directories must not be symbolic links"))
  (unless (file-exists-p directory)
    (let ((mode (default-file-modes)))
      (unwind-protect
          (progn (set-default-file-modes #o700) (make-directory directory t))
        (set-default-file-modes mode)))
    (set-file-modes directory #o700))
  (let ((attributes (file-attributes directory 'integer)))
    (unless (and (file-directory-p directory)
                 (equal (file-attribute-user-id attributes) (user-uid))
                 (zerop (logand (file-modes directory) #o077)))
      (user-error "Matrix profile directory must be owned by you with mode 0700"))))

(defun matrix-runtime-state (app)
  "Return the committed presentation model of APP."
  (appkit-app-model app))

(defun matrix-runtime-accounts ()
  "Return live Matrix Apps sorted by local profile name."
  (let (accounts)
    (maphash (lambda (_store app)
               (when (appkit-app-live-p app) (push app accounts)))
             matrix-runtime--accounts)
    (sort accounts (lambda (left right)
                     (string< (plist-get (matrix-runtime-state left) :account)
                              (plist-get (matrix-runtime-state right) :account))))))

(defun matrix-runtime-room (app room-id)
  "Return APP's room summary for ROOM-ID."
  (seq-find (lambda (room) (equal room-id (plist-get room :room_id)))
            (plist-get (matrix-runtime-state app) :rooms)))

(defun matrix-runtime-room-items (app room-id)
  "Return the committed ordered presentation rows for APP's ROOM-ID."
  (plist-get (gethash room-id (plist-get (matrix-runtime-state app) :timelines))
             :items))

(defun matrix-runtime--enqueue (model method params &optional context call)
  "Admit one METHOD/PARAMS request into MODEL and return its closed command.
CONTEXT is local correlation only.  CALL may supply the external return handle."
  (let* ((id (1+ (plist-get model :next-id)))
         (call (or call (matrix-rpc-call-create))))
    (setf (plist-get model :next-id) id
          (matrix-rpc-call-id call) id
          (matrix-rpc-call-method call) method
          (matrix-rpc-call-params call) params)
    (puthash id (list :method method :context context)
             (plist-get model :pending))
    (appkit-command-source-intent
     :key 'backend :expected-identity (plist-get model :transport)
     :payload call :result-mapper #'matrix-rpc-intent-result)))

(defun matrix-runtime--room-wanted-p (model room-id)
  "Whether a live exact Surface in MODEL needs ROOM-ID."
  (let (wanted)
    (maphash (lambda (surface watch)
               (when (and (appkit-surface-live-p surface)
                          (equal room-id (plist-get watch :room-id)))
                 (setq wanted t)))
             (plist-get model :watches))
    wanted))

(defun matrix-runtime--subscribe (model room-id)
  "Install a new snapshot fence for ROOM-ID and return its subscription command."
  (let* ((timelines (plist-get model :timelines))
         (old (gethash room-id timelines))
         (subscription (format "timeline-%d" (1+ (plist-get model :next-id)))))
    (puthash room-id
             (list :subscription_id subscription :revision nil :loading t
                   :items (plist-get old :items) :error nil)
             timelines)
    (matrix-runtime--enqueue
     model "timeline.subscribe"
     (list :room_id room-id :subscription_id subscription)
     (list :room-id room-id :subscription-id subscription))))

(defun matrix-runtime--release-room (model room-id)
  "Release unobserved ROOM-ID in MODEL, returning an optional command."
  (when (and room-id (not (matrix-runtime--room-wanted-p model room-id)))
    (when-let* ((timeline (gethash room-id (plist-get model :timelines))))
      (remhash room-id (plist-get model :timelines))
      (when (and (plist-get model :session) (plist-get model :backend-active))
        (matrix-runtime--enqueue
         model "timeline.unsubscribe"
         (list :room_id room-id
               :subscription_id (plist-get timeline :subscription_id)))))))

(defun matrix-runtime--prune-watches (model)
  "Retire dead exact Surface registrations in MODEL; return release commands."
  (let (dead rooms commands)
    (maphash (lambda (surface watch)
               (unless (appkit-surface-live-p surface)
                 (push surface dead)
                 (push (plist-get watch :room-id) rooms)))
             (plist-get model :watches))
    (dolist (surface dead) (remhash surface (plist-get model :watches)))
    (dolist (room (delete-dups rooms))
      (when-let* ((command (matrix-runtime--release-room model room)))
        (push command commands)))
    commands))

(defun matrix-runtime--broadcast (model kind &optional room-id)
  "Return exact directed notifications for committed KIND in MODEL."
  (let (commands)
    (maphash
     (lambda (_surface watch)
       (when (or (null room-id) (null (plist-get watch :room-id))
                 (equal room-id (plist-get watch :room-id)))
         (push (appkit-command-post-message
                :target (plist-get watch :address)
                :message (list 'matrix-update kind room-id)
                :delivery 'report)
               commands)))
     (plist-get model :watches))
    commands))

(defun matrix-runtime--finish-resource (call result error)
  "Settle CALL's live Resource gate after response commit."
  (when (and (matrix-runtime-resource-call-active call)
             (appkit-app-live-p (matrix-runtime-resource-call-app call)))
    (let ((callback (if error
                        (matrix-runtime-resource-call-reject call)
                      (matrix-runtime-resource-call-resolve call))))
      (setf (matrix-runtime-resource-call-active call) nil
            (matrix-runtime-resource-call-resolve call) nil
            (matrix-runtime-resource-call-reject call) nil)
      (funcall callback (or error result)))))

(defun matrix-runtime--settle-resource (_context input _observe resolve _reject)
  "Deliver a committed presentation response INPUT outside the reducer."
  (pcase-let ((`(,call ,result ,error) input))
    (matrix-runtime--finish-resource call result error))
  (funcall resolve nil)
  nil)

(defun matrix-runtime--complete (model id result error &optional terminal)
  "Commit RESULT/ERROR for request ID in MODEL and return follow-on commands.
TERMINAL suppresses per-request refreshes while closing the whole account."
  (when-let* ((operation (gethash id (plist-get model :pending))))
    (remhash id (plist-get model :pending))
    (let* ((method (plist-get operation :method))
           (context (plist-get operation :context))
           commands)
      (when (and error (not terminal) (not (plist-get context :resource)))
        (setf (plist-get model :last-error)
              (or (plist-get error :message) "Matrix operation failed")))
      (pcase (unless terminal method)
        ((or "session.login" "session.restore")
         (if error
             (setf (plist-get model :connection) 'error)
           (unless (and (stringp (plist-get result :user_id))
                        (stringp (plist-get result :device_id))
                        (stringp (plist-get result :homeserver)))
             (signal 'matrix-protocol-error '("Invalid restored session identity")))
           (setf (plist-get model :session) result
                 (plist-get model :connection) 'syncing
                 (plist-get model :last-error) nil)
           (push (matrix-runtime--enqueue model "rooms.list" nil) commands)
           (maphash
            (lambda (_surface watch)
              (when-let* ((room (plist-get watch :room-id))
                          ((not (gethash room (plist-get model :timelines)))))
                (push (matrix-runtime--subscribe model room) commands)))
            (plist-get model :watches)))
         (setq commands (nconc commands (matrix-runtime--broadcast model 'session))))
        ("rooms.list"
         (unless error
           (setf (plist-get model :rooms)
                 (matrix-state-rooms (plist-get result :rooms))))
         (setq commands (nconc commands (matrix-runtime--broadcast model 'rooms))))
        ((or "room.create" "room.join" "room.leave")
         (unless error
           (push (matrix-runtime--enqueue model "rooms.list" nil) commands)))
        ("timeline.subscribe"
         (when error
           (let* ((room (plist-get context :room-id))
                  (timeline (gethash room (plist-get model :timelines))))
             (when (equal (plist-get timeline :subscription_id)
                          (plist-get context :subscription-id))
               (puthash room (plist-put (plist-put (copy-sequence timeline)
                                                  :loading nil)
                                        :error (plist-get error :message))
                        (plist-get model :timelines))
               (setq commands
                     (nconc commands (matrix-runtime--broadcast model 'timeline room)))))))
        ("session.logout"
         (unless error
           (setf (plist-get model :session) nil
                 (plist-get model :backend-active) nil
                 (plist-get model :connection) 'stopped)
           (clrhash (plist-get model :timelines))
           (setf (plist-get model :rooms) nil)
           (setq commands (nconc commands (matrix-runtime--broadcast model 'session))))))
      (when-let* ((resource (plist-get context :resource)))
        (push
         (appkit-command-start-effect
          (appkit-effect-create
           :key (list 'matrix-resource-result id) :input (list resource result error)
           :start #'matrix-runtime--settle-resource
           :success (lambda (_input _value) '(matrix-resource-delivered))
           :failure (lambda (_input _error) '(matrix-resource-delivered))))
         commands))
      ;; Source ownership is account-owned, not conditional on result delivery
      ;; to a still-live room.  Actual cleanup runs only after this commit.
      (when (fboundp 'matrix-compose-complete-command)
        (when-let* ((command (matrix-compose-complete-command context id)))
          (push command commands)))
      (when-let* ((target (plist-get context :target)))
        (push (appkit-command-post-message
               :target target
               :message (list 'matrix-result id method result error context)
               :delivery 'report)
              commands))
      (when (and error (not terminal) (not (plist-get context :resource)))
        (setq commands (nconc commands (matrix-runtime--broadcast model 'error))))
      commands)))

(defun matrix-runtime--event (model event)
  "Commit one normalized backend EVENT to MODEL; return closed commands."
  (pcase (plist-get event :type)
    ("status"
     (let ((state (pcase (plist-get event :state)
                    ("connecting" 'connecting) ("syncing" 'syncing)
                    ("online" 'online) ("offline" 'offline)
                    ("error" 'error)
                    (_ (signal 'matrix-protocol-error '("Unknown connection state"))))))
       (setf (plist-get model :connection) state)
       (when-let* ((message (plist-get event :message)))
         (setf (plist-get model :last-error) message))
       (when (eq state 'online) (setf (plist-get model :last-error) nil))
       (matrix-runtime--broadcast model 'status)))
    ("session"
     ;; Successful auth responses own admission of subscriptions; this event
     ;; refreshes identity without starting work before the request settles.
     (setf (plist-get model :session)
           (list :user_id (plist-get event :user_id)
                 :device_id (plist-get event :device_id)
                 :homeserver (plist-get event :homeserver)))
     (matrix-runtime--broadcast model 'session))
    ("rooms"
     (setf (plist-get model :rooms)
           (matrix-state-rooms (plist-get event :rooms)))
     (matrix-runtime--broadcast model 'rooms))
    ((or "timeline_reset" "timeline_diff")
     (let* ((room (plist-get event :room_id))
            (timeline (gethash room (plist-get model :timelines))))
       (when timeline
         (condition-case condition
             (let ((updated (matrix-state-timeline-update timeline event)))
               (unless (eq updated 'stale)
                 (puthash room updated (plist-get model :timelines))
                 (matrix-runtime--broadcast model 'timeline room)))
           (matrix-timeline-gap
            (cons (matrix-runtime--subscribe model room)
                  (matrix-runtime--broadcast model 'timeline room)))
           (matrix-protocol-error
            ;; An invalid batch never partly changes visible rows.  Stop this
            ;; subscription rather than retrying a malformed snapshot forever.
            (puthash room (list :subscription_id nil :revision nil :loading nil
                                :items (plist-get timeline :items)
                                :error (error-message-string condition))
                     (plist-get model :timelines))
            (cons (matrix-runtime--enqueue
                   model "timeline.unsubscribe"
                   (list :room_id room :subscription_id
                         (plist-get timeline :subscription_id)))
                  (matrix-runtime--broadcast model 'timeline room)))))))
    ("verification"
     (puthash (cons (plist-get event :user_id) (plist-get event :flow_id))
              event (plist-get model :verifications))
     (matrix-runtime--broadcast model 'verification))
    (_ (signal 'matrix-protocol-error '("Unknown backend notification")))))

(defun matrix-runtime--closed (model reason)
  "Close MODEL's backend authority with content-free REASON; settle requests."
  (setf (plist-get model :backend-active) nil
        (plist-get model :connection) 'error
        (plist-get model :last-error)
        (pcase reason
          ('startup-failed "Cannot start the Matrix backend; check its executable and private store")
          ('source-overflow "Backend event queue overflow; reconnect to resynchronize")
          ('invalid-notification "Backend protocol mismatch; rebuild the matching backend")
          (_ "Matrix backend stopped; reconnect to restore this device")))
  (let ((ids (hash-table-keys (plist-get model :pending))) commands)
    (dolist (id ids)
      (setq commands
            (nconc commands (matrix-runtime--complete
                             model id nil (matrix-rpc-uncertain-error) t))))
    (nconc commands (matrix-runtime--broadcast model 'error))))

(defun matrix-runtime--update (_context original message)
  "Commit one account MESSAGE against ORIGINAL and return a closed transition."
  (let* ((model (copy-sequence original))
         (commands (matrix-runtime--prune-watches model)))
    (condition-case condition
        (pcase message
          (`(matrix-request ,call ,context)
           (if (>= (hash-table-count (plist-get model :pending)) 64)
               (progn
                 (setf (matrix-rpc-call-params call) nil)
                 (setq model (plist-put model :last-error "Too many pending Matrix operations")))
             (push (matrix-runtime--enqueue
                    model (matrix-rpc-call-method call) (matrix-rpc-call-params call)
                    context call) commands)))
          (`(matrix-watch ,surface ,address ,room)
           (puthash surface (list :address address :room-id room)
                    (plist-get model :watches))
           (when (and room (plist-get model :session)
                      (not (gethash room (plist-get model :timelines))))
             (push (matrix-runtime--subscribe model room) commands)))
          (`(matrix-unwatch ,surface)
           (when-let* ((watch (gethash surface (plist-get model :watches))))
             (remhash surface (plist-get model :watches))
             (when-let* ((command (matrix-runtime--release-room
                                  model (plist-get watch :room-id))))
               (push command commands))))
          (`(matrix-source (event ,event))
           (setq commands (nconc commands (matrix-runtime--event model event))))
          (`(matrix-source (result ,id ,result ,error))
           (setq commands
                 (nconc commands (matrix-runtime--complete model id result error))))
          (`(matrix-closed ,reason)
           (setq commands (nconc commands (matrix-runtime--closed model reason))))
          (`(matrix-intent ,id ,outcome)
           (unless (eq outcome 'accepted)
             (setq commands
                   (nconc commands
                          (matrix-runtime--complete
                           model id nil
                           (list :code -32097
                                 :message (if (eq outcome 'backpressured)
                                              "Backend is busy; operation was not sent"
                                            "Backend is unavailable; operation was not sent")
                                 :data '(:code "transport_not_sent")))))))
          (_ nil))
      (matrix-protocol-error
       (setq commands (nconc commands (matrix-runtime--closed model 'protocol-error)))
       (setf (plist-get model :last-error) (error-message-string condition))))
    (appkit-next :model model :render appkit-render-none :commands commands)))

(defun matrix-runtime--sources (model)
  "Return MODEL's single desired account transport Source."
  (when (plist-get model :backend-active)
    (let ((transport (plist-get model :transport)))
      (list (appkit-source-spec-create
             :key 'backend :identity transport :input transport
             :start #'matrix-rpc-source-start
             :event (lambda (_transport event) (list 'matrix-source event))
             :closed (lambda (_transport &rest reason)
                       (list 'matrix-closed (car reason)))
             :outbound #'matrix-rpc-source-outbound
             :emission-policy 'lossless :pending-limit 1024
             :outbound-pending-limit 64
             :cancellation-requirement 'quiescent-transport)))))

(defun matrix-runtime--init (_context input)
  "Create a secret-free account model from startup INPUT."
  (let* ((model (car input)) (startup (cadr input))
         (command (matrix-runtime--enqueue model (matrix-rpc-call-method startup)
                                            (matrix-rpc-call-params startup)
                                            nil startup)))
    (appkit-next :model model :render appkit-render-none :commands (list command))))

(defun matrix-runtime--shutdown (app)
  "Release registries for exact APP without initiating another transition."
  (let* ((model (appkit-app-model app))
         (transport (plist-get model :transport))
         (store (matrix-rpc-transport-store transport)))
    (when (eq app (gethash store matrix-runtime--accounts))
      (remhash store matrix-runtime--accounts))
    (puthash store transport matrix-runtime--retiring)
    (when (fboundp 'matrix-avatar-clear-cache)
      (matrix-avatar-clear-cache app))
    (when (fboundp 'matrix-media-clear-cache)
      (matrix-media-clear-cache app))
    (when (fboundp 'matrix-compose-release-capture)
      (maphash
       (lambda (_id operation)
         (matrix-compose-release-capture (plist-get operation :context)))
       (plist-get model :pending)))
    (clrhash (plist-get model :pending))
    (clrhash (plist-get model :watches))))

(defconst matrix-runtime--type
  (appkit-app-type-create
   :name 'matrix-account :init #'matrix-runtime--init
   :update #'matrix-runtime--update :sources #'matrix-runtime--sources
   :shutdown #'matrix-runtime--shutdown)
  "Matrix's account-level App type.")

(defun matrix-runtime--await-retiring (store)
  "Ensure STORE's old backend has exited before reopening its device store."
  (when-let* ((transport (gethash store matrix-runtime--retiring)))
    (let ((process (matrix-rpc-transport-process transport))
          (deadline (+ (float-time) 4)))
      (while (and process (process-live-p process) (< (float-time) deadline))
        (accept-process-output process 0.05))
      (when (and process (process-live-p process))
        (user-error "Previous Matrix backend is still stopping")))
    (remhash store matrix-runtime--retiring)))

(defun matrix-runtime-connect (account &optional login)
  "Start ACCOUNT and return its App; nil LOGIN restores the saved device.
LOGIN is a plist with :user_id, :password and optional :homeserver/:device_name.
Its secrets are serialized once through a private Source and never stored in
the account's presentation model.  Normal reconnect must pass nil LOGIN."
  (let* ((store (matrix-runtime--profile-directory account))
         (existing (gethash store matrix-runtime--accounts)))
    (when (and existing (appkit-app-live-p existing)
               (not (plist-get (matrix-runtime-state existing) :backend-active)))
      (matrix-runtime-disconnect existing)
      (setq existing nil))
    (if (and existing (appkit-app-live-p existing))
        (if login (user-error "Disconnect this profile before logging in") existing)
      (when (and login (matrix-runtime-profile-exists-p account))
        (user-error "This profile already has device authority; restore it instead"))
      (unless (or login (matrix-runtime-profile-exists-p account))
        (user-error "No saved session for this profile; log in explicitly"))
      (matrix-runtime--private-directory (expand-file-name matrix-data-directory))
      (matrix-runtime--private-directory store)
      (matrix-runtime--await-retiring store)
      (let* ((transport (matrix-rpc-transport-create
                         :store store :program (matrix-rpc-program)))
             (model (list :account account :connection 'starting :session nil
                          :rooms nil :last-error nil :next-id 0
                          :backend-active t :transport transport
                          :pending (make-hash-table :test #'eql)
                          :watches (make-hash-table :test #'eq)
                          :timelines (make-hash-table :test #'equal)
                          :verifications (make-hash-table :test #'equal)))
             (call (matrix-rpc-call-create
                    :method (if login "session.login" "session.restore")
                    :params login))
             (app (appkit-app-start
                   matrix-runtime--type :identity (list 'matrix-account store)
                   :input (list model call) :surface-limit 32
                   :command-limit 128 :folded-command-limit 512 :max-sources 1)))
        (puthash store app matrix-runtime--accounts)
        app))))

(defun matrix-runtime-disconnect (app)
  "Stop APP and its Surfaces, preserving session and device keys for restore."
  (appkit-app-close app))

(defun matrix-runtime-request (app method params &optional context)
  "Request METHOD with PARAMS from APP, returning a local request ID.
CONTEXT may contain an exact :target Surface address for committed completion.
A request is account-owned; killing the initiating Surface does not undo it."
  (unless (and (appkit-app-live-p app)
               (plist-get (matrix-runtime-state app) :backend-active))
    (user-error "Matrix account is disconnected"))
  (unless (plist-get (matrix-runtime-state app) :session)
    (user-error "Matrix session is not ready"))
  (let ((call (matrix-rpc-call-create :method method :params params)))
    (appkit-app-send app (list 'matrix-request call context))
    (or (matrix-rpc-call-id call)
        (user-error "Matrix request was not admitted; try again when the account is idle"))))

(defun matrix-runtime-resource-request (app method params resolve reject)
  "Read a Resource through APP's gated METHOD/PARAMS transport.
Defer admission outside the declaring render pass; completion follows commit.
Cancellation revokes publication, not an already accepted backend read."
  (let ((call (matrix-runtime-resource-call-create
               :app app :resolve resolve :reject reject :active t)))
    (setf
     (matrix-runtime-resource-call-timer call)
     (run-at-time
      0 nil
      (lambda ()
        (setf (matrix-runtime-resource-call-timer call) nil)
        (when (matrix-runtime-resource-call-active call)
          (condition-case nil
              (matrix-runtime-request app method params (list :resource call))
            (error (matrix-runtime--finish-resource call nil 'request-not-sent)))))))
    (appkit-cancellation-create
     :kind 'logical
     :cancel (lambda ()
               (when-let* ((timer (matrix-runtime-resource-call-timer call)))
                 (cancel-timer timer))
               (setf (matrix-runtime-resource-call-active call) nil
                     (matrix-runtime-resource-call-timer call) nil
                     (matrix-runtime-resource-call-resolve call) nil
                     (matrix-runtime-resource-call-reject call) nil)))))

(defun matrix-runtime-register-surface (app surface &optional room-id)
  "Register exact SURFACE in APP and subscribe to ROOM-ID if supplied."
  (unless (and (appkit-surface-live-p surface)
               (eq app (appkit-surface-app surface)))
    (error "Cannot register a detached Matrix Surface"))
  (let ((address (plist-get (appkit-surface-model surface) :address)))
    (unless (appkit-runtime-address-p address)
      (error "Matrix Surface has no exact runtime address"))
    (appkit-app-send app (list 'matrix-watch surface address room-id))))

(defun matrix-runtime-unregister-surface (app surface)
  "Release exact SURFACE from APP, including after its host has detached."
  (when (and (appkit-app-live-p app)
             (gethash surface (plist-get (matrix-runtime-state app) :watches)))
    (appkit-app-send app (list 'matrix-unwatch surface))))

(defun matrix-runtime-refresh (app)
  "Refresh APP's room summaries from the backend."
  (matrix-runtime-request app "rooms.list" nil))

(defun matrix-room-members (app room-id resolve &optional reject display-name)
  "Request ROOM-ID's joined members from APP.
When DISPLAY-NAME is non-nil, ask the backend for that exact display name
only.  Call RESOLVE with `matrix-state-members' output, or REJECT with an
error value.  The read is account-owned and cancellable; return its
cancellation."
  (matrix-runtime-resource-request
   app "room.members"
   (append (list :room_id room-id)
           (when display-name (list :display_name display-name)))
   (lambda (result)
     (condition-case condition
         (funcall resolve (matrix-state-members (plist-get result :members)))
       (error (funcall (or reject #'ignore) condition))))
   (or reject #'ignore)))

(defun matrix-room-member-resolve (app room-id display-name resolve &optional reject)
  "Resolve the first joined member of ROOM-ID named DISPLAY-NAME.
Call RESOLVE with the member plist or nil; REJECT with an error value.  A
matrix room may contain duplicate display names, so this is best-effort."
  (matrix-room-members
   app room-id
   (lambda (members)
     (funcall resolve (matrix-state-member-by-display-name members display-name)))
   reject display-name))

(provide 'matrix-runtime)
;;; matrix-runtime.el ends here
