;;; matrix-reply.el --- SDK-owned reply relationships -*- lexical-binding: t; -*-

;;; Commentary:
;; Quotes come only from SDK reply details, never message fallback text.  Both
;; quote actions and composer targets resolve exact identities at use time.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'matrix)
(require 'appkit-chatbuf)
(require 'appkit-chat-timeline)
(require 'appkit-chat-history)
(require 'appkit-ui)
(require 'url-util)

(declare-function matrix-room--timeline "matrix-room" (state room-id))
(defvar matrix-room--width)
(defvar matrix-room-message-key-property)

(defvar-local matrix-reply--surface nil
  "Surface owning this host's serializable reply generation token.")
(defvar-local matrix-reply--generation nil
  "Serializable token fencing reply context from a successor Surface.")

(defun matrix-reply--generation (surface)
  "Return this host's reply generation token for exact SURFACE."
  (unless (eq surface matrix-reply--surface)
    (setq matrix-reply--surface surface
          matrix-reply--generation (symbol-name (gensym "matrix-reply-"))))
  matrix-reply--generation)

(defun matrix-reply--text (text)
  "Return plain one-line TEXT without remote display or action properties."
  (appkit-ui-one-line-text
   (replace-regexp-in-string
    "[[:cntrl:]\u2028\u2029]+" " "
    (if (stringp text) (substring-no-properties text) "") nil t)))

(defun matrix-reply--preview (details)
  "Return an explicit SDK DETAILS preview, without parsing fallback markup."
  (pcase (plist-get details :kind)
    ("redacted" "[Message redacted]")
    ("encrypted" "[Unable to decrypt encrypted message]")
    ("state" "[Room state event]")
    (_ (let ((body (matrix-reply--text (plist-get details :body))))
         (if (string-empty-p body) "[Message has no text]" body)))))

(defun matrix-reply--label (reply)
  "Describe SDK REPLY details, retaining unresolved relationships visibly."
  (pcase (plist-get reply :status)
    ("ready"
     (let ((sender (matrix-reply--text
                    (or (plist-get reply :sender_name) (plist-get reply :sender)))))
       (concat (if (string-empty-p sender) "Unknown sender" sender)
               ": " (matrix-reply--preview reply))))
    ("pending" "[Loading replied message…]")
    ("error" "[Replied message details failed to load]")
    (_ "[Replied message unavailable]")))

(defun matrix-reply--context (surface)
  "Capture serializable room, account and generation identity for SURFACE."
  (let* ((state (matrix-runtime-state (appkit-surface-app surface)))
         (room-id (cadr (appkit-surface-identity surface))))
    (list :generation (matrix-reply--generation surface)
          :account (plist-get state :account)
          :session (copy-tree (plist-get state :session))
          :room-id room-id
          :subscription (plist-get (matrix-room--timeline state room-id) :subscription_id))))

(defun matrix-reply--check-surface (surface)
  "Reject a detached, replaced, or inactive room SURFACE."
  (unless (and (appkit-surface-live-p surface)
               (eq 'room (car (appkit-surface-identity surface)))
               (eq surface (appkit-current-surface))
               (eq surface (appkit-app-surface
                            (appkit-surface-app surface) (appkit-surface-identity surface)))
               (plist-get (matrix-runtime-state (appkit-surface-app surface)) :backend-active)
               (plist-get (matrix-runtime-state (appkit-surface-app surface)) :session))
    (user-error "This Matrix room is no longer ready for replies")))

(defun matrix-reply--check-context (surface context)
  "Reject stale CONTEXT before resolving any event in SURFACE."
  (matrix-reply--check-surface surface)
  (let ((current (matrix-reply--context surface)))
    (unless (and (stringp (plist-get context :subscription))
                 (not (string-empty-p (plist-get context :subscription)))
                 (seq-every-p
                  (lambda (key) (equal (plist-get current key) (plist-get context key)))
                  '(:generation :account :session :room-id :subscription)))
      (user-error "The reply target belongs to an old room or timeline"))))

(defun matrix-reply--item (surface key)
  "Return only the canonical SDK row matching exact KEY in SURFACE."
  (seq-find (lambda (item) (equal key (plist-get item :id)))
            (matrix-runtime-room-items (appkit-surface-app surface)
                                       (cadr (appkit-surface-identity surface)))))

(defun matrix-reply--event-id-p (event-id)
  "Return non-nil for a serializable server EVENT-ID, never a local row key."
  (and (stringp event-id) (> (length event-id) 1)
       (eq (aref event-id 0) ?$)
       (not (string-match-p "[[:space:][:cntrl:]]" event-id))))

(defun matrix-reply-validate (surface aux)
  "Resolve AUX's exact current SDK reply target in SURFACE or raise user-error."
  (unless (appkit-surface-live-p surface)
    (user-error "This Matrix room is no longer available"))
  (with-current-buffer (appkit-surface-buffer surface)
    (matrix-reply--check-context surface aux)
    (when (eq (appkit-chatbuf-aux-type) 'edit)
      (user-error "Finish or cancel the current message edit first"))
    (when (plist-get (appkit-surface-model surface) :operation)
      (user-error "Wait for the current Matrix operation"))
    (let ((item (matrix-reply--item surface (plist-get aux :message-id))))
      (unless (and (eq (plist-get aux :aux-type) 'reply)
                   (matrix-reply--event-id-p (plist-get aux :event-id))
                   item (equal (plist-get item :event_id) (plist-get aux :event-id))
                   (eq (plist-get item :can_reply) t))
        (user-error "The original reply target is no longer available or replyable"))
      (plist-get aux :event-id))))

(defun matrix-room-begin-reply (surface item)
  "Stage an exact SDK ITEM reply in SURFACE without replacing its rich draft."
  (unless (appkit-surface-live-p surface)
    (user-error "This Matrix room is no longer available"))
  (with-current-buffer (appkit-surface-buffer surface)
    (matrix-reply--check-surface surface)
    (when (plist-get (appkit-surface-model surface) :operation)
      (user-error "Wait for the current Matrix operation"))
    (when (eq (appkit-chatbuf-aux-type) 'edit)
      (user-error "Finish or cancel the current message edit first"))
    (let* ((current (matrix-reply--item surface (plist-get item :id)))
           (aux (append (list :aux-type 'reply :message-id (plist-get item :id)
                              :event-id (plist-get item :event_id))
                        (matrix-reply--context surface))))
      (unless (and (eq (plist-get item :can_reply) t)
                   current (equal (plist-get item :event_id) (plist-get current :event_id)))
        (user-error "The selected message is stale or cannot be replied to"))
      (matrix-reply-validate surface aux)
      (setq aux (append aux
                        (list :sender-name (matrix-reply--text
                                           (or (plist-get current :sender_name)
                                               (plist-get current :sender)))
                              :preview (matrix-reply--preview current))))
      (appkit-chatbuf-input-state-sync)
      (appkit-chatbuf-aux-set aux)
      (appkit-surface-send surface '(matrix-composer-context))
      (appkit-chatbuf-enter-input))))

(defun matrix-reply-cancel ()
  "Cancel an idle reply target, preserving draft text and attachment objects."
  (interactive)
  (let ((surface (matrix--current-surface 'room)))
    (unless (eq (appkit-chatbuf-aux-type) 'reply)
      (user-error "There is no reply target to cancel"))
    (when (plist-get (appkit-surface-model surface) :operation)
      (user-error "Wait for the pending reply before cancelling"))
    (appkit-chatbuf-input-state-sync)
    (appkit-chatbuf-aux-reset)
    (appkit-surface-send surface '(matrix-composer-context))))

(defun matrix-reply-composer-text (&optional width)
  "Return the shared reply context card bounded to WIDTH, or nil."
  (let* ((aux (appkit-chatbuf-aux-state))
         (surface (appkit-current-surface)))
    (when (eq (plist-get aux :aux-type) 'reply)
      (let* ((item (and surface (matrix-reply--item surface (plist-get aux :message-id))))
             (current (and item (equal (plist-get item :event_id) (plist-get aux :event-id)))))
        (appkit-chatbuf-aux-render
         :title "Replying to message"
         :preview (appkit-ui-one-line-preview-create
                   :label (if current
                              (matrix-reply--text
                               (or (plist-get item :sender_name) (plist-get item :sender)))
                            (plist-get aux :sender-name))
                   :separator ": "
                   :text (if current (matrix-reply--preview item)
                           "[Reply target no longer available]"))
         :cancel-action #'matrix-reply-cancel
         :cancel-help "Cancel reply target; keep text and attachments"
         :width (or width (and (boundp 'matrix-room--width) matrix-room--width)))))))

(defun matrix-reply--permalink (room-id event-id)
  "Return an exact Matrix permalink for ROOM-ID and EVENT-ID."
  (format "https://matrix.to/#/%s/%s"
          (url-hexify-string room-id) (url-hexify-string event-id)))

(defun matrix-reply--follow (surface context)
  "Follow CONTEXT's exact SDK relationship in SURFACE, never a nearby row."
  (matrix-reply--check-context surface context)
  (let* ((source (matrix-reply--item surface (plist-get context :message-id)))
         (event-id (plist-get context :event-id)))
    (unless (and source (matrix-reply--event-id-p event-id)
                 (equal (plist-get source :event_id) (plist-get context :source-event-id))
                 (equal (plist-get (plist-get source :reply) :event_id) event-id))
      (user-error "This reply relationship changed; use the current quote"))
    (let* ((target (seq-find
                    (lambda (item) (equal event-id (plist-get item :event_id)))
                    (matrix-runtime-room-items
                     (appkit-surface-app surface) (plist-get context :room-id))))
           (key (plist-get target :id))
           (row (and key (appkit-chat-timeline-row key)))
           (position (and row (appkit-chat-timeline-key-position key))))
      ;; A canonical loaded event may be outside the exact history window.
      ;; Reveal that loaded window, then resolve its key again after projection.
      (when (and target (not position)
                 (not (member (plist-get target :kind) '("state" "day" "read_marker"))))
        (appkit-chat-history-window-set key nil)
        (appkit-surface-send surface
                             (list 'matrix-update 'timeline (plist-get context :room-id)))
        (setq row (appkit-chat-timeline-row key)
              position (and row (appkit-chat-timeline-key-position key))))
      (matrix-reply--check-context surface context)
      (if (and position
               (equal (plist-get (matrix-reply--item surface key) :event_id) event-id)
               (equal (plist-get (plist-get (matrix-reply--item
                                            surface (plist-get context :message-id))
                                           :reply)
                                 :event_id)
                      event-id)
               (equal (plist-get (appkit-chat-timeline-row-payload row) :event_id) event-id)
               (equal (get-text-property position matrix-room-message-key-property) key))
          (progn (goto-char position) (appkit-chatbuf-update-context-mode) key)
        (message "Original reply target is not in loaded history. Use Load older or open %s"
                 (matrix-reply--permalink (plist-get context :room-id) event-id))
        nil))))

(defun matrix-reply-insert (item)
  "Insert ITEM's SDK relationship as one safe, actionable quote line."
  (when-let* ((reply (plist-get item :reply)))
    (let* ((surface (appkit-current-surface))
           (context (append (list :message-id (plist-get item :id)
                                  :source-event-id (plist-get item :event_id)
                                  :event-id (plist-get reply :event_id))
                            (when surface (matrix-reply--context surface))))
           (label (appkit-ui-render-one-line-preview
                   (appkit-ui-one-line-preview-create :label "↪" :separator " "
                                                      :text (matrix-reply--label reply))
                   (max 12 (- (or (and (boundp 'matrix-room--width) matrix-room--width) 80) 4)))))
      (appkit-ui-insert-action-button
       label (lambda () (matrix-reply--follow (matrix--current-surface 'room) context))
       :face 'shadow :help-echo "Follow the exact original reply target (Return or click)"
       :properties (list 'matrix-reply-context context 'rear-nonsticky t))
      (insert "\n"))))

(defun matrix-reply-goto ()
  "Follow the exact SDK reply quote or replied-to relationship at point."
  (interactive)
  (let* ((surface (matrix--current-surface 'room))
         (key (get-text-property (point) matrix-room-message-key-property))
         (item (and key (matrix-reply--item surface key)))
         (reply (plist-get item :reply))
         (context (or (get-text-property (point) 'matrix-reply-context)
                      (and reply
                           (append (list :message-id key
                                         :source-event-id (plist-get item :event_id)
                                         :event-id (plist-get reply :event_id))
                                   (matrix-reply--context surface))))))
    (unless context (user-error "There is no SDK reply relationship at point"))
    (matrix-reply--follow surface context)))

(provide 'matrix-reply)
;;; matrix-reply.el ends here
