;;; matrix-room.el --- Matrix chat timeline and persistent draft -*- lexical-binding: t; -*-

;;; Commentary:
;; SDK timeline IDs own generated rows.  The composer is a separate AppKit
;; region, never rebuilt by sync.  An accepted send clears only its captured
;; revision; delivery and encryption state remain SDK authority.
;; SDK read positions and their matching timeline revision locate the boundary.
;; Partial reading keeps it fixed; authoritative completion removes it.
;; `matrix-room-read-through' explicitly acknowledges the selected remote event
;; (newest loaded eligible event from the composer) through private SDK receipts.
;; SDK cache-derived counts lack completeness evidence; notification badges
;; remain separate from exact unread-message progress.

;;; Code:

(require 'matrix)
(require 'appkit-chatbuf)
(require 'appkit-chat-timeline)
(require 'appkit-chat-history)
(require 'appkit-chat-ins)
(require 'appkit-chat-read)
(require 'appkit-chat-avatar)
(require 'appkit-name-color)
(require 'appkit-ui)
(require 'appkit-presentation)
(require 'matrix-media)
(require 'matrix-compose)
(require 'matrix-mention)
(require 'matrix-reply)

(declare-function matrix-user-open "matrix-user" (app user-id &optional select))
(declare-function appkit-media-inline-host-at-point "appkit-media-inline"
                  (&optional position))
(declare-function appkit-media-inline-host-activate "appkit-media-inline"
                  (host &optional presentation))

(defcustom matrix-room-state-events 'all
  "Which room state changes the timeline shows as one-line notices.
`all' shows every change the backend describes, `no-membership' hides
joins, leaves and profile changes (busy bridged rooms), nil hides them all."
  :type '(choice (const :tag "All described changes" all)
                 (const :tag "All but membership and profile changes" no-membership)
                 (const :tag "None" nil))
  :group 'matrix)

(defcustom matrix-send-typing-notices t
  "Whether rooms are told while you compose a message."
  :type 'boolean
  :group 'matrix)

(defvar-local matrix-room--typing-revision nil
  "Composer revision last considered for a typing notice.")

(defvar-local matrix-room--typing-sent nil
  "Last typing notice sent from this room, as (TYPING-P . TIME).")

(defvar-local matrix-room--width 80
  "Width of the last committed room presentation.")

(defvar-local matrix-room--unread-key nil
  "Stable reading-session boundary returned by the shared read projection.")

(defvar-keymap matrix-room-navigation-map
  :doc "Fast navigation under `M-g', after telega's chatbuf fastnav keys."
  "r" #'matrix-room-read-all
  ">" #'matrix-room-read-all
  "u" #'matrix-room-goto-unread
  "m" #'matrix-room-next-mention
  "@" #'matrix-room-next-mention
  "o" #'matrix-room-next-outgoing)

(defconst matrix-room-message-key-property 'matrix-message-id
  "Stable SDK timeline identity attached to generated rows.")

(defvar-keymap matrix-room-timeline-mode-map
  :doc "Keymap active over generated Matrix timeline rows."
  "i" #'matrix-room-open-user-at-point)

(appkit-bindings-define 'matrix-room-timeline-mode-map 'read-only
  'return #'matrix-room-return
  'app-menu #'matrix-room-transient
  'refresh #'matrix-room-refresh
  'next-item #'matrix-room-next-message
  'previous-item #'matrix-room-previous-message
  'message-menu #'matrix-message-transient
  'reply #'matrix-message-reply
  'edit #'matrix-message-edit
  'delete #'matrix-message-delete
  'react #'matrix-message-react
  'history-older #'matrix-room-load-older
  'latest #'matrix-room-latest
  'quit #'quit-window
  'help #'describe-mode)

(define-minor-mode matrix-room-timeline-mode
  "Navigation over the read-only Matrix timeline."
  :lighter nil :keymap matrix-room-timeline-mode-map)

(defvar matrix-room-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map appkit-chatbuf-mode-map)
    (define-key map (kbd "C-c C-r") #'matrix-accounts)
    (define-key map (kbd "C-c C-u") #'matrix-room-review-send)
    map)
  "Keymap for `matrix-room-mode'.")

(appkit-bindings-define 'matrix-room-mode-map 'input
  'return #'matrix-room-return
  'newline #'newline
  'submit #'matrix-room-send
  'input-options #'matrix-composer-transient
  'attach #'matrix-attachment-transient
  'attach-file #'matrix-compose-attach-file
  'attach-clipboard #'matrix-compose-attach-clipboard
  'detach #'matrix-compose-detach
  'cancel #'matrix-compose-cancel
  'app-menu #'matrix-room-transient
  'navigation matrix-room-navigation-map
  'refresh #'matrix-room-refresh
  'latest #'matrix-room-latest
  'help #'describe-mode)

(define-derived-mode matrix-room-mode appkit-chatbuf-mode "Matrix-Room"
  "Matrix room with a read-only timeline and structured editable composer.
RET submits text and staged attachments; Shift-Return inserts a newline.
\\[matrix-room-read-through] acknowledges through the selected message, or newest
loaded message from the composer, without changing the draft or session bar.
Closing this buffer does not stop account sync."
  (setq-local appkit-chatbuf-input-sync-function #'appkit-chatbuf-input-state-sync)
  (matrix-compose-setup)
  (matrix-mention-setup)
  (add-hook 'post-command-hook #'matrix-room--notice-typing nil t)
  (appkit-chatbuf-use-timeline-mode #'matrix-room-timeline-mode))

(defun matrix-room--edit-state ()
  "Return the SDK event edit context, if this composer is editing a message."
  (let ((aux (appkit-chatbuf-aux-state)))
    (and (eq (plist-get aux :aux-type) 'edit) aux)))

(defun matrix-room--mutation-params (surface item capability)
  "Capture ITEM's server event in SURFACE's current SDK subscription.
CAPABILITY is the SDK's `:can_edit', `:can_react' or `:can_redact' permission."
  (let* ((app (appkit-surface-app surface))
         (state (matrix-runtime-state app))
         (room-id (cadr (appkit-surface-identity surface)))
         (subscription (plist-get (matrix-room--timeline state room-id) :subscription_id))
         (event-id (plist-get item :event_id)))
    (unless (and (appkit-surface-live-p surface)
                 (eq surface (appkit-app-surface app (appkit-surface-identity surface)))
                 (plist-get state :backend-active) (plist-get state :session))
      (user-error "This Matrix room is not ready for message operations"))
    (when (plist-get (appkit-surface-model surface) :operation)
      (user-error "Wait for the current Matrix operation"))
    (unless (and (stringp subscription) (not (string-empty-p subscription))
                 (stringp event-id) (not (string-empty-p event-id))
                 (eq (plist-get item capability) t))
      (user-error "The selected message cannot be %s"
                  (pcase capability
                    (:can_edit "edited") (:can_react "reacted to") (_ "deleted"))))
    (list :room_id room-id :subscription_id subscription :event_id event-id)))

(defun matrix-room-begin-edit (surface item)
  "Edit exact SDK ITEM in SURFACE, retaining the preceding ordinary draft."
  (with-current-buffer (appkit-surface-buffer surface)
    (matrix-room--mutation-params surface item :can_edit)
    (unless (and (equal (plist-get item :kind) "message")
                 (stringp (plist-get item :body)))
      (user-error "This message has no editable plaintext"))
    (when (matrix-room--edit-state)
      (user-error "Finish or cancel the current message edit first"))
    (appkit-chatbuf-input-state-sync)
    (appkit-chatbuf-aux-set
     (list :aux-type 'edit :message-id (plist-get item :id)
           :event-id (plist-get item :event_id)
           :subscription (plist-get (matrix-room--timeline
                                    (matrix-runtime-state (appkit-surface-app surface))
                                    (cadr (appkit-surface-identity surface))) :subscription_id)
           :saved-input (copy-sequence (appkit-chatbuf-input-state))
           :saved-aux (copy-tree (appkit-chatbuf-aux-state))
           :original-body (substring-no-properties (plist-get item :body))))
    (appkit-chatbuf-input-replace (plist-get item :body))
    (appkit-surface-send surface '(matrix-composer-context))
    (appkit-chatbuf-enter-input)))

(defun matrix-room--restore-edit-draft ()
  "Restore the ordinary draft saved by the active message edit."
  (let* ((edit (matrix-room--edit-state))
         (saved (plist-get edit :saved-input))
         (aux (plist-get edit :saved-aux)))
    (appkit-chatbuf-aux-set aux)
    (appkit-chatbuf-input-replace (or saved ""))))

(defun matrix-room-cancel-edit ()
  "Cancel editing without publishing and restore the preceding draft."
  (interactive)
  (let ((surface (matrix--current-surface 'room)))
    (unless (matrix-room--edit-state) (user-error "There is no message edit to cancel"))
    (when (plist-get (appkit-surface-model surface) :operation)
      (user-error "Wait for the pending edit before cancelling"))
    (matrix-room--restore-edit-draft)
    (appkit-surface-send surface '(matrix-composer-context))))

(defun matrix-room--edit-params (surface edit)
  "Resolve EDIT against the current SDK snapshot, never a neighboring row."
  (let ((item (seq-find
               (lambda (candidate) (equal (plist-get candidate :id) (plist-get edit :message-id)))
               (matrix-runtime-room-items (appkit-surface-app surface)
                                          (cadr (appkit-surface-identity surface))))))
    (unless (and item
                 (equal (plist-get item :event_id) (plist-get edit :event-id))
                 (equal (plist-get edit :subscription)
                        (plist-get (matrix-room--timeline
                                    (matrix-runtime-state (appkit-surface-app surface))
                                    (cadr (appkit-surface-identity surface))) :subscription_id)))
      (user-error "The message edit target or its timeline is no longer available"))
    (matrix-room--mutation-params surface item :can_edit)))

(defun matrix-room--metadata (state room-id)
  "Return ROOM-ID metadata from pass-scoped STATE."
  (seq-find (lambda (room) (equal room-id (plist-get room :room_id)))
            (plist-get state :rooms)))

(defun matrix-room--timeline (state room-id)
  "Read ROOM-ID's authoritative timeline from account STATE."
  (when-let* ((timelines (plist-get state :timelines)))
    (gethash room-id timelines)))

(defun matrix-room--sync-history (model state)
  "Derive exact history bounds from committed SDK STATE, fencing old requests."
  (let* ((timeline (matrix-room--timeline state (cadr (plist-get model :identity))))
         (subscription (plist-get timeline :subscription_id)))
    (unless (equal subscription (plist-get model :history-subscription))
      (appkit-chat-history-reset-state)
      (setq model (plist-put (copy-sequence model) :history-subscription subscription))
      (setq model (plist-put model :history-error nil)))
    (when (numberp (plist-get timeline :revision))
      ;; An empty reset is a valid live window, not proof of exhaustion.
      (appkit-chat-history-window-set
       (plist-get (car (plist-get timeline :items)) :id) nil))
    model))

(defun matrix-room--footer (state timeline model)
  "Return passive history, transport and active composer context."
  (concat
   (appkit-chat-history-delimiter-string
    matrix-room--width :loading-text "Loading older messages")
   "\n"
   (when (plist-get timeline :loading) "Opening timeline…\n")
   (when-let* ((error (plist-get timeline :error)))
     (concat (propertize (format "Timeline: %s" error) 'face 'error) "\n"))
   (when-let* ((error (plist-get model :history-error)))
     (concat (propertize (format "History: %s" error) 'face 'error) "\n"))
   (when-let* ((error (plist-get state :last-error)))
     (concat (propertize (format "Account: %s" error) 'face 'error) "\n"))
   (when (plist-get model :operation) "Waiting for account operation…\n")
   (when-let* ((edit (matrix-room--edit-state)))
     (appkit-chatbuf-aux-render
      :title "Editing message"
      :preview (appkit-ui-one-line-preview-create :text (plist-get edit :original-body))
      :cancel-action #'matrix-room-cancel-edit
      :cancel-help "Cancel message edit and restore the previous draft"
      :width matrix-room--width))
   (matrix-reply-composer-text matrix-room--width)))

(defun matrix-room--row-context (previous item)
  "Group adjacent messages by stable sender and a five-minute interval."
  (list
   :self (eq (plist-get item :own) t)
   :unread (equal (plist-get item :id) matrix-room--unread-key)
   :compact
   (and (not (equal (plist-get item :id) matrix-room--unread-key))
        (equal (plist-get previous :kind) "message")
        (equal (plist-get item :kind) "message")
        (equal (plist-get previous :sender) (plist-get item :sender))
        (plist-get item :sender)
        (equal (plist-get previous :sender_name) (plist-get item :sender_name))
        (equal (plist-get previous :avatar_url) (plist-get item :avatar_url))
        (not (member (plist-get previous :send_state) '("pending" "failed")))
        (not (member (plist-get item :send_state) '("pending" "failed")))
        (not (plist-get previous :error))
        (not (plist-get item :error))
        (numberp (plist-get previous :timestamp))
        (numberp (plist-get item :timestamp))
        (<= 0 (- (plist-get item :timestamp) (plist-get previous :timestamp)) 300000))))

(defun matrix-room--time-label (item)
  "Present SDK ITEM's delivery state and local time without implying delivery."
  (string-join
   (delq nil
         (list (when-let* ((state (plist-get item :send_state))
                          ((not (equal state "sent"))))
                 (format "[%s]" state))
               (when-let* ((timestamp (plist-get item :timestamp))
                           ((numberp timestamp)))
                 (format-time-string "%H:%M" (seconds-to-time (/ timestamp 1000.0))))))
   " "))

(defun matrix-room--insert-time (item prefix)
  "Insert ITEM's clock and queue status using shared layout with PREFIX."
  (let ((label (matrix-room--time-label item)))
    (unless (string-empty-p label)
      (let ((span (appkit-chat-ins-insert-right-aligned-text
                   label matrix-room--width
                   :face (pcase (plist-get item :send_state)
                           ("failed" 'error) ("pending" 'warning) (_ 'shadow))
                   :left-prefix-width (string-width prefix))))
        (add-text-properties
         (car span) (cdr span)
         (list 'help-echo
               (string-join
                (delq nil
                      (list (when-let* ((timestamp (plist-get item :timestamp))
                                       ((numberp timestamp)))
                              (format-time-string
                               "%Y-%m-%d %H:%M:%S"
                               (seconds-to-time (/ timestamp 1000.0))))
                            (plist-get item :send_state)))
                " · ")))))))

(defun matrix-room--video-resolver (app room item)
  "Return a resolver fetching APP's ROOM ITEM video for inline playback."
  (lambda (resolve reject)
    (pcase-let ((`(,_kind ,_source ,name) (matrix-media-attachment item)))
      (matrix-media-save
       app room (plist-get (matrix-room--timeline (matrix-runtime-state app) room)
                           :subscription_id)
       item (matrix-media--open-path name)
       (lambda (path) (funcall resolve `((file . ,path) (name . ,name))))
       reject))
    nil))

(defun matrix-room--insert-poster (item prefixes open)
  "Insert ITEM's Canvas image or video poster, sized by PREFIXES.
Until it is ready, insert a status line that runs OPEN when activated."
  (let* ((image (matrix-media-poster item))
         (surface (appkit-current-surface))
         (app (appkit-surface-app surface))
         (room (cadr (appkit-surface-identity surface)))
         (timeline (matrix-room--timeline (matrix-runtime-state app) room))
         (key (matrix-media-resource-key app room (plist-get timeline :subscription_id) item))
         (state (and key (appkit-resource-state surface key)))
         (width (max 1 (floor
                        (min appkit-media-preview-max-width
                             (/ (* (max 1 (- matrix-room--width
                                             (string-width (plist-get prefixes :rest-body))))
                                   (appkit-media--char-pixel-width)
                                   (appkit-media--base-char-pixel-height))
                                (float (appkit-media--char-pixel-height)))))))
         (video (when (plist-get image :video)
                  (append (list :resolve (matrix-room--video-resolver app room item))
                          image)))
         (poster (and key (matrix-media-cached-poster app key width video))))
    (if (and poster (not (stringp poster)))
        ;; Each call authors a fresh occurrence; committed Surface mounting
        ;; supplies viewer actions and distinct animated playback leases.
        (let ((rows (appkit-media-image-slice-rows poster)))
          (while rows
            (insert (pop rows))
            (if rows (appkit-media-insert-slice-newline) (insert "\n"))))
      (let ((failed (or (stringp poster)
                        (and state (eq (appkit-resource-state-status state) 'failed)))))
        (appkit-chat-ins-insert-prefixed-line
         (cond
          ((not (matrix-media-rendering-available-p)) "Preview needs a graphical display")
          ((not key) "Preview unavailable: missing event identity")
          ((stringp poster) (string-trim poster "\\[" "\\]\n*"))
          (failed "Preview unavailable: download failed")
          (t "Loading preview…"))
         :prefix "" :face (if failed 'error 'shadow)
         :action open :help-echo "Open this attachment")))))

(defun matrix-room--media (item)
  "Return ITEM's media card as (:kind KIND :title TITLE :details DETAILS), or nil."
  (when (equal (plist-get item :kind) "message")
    (if-let* ((image (plist-get item :image)))
        (let ((width (plist-get image :width))
              (height (plist-get image :height))
              (size (plist-get image :size)))
          (list :kind 'image :title (plist-get image :filename)
                :details (delq nil (list (when (and (natnump width) (natnump height))
                                           (format "%d×%d" width height))
                                         (when (natnump size) (file-size-human-readable size))))))
      (when-let* ((attachment (plist-get item :attachment)))
        (let ((size (plist-get attachment :size))
              (duration (plist-get attachment :duration_ms)))
          (list :kind (intern (or (plist-get attachment :kind) "file"))
                :title (plist-get attachment :filename)
                :details (delq nil (list (when (natnump duration)
                                           (appkit-chat-ins-format-duration (/ duration 1000.0)))
                                         (when (natnump size)
                                           (file-size-human-readable size))))))))))

(defun matrix-room--open-media (row-key preview)
  "Open SDK row ROW-KEY's media: play or view its PREVIEW, else download it."
  (if-let* (((marker-buffer preview))
            ((fboundp 'appkit-media-inline-host-at-point))
            (host (appkit-media-inline-host-at-point (marker-position preview))))
      (appkit-media-inline-host-activate host)
    (matrix-room-open-attachment row-key)))

(defun matrix-room--insert-media-card (item body-prefix prefixes)
  "Insert ITEM's image, video, audio or file as one AppKit media card.
Its border continues BODY-PREFIX; PREFIXES size an image or video poster."
  (when-let* ((media (matrix-room--media item)))
    (let* ((row-key (plist-get item :id))
           (preview (make-marker))
           (downloadable (stringp (plist-get item :event_id)))
           (open (when downloadable (lambda () (matrix-room--open-media row-key preview))))
           (card-prefix (let ((appkit-ui-card-indent-prefix-state body-prefix))
                          (appkit-ui-card-prefix-state))))
      (appkit-chat-ins-insert-media-card
       :kind (plist-get media :kind)
       :title (matrix--one-line (or (plist-get media :title) ""))
       :details (plist-get media :details)
       :prefix card-prefix :title-face 'bold :meta-face 'shadow
       :context (appkit-media-card-context-create
                 :payload (list :row-key row-key)
                 :kind (plist-get media :kind) :title (plist-get media :title)
                 :open-action open
                 :save-as-action (when downloadable
                                   (lambda () (matrix-room-save-attachment row-key))))
       :open-help-echo "Open this attachment"
       :body-inserter
       (when (matrix-media-poster item)
         (lambda (card-prefix-state)
           (let ((start (point)))
             (set-marker preview start)
             (matrix-room--insert-poster item prefixes open)
             (appkit-ui-apply-line-prefix start (point) card-prefix-state))))))))

(defun matrix-room--insert-reactions (item)
  "Insert ITEM's SDK reaction chips; a chip toggles its reaction.
The row's body prefix is applied over them afterwards, so add none here."
  (let ((row-key (plist-get item :id)))
    (appkit-chat-ins-insert-reaction-line
     (append (plist-get item :reactions) nil)
     :prefix ""
     :label-function
     (lambda (reaction)
       (format "[%s %d]" (matrix--one-line (plist-get reaction :key))
               (plist-get reaction :count)))
     :selected-p-function (lambda (reaction) (eq (plist-get reaction :own) t))
     :action-function
     (when (eq (plist-get item :can_react) t)
       (lambda (reaction) (matrix-room-toggle-reaction row-key (plist-get reaction :key))))
     :help-echo-function
     (lambda (reaction)
       (if (eq (plist-get reaction :own) t) "Remove your reaction" "Add this reaction")))))

(defun matrix-room--item (surface row-key)
  "Return SURFACE's current SDK row ROW-KEY, or signal that it is gone."
  (or (seq-find (lambda (item) (equal row-key (plist-get item :id)))
                (matrix-runtime-room-items (appkit-surface-app surface)
                                           (cadr (appkit-surface-identity surface))))
      (user-error "The message is no longer available")))

(defun matrix-room-toggle-reaction (row-key reaction)
  "Toggle REACTION on this room's message whose SDK row is ROW-KEY."
  (let* ((surface (matrix--current-surface 'room))
         (item (matrix-room--item surface row-key)))
    (matrix--request surface "message.react"
                     (append (matrix-room--mutation-params surface item :can_react)
                             (list :key reaction)))))

(defun matrix-room--request (method &rest params)
  "Request room METHOD with PARAMS for the current room view."
  (let ((surface (matrix--current-surface 'room)))
    (matrix--request surface method
                     (append (list :room_id (cadr (appkit-surface-identity surface))) params))))

(defun matrix-room--summary ()
  "Return the current room's ID and summary as (ROOM-ID . SUMMARY)."
  (let* ((surface (matrix--current-surface 'room))
         (room-id (cadr (appkit-surface-identity surface))))
    (cons room-id (matrix-runtime-room (appkit-surface-app surface) room-id))))

(defun matrix-room--name ()
  "Return the current room's name, or its ID."
  (pcase-let ((`(,room-id . ,summary) (matrix-room--summary)))
    (or (plist-get summary :name) room-id)))

(defun matrix-room-invite (user-id)
  "Invite USER-ID to the current room."
  (interactive (list (matrix--read-user-id "Invite user ID: ")))
  (matrix-room--request "room.invite" :user_id user-id))

(defun matrix-room-rename (name)
  "Rename the current room to NAME."
  (interactive (list (read-string "Room name: " (matrix-room--name))))
  (matrix-room--request "room.set_name" :text (string-trim name)))

(defun matrix-room-set-topic (topic)
  "Set the current room's TOPIC; an empty topic removes it."
  (interactive (list (read-string "Room topic: " (plist-get (cdr (matrix-room--summary)) :topic))))
  (matrix-room--request "room.set_topic" :text (string-trim topic)))

(defun matrix-room-leave ()
  "Leave the current room after confirming."
  (interactive)
  (when (yes-or-no-p (format "Leave %s? " (matrix-room--name)))
    (matrix-room--request "room.leave")))

(defun matrix-room--local-echo-params (surface item capability)
  "Capture SURFACE's unsent ITEM for an SDK send-queue CAPABILITY.
CAPABILITY is the backend's `:can_retry' or `:can_discard'."
  (let* ((app (appkit-surface-app surface))
         (state (matrix-runtime-state app))
         (room-id (cadr (appkit-surface-identity surface)))
         (subscription (plist-get (matrix-room--timeline state room-id) :subscription_id))
         (transaction (plist-get item :transaction_id)))
    (unless (and (plist-get state :backend-active) (plist-get state :session)
                 (stringp subscription) (stringp transaction)
                 (eq (plist-get item capability) t))
      (user-error "This message cannot be %s"
                  (if (eq capability :can_retry) "retried" "discarded")))
    (list :room_id room-id :subscription_id subscription :transaction_id transaction)))

(defun matrix-room-retry-send (row-key)
  "Send this room's failed message with SDK row ROW-KEY again."
  (let* ((surface (matrix--current-surface 'room))
         (item (matrix-room--item surface row-key)))
    (matrix--request surface "message.retry"
                     (matrix-room--local-echo-params surface item :can_retry))))

(defun matrix-room-discard-send (row-key)
  "Withdraw this room's unsent message with SDK row ROW-KEY after confirming."
  (let* ((surface (matrix--current-surface 'room))
         (params (matrix-room--local-echo-params
                  surface (matrix-room--item surface row-key) :can_discard)))
    (when (yes-or-no-p "Discard this unsent Matrix message? ")
      (matrix--request surface "message.discard"
                       (matrix-room--local-echo-params
                        surface (matrix-room--item surface row-key) :can_discard)
                       (list :transaction-id (plist-get params :transaction_id))))))

(defun matrix-room--insert-send-actions (item)
  "Insert retry and discard buttons under ITEM when its sending failed."
  (when (equal (plist-get item :send_state) "failed")
    (let ((row-key (plist-get item :id))
          (start (point)))
      (when (eq (plist-get item :can_retry) t)
        (insert-text-button "[Retry]" 'follow-link t 'help-echo "Send this message again"
                            'action (lambda (_) (matrix-room-retry-send row-key)))
        (insert " "))
      (when (eq (plist-get item :can_discard) t)
        (insert-text-button "[Discard]" 'follow-link t 'help-echo "Withdraw this message"
                            'action (lambda (_) (matrix-room-discard-send row-key))))
      (unless (= start (point)) (insert "\n")))))

(defun matrix-room--save-attachment (surface item path on-saved)
  "Download SURFACE's ITEM attachment to PATH, then call ON-SAVED with PATH."
  (let* ((app (appkit-surface-app surface))
         (room (cadr (appkit-surface-identity surface))))
    (matrix-media-save app room
                       (plist-get (matrix-room--timeline (matrix-runtime-state app) room)
                                  :subscription_id)
                       item path on-saved)))

(defun matrix-room-save-attachment (row-key)
  "Download the attachment of SDK row ROW-KEY to a new file."
  (let* ((surface (matrix--current-surface 'room))
         (name (nth 2 (or (matrix-media-attachment (matrix-room--item surface row-key))
                          (user-error "This message has no downloadable attachment"))))
         (path (read-file-name "Save attachment to: " matrix-download-directory nil nil name)))
    (when (file-directory-p path)
      (setq path (expand-file-name name path)))
    (when (file-exists-p path)
      (user-error "%s already exists" (abbreviate-file-name path)))
    (matrix-room--save-attachment
     surface (matrix-room--item surface row-key) path
     (lambda (path) (message "Matrix: saved %s" (abbreviate-file-name path))))))

(defun matrix-room-open-attachment (row-key)
  "Download a private copy of the attachment of SDK row ROW-KEY and open it.
The download and the selected Appkit media owner are separate keyed Effects,
so closing the room or replacing the presentation revokes both."
  (let* ((surface (matrix--current-surface 'room))
         (app (appkit-surface-app surface))
         (room (cadr (appkit-surface-identity surface)))
         (item (matrix-room--item surface row-key))
         (timeline (matrix-room--timeline (matrix-runtime-state app) room))
         (subscription (plist-get timeline :subscription_id)))
    (unless (and (stringp subscription) (not (string-empty-p subscription)))
      (user-error "This room's timeline is not ready"))
    (pcase-let ((`(,kind ,_source ,name)
                 (or (matrix-media-attachment item)
                     (user-error "This message has no downloadable attachment"))))
      (appkit-surface-send
       surface
       (list 'matrix-media-open
             (list :surface surface :app app :room room
                   :subscription subscription
                   :item item :kind kind :name name
                   :path (matrix-media--open-path name)))))))

(defun matrix-room--typing-text (names)
  "Describe who of NAMES is typing."
  (pcase names
    (`(,one) (format "%s is typing…" one))
    (`(,one ,two) (format "%s and %s are typing…" one two))
    (`(,one . ,rest) (format "%s and %d others are typing…" one (length rest)))))

(defun matrix-room--notice-typing ()
  "Tell the room whether the composer holds a draft, at most every 2 seconds.
The SDK debounces further and lets a notice lapse 4 seconds after the last."
  (when-let* ((matrix-send-typing-notices)
              (surface (appkit-current-surface))
              ((eq surface matrix--registered-surface))
              (revision (appkit-chatbuf-composer-revision))
              ((not (eql revision matrix-room--typing-revision)))
              (app (appkit-surface-app surface))
              ((plist-get (matrix-runtime-state app) :session)))
    (setq matrix-room--typing-revision revision)
    (let ((typing (not (string-blank-p (appkit-chatbuf-input-string))))
          (now (float-time)))
      (when (if typing
                (or (not (car matrix-room--typing-sent))
                    (> (- now (cdr matrix-room--typing-sent)) 2))
              (car matrix-room--typing-sent))
        (setq matrix-room--typing-sent (cons typing now))
        (matrix-runtime-resource-request
         app "room.typing"
         (list :room_id (cadr (appkit-surface-identity surface))
               :typing (if typing t :json-false))
         #'ignore #'ignore)))))

(defun matrix-room--shown-p (item)
  "Whether the timeline presents SDK row ITEM."
  (pcase (plist-get item :kind)
    ("read_marker" nil)
    ("state" (and (eq (plist-get item :described) t)
                  (pcase matrix-room-state-events
                    ('all t)
                    ('no-membership
                     (not (equal (plist-get item :state_type) "m.room.member"))))))
    (_ t)))

(defun matrix-room--avatar-uris (item)
  "Return the avatar URIs ITEM's row shows: its sender's and its people's."
  (cons (plist-get item :avatar_url)
        (mapcar (lambda (part) (plist-get part :avatar_url))
                (append (plist-get item :parts) nil))))

(defun matrix-room--notice-user (app part)
  "Return notice PART's user in their colour beside their avatar, as a link."
  (let* ((user-id (plist-get part :user_id))
         (uri (plist-get part :avatar_url))
         (image (when (and (matrix-avatar-resource-key app uri)
                           (appkit-media-inline-image-rendering-available-p))
                  (matrix-avatar-cached-image
                   app uri (appkit-chat-avatar-line-pixel-height)))))
    (with-temp-buffer
      (appkit-ui-insert-action-button
       (concat (when image (concat (appkit-chat-avatar-one-line-string image "") " "))
               (matrix--one-line (or (plist-get part :name) user-id)))
       (lambda () (matrix-user-open app user-id t))
       :face (appkit-name-color-face user-id)
       :help-echo user-id
       :properties (list 'matrix-user-id user-id))
      (buffer-string))))

(defun matrix-room--notice-label (item width)
  "Return state ITEM's sentence within WIDTH columns.
Its people show in their colours beside their avatars; a sentence too wide
for that is plain text, elided."
  (let* ((app (appkit-surface-app (appkit-current-surface)))
         (label (concat (mapconcat
                         (lambda (part)
                           (if (stringp (plist-get part :user_id))
                               (matrix-room--notice-user app part)
                             (matrix--one-line (or (plist-get part :text) ""))))
                         (append (plist-get item :parts) nil) ""))))
    (if (and (plist-get item :parts) (<= (string-width label) width))
        label
      (appkit-presentation-elide-string-for-columns
       (matrix--one-line (plist-get item :body)) width 'shadow))))

(defun matrix-room-open-user-at-point ()
  "Open the page of the user at point: a notice's person, else the sender."
  (interactive)
  (let* ((surface (matrix--current-surface 'room))
         (key (get-text-property (point) matrix-room-message-key-property))
         (user-id (or (get-text-property (point) 'matrix-user-id)
                      (and key (plist-get (matrix-room--item surface key) :sender)))))
    (unless (stringp user-id) (user-error "No user at point"))
    (matrix-user-open (appkit-surface-app surface) user-id t)))

(defun matrix-room--print-row (row)
  "Insert SDK ROW using shared name, avatar-prefix, timestamp and divider layout."
  (let* ((item (appkit-chat-timeline-row-payload row))
         (kind (plist-get item :kind))
         (context (appkit-chat-timeline-row-context row))
         (timestamp (plist-get item :timestamp))
         start)
    (when (plist-get context :unread)
      (appkit-chat-read-insert-divider matrix-room--width))
    (setq start (point))
    (pcase kind
      ("state"
       ;; AppKit's gray-tip row with the time at the right edge, as QQ's.
       (let ((time (matrix-room--time-label item)))
         (appkit-chat-ins-insert-tip-row
          (matrix-room--notice-label
           item (max 10 (- matrix-room--width (string-width time) 12)))
          'shadow matrix-room--width :time time)))
      ("day"
       (appkit-chat-ins-insert-divider-row
        (if (numberp timestamp)
            (format-time-string "%Y-%m-%d" (seconds-to-time (/ timestamp 1000.0)))
          (matrix--one-line (or (plist-get item :body) "Day")))
        'shadow matrix-room--width))
      (_
       (let* ((sender (or (plist-get item :sender) "Unknown sender"))
              (name (matrix--one-line (or (plist-get item :sender_name) sender)))
              (face (delq nil (list (and (plist-get context :self) 'bold)
                                    (appkit-name-color-face sender))))
              (app (appkit-surface-app (appkit-current-surface)))
              (uri (plist-get item :avatar_url))
              (pixel-size (when (and (matrix-avatar-resource-key app uri)
                                     (appkit-media-inline-image-rendering-available-p))
                            (appkit-chat-avatar-two-line-pixel-size)))
              (prefixes
               (appkit-chat-avatar-prefixes
                (and pixel-size (matrix-avatar-cached-image app uri pixel-size))
                (propertize "@" 'face face) :pixel-size pixel-size))
              (compact (plist-get context :compact)))
         (unless compact
           (let ((heading (point)))
             (insert (propertize name 'face face 'help-echo sender))
             (matrix-room--insert-time item (plist-get prefixes :header))
             (insert "\n")
             (appkit-ui-apply-line-prefix
              heading (point)
              (appkit-ui-make-prefix-state
               (plist-get prefixes :header) (plist-get prefixes :rest-body)))
             (appkit-ui-append-face heading (point) 'appkit-chat-heading)))
         (let ((body-start (point))
               (body-prefix
                (appkit-ui-make-prefix-state
                 (if compact (plist-get prefixes :rest-body) (plist-get prefixes :first-body))
                 (plist-get prefixes :rest-body)))
               body-text-start)
           (matrix-reply-insert item body-prefix)
           (setq body-text-start (point))
           ;; Never interpret formatted_body as HTML or executable markup.
           ;; An attachment's card already names it; show only a real caption.
           (insert (cond
                    ((equal kind "encrypted")
                     (propertize "[Unable to decrypt encrypted event]" 'face 'warning))
                    ((equal (plist-get item :body)
                            (plist-get (plist-get item :attachment) :filename))
                     "")
                    (t (substring-no-properties (or (plist-get item :body) "")))))
           (unless (bolp) (insert "\n"))
           (when (eq (plist-get item :highlighted) t)
             (add-face-text-property body-text-start (point) 'matrix-highlight t))
           (let ((media (matrix-room--media item)))
             (when (and (= body-start (point)) (not media))
               (insert "\n"))
             (let ((text-end (point)) tail)
               (appkit-ui-apply-line-prefix body-text-start text-end body-prefix)
               ;; The card draws its own border from the body prefix.
               (when media (matrix-room--insert-media-card item body-prefix prefixes))
               (setq tail (point))
               (matrix-room--insert-reactions item)
               (when-let* ((error (plist-get item :error)))
                 (insert (propertize error 'face (if (eq (plist-get item :can_retry) t)
                                                     'error 'warning))
                         "\n"))
               (matrix-room--insert-send-actions item)
               (appkit-ui-apply-line-prefix tail (point) body-prefix)
               (when compact
                 ;; The clock ends the first text line, or the card's title.
                 (save-excursion
                   (goto-char (if (< body-text-start text-end) body-text-start text-end))
                   (end-of-line)
                   (matrix-room--insert-time item (plist-get prefixes :rest-body))))))))))
    (add-text-properties start (point)
                         (list matrix-room-message-key-property
                               (appkit-chat-timeline-row-key row)
                               'read-only t 'rear-nonsticky '(read-only)))))

(defun matrix-room--bind-composer ()
  "Bind a new composer once, using AppKit's canonical draft."
  (appkit-chatbuf-bind-input-region
   :visible-p t :prompt ">>> " :input-text (appkit-chatbuf-input-state)))

(defun matrix-room--render (surface state model request)
  "Project a pass-scoped SDK snapshot without changing the canonical draft."
  (if (and (appkit-projection-change-resources request)
           (not (or (appkit-projection-change-full-p request)
                    (appkit-projection-change-keys request)
                    (appkit-projection-change-rekeys request)
                    (appkit-projection-change-geometry-p request)
                    (appkit-projection-change-frame-p request)
                    (appkit-projection-change-position request))))
      (progn
        (appkit-chat-timeline-invalidate
         (appkit-chat-timeline-dependent-keys
          (appkit-projection-change-resources request)))
        (appkit-render-result-create
         :resource-interest-update
         (appkit-resource-interest-update-create :mode 'unchanged)))
    (let* ((room-id (cadr (plist-get model :identity)))
           (room (matrix-room--metadata state room-id))
           (timeline (matrix-room--timeline state room-id))
           (app (appkit-surface-app surface))
           (items
            ;; Bounds refer to raw SDK rows, including protocol-only entries.
            (seq-filter
             #'matrix-room--shown-p
             (plist-get (appkit-chat-history-window-slice
                         (plist-get timeline :items) (lambda (item) (plist-get item :id)))
                        :entries)))
           (header ""))
      (setq matrix-room--width (matrix--width surface))
      (when (appkit-chat-history-older-loaded-p)
        (let ((width matrix-room--width))
          (setq header (with-temp-buffer
                         (appkit-chat-ins-insert-divider-row
                          "Beginning of history" 'shadow width)
                         (buffer-string)))))
      (setq matrix-room--unread-key
            (appkit-chat-read-update
             (mapcar (lambda (item) (plist-get item :id)) items)
             :first-unread-key (matrix-room--first-unread-key timeline)
             :known-p (and timeline t)
             :unread-count (plist-get (plist-get timeline :read_state) :unread_messages)
             :all-read-p (eq (plist-get (plist-get timeline :read_state) :all_read) t)))
      (matrix--frame
       state (or (plist-get room :name) room-id) (plist-get room :topic)
       (pcase (plist-get room :encrypted)
         ('t (propertize "[E2EE]" 'face 'success
                         'help-echo "Encrypted room; Matrix SDK enforces device trust."))
         (_ (propertize "[?E2EE]" 'face 'warning
                         'help-echo "Room is not known encrypted; sending requires consent."))))
      (when-let* ((count (appkit-chat-read-count-text
                         (plist-get (plist-get timeline :read_state) :unread_messages))))
        (setq matrix--header-line (concat matrix--header-line "  " count)))
      (when-let* ((typing (matrix-room--typing-text (plist-get timeline :typing))))
        (setq matrix--header-line
              (concat matrix--header-line "  " (propertize typing 'face 'shadow))))
      (let ((footer (matrix-room--footer state timeline model)))
        (appkit-chat-timeline-ensure
         :printer #'matrix-room--print-row
         :anchor-property matrix-room-message-key-property
         :header header :footer footer
         :after-mutation-function #'appkit-chatbuf-update-context-mode)
        (appkit-chat-timeline-sync
         (appkit-chat-timeline-project
          items (lambda (item) (plist-get item :id))
          :context-function #'matrix-room--row-context
          :dependencies-function
          (lambda (item)
            (delq nil
                  (cons (matrix-media-resource-key
                         app room-id (plist-get timeline :subscription_id) item)
                        (mapcar (lambda (uri) (matrix-avatar-resource-key app uri))
                                (matrix-room--avatar-uris item))))))
         :changed-resources (appkit-projection-change-resources request)
         :force-keys (when (appkit-projection-change-geometry-p request)
                       (appkit-chat-timeline-keys)))
        (appkit-chat-timeline-set-frame
         header footer :bind-input-function #'matrix-room--bind-composer
         :composer-visible-p t))
      (let (media-rows media-keys)
        (dolist (item items)
          (when-let* ((demand (matrix-media-demand
                              app room-id (plist-get timeline :subscription_id) item)))
            (push (cons (plist-get item :id) demand) media-rows)
            (push (appkit-resource-demand-key demand) media-keys)))
        (matrix-media-retain app room-id media-keys)
        (matrix--avatar-resource-result
         surface (mapcan (lambda (item)
                           (mapcar (lambda (uri) (cons (plist-get item :id) uri))
                                   (matrix-room--avatar-uris item)))
                         items)
         (nreverse media-rows))))))

(defun matrix-room--defer-observation (_context surface _observe resolve reject)
  "Check committed geometry at idle, with cancellation owned by the Surface."
  (let ((timer
         (run-with-idle-timer
          0 nil
          (lambda ()
            (condition-case nil
                (progn
                  (when (appkit-surface-live-p surface)
                    (with-current-buffer (appkit-surface-buffer surface)
                      (when-let* ((observer (appkit-chat-timeline-scroll-observer)))
                        (appkit-scroll-observer-check observer))))
                  (funcall resolve nil))
              (error (funcall reject nil)))))))
    (appkit-cancellation-create :kind 'transport
                                :cancel (lambda () (cancel-timer timer)))))

(defun matrix-room--observe-command ()
  "Return a bounded post-commit recheck; never start requests inside a renderer."
  (appkit-command-start-effect
   (appkit-effect-create
    :key 'matrix-history-observation :input (appkit-current-surface)
    :start #'matrix-room--defer-observation
    :success (lambda (_input _value) '(matrix-history-observed))
    :failure (lambda (_input _error) '(matrix-history-observer-failed)))))

(defun matrix-room--visible-start (_window position boundary)
  "Page when the actual visible edge approaches BOUNDARY, not on point motion."
  (let* ((surface (matrix--current-surface 'room))
         (model (appkit-surface-model surface))
         (state (matrix-runtime-state (appkit-surface-app surface)))
         (timeline (matrix-room--timeline state (cadr (plist-get model :identity)))))
    (when (and (plist-get state :backend-active)
               (not (plist-get timeline :loading))
               (not (plist-get timeline :error))
               (not (plist-get model :history-error))
               (appkit-chat-history-autoload-older-p position boundary 200))
      (matrix-room-load-older))))

(defun matrix-room-load-older ()
  "Load an older SDK page without blocking sends or moving the reading anchor."
  (interactive)
  (let* ((surface (matrix--current-surface 'room))
         (model (appkit-surface-model surface))
         (app (appkit-surface-app surface))
         (room-id (cadr (plist-get model :identity)))
         (timeline (matrix-room--timeline (matrix-runtime-state app) room-id)))
    (when (or (plist-get timeline :loading)
              (not (appkit-chat-history-window-known-p)))
      (user-error "Wait for the timeline snapshot"))
    (when (appkit-chat-history-older-loaded-p) (user-error "Already at the beginning"))
    (unless (appkit-chat-history-loading-p)
      (let* ((owner (appkit-chat-history-request-start surface 'older))
             (owned (list :target (plist-get model :address) :history-owner owner
                          :subscription (plist-get timeline :subscription_id))))
        (appkit-surface-send surface '(matrix-history-started))
        (condition-case nil
            (matrix-runtime-request
             app "timeline.paginate"
             (list :room_id room-id :subscription_id (plist-get owned :subscription) :limit 50)
             owned)
          (error
           (appkit-surface-send
            surface
            (list 'matrix-result nil "timeline.paginate" nil
                  '(:message "History request failed; use Load older to retry.") owned))))))))

(defun matrix-room--history-complete (model state result error context)
  "Settle only CONTEXT's exact owner and still-current SDK subscription."
  (let ((subscription
         (plist-get (matrix-room--timeline state (cadr (plist-get model :identity)))
                    :subscription_id)))
    (when (and (equal (plist-get model :address) (plist-get context :target))
               (equal subscription (plist-get context :subscription))
               (equal subscription (plist-get model :history-subscription))
               (appkit-chat-history-request-end (plist-get context :history-owner)))
      (unless error
        (appkit-chat-history-older-loaded-set (eq (plist-get result :hit_start) t)))
      (plist-put (copy-sequence model) :history-error
                 (when error (matrix--error-text error))))))

(defun matrix-room-return ()
  "Send only in input; activate image playback or exact timeline controls."
  (interactive)
  (if-let* (((not (appkit-chatbuf-point-in-input-p)))
             ((fboundp 'appkit-media-inline-host-at-point))
             (host (appkit-media-inline-host-at-point)))
      (appkit-media-inline-host-activate host)
    (appkit-chatbuf-dispatch-return #'matrix-room-send)))

(defun matrix-room--move-message (backward &optional predicate what wrap)
  "Move to the next rendered message, or previous when BACKWARD.
PREDICATE, given a row's SDK item, restricts the target; WHAT names it when
there is none.  WRAP continues from the other end of the loaded history."
  (matrix--current-surface 'room)
  (let* ((key (get-text-property (point) matrix-room-message-key-property))
         (position (or (and key (appkit-chat-timeline-key-position key)) (point)))
         (keys (appkit-chat-timeline-keys))
         (matches
          (seq-filter
           (lambda (candidate)
             (let ((item (appkit-chat-timeline-row-payload
                          (appkit-chat-timeline-row candidate))))
               (and (not (equal (plist-get item :kind) "day"))
                    (or (null predicate) (funcall predicate item)))))
           (if backward (reverse keys) keys)))
         (target
          (or (seq-find (lambda (candidate)
                          (funcall (if backward #'< #'>)
                                   (appkit-chat-timeline-key-position candidate) position))
                        matches)
              (and wrap (car matches)))))
    (unless target
      (user-error "No %s" (or what (if backward "previous message" "next message"))))
    (goto-char (appkit-chat-timeline-key-position target))
    (appkit-chatbuf-update-context-mode)))

(defun matrix-room-next-mention ()
  "Move to the next loaded message that mentions you."
  (interactive)
  (matrix-room--move-message
   nil (lambda (item) (eq (plist-get item :highlighted) t)) "message mentioning you" t))

(defun matrix-room-next-outgoing ()
  "Move to the next loaded message you sent."
  (interactive)
  (matrix-room--move-message
   nil (lambda (item) (eq (plist-get item :own) t)) "message of yours" t))

(defun matrix-room-goto-unread ()
  "Move to the first unread message."
  (interactive)
  (matrix--current-surface 'room)
  (let ((position (and matrix-room--unread-key
                       (appkit-chat-timeline-key-position matrix-room--unread-key))))
    (unless position (user-error "No unread messages in the loaded history"))
    (goto-char position)
    (appkit-chatbuf-update-context-mode)))

(defun matrix-room-read-all ()
  "Mark the room read through its newest message and return to the composer."
  (interactive)
  (matrix-room-latest)
  (matrix-room-read-through))

(defun matrix-room-next-message ()
  "Move to the next rendered message."
  (interactive)
  (matrix-room--move-message nil))

(defun matrix-room-previous-message ()
  "Move to the previous rendered message, skipping date/read separators."
  (interactive)
  (matrix-room--move-message t))

(defun matrix-room-edit-draft ()
  "Focus the canonical input without modifying its text or modal state."
  (interactive)
  (matrix--current-surface 'room)
  (appkit-chatbuf-focus-input))

(defun matrix-room-latest ()
  "Return to the live tail and composer without changing its draft."
  (interactive)
  (matrix-room-edit-draft)
  (when (eq (window-buffer) (current-buffer)) (recenter -1)))

(defun matrix-room--result (model method result error context)
  "Return a settled transition without clearing a draft newer than CONTEXT."
  (cond
   ((equal method "message.edit")
    (if error
        (progn
          (when (equal (plist-get (plist-get error :data) :code) "transport_uncertain")
            (setq model (plist-put model :uncertain-send t)))
          (matrix--feedback-next
           model (concat "Edit not confirmed: " (matrix--error-text error) " Edits retained.")))
      (appkit-chatbuf-input-state-sync)
      (let ((same-edit
             (and (equal (plist-get (matrix-room--edit-state) :event-id)
                         (plist-get context :edit-event))
                  (= (appkit-chatbuf-composer-revision) (plist-get context :revision))
                  (equal (substring-no-properties (appkit-chatbuf-input-state))
                         (plist-get context :body)))))
        (when same-edit (matrix-room--restore-edit-draft))
        (matrix--feedback-next
         model (if same-edit "Edit accepted by SDK; previous draft restored."
                 "Edit accepted by SDK; newer edits retained.")))))
   ((not (equal method "message.send"))
    (matrix--feedback-next
     model (if error (matrix--error-text error)
             (pcase method
               ("message.redact" "Redaction accepted by SDK.")
               ("message.react" (if (eq (plist-get result :added) t)
                                    "Reaction added." "Reaction removed."))
               ("message.retry" "Message queued to send again.")
               ("room.invite" "Invitation sent.")
               ("room.set_name" "Room renamed.")
               ("room.set_topic" "Topic changed.")
               ("room.leave" "You left the room.")
               ("message.discard" (if (eq (plist-get result :discarded) t)
                                      "Unsent message discarded."
                                    "The message was already sent."))
               (_ (format "%s completed" method))))))
   (t (matrix-compose-result model result error context))))

(defun matrix-room--read-capable-p (item)
  "Whether ITEM is a canonical remote SDK event that can be acknowledged."
  (and (eq (plist-get item :can_read) t)
       (stringp (plist-get item :event_id))
       (matrix-state-event-row-p item)))

(defun matrix-room--first-unread-key (timeline)
  "Resolve TIMELINE's SDK-derived unread index in its exact revision.
The fully-read marker alone may lag the user's private read receipt."
  (let ((index (plist-get (plist-get timeline :read_state) :first_unread_index)))
    (when (and (integerp index) (>= index 0))
      (let ((item (nth index (plist-get timeline :items))))
        (when (matrix-room--read-capable-p item)
          (plist-get item :id))))))

(defun matrix-room-read-through ()
  "Acknowledge the selected remote event, or newest loaded event in composer.
Advance fully-read and private receipts.
SDK complete-read evidence clears the bar."
  (interactive)
  (let* ((surface (matrix--current-surface 'room))
         (app (appkit-surface-app surface))
         (state (matrix-runtime-state app))
         (model (appkit-surface-model surface))
         (room-id (cadr (appkit-surface-identity surface)))
         (subscription (plist-get (matrix-room--timeline state room-id) :subscription_id))
         (row (appkit-chat-read-target
               nil (lambda (candidate)
                     (matrix-room--read-capable-p
                      (appkit-chat-timeline-row-payload candidate)))))
         (event-id (and row (plist-get (appkit-chat-timeline-row-payload row) :event_id))))
    (unless (and event-id subscription
                 (appkit-surface-live-p surface)
                 (eq surface (appkit-app-surface app (appkit-surface-identity surface)))
                 (plist-get state :backend-active) (plist-get state :session))
      (user-error "No read-capable message in this live Matrix timeline"))
    ;; A receipt owns neither the composer nor the durable-send operation slot.
    (matrix-runtime-request
     app "timeline.read"
     (list :room_id room-id :subscription_id subscription :event_id event-id)
     (list :target (plist-get model :address) :subscription subscription))))

(defvar matrix-room--type
  (matrix--surface-type 'matrix-room #'matrix-room-mode #'matrix-room--render)
  "Generated Matrix room Surface type.")

(defun matrix-room-open (app room-id &optional select)
  "Open APP's ROOM-ID, retaining existing reading position and live draft."
  (let* ((identity (list 'room room-id))
         (existing (appkit-app-surface app identity))
         (surface
          (matrix--open
           app matrix-room--type identity
           (format "*Matrix:%s:%s*" (plist-get (matrix-runtime-state app) :account) room-id)
           select room-id)))
    (unless existing
      (appkit-chat-timeline-install-history-observer
       surface :start-function #'matrix-room--visible-start)
      (when select
        (with-current-buffer (appkit-surface-buffer surface)
          (appkit-chatbuf-focus-input))))
    surface))

(defun matrix-room-send ()
  "Submit captured text, attachments and reply target through the SDK queue.
Late acceptance cannot clear an intervening draft or composer context.
An uncertain submission is never retried with a new transaction ID."
  (interactive)
  (let* ((surface (matrix--current-surface 'room))
         (app (appkit-surface-app surface))
         (session (copy-tree (plist-get (matrix-runtime-state app) :session)))
         (model (appkit-surface-model surface))
         (room-id (cadr (plist-get model :identity)))
         (capture (matrix-compose-capture surface))
         (body (plist-get capture :body))
         (revision (plist-get capture :revision))
         (aux (plist-get capture :aux))
         (room (matrix-runtime-room app room-id))
         (edit (matrix-room--edit-state))
         context submitted)
    (when (and (string-empty-p (string-trim body))
               (not (plist-get capture :attachments)))
      (user-error "The draft is empty"))
    (when (plist-get model :uncertain-send)
      (user-error "Previous submission is uncertain; inspect the timeline, then use C-c C-u"))
    (unless edit
      (setq context (matrix-compose-retain-capture surface capture)))
    (unwind-protect
        (progn
          (unless (or (eq (plist-get room :encrypted) t)
                      (yes-or-no-p
                       "Room is not known encrypted. Send potentially unencrypted text and attachments? "))
            (user-error "Send cancelled; draft retained"))
          (unless (and (appkit-surface-live-p surface)
                       (eq surface (appkit-app-surface app (appkit-surface-identity surface)))
                       (equal session (plist-get (matrix-runtime-state app) :session)))
            (user-error "The room or account changed during confirmation; draft retained"))
          (unless (equal aux (appkit-chatbuf-aux-state))
            (user-error "The composer context changed during confirmation; draft retained"))
          (when (eq (plist-get aux :aux-type) 'reply)
            (matrix-reply-validate surface aux))
          (if edit
              (let ((params (matrix-room--edit-params surface edit)))
                (matrix--request surface "message.edit" (append params (list :body body))
                                 (list :revision revision :body body
                                       :edit-event (plist-get edit :event-id))))
            (matrix--request surface "message.send" (plist-get capture :params) context))
          (setq submitted t))
      (unless submitted
        (matrix-compose-release-capture context)))))

(defun matrix-room-review-send ()
  "Acknowledge an uncertain submission without sending or changing the draft."
  (interactive)
  (let ((surface (matrix--current-surface 'room)))
    (unless (plist-get (appkit-surface-model surface) :uncertain-send)
      (user-error "There is no uncertain submission to review"))
    (when (yes-or-no-p
           "The previous send may have succeeded. Have you checked the timeline? ")
      (appkit-surface-send surface '(matrix-send-reviewed)))))

(defun matrix-room-refresh ()
  "Refresh account room summaries without replacing the live SDK timeline."
  (interactive)
  (matrix-runtime-refresh (appkit-surface-app (matrix--current-surface 'room))))

(provide 'matrix-room)
;;; matrix-room.el ends here
