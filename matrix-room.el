;;; matrix-room.el --- Matrix chat timeline and persistent draft -*- lexical-binding: t; -*-

;;; Commentary:
;; SDK timeline IDs own generated rows.  The composer is a separate AppKit
;; region, never rebuilt by sync.  An accepted send clears only its captured
;; revision; delivery and encryption state remain SDK authority.

;;; Code:

(require 'matrix)
(require 'appkit-chatbuf)
(require 'appkit-chat-timeline)
(require 'appkit-chat-history)
(require 'appkit-chat-ins)
(require 'appkit-chat-avatar)
(require 'appkit-name-color)
(require 'appkit-ui)
(require 'matrix-media)
(require 'matrix-compose)
(require 'matrix-reply)

(declare-function appkit-media-inline-host-at-point "appkit-media-inline"
                  (&optional position))
(declare-function appkit-media-inline-host-activate "appkit-media-inline"
                  (host &optional presentation))

(defvar-local matrix-room--width 80
  "Width of the last committed room presentation.")

(defvar matrix-room-navigation-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map matrix-navigation-map)
    (define-key map (kbd "<") #'matrix-room-load-older)
    (define-key map (kbd ">") #'matrix-room-latest)
    map)
  "Native navigation prefix, also reused by optional Evil integration.")

(defconst matrix-room-message-key-property 'matrix-message-id
  "Stable SDK timeline identity attached to generated rows.")

(defvar matrix-room-timeline-mode-map
  (make-sparse-keymap)
  "Keymap active over generated Matrix timeline rows.")

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
Closing this buffer does not stop account sync."
  (setq-local appkit-chatbuf-input-sync-function #'appkit-chatbuf-input-state-sync)
  (matrix-compose-setup)
  (appkit-chatbuf-use-timeline-mode #'matrix-room-timeline-mode))

(defun matrix-room--edit-state ()
  "Return the SDK event edit context, if this composer is editing a message."
  (let ((aux (appkit-chatbuf-aux-state)))
    (and (eq (plist-get aux :aux-type) 'edit) aux)))

(defun matrix-room--mutation-params (surface item capability)
  "Capture ITEM's server event in SURFACE's current SDK subscription.
CAPABILITY is the SDK's `:can_edit' or `:can_redact' permission."
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
                  (if (eq capability :can_edit) "edited" "deleted")))
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
  "Return passive history, transport and submission status."
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
   (when-let* ((notice (plist-get model :notice))) (concat notice "\n"))
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
   :compact
   (and (equal (plist-get previous :kind) "message")
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

(defun matrix-room--insert-image (item prefixes)
  "Insert ITEM's committed Canvas image below its caption with PREFIXES."
  (when-let* ((image (plist-get item :image)))
    (let* ((surface (appkit-current-surface))
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
           (poster (and key (matrix-media-cached-poster app key width))))
      (cond
       ((and poster (not (stringp poster)))
        ;; Each call authors a fresh occurrence; committed Surface mounting
        ;; supplies viewer actions and distinct animated playback leases.
        (let ((rows (appkit-media-image-slice-rows poster)))
          (while rows
            (insert (pop rows))
            (if rows (appkit-media-insert-slice-newline) (insert "\n")))))
       (t
        (insert
         (propertize
          (concat
           (cond
            ((not (matrix-media-rendering-available-p)) "[Image: Canvas display required")
            ((not key) "[Image unavailable: missing event identity")
            ((stringp poster) (substring poster 0 -1))
            ((and state (eq (appkit-resource-state-status state) 'failed))
             "[Image unavailable: acquisition failed")
            (t "[Image loading"))
           (when-let* ((filename (plist-get image :filename))
                        ((stringp filename)))
             (concat " · " (matrix--one-line filename)))
           "]\n")
          'face (if (or (stringp poster)
                        (and state (eq (appkit-resource-state-status state) 'failed)))
                    'error 'shadow))))))))

(defun matrix-room--print-row (row)
  "Insert SDK ROW using shared name, avatar-prefix, timestamp and divider layout."
  (let* ((item (appkit-chat-timeline-row-payload row))
         (kind (plist-get item :kind))
         (context (appkit-chat-timeline-row-context row))
         (timestamp (plist-get item :timestamp))
         (start (point)))
    (pcase kind
      ((or "day" "read_marker")
       (appkit-chat-ins-insert-divider-row
        (if (and (equal kind "day") (numberp timestamp))
            (format-time-string "%Y-%m-%d" (seconds-to-time (/ timestamp 1000.0)))
          (matrix--one-line (or (plist-get item :body) "Read up to here")))
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
               (plist-get prefixes :header) (plist-get prefixes :rest-body)))))
         (let ((body-start (point))
               body-text-start)
           (matrix-reply-insert item)
           (setq body-text-start (point))
           ;; Never interpret formatted_body as HTML or executable markup.
           (insert (if (equal kind "encrypted")
                       (propertize "[Unable to decrypt encrypted event]" 'face 'warning)
                     (substring-no-properties (or (plist-get item :body) ""))))
           (when (or (= body-start (point)) (not (bolp))) (insert "\n"))
           (when (equal kind "message")
             (matrix-room--insert-image item prefixes))
           (when compact
             (save-excursion
               (goto-char body-text-start)
               (end-of-line)
               (matrix-room--insert-time item (plist-get prefixes :rest-body))))
           (when-let* ((error (plist-get item :error)))
             (insert (propertize (format "Error: %s\n" error) 'face 'error)))
           (appkit-ui-apply-line-prefix
            body-start (point)
            (appkit-ui-make-prefix-state
             (if compact (plist-get prefixes :rest-body) (plist-get prefixes :first-body))
             (plist-get prefixes :rest-body)))))))
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
            (seq-remove
             (lambda (item) (equal (plist-get item :kind) "state"))
             (plist-get (appkit-chat-history-window-slice
                         (plist-get timeline :items) (lambda (item) (plist-get item :id)))
                        :entries)))
           (header (if (appkit-chat-history-older-loaded-p) "Beginning of history\n" "")))
      (setq matrix-room--width (matrix--width surface))
      (matrix--frame
       state (or (plist-get room :name) room-id) nil
       (pcase (plist-get room :encrypted)
         ('t (propertize "[E2EE]" 'face 'success
                         'help-echo "Encrypted room; Matrix SDK enforces device trust."))
         (_ (propertize "[?E2EE]" 'face 'warning
                         'help-echo "Room is not known encrypted; sending requires consent."))))
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
                  (list (matrix-avatar-resource-key app (plist-get item :avatar_url))
                        (matrix-media-resource-key
                         app room-id (plist-get timeline :subscription_id) item)))))
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
         surface (mapcar (lambda (item)
                           (cons (plist-get item :id) (plist-get item :avatar_url)))
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

(defun matrix-room--move-message (backward)
  "Move to the next semantic message, or previous when BACKWARD."
  (matrix--current-surface 'room)
  (let* ((key (get-text-property (point) matrix-room-message-key-property))
         (position (or (and key (appkit-chat-timeline-key-position key)) (point)))
         (keys (appkit-chat-timeline-keys))
         (target
          (seq-find
           (lambda (candidate)
             (and (not (member (plist-get (appkit-chat-timeline-row-payload
                                          (appkit-chat-timeline-row candidate)) :kind)
                               '("day" "read_marker")))
                  (funcall (if backward #'< #'>)
                           (appkit-chat-timeline-key-position candidate) position)))
           (if backward (reverse keys) keys))))
    (unless target (user-error "No %s message" (if backward "previous" "next")))
    (goto-char (appkit-chat-timeline-key-position target))
    (appkit-chatbuf-update-context-mode)))

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
  "Settle METHOD without clearing a draft newer than captured CONTEXT."
  (cond
   ((equal method "message.edit")
    (if error
        (progn
          (when (equal (plist-get (plist-get error :data) :code) "transport_uncertain")
            (setq model (plist-put model :uncertain-send t)))
          (plist-put model :notice
                     (concat "Edit not confirmed: " (matrix--error-text error) " Edits retained.")))
      (appkit-chatbuf-input-state-sync)
      (let ((same-edit
             (and (equal (plist-get (matrix-room--edit-state) :event-id)
                         (plist-get context :edit-event))
                  (= (appkit-chatbuf-composer-revision) (plist-get context :revision))
                  (equal (substring-no-properties (appkit-chatbuf-input-state))
                         (plist-get context :body)))))
        (when same-edit (matrix-room--restore-edit-draft))
        (plist-put model :notice
                   (if same-edit "Edit accepted by SDK; previous draft restored."
                     "Edit accepted by SDK; newer edits retained.")))))
   ((not (equal method "message.send"))
    model)
   (t (matrix-compose-result model result error context))))

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
