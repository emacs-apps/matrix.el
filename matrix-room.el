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
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'matrix-room-refresh)
    (define-key map (kbd "n") #'matrix-room-next-message)
    (define-key map (kbd "p") #'matrix-room-previous-message)
    (define-key map (kbd "o") #'matrix-message-transient)
    (define-key map (kbd "?") #'matrix-room-transient)
    map)
  "Keymap active over generated Matrix timeline rows.")

(define-minor-mode matrix-room-timeline-mode
  "Navigation over the read-only Matrix timeline."
  :lighter nil :keymap matrix-room-timeline-mode-map)

(defvar matrix-room-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map appkit-chatbuf-mode-map)
    (define-key map (kbd "C-c C-c") #'matrix-room-send)
    (define-key map (kbd "C-c RET") #'matrix-room-send)
    (define-key map (kbd "C-c C-o") #'matrix-composer-transient)
    (define-key map (kbd "C-c '") #'matrix-room-edit-draft)
    (define-key map (kbd "C-c C-r") #'matrix-accounts)
    (define-key map (kbd "C-c C-u") #'matrix-room-review-send)
    (define-key map (kbd "M-g") matrix-room-navigation-map)
    (define-key map (kbd "RET") #'matrix-room-return)
    map)
  "Keymap for `matrix-room-mode'.")

(define-derived-mode matrix-room-mode appkit-chatbuf-mode "Matrix-Room"
  "Matrix room with read-only timeline and editable plain-text composer.
C-c C-c submits the current draft.  Closing this buffer does not stop sync."
  (setq-local appkit-chatbuf-input-sync-function #'appkit-chatbuf-input-state-sync)
  (appkit-chatbuf-use-timeline-mode #'matrix-room-timeline-mode))

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
   (when-let* ((notice (plist-get model :notice))) (concat notice "\n"))))

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
              (prefixes (appkit-chat-avatar-prefixes nil (propertize "@" 'face face)))
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
         (let ((body-start (point)))
           ;; Never interpret formatted_body as HTML or executable markup.
           (insert (if (equal kind "encrypted")
                       (propertize "[Unable to decrypt encrypted event]" 'face 'warning)
                     (substring-no-properties (or (plist-get item :body) ""))))
           (when (or (= body-start (point)) (not (bolp))) (insert "\n"))
           (when compact
             (save-excursion
               (goto-char body-start)
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
  (let* ((room-id (cadr (plist-get model :identity)))
         (room (matrix-room--metadata state room-id))
         (timeline (matrix-room--timeline state room-id))
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
        ;; Bounds refer to raw SDK rows, including protocol-only entries.
        (seq-remove
         (lambda (item) (equal (plist-get item :kind) "state"))
         (plist-get (appkit-chat-history-window-slice
                     (plist-get timeline :items) (lambda (item) (plist-get item :id)))
                    :entries))
        (lambda (item) (plist-get item :id))
        :context-function #'matrix-room--row-context)
       :force-keys (when (appkit-projection-change-geometry-p request)
                     (appkit-chat-timeline-keys)))
      (appkit-chat-timeline-set-frame
       header footer :bind-input-function #'matrix-room--bind-composer
       :composer-visible-p t))))

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
  "Insert a newline in input; activate only an exact timeline action otherwise."
  (interactive)
  (cond
   ((appkit-chatbuf-point-in-input-p) (newline))
   ((get-text-property (point) appkit-ui-action-property)
    (appkit-ui-activate-at (point)))
   ((button-at (point)) (push-button (point)))
   (t (appkit-chatbuf-focus-input))))

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

(defun matrix-room--result (model method _result error context)
  "Settle METHOD without clearing a draft newer than captured CONTEXT."
  (if (not (equal method "message.send"))
      model
    (if error
        (progn
          (when (equal (plist-get (plist-get error :data) :code) "transport_uncertain")
            (setq model
                  (plist-put model :uncertain-send t)))
          (plist-put model :notice
                     (concat "Send not confirmed: " (matrix--error-text error)
                             " Draft retained; inspect the timeline before any further action.")))
      ;; Synchronize first: a queued result can arrive between editing and the
      ;; next command.  Equal text alone does not establish the same revision.
      (appkit-chatbuf-input-state-sync)
      (let ((same-draft
             (and (= (appkit-chatbuf-composer-revision) (plist-get context :revision))
                  (equal (substring-no-properties (appkit-chatbuf-input-state))
                         (plist-get context :body)))))
        (when same-draft
          (appkit-chatbuf-input-history-push (plist-get context :body))
          (appkit-chatbuf-input-replace ""))
        (plist-put model :notice
                   (concat "Accepted into SDK send queue, not yet necessarily delivered. "
                           (if same-draft "" "Newer draft retained.")))))))

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
  "Submit the captured plain-text draft once, retaining it until acceptance.
Late acceptance cannot clear intervening edits, even edits back to the same
text.  An uncertain submission is never retried with a new transaction ID."
  (interactive)
  (let* ((surface (matrix--current-surface 'room))
         (app (appkit-surface-app surface))
         (session (copy-tree (plist-get (matrix-runtime-state app) :session)))
         (model (appkit-surface-model surface))
         (room-id (cadr (plist-get model :identity))))
    (appkit-chatbuf-input-state-sync)
    (let ((body (substring-no-properties (appkit-chatbuf-input-state)))
          (revision (appkit-chatbuf-composer-revision))
          (room (matrix-runtime-room app room-id)))
      (when (string-empty-p (string-trim body)) (user-error "The draft is empty"))
      (when (plist-get model :uncertain-send)
        (user-error "Previous submission is uncertain; inspect the timeline, then use C-c C-u"))
      (unless (or (eq (plist-get room :encrypted) t)
                  (yes-or-no-p "Room is not known to be encrypted. Send potentially unencrypted text? "))
        (user-error "Send cancelled; draft retained"))
      (unless (and (appkit-surface-live-p surface)
                   (eq surface (appkit-app-surface app (appkit-surface-identity surface)))
                   (equal session (plist-get (matrix-runtime-state app) :session)))
        (user-error "The room or account changed during confirmation; draft retained"))
      (matrix--request surface "message.send" (list :room_id room-id :body body)
                       (list :revision revision :body body)))))

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
