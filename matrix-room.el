;;; matrix-room.el --- Matrix chat timeline and persistent draft -*- lexical-binding: t; -*-

;;; Commentary:
;; SDK timeline IDs own generated rows.  The composer is a separate AppKit
;; region, never rebuilt by sync.  An accepted send clears only its captured
;; revision; delivery and encryption state remain SDK authority.

;;; Code:

(require 'matrix)
(require 'appkit-chatbuf)
(require 'appkit-chat-timeline)

(defconst matrix-room-message-key-property 'matrix-message-id
  "Stable SDK timeline identity attached to generated rows.")

(defvar matrix-room-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'matrix-room-refresh)
    (define-key map (kbd "v") #'matrix-devices)
    map)
  "Keymap active over generated Matrix timeline rows.")

(define-minor-mode matrix-room-timeline-mode
  "Navigation over the read-only Matrix timeline."
  :lighter nil :keymap matrix-room-timeline-mode-map)

(defvar matrix-room-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map appkit-chatbuf-mode-map)
    (define-key map (kbd "C-c C-c") #'matrix-room-send)
    (define-key map (kbd "C-c C-r") #'matrix-accounts)
    (define-key map (kbd "C-c C-u") #'matrix-room-review-send)
    (define-key map (kbd "C-c C-v") #'matrix-devices)
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

(defun matrix-room--header (state room-id)
  "Return the generated header from account STATE and ROOM-ID."
  (let ((room (matrix-room--metadata state room-id)))
    (format "%s\n%s   [%s]   %s\n\n"
            (matrix--one-line (or (plist-get room :name) room-id))
            (matrix--one-line room-id)
            (plist-get state :connection)
            (if (eq (plist-get room :encrypted) t)
                "Encrypted room — device trust enforced by Matrix SDK"
              "WARNING: room is not known to be encrypted"))))

(defun matrix-room--footer (state timeline model)
  "Return sync and compose status from STATE, TIMELINE and MODEL."
  (concat
   "\n"
   (when (plist-get timeline :loading) "Loading encrypted timeline…\n")
   (when-let* ((error (plist-get timeline :error)))
     (concat (propertize (format "Timeline error: %s" error) 'face 'error) "\n"))
   (when-let* ((error (plist-get state :last-error)))
     (concat (propertize (format "Account error: %s" error) 'face 'error) "\n"))
   (when-let* ((notice (plist-get model :notice))) (concat notice "\n"))
   "C-c C-c: send    C-c C-r: rooms    C-c C-v: devices    C-c C-u: review uncertain send\n"))

(defun matrix-room--print-row (row)
  "Insert plain-text ROW, with honest encryption and SDK delivery status."
  (let* ((item (appkit-chat-timeline-row-payload row))
         (kind (plist-get item :kind))
         (send-state (plist-get item :send_state))
         (timestamp (plist-get item :timestamp))
         (start (point)))
    (pcase kind
      ((or "day" "read_marker" "state")
       (insert (propertize
                (if (and (equal kind "day") (numberp timestamp))
                    (format-time-string "%Y-%m-%d" (seconds-to-time (/ timestamp 1000.0)))
                  (or (plist-get item :body) ""))
                'face 'shadow)
               "\n"))
      (_
       (insert (propertize (matrix--one-line (or (plist-get item :sender) "Unknown sender"))
                           'face 'font-lock-keyword-face))
       (when (numberp timestamp)
         (insert (propertize
                  (concat "  " (format-time-string "%H:%M" (seconds-to-time (/ timestamp 1000.0))))
                  'face 'shadow)))
       (when send-state
         (insert (propertize (format "  [%s]" send-state)
                             'face (if (equal send-state "failed") 'error 'shadow))))
       (insert "\n")
       (if (equal kind "encrypted")
           (insert (propertize "[Unable to decrypt encrypted event]" 'face 'warning))
         ;; Never interpret Matrix formatted_body as HTML or executable markup.
         (insert (substring-no-properties (or (plist-get item :body) ""))))
       (insert "\n")
       (when-let* ((error (plist-get item :error)))
         (insert (propertize (format "  Error: %s\n" error) 'face 'error)))))
    (insert "\n")
    (add-text-properties start (point)
                         (list matrix-room-message-key-property
                               (appkit-chat-timeline-row-key row)
                               'read-only t 'rear-nonsticky '(read-only)))))

(defun matrix-room--bind-composer ()
  "Bind a new composer once, using AppKit's canonical draft."
  (appkit-chatbuf-bind-input-region
   :visible-p t :prompt "Message: " :input-text (appkit-chatbuf-input-state)))

(defun matrix-room--render (_surface state model)
  "Render pass-scoped account STATE and room presentation MODEL."
  (let* ((room-id (cadr (plist-get model :identity)))
         (timelines (plist-get state :timelines))
         (timeline (and (hash-table-p timelines) (gethash room-id timelines)))
         (header (matrix-room--header state room-id))
         (footer (matrix-room--footer state timeline model)))
    (appkit-chat-timeline-ensure
     :printer #'matrix-room--print-row
     :anchor-property matrix-room-message-key-property
     :header header :footer footer
     :after-mutation-function #'appkit-chatbuf-update-context-mode)
    (appkit-chat-timeline-sync
     (appkit-chat-timeline-project
      (plist-get timeline :items) (lambda (item) (plist-get item :id))))
    (appkit-chat-timeline-set-frame
     header footer :bind-input-function #'matrix-room--bind-composer
     :composer-visible-p t)))

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
  "Open APP's ROOM-ID, optionally SELECT it, without replacing a live draft."
  (let* ((identity (list 'room room-id))
         (surface
          (or (appkit-app-surface app identity)
              (matrix--register
               app (appkit-open-generated-surface
                    matrix-room--type :app app :identity identity :input identity
                    :buffer-name (format "*Matrix:%s:%s*"
                                         (plist-get (matrix-runtime-state app) :account)
                                         room-id))
               room-id))))
    (when select
      (pop-to-buffer (appkit-surface-buffer surface))
      (goto-char (point-max)))
    surface))

(defun matrix-room-send ()
  "Submit the captured plain-text draft once, retaining it until acceptance.
Late acceptance cannot clear intervening edits, even edits back to the same
text.  An uncertain submission is never retried with a new transaction ID."
  (interactive)
  (let* ((surface (matrix--current-surface 'room))
         (model (appkit-surface-model surface))
         (room-id (cadr (plist-get model :identity))))
    (appkit-chatbuf-input-state-sync)
    (let ((body (substring-no-properties (appkit-chatbuf-input-state)))
          (room (matrix-runtime-room (appkit-surface-app surface) room-id)))
      (when (string-empty-p (string-trim body)) (user-error "The draft is empty"))
      (when (plist-get model :uncertain-send)
        (user-error "Previous submission is uncertain; inspect the timeline, then use C-c C-u"))
      (unless (or (eq (plist-get room :encrypted) t)
                  (yes-or-no-p "Room is not known to be encrypted. Send potentially unencrypted text? "))
        (user-error "Send cancelled; draft retained"))
      (matrix--request surface "message.send" (list :room_id room-id :body body)
                       (list :revision (appkit-chatbuf-composer-revision) :body body)))))

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
