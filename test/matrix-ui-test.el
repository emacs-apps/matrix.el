;;; matrix-ui-test.el --- Consumer-visible draft boundaries -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'matrix-room)
(require 'matrix-verify)
(require 'matrix-transient)

(defun matrix-ui-test--state (body)
  "Build a small account snapshot with a real SDK-shaped timeline BODY."
  (let ((timelines (make-hash-table :test #'equal)))
    (puthash "!room:test"
             (list :subscription_id "subscription" :revision 0
                   :items (list (list :id "stable-row" :kind "message"
                                      :sender "@other:test" :body body)))
             timelines)
    (list :account "ui-test" :connection 'online
          :session '(:user_id "@me:test" :device_id "DEVICE")
          :rooms (copy-tree '((:room_id "!room:test" :name "Encrypted room"
                              :membership "join" :encrypted t)))
          :timelines timelines :verifications (make-hash-table :test #'equal))))

(defun matrix-ui-test--items-state (items &optional read-state)
  "Return a snapshot with authoritative ITEMS and nullable READ-STATE metadata."
  (let* ((state (matrix-ui-test--state ""))
         (timelines (plist-get state :timelines))
         (timeline (gethash "!room:test" timelines)))
    (puthash "!room:test"
             (plist-put (plist-put timeline :items items) :read_state read-state) timelines)
    state))

(defun matrix-ui-test--open (app &optional buffer)
  "Open the actual room Surface on APP, optionally reusing BUFFER."
  (appkit-open-generated-surface
   matrix-room--type :app app :identity '(room "!room:test")
   :input '(room "!room:test") :buffer buffer :buffer-name " *Matrix UI test*"))

(defmacro matrix-ui-test--with-room (&rest body)
  "Run BODY with a real generated room, without a network backend."
  (declare (indent 0) (debug t))
  `(let (app surface buffer)
     (unwind-protect
         (progn
           (setq app
                 (appkit-app-start
                  (appkit-app-type-create
                   :name 'matrix-ui-test
                   :init (lambda (_context input)
                           (appkit-next :model input :render appkit-render-none))
                   :update (lambda (_context _model input)
                             (appkit-next :model input :render appkit-render-none)))
                  :input (matrix-ui-test--state "Initial message")))
           (setq surface (matrix-ui-test--open app)
                 buffer (appkit-surface-buffer surface))
           (with-current-buffer buffer ,@body))
       (when surface (appkit-surface-stop surface))
       (when app (appkit-app-close app))
       (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun matrix-ui-test--capture (surface token)
  "Capture this draft for SURFACE under TOKEN before a directed response."
  (appkit-chatbuf-input-state-sync)
  (let ((context (append
                  (list :target (plist-get (appkit-surface-model surface) :address)
                        :method "message.send" :token token)
                  (matrix-compose-retain-capture
                   surface (matrix-compose-capture surface)))))
    (appkit-surface-send surface (list 'matrix-operation context))
    context))

(defun matrix-ui-test--settle (surface context &optional error)
  "Deliver an actual result message to SURFACE for CONTEXT and optional ERROR."
  (appkit-surface-send surface
                       (list 'matrix-result 1 "message.send"
                             (list :accepted (max 1 (length (plist-get context :attachments)))
                                   :total (max 1 (length (plist-get context :attachments))))
                             error context)))

(ert-deftest matrix-ui-sync-preserves-draft-and-read-only-rows ()
  (matrix-ui-test--with-room
    (appkit-chatbuf-input-replace "Unsubmitted draft")
    (let ((revision (appkit-chatbuf-composer-revision)))
      (appkit-app-send app (matrix-ui-test--state "Updated message"))
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (should (equal (appkit-chatbuf-input-string) "Unsubmitted draft"))
      (should (= revision (appkit-chatbuf-composer-revision)))
      (should (equal (appkit-chat-timeline-keys) '("stable-row")))
      (should (string-match-p "Updated message" (buffer-string)))
      (goto-char (point-min))
      (should-error (insert "not allowed") :type 'text-read-only))))

(ert-deftest matrix-ui-accepted-captured-draft-clears-only-once ()
  (matrix-ui-test--with-room
    (appkit-chatbuf-input-replace "Captured message")
    (let ((context (matrix-ui-test--capture surface 1)))
      (matrix-ui-test--settle surface context)
      (should (equal (appkit-chatbuf-input-string) ""))
      (appkit-chatbuf-input-replace "Next message")
      (matrix-ui-test--settle surface context)
      (should (equal (appkit-chatbuf-input-string) "Next message")))))

(ert-deftest matrix-ui-operation-feedback-does-not-become-composer-content ()
  (matrix-ui-test--with-room
    (let ((empty-room (buffer-substring-no-properties (point-min) (point-max))))
      (appkit-chatbuf-input-replace "Captured message")
      (matrix-ui-test--settle surface (matrix-ui-test--capture surface 1))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     empty-room)))
    (appkit-chatbuf-input-replace "Unsent ordinary draft")
    (let ((room-with-draft (buffer-substring-no-properties (point-min) (point-max))))
      (dolist (error '(nil (:message "Redaction rejected")))
        (let ((context (list :target (plist-get (appkit-surface-model surface) :address)
                             :method "message.redact" :token 2)))
          (appkit-surface-send surface (list 'matrix-operation context))
          (appkit-surface-send
           surface (list 'matrix-result 2 "message.redact" nil error context))
          (should (equal (buffer-substring-no-properties (point-min) (point-max))
                         room-with-draft)))))))

(ert-deftest matrix-ui-failure-and-late-acceptance-preserve-edits ()
  (matrix-ui-test--with-room
    (appkit-chatbuf-input-replace "Original")
    (let ((context (matrix-ui-test--capture surface 1)))
      (matrix-ui-test--settle surface context '(:message "Unable to encrypt for unverified device"))
      (should (equal (appkit-chatbuf-input-string) "Original")))
    (let ((context (matrix-ui-test--capture surface 2)))
      (appkit-chatbuf-input-replace "Newer draft")
      (matrix-ui-test--settle surface context)
      (should (equal (appkit-chatbuf-input-string) "Newer draft")))
    ;; Equality of text is insufficient: editing away and back is a newer draft.
    (let ((context (matrix-ui-test--capture surface 3)))
      (appkit-chatbuf-input-replace "Different text")
      (appkit-chatbuf-input-replace "Newer draft")
      (matrix-ui-test--settle surface context)
      (should (equal (appkit-chatbuf-input-string) "Newer draft")))))

(ert-deftest matrix-ui-old-operation-cannot-settle-current-send ()
  (matrix-ui-test--with-room
    (appkit-chatbuf-input-replace "First message")
    (let ((old (matrix-ui-test--capture surface 1)))
      (matrix-ui-test--settle surface old '(:message "Rejected"))
      (appkit-chatbuf-input-replace "Second message")
      (let ((current (matrix-ui-test--capture surface 2)))
        (matrix-ui-test--settle surface old)
        (should (equal (appkit-chatbuf-input-string) "Second message"))
        (should (equal (plist-get (appkit-surface-model surface) :operation) current))
        (matrix-ui-test--settle surface current)
        (should (equal (appkit-chatbuf-input-string) ""))))))

(ert-deftest matrix-ui-dead-generation-cannot-clear-successor-draft ()
  (matrix-ui-test--with-room
    (appkit-chatbuf-input-replace "Old host draft")
    (let ((old (matrix-ui-test--capture surface 1)))
      (appkit-surface-stop surface)
      (setq surface (matrix-ui-test--open app buffer))
      (appkit-chatbuf-input-replace "Successor draft")
      ;; Deliberately reuse the operation token: the exact generation address
      ;; must still reject a result that belonged to the dead Surface.
      (let ((current (matrix-ui-test--capture surface 1)))
        (should-not (equal (plist-get current :target) (plist-get old :target)))
        (matrix-ui-test--settle surface old)
        (should (equal (appkit-chatbuf-input-string) "Successor draft"))
        (should (equal (plist-get (appkit-surface-model surface) :operation) current))
        (matrix-ui-test--settle surface current)
        (should (equal (appkit-chatbuf-input-string) ""))))))

(ert-deftest matrix-ui-uncertain-send-needs-review-without-losing-draft ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (appkit-chatbuf-input-replace "Possibly queued")
    (let ((context (matrix-ui-test--capture surface 1)))
      (matrix-ui-test--settle surface context (matrix-rpc-uncertain-error)))
    (should (plist-get (appkit-surface-model surface) :uncertain-send))
    (should-error (matrix-room-send) :type 'user-error)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (matrix-room-review-send))
    (should-not (plist-get (appkit-surface-model surface) :uncertain-send))
    (should-not (plist-get (appkit-surface-model surface) :operation))
    (should (equal (appkit-chatbuf-input-string) "Possibly queued"))))

(ert-deftest matrix-ui-local-rejection-is-not-an-uncertain-network-write ()
  (matrix-ui-test--with-room
    (appkit-chatbuf-input-replace "Not yet sent")
    ;; This real runtime admission fails before any Source exists or bytes leave.
    (matrix--request
     surface "message.send"
     '(:room_id "!room:test" :subscription_id "subscription" :body "Not yet sent" :attachments [])
     (matrix-compose-retain-capture surface (matrix-compose-capture surface)))
    (should-not (plist-get (appkit-surface-model surface) :uncertain-send))
    (should-not (plist-get (appkit-surface-model surface) :operation))
    (should (equal (appkit-chatbuf-input-string) "Not yet sent"))))

(ert-deftest matrix-ui-send-consent-cannot-authorize-a-reopened-room ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (appkit-chatbuf-input-replace "Captured before consent")
    (let ((state (matrix-ui-test--state "Unencrypted room"))
          replacement requested)
      (setf (plist-get (car (plist-get state :rooms)) :encrypted) :json-false)
      (appkit-app-send app state)
      (unwind-protect
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _)
                       (appkit-surface-stop surface)
                       (setq replacement (matrix-ui-test--open app buffer))
                       (setq-local matrix--registered-surface replacement)
                       (appkit-chatbuf-input-replace "Replacement draft")
                       t))
                    ((symbol-function 'matrix--request)
                     (lambda (&rest _) (setq requested t))))
            (should-error (matrix-room-send) :type 'user-error)
            (should-not requested)
            (should (equal (appkit-chatbuf-input-string) "Replacement draft")))
        (when replacement (appkit-surface-stop replacement))))))

(ert-deftest matrix-ui-send-acceptance-retains-edits-made-during-consent ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (appkit-chatbuf-input-replace "Original draft")
    (let ((state (matrix-ui-test--state "Unencrypted room"))
          context)
      (setf (plist-get (car (plist-get state :rooms)) :encrypted) :json-false)
      (appkit-app-send app state)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _)
                   (appkit-chatbuf-input-replace "Intervening edit")
                   (appkit-chatbuf-input-replace "Original draft")
                   t))
                ((symbol-function 'matrix-runtime-request)
                 (lambda (_app _method _params owned)
                   (setq context owned)
                   1)))
        (matrix-room-send))
      (matrix-ui-test--settle surface context)
      (should (equal (appkit-chatbuf-input-string) "Original draft"))
      (should-not (plist-get (appkit-surface-model surface) :operation)))))

(ert-deftest matrix-ui-reopening-room-preserves-reading-position ()
  (save-window-excursion
    (matrix-ui-test--with-room
      (switch-to-buffer buffer)
      (goto-char (point-min))
      (search-forward "Initial")
      (let ((reading-point (point)))
        (matrix-room-open app "!room:test" t)
        (should (= (point) reading-point))))))

(ert-deftest matrix-ui-wrapped-heading-keeps-avatar-on-the-first-line ()
  (save-window-excursion
    (matrix-ui-test--with-room
      (switch-to-buffer buffer)
      (delete-other-windows)
      (split-window-right 28)
      (appkit-surface-enable-responsive-geometry surface #'matrix--geometry)
      (let ((name "Long display name 中文名称 with a heading wider than the viewport"))
        (appkit-app-send
         app (matrix-ui-test--items-state
              (list (list :id "wrapped" :kind "message" :sender "@other:test"
                          :sender_name name :timestamp 1700000000000 :body "Message payload"))))
        (appkit-surface-send surface '(matrix-update timeline "!room:test"))
        (goto-char (point-min))
        (search-forward name)
        (let ((heading-prefix (get-text-property (line-beginning-position) 'line-prefix))
              (heading-wrap (get-text-property (line-beginning-position) 'wrap-prefix)))
          (re-search-forward "[0-9][0-9]:[0-9][0-9]")
          (let ((clock-prefix (get-text-property (line-beginning-position) 'line-prefix)))
            (search-forward "Message payload")
            (let ((body-prefix (get-text-property (line-beginning-position) 'line-prefix)))
              (should-not (equal heading-prefix body-prefix))
              (should (equal heading-wrap body-prefix))
              (should (equal clock-prefix body-prefix)))))))))

(ert-deftest matrix-ui-compact-multiline-clock-stays-with-the-first-content-line ()
  (matrix-ui-test--with-room
    (appkit-app-send
     app (matrix-ui-test--items-state
          '((:id "first" :kind "message" :sender "@other:test"
             :timestamp 1700000000000 :body "Earlier body")
            (:id "second" :kind "message" :sender "@other:test"
             :timestamp 1700000060000 :body "第一行正文\n第二行正文\n"))))
    (appkit-surface-send surface '(matrix-update timeline "!room:test"))
    (goto-char (appkit-chat-timeline-key-position "second"))
    (search-forward "第一行正文")
    (let ((first-line (line-number-at-pos)))
      (re-search-forward "[0-9][0-9]:[0-9][0-9]")
      (should (= first-line (line-number-at-pos)))
      (search-forward "第二行正文")
      (should (> (line-number-at-pos) first-line)))))

(ert-deftest matrix-ui-delivery-and-display-identity-changes-break-compaction ()
  (matrix-ui-test--with-room
    (let* ((first '(:id "first" :kind "message" :sender "@other:test"
                   :sender_name "Original identity" :timestamp 1700000000000
                   :body "Earlier body"))
           (second '(:id "second" :kind "message" :sender "@other:test"
                    :sender_name "Original identity" :timestamp 1700000060000
                    :body "Later body")))
      (appkit-app-send app (matrix-ui-test--items-state (list first second)))
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (goto-char (appkit-chat-timeline-key-position "second"))
      (should-not (search-forward "Original identity"
                                 (appkit-chat-timeline-footer-start-position) t))
      (dolist (change '((:send_state . "failed") (:sender_name . "Updated identity")
                        (:avatar_url . "mxc://test/new-avatar")))
        (let ((updated (plist-put (copy-sequence second) (car change) (cdr change))))
          (appkit-app-send app (matrix-ui-test--items-state (list first updated)))
          (appkit-surface-send surface '(matrix-update timeline "!room:test"))
          (goto-char (appkit-chat-timeline-key-position "second"))
          (should (search-forward (plist-get updated :sender_name)
                                  (appkit-chat-timeline-footer-start-position) t)))))))

(ert-deftest matrix-ui-pagination-fences-subscriptions-and-does-not-settle-send ()
  (matrix-ui-test--with-room
    (appkit-chatbuf-input-replace "Concurrent send")
    (let* ((send (matrix-ui-test--capture surface 1))
           (address (plist-get (appkit-surface-model surface) :address))
           (old (list :target address :subscription "subscription"
                      :history-owner (appkit-chat-history-request-start surface 'older)))
           (state (matrix-ui-test--state "Initial message"))
           (timeline (gethash "!room:test" (plist-get state :timelines))))
      (puthash "!room:test" (plist-put timeline :subscription_id "replacement")
               (plist-get state :timelines))
      (appkit-app-send app state)
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (let ((current (list :target address :subscription "replacement"
                           :history-owner (appkit-chat-history-request-start surface 'older))))
        (appkit-surface-send surface
                             (list 'matrix-result 1 "timeline.paginate" '(:hit_start t) nil old))
        (should (appkit-chat-history-loading-p))
        (should-not (appkit-chat-history-older-loaded-p))
        (appkit-surface-send surface
                             (list 'matrix-result 2 "timeline.paginate" nil
                                   '(:message "History unavailable") current))
        (should-not (appkit-chat-history-loading-p))
        (should (plist-get (appkit-surface-model surface) :history-error))
        (appkit-surface-send surface '(matrix-update timeline "!room:test"))
        (should (plist-get (appkit-surface-model surface) :history-error))
        (setq current
              (plist-put current :history-owner
                         (appkit-chat-history-request-start surface 'older)))
        (appkit-surface-send surface '(matrix-history-started))
        (appkit-surface-send surface
                             (list 'matrix-result 3 "timeline.paginate" '(:hit_start t) nil current))
        (should (appkit-chat-history-older-loaded-p))
        (should-not (appkit-chat-history-loading-p))
        (should-not (plist-get (appkit-surface-model surface) :history-error))
        (should (equal (plist-get (appkit-surface-model surface) :operation) send))
        (should (equal (appkit-chatbuf-input-string) "Concurrent send"))))))

(ert-deftest matrix-ui-pagination-preserves-anchor-behind-hidden-sdk-boundary ()
  (matrix-ui-test--with-room
    (appkit-chatbuf-input-replace "Unsubmitted draft")
    (goto-char (point-min))
    (search-forward "Initial")
    (let* ((offset (- (point) (appkit-chat-timeline-key-position "stable-row")))
           (state (matrix-ui-test--state "Initial message"))
           (timeline (gethash "!room:test" (plist-get state :timelines))))
      (puthash "!room:test"
               (plist-put timeline :items
                          (cons '(:id "hidden-edge" :kind "state" :body "m.room.member")
                                (cons '(:id "older" :kind "message" :body "Earlier message"
                                        :sender "@older:test")
                                      (plist-get timeline :items))))
               (plist-get state :timelines))
      (appkit-app-send app state)
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (should (equal (appkit-chat-history-window-first-key) "hidden-edge"))
      (should-not (member "hidden-edge" (appkit-chat-timeline-keys)))
      (should (equal (get-text-property (point) matrix-room-message-key-property) "stable-row"))
      (should (= offset (- (point) (appkit-chat-timeline-key-position "stable-row"))))
      (should (equal (appkit-chatbuf-input-string) "Unsubmitted draft")))))

(ert-deftest matrix-ui-empty-sdk-snapshot-is-not-history-exhaustion ()
  (matrix-ui-test--with-room
    (let* ((state (matrix-ui-test--state "Initial message"))
           (timelines (plist-get state :timelines)))
      (puthash "!room:test" (plist-put (gethash "!room:test" timelines) :items nil) timelines)
      (appkit-app-send app state)
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (should (appkit-chat-history-window-known-p))
      (should-not (appkit-chat-history-older-loaded-p))
      (should-not (appkit-chat-history-window-empty-p)))))

(ert-deftest matrix-ui-message-menu-resolves-captured-key-not-current-point ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (goto-char (appkit-chat-timeline-key-position "stable-row"))
    (let ((scope (matrix-menu--capture 'room t))
          (kill-ring nil))
      (appkit-chatbuf-input-replace "Private unsent draft")
      (appkit-chatbuf-focus-input)
      (appkit-app-send app (matrix-ui-test--state "Edited message"))
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (matrix-message-copy-text scope)
      (should (equal (car kill-ring) "Edited message"))
      (let* ((state (matrix-ui-test--state "Different message"))
             (timeline (gethash "!room:test" (plist-get state :timelines))))
        (setf (plist-get (car (plist-get timeline :items)) :id) "different-row")
        (appkit-app-send app state)
        (appkit-surface-send surface '(matrix-update timeline "!room:test"))
        (goto-char (appkit-chat-timeline-key-position "different-row"))
        (should-error (matrix-message-copy-text scope) :type 'user-error)
        (should (equal (car kill-ring) "Edited message"))))))

(ert-deftest matrix-ui-message-menu-rejects-reopened-host-with-same-key ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (goto-char (appkit-chat-timeline-key-position "stable-row"))
    (let ((scope (matrix-menu--capture 'room t))
          (kill-ring nil))
      (appkit-surface-stop surface)
      (setq surface (matrix-ui-test--open app buffer))
      (should-error (matrix-message-copy-text scope) :type 'user-error)
      (should-not kill-ring))))

(ert-deftest matrix-ui-directories-retire-completed-sas-actions ()
  (matrix-ui-test--with-room
    (let (directories)
      (unwind-protect
          (progn
            (dolist (spec `((,matrix-root--type root) (,matrix-verify--type verify)))
              (push (appkit-open-generated-surface
                     (car spec) :app app :identity (cdr spec) :input (cdr spec)
                     :buffer-name " *Matrix directory test*")
                    directories))
            (with-current-buffer (appkit-surface-buffer (cadr directories))
              (goto-char (point-min))
              (search-forward "Encrypted room")
              (should-error (insert "not editable")))
            (let* ((trust (car directories))
                   (state (matrix-ui-test--state "Initial message"))
                   (flows (plist-get state :verifications))
                   (flow '(:user_id "@me:test" :flow_id "flow" :state "sas"
                           :decimals (1234 5678 9012))))
              (puthash '("@me:test" . "flow") flow flows)
              (appkit-app-send app state)
              (appkit-surface-send trust '(matrix-update verification nil))
              (with-current-buffer (appkit-surface-buffer trust)
                (should (gethash '("@me:test" . "flow") matrix-verify--sas-shown))
                (should (appkit-directory-entry-for-key
                         (appkit-directory-surface) '(confirm "@me:test" "flow"))))
              (puthash '("@me:test" . "flow") (plist-put (copy-sequence flow) :state "done") flows)
              (appkit-surface-send trust '(matrix-update verification nil))
              (with-current-buffer (appkit-surface-buffer trust)
                (should-not (gethash '("@me:test" . "flow") matrix-verify--sas-shown))
                (should-not (appkit-directory-entry-for-key
                             (appkit-directory-surface) '(confirm "@me:test" "flow"))))))
        (dolist (directory directories)
          (let ((host (appkit-surface-buffer directory)))
            (appkit-surface-stop directory)
            (when (buffer-live-p host) (kill-buffer host))))))))

(ert-deftest matrix-ui-return-sends-captured-draft-only-once ()
  "Native and modal Return send multiline input through the real operation gate."
  (save-window-excursion
    (dolist (state (if (featurep 'evil) '(nil normal motion insert) '(nil)))
      (dolist (key '("RET" "<return>"))
        (matrix-ui-test--with-room
          (switch-to-buffer buffer)
          (setq-local matrix--registered-surface surface)
          (when (featurep 'evil)
            (evil-local-mode (if state 1 -1))
            (when state (funcall (intern (format "evil-%s-state" state)))))
          (appkit-chatbuf-input-replace "First line\nSecond line")
          (appkit-chatbuf-focus-input)
          (let (requests context)
            (cl-letf (((symbol-function 'matrix-runtime-request)
                       (lambda (_app method params owned)
                         (push (list method params) requests)
                         (setq context owned)
                         1)))
              (execute-kbd-macro (kbd key))
              (should (plist-get (appkit-surface-model surface) :operation))
              (should (equal (appkit-chatbuf-input-string) "First line\nSecond line"))
              (should-error (execute-kbd-macro (kbd key)) :type 'user-error)
              (should (= (length requests) 1))
              (matrix-ui-test--settle surface context)
              (should (equal (appkit-chatbuf-input-string) ""))
              (should-not (plist-get (appkit-surface-model surface) :operation)))))))))

(ert-deftest matrix-ui-return-on-timeline-preserves-reading-context ()
  (when (require 'evil nil t)
    (require 'matrix-evil)
    (matrix-evil-setup))
  (save-window-excursion
    (dolist (state (if (featurep 'evil) '(nil normal motion insert emacs) '(nil)))
      (dolist (key '("RET" "<return>"))
        (matrix-ui-test--with-room
          (switch-to-buffer buffer)
          (setq-local matrix--registered-surface surface)
          (appkit-chatbuf-input-replace "Unsubmitted")
          (when (featurep 'evil)
            (evil-local-mode (if state 1 -1))
            (when state (evil-change-state state)))
          (goto-char (point-min))
          (search-forward "Initial message")
          (goto-char (match-beginning 0))
          (appkit-chatbuf-update-context-mode)
          (let ((start (point)) (activated 0))
            (cl-letf (((symbol-function 'matrix-runtime-request)
                       (lambda (&rest _) (ert-fail "Timeline Return must not send"))))
              (execute-kbd-macro (kbd key))
              (should (= (point) start))
              (should-not (appkit-chatbuf-point-in-input-p))
              (should (equal (appkit-chatbuf-input-string) "Unsubmitted"))
              (when state (should (eq evil-state state)))
              (let ((inhibit-read-only t))
                (appkit-ui-add-action start (1+ start)
                                      (lambda () (cl-incf activated) nil)))
              (execute-kbd-macro (kbd key))
              (should (= activated 1))
              (should (= (point) start))
              (should (equal (appkit-chatbuf-input-string) "Unsubmitted"))
              (when state (should (eq evil-state state))))))))))

(defun matrix-ui-test--editable-state (body)
  "Return an account snapshot with a server-owned editable message."
  (let* ((state (matrix-ui-test--state body))
         (item (car (plist-get (gethash "!room:test" (plist-get state :timelines)) :items))))
    (setf (plist-get state :backend-active) t
          (plist-get item :sender) "@me:test"
          (plist-get item :own) t
          (plist-get item :event_id) "$owned:test"
          (plist-get item :can_edit) t
          (plist-get item :can_redact) t)
    (setf (plist-get (gethash "!room:test" (plist-get state :timelines)) :items)
          (list item))
    state))

(ert-deftest matrix-ui-evil-i-edits-exact-message-and-restores-ordinary-draft-on-acceptance ()
  (require 'evil) (require 'matrix-evil)
  (save-window-excursion
    (matrix-ui-test--with-room
      (setq-local matrix--registered-surface surface)
      (appkit-app-send app (matrix-ui-test--editable-state "Original message"))
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (appkit-chatbuf-input-replace "Unsent ordinary draft")
      (switch-to-buffer buffer)
      (evil-local-mode 1) (evil-normal-state)
      (goto-char (appkit-chat-timeline-key-position "stable-row"))
      (appkit-chatbuf-update-context-mode)
      (execute-kbd-macro (kbd "i"))
      (should (equal (appkit-chatbuf-input-string) "Original message"))
      (should (equal (plist-get (matrix-room--edit-state) :event-id) "$owned:test"))
      (should (eq evil-state 'insert))
      (appkit-chatbuf-input-replace "Edited message")
      (let (request context)
        (cl-letf (((symbol-function 'matrix-runtime-request)
                   (lambda (_app method params owned)
                     (setq request (list method params) context owned) 1)))
          (execute-kbd-macro (kbd "<return>")))
        (should (equal request
                       '("message.edit" (:room_id "!room:test" :subscription_id "subscription"
                                        :event_id "$owned:test" :body "Edited message"))))
        (should (equal (appkit-chatbuf-input-string) "Edited message"))
        (appkit-surface-send surface (list 'matrix-result 1 "message.edit" nil nil context))
        (should-not (matrix-room--edit-state))
        (should (equal (appkit-chatbuf-input-string) "Unsent ordinary draft"))))))

(ert-deftest matrix-ui-edit-error-and-late-acceptance-cannot-discard-newer-edits ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (appkit-app-send app (matrix-ui-test--editable-state "Original"))
    (appkit-surface-send surface '(matrix-update timeline "!room:test"))
    (appkit-chatbuf-input-replace "Saved ordinary draft")
    (goto-char (appkit-chat-timeline-key-position "stable-row"))
    (matrix-message-edit (matrix-menu--capture 'room t))
    (appkit-chatbuf-input-replace "Captured edit")
    (let (context)
      (cl-letf (((symbol-function 'matrix-runtime-request)
                 (lambda (_app _method _params owned) (setq context owned) 1)))
        (matrix-room-send))
      (appkit-surface-send surface
                           (list 'matrix-result 1 "message.edit" nil '(:message "Forbidden") context))
      (should (equal (appkit-chatbuf-input-string) "Captured edit"))
      (should (matrix-room--edit-state))
      (cl-letf (((symbol-function 'matrix-runtime-request)
                 (lambda (_app _method _params owned) (setq context owned) 2)))
        (matrix-room-send))
      (appkit-chatbuf-input-replace "Newer edit")
      (appkit-surface-send surface (list 'matrix-result 2 "message.edit" nil nil context))
      (should (equal (appkit-chatbuf-input-string) "Newer edit"))
      (should (matrix-room--edit-state))
      (matrix-room-cancel-edit)
      (should-not (matrix-room--edit-state))
      (should (equal (appkit-chatbuf-input-string) "Saved ordinary draft")))))

(ert-deftest matrix-ui-edit-permission-and-subscription-revocation-retain-draft ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (let* ((state (matrix-ui-test--editable-state "Original"))
           (timeline (gethash "!room:test" (plist-get state :timelines)))
           (item (car (plist-get timeline :items))))
      (setf (plist-get item :can_edit) nil)
      (appkit-app-send app state)
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (appkit-chatbuf-input-replace "Private draft")
      (goto-char (appkit-chat-timeline-key-position "stable-row"))
      (should-error (matrix-message-edit (matrix-menu--capture 'room t)) :type 'user-error)
      (should (equal (appkit-chatbuf-input-string) "Private draft"))
      (setf (plist-get item :can_edit) t)
      (appkit-app-send app state)
      (matrix-message-edit (matrix-menu--capture 'room t))
      (appkit-chatbuf-input-replace "Keep these edits")
      (setf (plist-get timeline :subscription_id) "replacement")
      (appkit-app-send app state)
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (cl-letf (((symbol-function 'matrix-runtime-request)
                 (lambda (&rest _) (ert-fail "Stale edit must not be sent"))))
        (should-error (matrix-room-send) :type 'user-error))
      (should (equal (appkit-chatbuf-input-string) "Keep these edits")))))

(ert-deftest matrix-ui-delete-revalidates-captured-message-after-confirmation ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (let* ((state (matrix-ui-test--editable-state "Selected message"))
           (item (car (plist-get (gethash "!room:test" (plist-get state :timelines)) :items)))
           (requests nil))
      (appkit-app-send app state)
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (appkit-chatbuf-input-replace "Unsent draft")
      (goto-char (appkit-chat-timeline-key-position "stable-row"))
      (let ((scope (matrix-menu--capture 'room t)))
        (cl-letf (((symbol-function 'matrix-runtime-request)
                   (lambda (_app method params _owned) (push (list method params) requests) 1))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
          (matrix-message-delete scope))
        (should-not requests)
        (cl-letf (((symbol-function 'matrix-runtime-request)
                   (lambda (&rest _) (ert-fail "Revoked permission must not redact")))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest _)
                     (setf (plist-get item :can_redact) nil)
                     (appkit-app-send app state) t)))
          (should-error (matrix-message-delete scope) :type 'user-error))
        (should (equal (appkit-chatbuf-input-string) "Unsent draft"))
        (setf (plist-get item :can_redact) t)
        (appkit-app-send app state)
        (let (context)
          (cl-letf (((symbol-function 'matrix-runtime-request)
                     (lambda (_app method params owned)
                       (push (list method params) requests) (setq context owned) 1))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (appkit-chatbuf-focus-input)
            (matrix-message-delete scope))
          (should (equal requests
                         '(("message.redact" (:room_id "!room:test" :subscription_id "subscription"
                                             :event_id "$owned:test")))))
          (appkit-surface-send surface (list 'matrix-result 1 "message.redact" nil nil context))
          (should (equal (appkit-chatbuf-input-string) "Unsent draft"))
          (should (equal (appkit-chat-timeline-keys) '("stable-row"))))))))

(ert-deftest matrix-ui-reaction-chips-and-command-toggle-the-exact-event ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (let ((requests nil))
      (appkit-app-send
       app (plist-put
            (matrix-ui-test--items-state
             (list (list :id "stable-row" :kind "message" :event_id "$e:test"
                         :sender "@other:test" :body "Hello" :can_react t
                         :reactions [(:key "👍" :count 2 :own t)
                                     (:key "🎉" :count 1 :own :json-false)])))
            :backend-active t))
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (goto-char (point-min))
      (search-forward "[👍 2]")
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'appkit-chat-reaction-selected))
      (cl-letf (((symbol-function 'matrix-runtime-request)
                 (lambda (_app method params _owned) (push (list method params) requests) 1)))
        (push-button (match-beginning 0))
        (appkit-surface-send surface (list 'matrix-result 1 "message.react" '(:added :json-false)
                                           nil (plist-get (appkit-surface-model surface) :operation)))
        (goto-char (appkit-chat-timeline-key-position "stable-row"))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "🎉")))
          (matrix-message-react (matrix-menu--capture 'room t))))
      (should (equal (nreverse requests)
                     '(("message.react" (:room_id "!room:test" :subscription_id "subscription"
                                         :event_id "$e:test" :key "👍"))
                       ("message.react" (:room_id "!room:test" :subscription_id "subscription"
                                         :event_id "$e:test" :key "🎉"))))))))

(ert-deftest matrix-ui-described-state-changes-render-as-notices ()
  (matrix-ui-test--with-room
    (let ((items (list '(:id "join" :kind "state" :described t :state_type "m.room.member"
                         :sender "@new:test" :body "Newcomer joined")
                       '(:id "topic" :kind "state" :described t :state_type "m.room.topic"
                         :sender "@other:test" :body "Robin changed the topic: Plans")
                       '(:id "rules" :kind "state" :sender "@other:test"
                         :body "m.room.join_rules")
                       '(:id "hello" :kind "message" :sender "@other:test" :body "Hello"))))
      (appkit-app-send app (matrix-ui-test--items-state items))
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (should (equal (appkit-chat-timeline-keys) '("join" "topic" "hello")))
      (should (string-match-p "─( Newcomer joined )─" (buffer-string)))
      (with-temp-buffer
        (appkit-chat-ins-insert-tip-row "Robin left" 'shadow 40 :time "12:00")
        (should (string-match-p "─ ( Robin left ) ─.* 12:00\n\\'" (buffer-string))))
      (should-not (string-match-p "join_rules" (buffer-string)))
      (let ((matrix-room-state-events 'no-membership))
        (appkit-surface-send surface '(matrix-update timeline "!room:test"))
        (should (equal (appkit-chat-timeline-keys) '("topic" "hello"))))
      (let ((matrix-room-state-events nil))
        (appkit-surface-send surface '(matrix-update timeline "!room:test"))
        (should (equal (appkit-chat-timeline-keys) '("hello")))))))

(ert-deftest matrix-ui-attachment-card-opens-a-private-downloaded-copy ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (let ((source (make-string 64 ?a))
          request opened)
      (appkit-app-send
       app (matrix-ui-test--items-state
            (list (list :id "file" :kind "message" :event_id "$f:test"
                        :sender "@other:test" :body "../notes.txt"
                        :attachment (list :kind "file" :source_id source
                                          :filename "../notes.txt" :size 2048)))))
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (should (string-match-p "\\[file\\] \\.\\./notes\\.txt (2k)" (buffer-string)))
      (should-not (string-match-p "^\\.\\./notes\\.txt$" (buffer-string)))
      (goto-char (point-min))
      (search-forward "[file]")
      (cl-letf (((symbol-function 'matrix-runtime-resource-request)
                 (lambda (_app method params resolve _reject)
                   (setq request (list method params))
                   (funcall resolve nil)))
                (matrix-attachment-open-function
                 (lambda (file kind) (setq opened (list file kind)))))
        (call-interactively (key-binding (kbd "RET"))))
      (should (equal (car request) "media.save"))
      (let ((path (plist-get (cadr request) :path)))
        (should (equal (file-name-nondirectory path) "notes.txt"))
        (should (string-prefix-p (file-name-as-directory matrix-media--open-directory) path))
        (should (equal (plist-get (cadr request) :source_id) source))
        (should (equal opened (list path "file")))))))

(ert-deftest matrix-ui-failed-send-offers-retry-and-confirmed-discard ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (let (requests)
      (appkit-app-send
       app (plist-put
            (matrix-ui-test--items-state
             (list (list :id "local" :kind "message" :transaction_id "txn-1" :own t
                         :sender "@me:test" :body "Hello" :send_state "failed"
                         :error "Not sent: some recipients have unverified devices"
                         :can_retry t :can_discard t)))
            :backend-active t))
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (should (string-match-p "unverified devices\n\\[Retry\\] \\[Discard\\]" (buffer-string)))
      (cl-letf (((symbol-function 'matrix-runtime-request)
                 (lambda (_app method params owned)
                   (push (list method params) requests)
                   (appkit-surface-send surface (list 'matrix-result 1 method nil nil owned))
                   1))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (goto-char (point-min))
        (search-forward "[Discard]")
        (push-button (match-beginning 0))
        (should-not requests)
        (search-backward "[Retry]")
        (push-button (point)))
      (should (equal requests '(("message.retry" (:room_id "!room:test"
                                                  :subscription_id "subscription"
                                                  :transaction_id "txn-1"))))))))

(ert-deftest matrix-ui-mention-completion-sends-a-pill-segment ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (setq matrix-mention--members
          (cons (float-time) '((:user_id "@alice:test" :display_name "Alice")
                               (:user_id "@me:test" :display_name "Me"))))
    (appkit-chatbuf-input-replace "hi @Al")
    (goto-char (point-max))
    (let* ((capf (matrix-mention-capf))
           (labels (all-completions "@" (nth 2 capf))))
      (should (member "@Alice" labels))
      (should-not (member "@Me" labels))
      (should (member "@other:test" labels))
      (delete-region (nth 0 capf) (nth 1 capf))
      (insert "@Alice")
      (funcall (plist-get (nthcdr 3 capf) :exit-function) "@Alice" 'finished))
    (goto-char (point-max))
    (insert "!")
    (appkit-chatbuf-input-state-sync)
    (let ((params (plist-get (matrix-compose-capture surface) :params)))
      (should (equal (plist-get params :body) "hi Alice !"))
      (should (equal (plist-get params :segments)
                     [(:text "hi ") (:text "Alice" :user_id "@alice:test")
                      (:text " ") (:text "!")])))))

(ert-deftest matrix-ui-room-commands-request-exact-room-changes ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (appkit-app-send app (plist-put (matrix-ui-test--state "Hi") :backend-active t))
    (let (requests)
      (cl-letf (((symbol-function 'matrix-runtime-request)
                 (lambda (_app method params owned)
                   (push (list method params) requests)
                   (appkit-surface-send surface (list 'matrix-result 1 method nil nil owned))
                   1))
                ((symbol-function 'yes-or-no-p) (lambda (prompt)
                                                  (should (string-match-p "Encrypted room" prompt))
                                                  t)))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "not-a-user")))
          (should-error (call-interactively #'matrix-room-invite) :type 'user-error))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) " @bob:test ")))
          (call-interactively #'matrix-room-invite))
        (matrix-room-set-topic "  Plans  ")
        (matrix-room-leave))
      (should (equal (nreverse requests)
                     '(("room.invite" (:room_id "!room:test" :user_id "@bob:test"))
                       ("room.set_topic" (:room_id "!room:test" :text "Plans"))
                       ("room.leave" (:room_id "!room:test"))))))))

(ert-deftest matrix-ui-typing-is-shown-and-sent-while-composing ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (let ((state (matrix-ui-test--state "Hi")) sent)
      (plist-put (gethash "!room:test" (plist-get state :timelines)) :typing '("Alice" "Bob"))
      (appkit-app-send app state)
      (appkit-surface-send surface '(matrix-update typing "!room:test"))
      (should (string-match-p "Alice and Bob are typing…" matrix--header-line))
      (cl-letf (((symbol-function 'matrix-runtime-resource-request)
                 (lambda (_app method params &rest _) (push (list method params) sent))))
        (appkit-chatbuf-input-replace "dr")
        (matrix-room--notice-typing)
        (appkit-chatbuf-input-replace "draft")
        (matrix-room--notice-typing)
        (appkit-chatbuf-input-replace "")
        (matrix-room--notice-typing)
        (let ((matrix-send-typing-notices nil))
          (appkit-chatbuf-input-replace "private")
          (matrix-room--notice-typing)))
      (should (equal (nreverse sent)
                     '(("room.typing" (:room_id "!room:test" :typing t))
                       ("room.typing" (:room_id "!room:test" :typing :json-false))))))))

(ert-deftest matrix-ui-fastnav-follows-telega-keys ()
  (dolist (binding '(("M-g r" . matrix-room-read-all) ("M-g >" . matrix-room-read-all)
                     ("M-g u" . matrix-room-goto-unread) ("M-g @" . matrix-room-next-mention)
                     ("M-g m" . matrix-room-next-mention) ("M-g o" . matrix-room-next-outgoing)))
    (should (eq (keymap-lookup matrix-room-mode-map (car binding)) (cdr binding))))
  (should (eq (keymap-lookup matrix-root-mode-map "M-g u") #'matrix-root-next-unread))
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (appkit-app-send
     app (matrix-ui-test--items-state
          (list '(:id "a" :kind "message" :sender "@other:test" :body "A")
                '(:id "mine" :kind "message" :sender "@me:test" :own t :body "Mine")
                '(:id "ping" :kind "message" :sender "@other:test" :highlighted t :body "@me")
                '(:id "z" :kind "message" :sender "@other:test" :body "Z"))))
    (appkit-surface-send surface '(matrix-update timeline "!room:test"))
    (goto-char (appkit-chat-timeline-key-position "a"))
    (matrix-room-next-mention)
    (should (equal (get-text-property (point) matrix-room-message-key-property) "ping"))
    (matrix-room-next-outgoing)
    (should (equal (get-text-property (point) matrix-room-message-key-property) "mine"))))

(ert-deftest matrix-ui-notice-people-are-coloured-links-to-their-pages ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (let (opened)
      (appkit-app-send
       app (matrix-ui-test--items-state
            (list '(:id "kick" :kind "state" :described t :state_type "m.room.member"
                    :sender "@mod:test" :body "Mod removed Newcomer: spam"
                    :parts [(:user_id "@mod:test" :name "Mod") (:text " removed ")
                            (:user_id "@new:test" :name "Newcomer") (:text ": spam")])
                  '(:id "hi" :kind "message" :sender "@other:test" :body "Hi"))))
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (should (string-match-p "( Mod removed Newcomer: spam )" (buffer-string)))
      (goto-char (point-min))
      (search-forward "Newcomer")
      (should (memq (appkit-name-color-face "@new:test")
                    (ensure-list (get-text-property (match-beginning 0) 'face))))
      (cl-letf (((symbol-function 'matrix-user-open)
                 (lambda (_app user-id &rest _) (push user-id opened))))
        (goto-char (match-beginning 0))
        (matrix-room-open-user-at-point)
        (push-button (match-beginning 0))
        (goto-char (appkit-chat-timeline-key-position "hi"))
        (matrix-room-open-user-at-point))
      (should (equal (nreverse opened) '("@new:test" "@new:test" "@other:test"))))))

(ert-deftest matrix-ui-user-page-shows-the-loaded-profile ()
  (matrix-ui-test--with-room
    (appkit-app-send app (plist-put (matrix-ui-test--state "Hi") :backend-active t))
    (let (user-surface)
      ;; This fixture App would take registration messages as its model.
      (cl-letf (((symbol-function 'matrix-runtime-register-surface) #'ignore)
                ((symbol-function 'matrix-runtime-unregister-surface) #'ignore)
                ((symbol-function 'matrix-runtime-request)
                 (lambda (_app method params owned)
                   (should (equal (list method params)
                                  '("user.profile" (:user_id "@alice:test"))))
                   (run-at-time 0 nil
                                (lambda ()
                                  (appkit-surface-send
                                   user-surface
                                   (list 'matrix-result 1 method
                                         '(:user_id "@alice:test" :display_name "Alice"
                                           :self :json-false :verified t :devices 2
                                           :rooms [(:room_id "!room:test" :name "Encrypted room")])
                                         nil owned))))
                   1)))
        (setq user-surface (matrix-user-open app "@alice:test"))
        (unwind-protect
            (with-current-buffer (appkit-surface-buffer user-surface)
              (should (string-match-p "Loading user profile" (buffer-string)))
              (accept-process-output nil 0.05)
              (let ((text (buffer-string)))
                (should (string-match-p "\\`.*Alice\n.*@alice:test" text))
                (should (string-match-p "Message  .*Copy ID" text))
                (should (string-match-p "Identity:   Verified" text))
                (should (string-match-p "Devices:    2" text))
                (should (string-match-p "Rooms in common (1)\n  Encrypted room" text))))
          (kill-buffer (appkit-surface-buffer user-surface)))))))

(ert-deftest matrix-ui-read-marker-projects-stable-canonical-boundary ()
  (matrix-ui-test--with-room
    (let* ((appkit-chat-show-unread-divider t)
           (before '(:id "before" :kind "message" :event_id "$z:test" :can_read t
                     :sender "@other:test" :timestamp 1000 :body "Before"))
           (after '(:id "after" :kind "message" :event_id "$a:test" :can_read t
                    :sender "@other:test" :timestamp 2000 :body "After"))
           (marker '(:id "marker" :kind "read_marker"))
           (day '(:id "day" :kind "day" :timestamp 2000)))
      (appkit-chatbuf-input-replace "Keep draft")
      (appkit-app-send app (matrix-ui-test--items-state
                           (list before marker day after) '(:first_unread_index 3)))
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (should (equal (appkit-chat-timeline-keys) '("before" "day" "after")))
      (should (equal matrix-room--unread-key "after"))
      (should (plist-get (appkit-chat-timeline-row-context
                         (appkit-chat-timeline-row "after")) :unread))
      (should-not (plist-get (appkit-chat-timeline-row-context
                             (appkit-chat-timeline-row "after")) :compact))
      ;; A missing/moved marker alone is not authoritative complete-read evidence.
      (appkit-app-send app (matrix-ui-test--items-state (list before after marker)))
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (should (equal matrix-room--unread-key "after"))
      (should (equal (appkit-chatbuf-input-string) "Keep draft"))
      (goto-char (text-property-any (point-min) (point-max) 'appkit-chat-read-divider t))
      (should-not (appkit-chat-read-target))
      (should-not (matrix-room--first-unread-key (list :items (list before after))))
      (appkit-app-send app (matrix-ui-test--items-state (list before after)))
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (should (equal matrix-room--unread-key "after"))
      ;; Another client's confirmed frontier clears even while old marker rows remain.
      (appkit-app-send app (matrix-ui-test--items-state
                           (list before marker after) '(:all_read t)))
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (should-not matrix-room--unread-key)
      (should-not (text-property-any (point-min) (point-max) 'appkit-chat-read-divider t))
      (should-not (plist-get (appkit-chat-timeline-row-context
                             (appkit-chat-timeline-row "after")) :unread))
      ;; New traffic with an unresolved position cannot revive the old marker.
      (appkit-app-send app (matrix-ui-test--items-state (list before marker after)))
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (should-not matrix-room--unread-key)
      (let ((new '(:id "new" :kind "message" :event_id "$new:test"
                   :can_read t :sender "@other:test" :body "New unread")))
        (appkit-app-send app (matrix-ui-test--items-state
                             (list before marker after new) '(:first_unread_index 3)))
        (appkit-surface-send surface '(matrix-update timeline "!room:test"))
        (should (equal matrix-room--unread-key "new")))
      (should (equal (appkit-chatbuf-input-string) "Keep draft")))))

(ert-deftest matrix-ui-exact-count-is-independent-of-notification-badge ()
  (matrix-ui-test--with-room
    (let* ((appkit-chat-show-unread-divider t)
           (items '((:id "before" :kind "message" :event_id "$z:test" :can_read t
                     :sender "@other:test" :timestamp 1000 :body "Before")
                    (:id "marker" :kind "read_marker")
                    (:id "after" :kind "message" :event_id "$a:test" :can_read t
                     :sender "@other:test" :timestamp 2000 :body "After"))))
      (appkit-chatbuf-input-replace "Keep draft")
      (dolist (count '(2 1 nil 0))
        (let* ((state (matrix-ui-test--items-state
                       items (list :unread_messages count :first_unread_index 2)))
               (room (car (plist-get state :rooms))))
          (setf (plist-get room :unread) 0)
          (appkit-app-send app state)
          (appkit-surface-send surface '(matrix-update timeline "!room:test"))
          (if (eql count 0)
              (progn
                (should-not matrix-room--unread-key)
                (should-not (text-property-any (point-min) (point-max)
                                              'appkit-chat-read-divider t)))
            (should (equal matrix-room--unread-key "after")))
          (should (string-match-p "\\[E2EE\\]" matrix--header-line))
          (should (string-match-p "· Encrypted room" matrix--header-line))
          (should-not (string-match-p "SDK enforces" matrix--header-line))
          (should (equal (appkit-chatbuf-input-string) "Keep draft")))))))

(ert-deftest matrix-ui-read-through-targets-loaded-event-without-owning-send ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (let* ((state (matrix-ui-test--items-state
                   '((:id "old" :kind "message" :event_id "$z:test" :can_read t :body "Old")
                     (:id "new" :kind "message" :event_id "$a:test" :can_read t :body "New")
                     (:id "echo" :kind "message" :body "Pending" :send_state "pending"))))
           requests context)
      (setq state (plist-put state :backend-active t))
      (appkit-app-send app state)
      (appkit-surface-send surface '(matrix-update timeline "!room:test"))
      (appkit-chatbuf-input-replace "Durable send draft")
      (let ((send (matrix-ui-test--capture surface 7)))
        (cl-letf (((symbol-function 'matrix-runtime-request)
                   (lambda (_app method params owned)
                     (push (list method params) requests) (setq context owned) 1)))
          (appkit-chatbuf-focus-input)
          (matrix-room-read-through)
          (should (equal (plist-get (cadar requests) :event_id) "$a:test"))
          (goto-char (appkit-chat-timeline-key-position "old"))
          (matrix-room-read-through)
          (should (equal (plist-get (cadar requests) :event_id) "$z:test"))
          (goto-char (appkit-chat-timeline-key-position "echo"))
          (should-error (matrix-room-read-through) :type 'user-error))
        (appkit-surface-send surface (list 'matrix-result 1 "timeline.read" nil nil context))
        (should (equal (plist-get (appkit-surface-model surface) :operation) send))
        (should (equal (appkit-chatbuf-input-string) "Durable send draft"))
        (setq context (plist-put context :subscription "replaced"))
        (appkit-surface-send surface
                             (list 'matrix-result 2 "timeline.read" nil
                                   '(:message "Stale result") context))
        (should (equal (plist-get (appkit-surface-model surface) :operation) send))))))

(provide 'matrix-ui-test)
;;; matrix-ui-test.el ends here
