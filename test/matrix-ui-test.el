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
          :rooms '((:room_id "!room:test" :name "Encrypted room"
                    :membership "join" :encrypted t))
          :timelines timelines :verifications (make-hash-table :test #'equal))))

(defun matrix-ui-test--items-state (items)
  "Return an account snapshot whose authoritative timeline contains ITEMS."
  (let* ((state (matrix-ui-test--state ""))
         (timelines (plist-get state :timelines))
         (timeline (gethash "!room:test" timelines)))
    (puthash "!room:test" (plist-put timeline :items items) timelines)
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
  (let ((context (list :target (plist-get (appkit-surface-model surface) :address)
                       :method "message.send" :token token
                       :revision (appkit-chatbuf-composer-revision)
                       :body (substring-no-properties (appkit-chatbuf-input-state)))))
    (appkit-surface-send surface (list 'matrix-operation context))
    context))

(defun matrix-ui-test--settle (surface context &optional error)
  "Deliver an actual result message to SURFACE for CONTEXT and optional ERROR."
  (appkit-surface-send surface
                       (list 'matrix-result 1 "message.send"
                             '(:transaction_id "sdk-transaction") error context)))

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
    (matrix--request surface "message.send" '(:room_id "!room:test" :body "Not yet sent")
                     (list :revision (appkit-chatbuf-composer-revision) :body "Not yet sent"))
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

(provide 'matrix-ui-test)
;;; matrix-ui-test.el ends here
