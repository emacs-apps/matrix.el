;;; matrix-ui-test.el --- Consumer-visible draft boundaries -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'matrix-room)

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
      (should (string-match-p "Accepted into SDK send queue"
                              (plist-get (appkit-surface-model surface) :notice)))
      (appkit-chatbuf-input-replace "Next message")
      (matrix-ui-test--settle surface context)
      (should (equal (appkit-chatbuf-input-string) "Next message")))))

(ert-deftest matrix-ui-failure-and-late-acceptance-preserve-edits ()
  (matrix-ui-test--with-room
    (appkit-chatbuf-input-replace "Original")
    (let ((context (matrix-ui-test--capture surface 1)))
      (matrix-ui-test--settle surface context '(:message "Unable to encrypt for unverified device"))
      (should (equal (appkit-chatbuf-input-string) "Original"))
      (should (string-match-p "Unable to encrypt"
                              (plist-get (appkit-surface-model surface) :notice))))
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

(provide 'matrix-ui-test)
;;; matrix-ui-test.el ends here
