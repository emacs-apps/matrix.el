;;; matrix-reply-test.el --- Reply identity and draft boundaries -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'button)
(require 'matrix-room)
(require 'matrix-reply)

(defun matrix-reply-test--item (id event body &optional reply)
  "Create a replyable SDK-shaped message with ID, EVENT, BODY and REPLY."
  (list :id id :event_id event :kind "message" :sender "@alice:test"
        :sender_name "Alice" :body body :can_reply t :reply reply))

(defun matrix-reply-test--state (items &optional subscription)
  "Return an active account snapshot with ITEMS in SUBSCRIPTION."
  (let ((timelines (make-hash-table :test #'equal)))
    (puthash "!room:test" (list :subscription_id (or subscription "sub")
                              :revision 1 :items items) timelines)
    (list :account "reply-test" :backend-active t :connection 'online
          :session '(:user_id "@me:test" :device_id "DEVICE")
          :rooms '((:room_id "!room:test" :name "Room" :encrypted t))
          :timelines timelines)))

(defun matrix-reply-test--publish (app surface items &optional subscription)
  "Commit ITEMS and reproject their visible rows on SURFACE."
  (appkit-app-send app (matrix-reply-test--state items subscription))
  (appkit-surface-send surface '(matrix-update timeline "!room:test")))

(defmacro matrix-reply-test--with-room (items &rest body)
  "Run BODY with a real room Surface initially displaying ITEMS."
  (declare (indent 1) (debug t))
  `(let (app surface buffer)
     (unwind-protect
         (progn
           (setq app
                 (appkit-app-start
                  (appkit-app-type-create
                   :name 'matrix-reply-test
                   :init (lambda (_context input)
                           (appkit-next :model input :render appkit-render-none))
                   :update (lambda (_context _model input)
                             (appkit-next :model input :render appkit-render-none)))
                  :input (matrix-reply-test--state ,items)))
           (setq surface
                 (appkit-open-generated-surface
                  matrix-room--type :app app :identity '(room "!room:test")
                  :input '(room "!room:test") :buffer-name " *Matrix reply test*"))
           (setq buffer (appkit-surface-buffer surface))
           (with-current-buffer buffer
             (setq-local matrix--registered-surface surface)
             ,@body))
       (when surface (appkit-surface-stop surface))
       (when app (appkit-app-close app))
       (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun matrix-reply-test--quote-position ()
  "Return the exact rendered reply action position."
  (text-property-not-all (point-min) (point-max) 'matrix-reply-context nil))

(defun matrix-reply-test--rich-draft ()
  "Install a property-rich draft with an actual structured attachment object."
  (appkit-chatbuf-input-replace
   (concat (propertize "Caption " 'face 'bold)
           (appkit-chatbuf-input-object-string
            "[image.png]" '(:type matrix-attachment :id "attachment-1"
                            :path "/tmp/image.png" :filename "image.png"
                            :mime-type "image/png"))))
  (appkit-chatbuf-input-state-sync)
  (copy-sequence (appkit-chatbuf-input-state)))

(ert-deftest matrix-reply-preview-transitions-follow-current-sdk-details ()
  (matrix-reply-test--with-room
      (list (matrix-reply-test--item
             "reply" "$reply" "Answer"
             '(:event_id "$original" :status "pending")))
    (should (string-match-p "Loading replied message" (buffer-string)))
    (let ((ready (list :event_id "$original" :status "ready" :kind "message"
                       :sender "@alice:test" :sender_name "Alice"
                       :body (propertize "First\nline\tsecond" 'display "FORGED"))))
      (matrix-reply-test--publish
       app surface (list (matrix-reply-test--item "reply" "$reply" "Answer" ready)))
      (should (string-match-p "Alice: First line second" (buffer-string)))
      (let ((position (matrix-reply-test--quote-position)))
        (should (button-at position))
        (save-excursion
          (goto-char position)
          (should-not (text-property-not-all position (line-end-position) 'display nil))))
      (matrix-reply-test--publish
       app surface
       (list (matrix-reply-test--item
              "reply" "$reply" "Answer" (plist-put (copy-sequence ready) :body "Edited quote"))))
      (should (string-match-p "Edited quote" (buffer-string)))
      (should-not (string-match-p "First line second" (buffer-string)))
      (dolist (case '(("ready" "redacted" "Message redacted")
                      ("ready" "encrypted" "Unable to decrypt encrypted message")
                      ("unavailable" "message" "Replied message unavailable")
                      ("error" "message" "details failed to load")))
        (let ((details (copy-sequence ready)))
          (setq details (plist-put details :status (nth 0 case))
                details (plist-put details :kind (nth 1 case)))
          (matrix-reply-test--publish
           app surface (list (matrix-reply-test--item "reply" "$reply" "Answer" details)))
          (should (string-match-p (nth 2 case) (buffer-string)))
          (should (matrix-reply-test--quote-position))
          (should-not (string-match-p "First line second" (buffer-string))))))))

(ert-deftest matrix-reply-plain-fallback-does-not-invent-a-relationship ()
  (matrix-reply-test--with-room
      (list (matrix-reply-test--item
             "plain" "$plain" "> <@alice:test> original\n\nAnswer"))
    (should-not (matrix-reply-test--quote-position))
    (goto-char (appkit-chat-timeline-key-position "plain"))
    (should-error (matrix-reply-goto) :type 'user-error)))

(ert-deftest matrix-reply-quote-follows-exact-loaded-key-without-submitting-draft ()
  (let ((original (matrix-reply-test--item "original-key" "$original" "Original"))
        (reply (matrix-reply-test--item
                "reply-key" "$reply" "Answer"
                '(:event_id "$original" :status "ready" :kind "message"
                  :sender "@alice:test" :body "Original"))))
    (matrix-reply-test--with-room (list original reply)
      (let ((draft (matrix-reply-test--rich-draft)))
        (matrix-room-begin-reply surface reply)
        (let ((aux (copy-tree (appkit-chatbuf-aux-state))))
          (goto-char (matrix-reply-test--quote-position))
          (push-button)
          (should (equal (get-text-property (point) matrix-room-message-key-property)
                         "original-key"))
          (should (equal-including-properties (appkit-chatbuf-input-state) draft))
          (should (equal (appkit-chatbuf-aux-state) aux))
          (should-not (plist-get (appkit-surface-model surface) :operation)))))))

(ert-deftest matrix-reply-unloaded-target-gives-exact-permalink-without-jumping ()
  (matrix-reply-test--with-room
      (list (matrix-reply-test--item
             "reply-key" "$reply" "Answer" '(:event_id "$missing" :status "unavailable")))
    (appkit-chatbuf-input-replace "Unsubmitted")
    (goto-char (matrix-reply-test--quote-position))
    (let ((position (point)) notice)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq notice (apply #'format format-string args)))))
        (should-not (matrix-reply-goto)))
      (should (= position (point)))
      (should (string-match-p "Load older" notice))
      (should (string-match-p (regexp-quote "https://matrix.to/#/%21room%3Atest/%24missing") notice))
      (should (equal (appkit-chatbuf-input-state) "Unsubmitted")))))

(ert-deftest matrix-reply-begin-and-cancel-preserve-rich-draft-and-context ()
  (let ((original (matrix-reply-test--item "original-key" "$original" "Original")))
    (matrix-reply-test--with-room (list original)
      (let ((draft (matrix-reply-test--rich-draft)))
        (matrix-room-begin-reply surface original)
        (should (appkit-chatbuf-point-in-input-p))
        (should (equal-including-properties (appkit-chatbuf-input-state) draft))
        (should (equal (matrix-reply-validate surface (appkit-chatbuf-aux-state)) "$original"))
        (should (equal (plist-get (appkit-chatbuf-aux-state) :message-id) "original-key"))
        (matrix-reply-test--publish
         app surface (list (plist-put (copy-sequence original) :body "Edited original")))
        (should (string-match-p "Edited original" (matrix-reply-composer-text 80)))
        (matrix-reply-cancel)
        (should-not (appkit-chatbuf-aux-state))
        (should (equal-including-properties (appkit-chatbuf-input-state) draft))))))

(ert-deftest matrix-reply-stale-row-and-subscription-cannot-retarget-draft ()
  (let ((original (matrix-reply-test--item "key" "$original" "Original")))
    (matrix-reply-test--with-room (list original)
      (let ((draft (matrix-reply-test--rich-draft)))
        (matrix-room-begin-reply surface original)
        (let ((aux (copy-tree (appkit-chatbuf-aux-state))))
          ;; Reusing the row key for a different event is not a new target.
          (matrix-reply-test--publish
           app surface (list (matrix-reply-test--item "key" "$replacement" "Replacement")))
          (should-error (matrix-room-begin-reply surface original) :type 'user-error)
          (should-error (matrix-reply-validate surface aux) :type 'user-error)
          (should (equal (appkit-chatbuf-aux-state) aux))
          (matrix-reply-test--publish app surface (list original) "successor-sub")
          (should-error (matrix-reply-validate surface aux) :type 'user-error)
          (should (equal-including-properties (appkit-chatbuf-input-state) draft)))))))

(ert-deftest matrix-reply-captured-quote-cannot-follow-mutated-relationship ()
  (let ((original (matrix-reply-test--item "original" "$original" "Original"))
        (replacement (matrix-reply-test--item "replacement" "$replacement" "Other"))
        (reply (matrix-reply-test--item
                "reply" "$reply" "Answer" '(:event_id "$original" :status "ready"))))
    (matrix-reply-test--with-room (list original replacement reply)
      (let* ((button (button-at (matrix-reply-test--quote-position)))
             (action (button-get button 'action))
             (draft (matrix-reply-test--rich-draft)))
        (matrix-reply-test--publish
         app surface
         (list original replacement
               (plist-put (copy-sequence reply) :reply
                          '(:event_id "$replacement" :status "ready"))))
        (appkit-chatbuf-focus-input)
        (let ((position (point)))
          (should-error (funcall action button) :type 'user-error)
          (should (= position (point))))
        (should (equal-including-properties (appkit-chatbuf-input-state) draft))))))

(ert-deftest matrix-reply-active-edit-and-lost-capability-preserve-context ()
  (let ((original (matrix-reply-test--item "key" "$original" "Original")))
    (matrix-reply-test--with-room (list original)
      (let ((draft (matrix-reply-test--rich-draft)))
        (appkit-chatbuf-aux-set '(:aux-type edit :message-id "edit"))
        (should-error (matrix-room-begin-reply surface original) :type 'user-error)
        (should (eq (appkit-chatbuf-aux-type) 'edit))
        (appkit-chatbuf-aux-reset)
        (matrix-room-begin-reply surface original)
        (let ((aux (copy-tree (appkit-chatbuf-aux-state))))
          (matrix-reply-test--publish
           app surface (list (plist-put (copy-sequence original) :can_reply nil)))
          (should-error (matrix-reply-validate surface aux) :type 'user-error)
          (should (equal (appkit-chatbuf-aux-state) aux))
          (should (equal-including-properties (appkit-chatbuf-input-state) draft)))))))

(ert-deftest matrix-reply-pending-operation-cannot-change-or-cancel-target ()
  (let ((original (matrix-reply-test--item "key" "$original" "Original")))
    (matrix-reply-test--with-room (list original)
      (let ((draft (matrix-reply-test--rich-draft)))
        (matrix-room-begin-reply surface original)
        (let ((aux (copy-tree (appkit-chatbuf-aux-state))))
          (appkit-surface-send
           surface (list 'matrix-operation
                         (list :target (plist-get (appkit-surface-model surface) :address)
                               :method "message.send" :token 1)))
          (should-error (matrix-room-begin-reply surface original) :type 'user-error)
          (should-error (matrix-reply-validate surface aux) :type 'user-error)
          (should-error (matrix-reply-cancel) :type 'user-error)
          (should (equal (appkit-chatbuf-aux-state) aux))
          (should (equal-including-properties (appkit-chatbuf-input-state) draft)))))))

(ert-deftest matrix-reply-successor-surface-rejects-captured-target ()
  (let ((original (matrix-reply-test--item "key" "$original" "Original")))
    (matrix-reply-test--with-room (list original)
      (matrix-room-begin-reply surface original)
      (let ((aux (copy-tree (appkit-chatbuf-aux-state))))
        (appkit-surface-stop surface)
        (setq surface
              (appkit-open-generated-surface
               matrix-room--type :app app :identity '(room "!room:test")
               :input '(room "!room:test") :buffer buffer))
        (setq-local matrix--registered-surface surface)
        (let ((draft (matrix-reply-test--rich-draft)))
          (appkit-chatbuf-aux-set aux)
          (should-error (matrix-reply-validate surface aux) :type 'user-error)
          (should (equal-including-properties (appkit-chatbuf-input-state) draft)))))))

(provide 'matrix-reply-test)
;;; matrix-reply-test.el ends here
