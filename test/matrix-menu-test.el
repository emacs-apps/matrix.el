;;; matrix-menu-test.el --- Captured menu boundaries -*- lexical-binding: t; -*-

;;; Code:

(require 'matrix-ui-test)

(defmacro matrix-menu-test--with-scope (scope &rest body)
  "Run BODY with SCOPE on a native Transient prefix object."
  (declare (indent 1) (debug t))
  `(let ((transient-current-prefix
          (make-instance 'transient-prefix :command 'matrix-room-transient :scope ,scope))
         (transient--prefix nil))
     ,@body))

(defun matrix-menu-test--publish (app surface state)
  "Commit STATE to APP and reconcile the actual SURFACE."
  (appkit-app-send app state)
  (appkit-surface-send surface '(matrix-update timeline "!room:test")))

(ert-deftest matrix-menu-inheritance-never-consults-another-buffer-or-generation ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (goto-char (appkit-chat-timeline-key-position "stable-row"))
    (let ((scope (matrix-menu--capture 'room t)))
      (appkit-chatbuf-focus-input)
      (matrix-menu-test--with-scope scope
        (with-temp-buffer
          (should (eq (matrix-menu--capture 'room t) scope))
          (should-not (matrix-menu--text-inapt-p)))
        (appkit-surface-stop surface)
        (setq surface (matrix-ui-test--open app buffer))
        (should (matrix-menu--view-inapt-p))
        (should (matrix-menu--text-inapt-p))
        (should-error (matrix-menu--capture 'room t) :type 'user-error)
        (should-error (matrix-account-transient) :type 'user-error)))))

(ert-deftest matrix-menu-direct-invocation-captures-without-an-active-transient ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (let ((transient-current-prefix nil)
          (transient--prefix nil))
      (goto-char (appkit-chat-timeline-key-position "stable-row"))
      (let ((scope (matrix-menu--capture 'room t)))
        (should (eq (matrix-menu-scope-surface scope) surface))
        (should (equal (matrix-menu-scope-item-key scope) "stable-row"))
        (should-not (matrix-menu-scope-input-p scope)))
      (appkit-chatbuf-focus-input)
      (should (matrix-menu-scope-input-p (matrix-menu--capture 'room)))
      (should-error (matrix-menu--capture 'room t) :type 'user-error))))

(ert-deftest matrix-menu-message-capabilities-follow-canonical-sdk-transitions ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (goto-char (appkit-chat-timeline-key-position "stable-row"))
    (let ((scope (matrix-menu--capture 'room t))
          (state (matrix-ui-test--state "Decrypted text"))
          (kill-ring nil))
      (matrix-menu-test--with-scope scope
        (should-not (matrix-menu--text-inapt-p))
        (should (matrix-menu--id-inapt-p))
        (should-error (matrix-message-copy-id scope) :type 'user-error)
        (should-error (matrix-message-copy-link scope) :type 'user-error)
        (should-not kill-ring)
        (let* ((timeline (gethash "!room:test" (plist-get state :timelines)))
               (item (car (plist-get timeline :items))))
          (setf (plist-get item :event_id) "$accepted:test")
          (setf (plist-get timeline :items) (list item))
          (matrix-menu-test--publish app surface state)
          (should-not (matrix-menu--id-inapt-p))
          (setf (plist-get item :kind) "encrypted"
                (plist-get item :body) "Encrypted message — keys unavailable")
          (matrix-menu-test--publish app surface state)
          (should (matrix-menu--text-inapt-p))
          (should-not (matrix-menu--id-inapt-p))
          (let ((before (copy-sequence kill-ring)))
            (should-error (matrix-message-copy-text scope) :type 'user-error)
            (should (equal kill-ring before)))
          (setf (plist-get item :id) "different-row")
          (matrix-menu-test--publish app surface state)
          (goto-char (appkit-chat-timeline-key-position "different-row"))
          (should (matrix-menu--message-inapt-p))
          (should (matrix-menu--id-inapt-p))
          (should-error (matrix-message-copy-id scope) :type 'user-error))))))

(ert-deftest matrix-menu-directory-action-resolves-key-and-refuses-a-removed-row ()
  (matrix-ui-test--with-room
    (let ((root (appkit-open-generated-surface
                 matrix-root--type :app app :identity '(root) :input '(root)
                 :buffer-name " *Matrix menu directory test*")))
      (unwind-protect
          (save-window-excursion
            (with-current-buffer (appkit-surface-buffer root)
              (setq-local matrix--registered-surface root)
              (goto-char (point-min))
              (search-forward "Encrypted room")
              (let ((scope (matrix-menu--capture 'root))
                    (state (matrix-ui-test--state "Current message")))
                (setf (plist-get state :rooms)
                      (list '(:room_id "!other:test" :name "Other room" :membership "join")
                            '(:room_id "!room:test" :name "Renamed room" :membership "join")))
                (appkit-app-send app state)
                (appkit-surface-send root '(matrix-update rooms nil))
                (goto-char (point-min))
                (search-forward "Other room")
                (matrix-menu-test--with-scope scope
                  (should-not (matrix-menu--root-entry-inapt-p))
                  (matrix-menu-root-open scope)
                  (should (eq (window-buffer) buffer))
                  (setf (plist-get state :rooms) (list (car (plist-get state :rooms))))
                  (appkit-app-send app state)
                  (appkit-surface-send root '(matrix-update rooms nil))
                  (should (matrix-menu--root-entry-inapt-p))
                  (should-error (matrix-menu-root-open scope) :type 'user-error)))))
        (let ((host (appkit-surface-buffer root)))
          (appkit-surface-stop root)
          (when (buffer-live-p host) (kill-buffer host)))))))

(ert-deftest matrix-menu-composer-capabilities-follow-draft-and-submission-state ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (let ((scope (matrix-menu--capture 'room))
          (state (matrix-ui-test--state "Current message")))
      (setf (plist-get state :backend-active) t)
      (matrix-menu-test--publish app surface state)
      (matrix-menu-test--with-scope scope
        (should (matrix-menu--send-inapt-p))
        (should (matrix-menu--review-inapt-p))
        (appkit-chatbuf-input-replace "Draft")
        (should-not (matrix-menu--send-inapt-p))
        (let ((context (matrix-ui-test--capture surface 'menu-send)))
          (should (matrix-menu--send-inapt-p))
          (matrix-ui-test--settle surface context (matrix-rpc-uncertain-error))
          (should (matrix-menu--send-inapt-p))
          (should-not (matrix-menu--review-inapt-p))
          (should (equal (appkit-chatbuf-input-string) "Draft"))
          (appkit-surface-send surface '(matrix-send-reviewed))
          (should (matrix-menu--review-inapt-p))
          (should-not (matrix-menu--send-inapt-p)))
        (setf (plist-get state :backend-active) nil)
        (matrix-menu-test--publish app surface state)
        (should (matrix-menu--send-inapt-p))
        (should (equal (appkit-chatbuf-input-string) "Draft"))))))

(ert-deftest matrix-menu-pagination-capabilities-follow-live-history-boundaries ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (let ((scope (matrix-menu--capture 'room))
          (state (matrix-ui-test--state "Current message")))
      (setf (plist-get state :backend-active) t)
      (matrix-menu-test--publish app surface state)
      (matrix-menu-test--with-scope scope
        (should-not (matrix-menu--history-inapt-p))
        (let ((owner (appkit-chat-history-request-start surface 'older)))
          (should (matrix-menu--history-inapt-p))
          (appkit-chat-history-request-end owner))
        (should-not (matrix-menu--history-inapt-p))
        (appkit-chat-history-older-loaded-set t)
        (should (matrix-menu--history-inapt-p))
        (appkit-chat-history-older-loaded-set nil)
        (setf (plist-get state :session) nil)
        (matrix-menu-test--publish app surface state)
        (should (matrix-menu--history-inapt-p))))))

(provide 'matrix-menu-test)
;;; matrix-menu-test.el ends here
