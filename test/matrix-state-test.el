;;; matrix-state-test.el --- Timeline boundary regressions -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'matrix-state)

(ert-deftest matrix-state-local-echo-set-keeps-row-identity ()
  (let* ((pending '(:id "local-1" :body "hello" :send_state "pending"))
         (sent '(:id "local-1" :event_id "$remote" :body "hello" :send_state "sent"))
         (old (list pending))
         (new (matrix-state-apply-changes
               old (vector (list :op "set" :index 0 :item sent)))))
    (should (equal new (list sent)))
    (should (equal old (list pending)))
    (should (equal (plist-get (car new) :id) (plist-get pending :id)))))

(ert-deftest matrix-state-ordered-batch-addresses-intermediate-snapshot ()
  (let* ((a '(:id "a" :body "first"))
         (b '(:id "b" :body "second"))
         (c '(:id "c" :body "third"))
         (edited '(:id "b" :body "edited"))
         (old (list a c)))
    (should
     (equal
      (matrix-state-apply-changes
       old (vector (list :op "insert" :index 1 :item b)
                   (list :op "set" :index 1 :item edited)
                   '(:op "remove" :index 0)))
      (list edited c)))
    (should (equal old (list a c)))))

(ert-deftest matrix-state-invalid-late-change-does-not-partly-apply ()
  (let* ((item '(:id "a" :body "original"))
         (old (list item)))
    (should-error
     (matrix-state-apply-changes
      old [(:op "set" :index 0 :item (:id "a" :body "changed"))
           (:op "remove" :index 2)])
     :type 'matrix-protocol-error)
    (should (equal old (list item)))))

(ert-deftest matrix-state-rejects-ambiguous-final-identities ()
  (let ((old '((:id "a" :body "original"))))
    (should-error
     (matrix-state-apply-changes
      old [(:op "push_back" :item (:id "a" :body "duplicate"))])
     :type 'matrix-protocol-error)
    (should (equal old '((:id "a" :body "original"))))))

(ert-deftest matrix-state-old-subscription-cannot-replace-new-room-view ()
  (let ((timeline '(:subscription_id "new" :revision 0
                   :items ((:id "a" :body "current")))))
    (should
     (eq 'stale
         (matrix-state-timeline-update
          timeline '(:type "timeline_reset" :subscription_id "old"
                     :revision 0 :items [(:id "b" :body "obsolete")]))))
    (should (equal (plist-get timeline :items) '((:id "a" :body "current"))))))

(ert-deftest matrix-state-gap-requires-reset-without-mutating-committed-rows ()
  (let ((timeline '(:subscription_id "one" :revision 3
                   :items ((:id "a" :body "current")))))
    (should-error
     (matrix-state-timeline-update
      timeline '(:type "timeline_diff" :subscription_id "one"
                 :revision 5 :changes [(:op "clear")]))
     :type 'matrix-timeline-gap)
    (should (= (plist-get timeline :revision) 3))
    (should (equal (plist-get timeline :items) '((:id "a" :body "current"))))))

(ert-deftest matrix-state-delayed-reset-cannot-rewind-newer-diffs ()
  (let ((timeline '(:subscription_id "one" :revision 2
                   :items ((:id "a" :body "edited")))))
    (should
     (eq 'stale
         (matrix-state-timeline-update
          timeline '(:type "timeline_reset" :subscription_id "one"
                     :revision 0 :items [(:id "a" :body "original")]))))
    (should
     (eq 'stale
         (matrix-state-timeline-update
          timeline '(:type "timeline_diff" :subscription_id "one"
                     :revision 2 :changes [(:op "clear")]))))))

(ert-deftest matrix-state-first-diff-needs-initial-snapshot ()
  (should-error
   (matrix-state-timeline-update
    '(:subscription_id "one" :revision nil :items nil)
    '(:type "timeline_diff" :subscription_id "one"
      :revision 1 :changes [(:op "push_back" :item (:id "a" :body "message"))]))
   :type 'matrix-timeline-gap))

(ert-deftest matrix-state-read-metadata-is-revision-and-subscription-owned ()
  (let* ((initial (matrix-state-timeline-update
                   '(:subscription_id "one")
                   '(:type "timeline_reset" :subscription_id "one" :revision 0
                     :items [(:id "a" :body "current")]
                     :read_state (:unread_messages 2))))
         (complete '(:type "timeline_diff" :subscription_id "one" :revision 1
                     :changes [] :read_state (:unread_messages 0 :all_read t)))
         (read (matrix-state-timeline-update initial complete)))
    (should (= (plist-get (plist-get initial :read_state) :unread_messages) 2))
    (should (= (plist-get (plist-get read :read_state) :unread_messages) 0))
    (should (eq (plist-get (plist-get read :read_state) :all_read) t))
    (should (equal (plist-get initial :items) (plist-get read :items)))
    (should (eq (matrix-state-timeline-update read complete) 'stale))
    (should (eq (matrix-state-timeline-update
                 initial (plist-put (copy-sequence complete) :subscription_id "old"))
                'stale))
    (should-error (matrix-state-timeline-update
                   initial (plist-put (copy-sequence complete) :revision 2))
                  :type 'matrix-timeline-gap)
    ;; Absent metadata becomes unknown, never a retained authoritative zero.
    (should-not
     (plist-get
      (matrix-state-timeline-update
       read '(:type "timeline_diff" :subscription_id "one" :revision 2 :changes []))
      :read_state))))

(ert-deftest matrix-state-members-keep-optional-profile-fields ()
  (let ((members [(:user_id "@alice:example.org" :display_name "Alice"
                   :avatar_url "mxc://example.org/alice")
                  (:user_id "@bob:example.org")]))
    (should (equal (matrix-state-members members)
                   '((:user_id "@alice:example.org" :display_name "Alice"
                      :avatar_url "mxc://example.org/alice")
                     (:user_id "@bob:example.org"))))))

(ert-deftest matrix-state-members-reject-ambiguous-or-malformed-identities ()
  (dolist (members (list [(:user_id "@a:example.org") (:user_id "@a:example.org")]
                         [(:display_name "No identity")]
                         [(:user_id "")]
                         [(:user_id "@a:example.org" :display_name 7)]
                         [(:user_id "@a:example.org" :avatar_url :not-a-string)]))
    (should-error (matrix-state-members members)
                  :type 'matrix-protocol-error)))

(ert-deftest matrix-state-member-by-display-name-keeps-backend-order ()
  (let ((members '((:user_id "@first:example.org" :display_name "Same")
                   (:user_id "@second:example.org" :display_name "Same"))))
    (should (equal (matrix-state-member-by-display-name members "Same")
                   (car members)))
    (should-not (matrix-state-member-by-display-name members "Absent"))))

(provide 'matrix-state-test)
;;; matrix-state-test.el ends here
