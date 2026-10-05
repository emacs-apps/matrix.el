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

(provide 'matrix-state-test)
;;; matrix-state-test.el ends here
