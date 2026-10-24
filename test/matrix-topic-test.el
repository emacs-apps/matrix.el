;;; matrix-topic-test.el --- Matrix account and room subscriptions -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'matrix-room)

(defmacro matrix-topic-test--with-account (&rest body)
  "Run BODY with a real account reducer and a recording backend Source.
CALLS records outbound protocol calls; EMIT injects normalized backend events.
OPEN opens a real room Surface, optionally with a supplied TYPE."
  (declare (indent 0) (debug t))
  `(let (app calls emit buffers)
     (unwind-protect
         (progn
           (setq app
                 (appkit-app-start
                  (appkit-app-type-create
                   :name 'matrix-topic-test
                   :init (appkit-app-type-init matrix-runtime--type)
                   :update (appkit-app-type-update matrix-runtime--type)
                   :subscribed (appkit-app-type-subscribed matrix-runtime--type)
                   :sources
                   (lambda (model)
                     (let ((transport (plist-get model :transport)))
                       (list
                        (appkit-source-spec-create
                         :key 'backend :identity transport :input transport
                         :start (lambda (_context _input emit-gate _close-gate)
                                  (setq emit emit-gate)
                                  (appkit-source-cancellation-create :kind 'logical))
                         :event (lambda (_input event) (list 'matrix-source event))
                         :closed (lambda (_input reason) (list 'matrix-closed reason))
                         :outbound (lambda (_context _input call _result-gate)
                                     (push (copy-sequence call) calls)
                                     'accepted)
                         :emission-policy 'lossless :pending-limit 32
                         :outbound-pending-limit 16)))))
                  :input
                  (list (list :account "topic-test" :connection 'starting :session nil
                              :rooms '((:room_id "!room:test" :name "Room"
                                        :membership "join" :encrypted t))
                              :next-id 0 :backend-active t :watched nil
                              :transport (matrix-rpc-transport-create :store "/topic-test/")
                              :pending (make-hash-table :test #'eql)
                              :timelines (make-hash-table :test #'equal)
                              :verifications (make-hash-table :test #'equal))
                        (matrix-rpc-call-create :method "session.restore"))))
           (cl-labels
               ((open (name &optional type)
                  ;; Distinct viewers deliberately share the same room topic.
                  (let ((surface
                         (appkit-open-generated-surface
                          (or type matrix-room--type) :app app
                          :identity (list 'viewer name) :input '(room "!room:test")
                          :buffer-name (format " *Matrix topic %s*" name))))
                    (push (appkit-surface-buffer surface) buffers)
                    surface)))
             ,@body))
       (when app (appkit-app-close app))
       (mapc #'kill-buffer (seq-filter #'buffer-live-p buffers)))))

(defun matrix-topic-test--pass (&rest owners)
  "Drain each App or Surface in OWNERS through bounded deterministic passes."
  (dolist (owner owners)
    (let ((loop (if (appkit-app-p owner)
                    (appkit-app-loop owner)
                  (appkit-surface-loop owner)))
          (remaining 64))
      (while (and (> (appkit-loop-pending-count loop) 0) (> remaining 0))
        (setq remaining (1- remaining))
        (appkit-loop-run-pass loop))
      (should (= (appkit-loop-pending-count loop) 0)))))

(defun matrix-topic-test--calls (calls method)
  "Return recorded CALLS for METHOD in send order."
  (reverse (seq-filter (lambda (call) (equal method (matrix-rpc-call-method call)))
                       calls)))

(defun matrix-topic-test--restore (emit)
  "Use EMIT to complete the fixture's initial restore request."
  (funcall emit '(result 1 (:user_id "@me:test" :device_id "DEVICE"
                           :homeserver "https://test") nil)))

(ert-deftest matrix-topic-room-interest-follows-first-and-last-viewer ()
  (matrix-topic-test--with-account
    (let ((first (open 'first))
          (second (open 'second)))
      (matrix-topic-test--pass app)
      (should (equal (plist-get (appkit-app-model app) :watched) '("!room:test")))
      (should-not (matrix-topic-test--calls calls "timeline.subscribe"))
      ;; The room is already watched when authentication finishes.
      (matrix-topic-test--restore emit)
      (matrix-topic-test--pass app)
      (let* ((subscriptions (matrix-topic-test--calls calls "timeline.subscribe"))
             (params (matrix-rpc-call-params (car subscriptions))))
        (should (= (length subscriptions) 1))
        (should (equal (plist-get params :room_id) "!room:test"))
        (kill-buffer (appkit-surface-buffer first))
        (matrix-topic-test--pass app)
        (should-not (matrix-topic-test--calls calls "timeline.unsubscribe"))
        (should (equal (plist-get (appkit-app-model app) :watched) '("!room:test")))
        (kill-buffer (appkit-surface-buffer second))
        (matrix-topic-test--pass app)
        (should-not (plist-get (appkit-app-model app) :watched))
        (should-not (gethash "!room:test" (plist-get (appkit-app-model app) :timelines)))
        (should (equal (mapcar #'matrix-rpc-call-params
                              (matrix-topic-test--calls calls "timeline.unsubscribe"))
                       (list params)))
        ;; Reopening after release gets a new fence; changing major mode releases it.
        (let ((third (open 'third)))
          (matrix-topic-test--pass app)
          (let ((new (matrix-rpc-call-params
                      (cadr (matrix-topic-test--calls calls "timeline.subscribe")))))
            (should (equal (plist-get new :room_id) "!room:test"))
            (should-not (equal (plist-get params :subscription_id)
                               (plist-get new :subscription_id))))
          (with-current-buffer (appkit-surface-buffer third) (fundamental-mode))
          (matrix-topic-test--pass app)
          (should-not (plist-get (appkit-app-model app) :watched))
          (should (= (length (matrix-topic-test--calls calls "timeline.unsubscribe")) 2)))))))

(ert-deftest matrix-topic-coalesced-typing-keeps-timeline-history-and-draft ()
  (matrix-topic-test--with-account
    (matrix-topic-test--restore emit)
    (matrix-topic-test--pass app)
    (let ((type (copy-sequence matrix-room--type)) notifications)
      (setf (appkit-surface-type-update type)
            (lambda (context model message)
              (when (eq (car-safe message) 'matrix-update) (push message notifications))
              (matrix--surface-update context model message)))
      (let ((surface (open 'coalesced type)))
        (matrix-topic-test--pass app)
        (let ((subscription
               (plist-get (matrix-rpc-call-params
                           (car (matrix-topic-test--calls calls "timeline.subscribe")))
                          :subscription_id)))
          (funcall emit
                   (list 'event
                         (list :type "timeline_reset" :room_id "!room:test"
                               :subscription_id subscription :revision 0
                               :items [(:id "initial" :kind "message"
                                        :sender "@other:test" :body "Initial message")])))
          (matrix-topic-test--pass app surface)
          (with-current-buffer (appkit-surface-buffer surface)
            (appkit-chatbuf-input-replace "Unsent draft")
            (should (equal (appkit-chat-history-window-first-key) "initial")))
          (setq notifications nil)
          ;; Both events commit before the room consumes its topic notification.
          (funcall emit
                   (list 'event
                         (list :type "timeline_diff" :room_id "!room:test"
                               :subscription_id subscription :revision 1
                               :changes [(:op "push_front"
                                          :item (:id "older" :kind "message"
                                                 :sender "@other:test"
                                                 :body "Newly loaded message"))])))
          (funcall emit
                   (list 'event
                         (list :type "typing" :room_id "!room:test"
                               :subscription_id subscription :users ["Alice"])))
          (matrix-topic-test--pass app surface)
          (should (equal notifications '((matrix-update typing "!room:test"))))
          (with-current-buffer (appkit-surface-buffer surface)
            (should (equal (appkit-chat-timeline-keys) '("older" "initial")))
            (should (equal (appkit-chat-history-window-first-key) "older"))
            (should (string-match-p "Newly loaded message" (buffer-string)))
            (should (string-match-p "Alice" matrix--header-line))
            (should (equal (appkit-chatbuf-input-string) "Unsent draft"))))))))

(provide 'matrix-topic-test)
;;; matrix-topic-test.el ends here
