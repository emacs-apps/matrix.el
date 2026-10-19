;;; matrix-notify-test.el --- Desktop notification wiring -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'matrix-runtime)
(require 'matrix-notify)

(defun matrix-notify-test--model ()
  (list :account "work" :rooms nil :notified nil
        :transport (matrix-rpc-transport-create :store "/store/")
        :watches (make-hash-table :test #'eq)))

(defun matrix-notify-test--operations (commands)
  (mapcar (lambda (command)
            (nth 2 (appkit-effect-spec-input
                    (appkit-command-start-effect-effect command))))
          commands))

(ert-deftest matrix-notify-runtime-retires-rooms-once-read ()
  "Notified rooms close when read; invitations wait to be answered."
  (let ((model (matrix-notify-test--model))
        (rooms [(:room_id "!a:x" :room_name "A" :count 2 :highlight t :kind "message")
                (:room_id "!b:x" :room_name "B" :count 1 :highlight :json-false
                 :kind "message")
                (:room_id "!i:x" :room_name "I" :count 1 :highlight :json-false
                 :kind "invite")]))
    (should (equal (matrix-notify-test--operations
                    (matrix-runtime--event
                     model (list :type "notifications" :rooms rooms :omitted 0)))
                   (list (list 'present (append rooms nil) 0))))
    (should (equal (sort (copy-sequence (plist-get model :notified)) #'string<)
                   '("!a:x" "!b:x")))
    (should (equal (matrix-notify-test--operations
                    (matrix-runtime--event
                     model '(:type "rooms"
                             :rooms [(:room_id "!a:x" :name "A" :unread 0 :mentions 1)
                                     (:room_id "!b:x" :name "B" :unread 0 :mentions 0)])))
                   '((retire ("!b:x")))))
    (should (equal (plist-get model :notified) '("!a:x")))
    (should-error (matrix-runtime--event
                   model '(:type "notifications" :omitted 0
                           :rooms [(:room_id "!a:x" :room_name "A" :count 0
                                    :highlight t :kind "message")]))
                  :type 'matrix-protocol-error)))

(ert-deftest matrix-notify-presents-per-room-and-skips-watched-rooms ()
  (let ((matrix-notifications 'all)
        (matrix-notification-content t)
        (matrix-notify--ids (make-hash-table :test #'equal))
        shown)
    (cl-letf (((symbol-function 'notifications-notify)
               (lambda (&rest args) (push args shown) (length shown)))
              ((symbol-function 'matrix-notify--watching-p)
               (lambda (_app room-id) (equal room-id "!seen:x"))))
      (matrix-notify--present
       "/store/" "work"
       '((:room_id "!a:x" :room_name "Team" :count 3 :highlight t :kind "message"
          :sender_name "Bob" :body "<b>@me</b> & you")
         (:room_id "!seen:x" :room_name "Open" :count 1 :highlight :json-false
          :kind "message" :body "already visible"))
       0)
      (should (= (length shown) 1))
      (should (equal (plist-get (car shown) :title) "Mention · Team"))
      (should (equal (plist-get (car shown) :body)
                     "Bob: &lt;b&gt;@me&lt;/b&gt; &amp; you (+2 more)"))
      (should (eq (plist-get (car shown) :urgency) 'critical))
      (should (equal (hash-table-keys matrix-notify--ids) '(("/store/" . "!a:x"))))
      (setq shown nil)
      (let ((matrix-notifications 'mentions))
        (matrix-notify--present
         "/store/" "work"
         '((:room_id "!b:x" :room_name "B" :count 1 :highlight :json-false
            :kind "message" :body "plain"))
         0))
      (should-not shown)
      (matrix-notify--present
       "/store/" "work"
       (cl-loop for n below 3
                collect (list :room_id (format "!r%d:x" n) :room_name (format "R%d" n)
                              :count 1 :highlight :json-false :kind "message"))
       5)
      (should (= (length shown) 1))
      (should (equal (plist-get (car shown) :body)
                     "New activity in 8 rooms: R0, R1, R2")))))

(provide 'matrix-notify-test)
;;; matrix-notify-test.el ends here
