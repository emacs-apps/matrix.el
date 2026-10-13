;;; matrix-member-test.el --- Member lookup wiring -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'matrix-runtime)

(ert-deftest matrix-room-members-forwards-exact-display-name-filter ()
  "A display-name lookup must not ask for the whole member list."
  (let (captured result)
    (cl-letf (((symbol-function 'matrix-runtime-resource-request)
               (lambda (_app method params resolve _reject)
                 (setq captured (list method params))
                 (funcall resolve
                          '(:members [(:user_id "@alice:example.org"
                                       :display_name "Alice"
                                       :avatar_url "mxc://example.org/a")]))
                 'cancellation)))
      (should (eq (matrix-room-members 'app "!room:example.org"
                                       (lambda (members) (setq result members))
                                       nil "Alice")
                  'cancellation))
      (should (equal captured
                     '("room.members"
                       (:room_id "!room:example.org" :display_name "Alice"))))
      (should (equal result
                     '((:user_id "@alice:example.org" :display_name "Alice"
                        :avatar_url "mxc://example.org/a")))))))

(ert-deftest matrix-room-members-without-filter-requests-all-joined ()
  (let (captured)
    (cl-letf (((symbol-function 'matrix-runtime-resource-request)
               (lambda (_app method params resolve _reject)
                 (setq captured (list method params))
                 (funcall resolve '(:members []))
                 'cancellation)))
      (matrix-room-members 'app "!room:example.org" #'ignore)
      (should (equal captured '("room.members" (:room_id "!room:example.org")))))))

(ert-deftest matrix-room-member-resolve-selects-by-display-name ()
  (cl-letf (((symbol-function 'matrix-runtime-resource-request)
             (lambda (_app _method _params resolve _reject)
               (funcall resolve
                        '(:members [(:user_id "@a:example.org" :display_name "A")
                                    (:user_id "@b:example.org" :display_name "B")]))
               'cancellation)))
    (let (result)
      (matrix-room-member-resolve 'app "!room:example.org" "B"
                                  (lambda (member) (setq result member)))
      (should (equal result '(:user_id "@b:example.org" :display_name "B"))))))

(provide 'matrix-member-test)
;;; matrix-member-test.el ends here
