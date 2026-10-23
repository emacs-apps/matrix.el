;;; matrix-directory-test.el --- Directory boundaries -*- lexical-binding: t; -*-

(require 'ert)
(require 'matrix-root)
(require 'matrix-ui-test)

(defmacro matrix-directory-test--with-root (&rest body)
  "Run BODY on a real generated directory with SDK-shaped snapshots."
  (declare (indent 0) (debug t))
  `(matrix-ui-test--with-room
     (let* ((root (appkit-open-generated-surface
                   matrix-root--type :app app :identity '(root) :input '(root)
                   :buffer-name " *Matrix directory test*"))
            (root-buffer (appkit-surface-buffer root)))
       (unwind-protect
           (with-current-buffer root-buffer
             (setq-local matrix--registered-surface root)
             ,@body)
         (appkit-surface-stop root)
         (when (buffer-live-p root-buffer) (kill-buffer root-buffer))))))

(defun matrix-directory-test--visible-text (text)
  "Resolve string display elisions in TEXT for terminal width assertions."
  (let ((position 0) fragments)
    (while (< position (length text))
      (let* ((next (next-single-property-change position 'display text (length text)))
             (display (get-text-property position 'display text)))
        (push (if (stringp display) display (substring text position next)) fragments)
        (setq position next)))
    (apply #'concat (nreverse fragments))))

(ert-deftest matrix-directory-mark-read-uses-whole-room-rpc ()
  (matrix-directory-test--with-root
    (goto-char (point-min))
    (search-forward "Encrypted room")
    (beginning-of-line)
    (let (request)
      (cl-letf (((symbol-function 'matrix--request)
                 (lambda (surface method params &rest _)
                   (setq request (list surface method params)))))
        (appkit-directory-mark-read))
      (should (equal request (list root "room.mark_read" '(:room_id "!room:test"))))
      (should (eq (lookup-key matrix-root-mode-map "r") #'appkit-directory-mark-read))
      (setq request nil)
      (cl-letf (((symbol-function 'matrix--request) (lambda (&rest _) (setq request t))))
        (should-error (matrix-root--operation nil 'mark-read '((room "!absent:test")))
                      :type 'user-error))
      (should-not request))))

(ert-deftest matrix-directory-narrow-rows-retain-destination-and-unread ()
  (matrix-directory-test--with-root
    (let* ((room '(:room_id "!narrow:test" :name "Room 界\nlong title"
                  :membership "join" :unread 123 :encrypted t
                  :preview "Optional\nactivity" :preview_sender "@other:test"
                  :timestamp 1700000000000))
           (entry (seq-find #'appkit-directory-entry-item-p
                            (matrix-root--entries (list :rooms (list room)) nil))))
      (dolist (width '(18 19 20 21 22 24 26 28 30))
        (with-temp-buffer
          (cl-letf (((symbol-function 'matrix--width) (lambda (_surface) width)))
            (matrix-root--insert-item nil entry))
          (let ((visible (matrix-directory-test--visible-text (buffer-string))))
            (should (= 1 (cl-count ?\n visible)))
            (should (<= (string-width (string-trim-right visible)) width))
            (should (string-match-p "Room" visible))
            (should (string-match-p "123" visible))
            (should-not (string-match-p "Optional" visible))))))))

(ert-deftest matrix-directory-preview-follows-telega-sender-and-media ()
  (let ((group (matrix-root--preview
                '(:preview "hi" :preview_sender "@a:test" :preview_sender_name "Alice")
                "@me:test"))
        (incoming (matrix-root--preview
                   '(:direct t :preview "hi" :preview_sender "@a:test") "@me:test"))
        (outgoing (matrix-root--preview
                   '(:direct t :preview "hi" :preview_sender "@me:test" :preview_sender_name "Me")
                   "@me:test"))
        (photo (matrix-root--preview
                '(:preview "" :preview_media "image" :preview_sender "@a:test") "@me:test")))
    (should (equal (list (appkit-ui-one-line-preview-label group)
                         (appkit-ui-one-line-preview-separator group))
                   '("Alice" ">")))
    (should-not (appkit-ui-one-line-preview-label incoming))
    (should (equal (appkit-ui-one-line-preview-label outgoing) "Me"))
    (should (equal (appkit-ui-one-line-preview-text photo) "Photo"))))

(ert-deftest matrix-directory-duplicate-titles-have-stable-order ()
  (let* ((a '(:room_id "!a:test" :name "Same" :membership "join" :timestamp 5))
         (b '(:room_id "!b:test" :name "Same" :membership "join" :timestamp 5))
         (new '(:room_id "!new:test" :name "Last alphabetically" :membership "join"
                :timestamp 6)))
    (dolist (rooms (list (list b a new) (list new a b)))
      (should
       (equal (mapcar #'appkit-directory-entry-key
                      (seq-filter #'appkit-directory-entry-item-p
                                  (matrix-root--entries (list :rooms rooms) nil)))
              '((room "!new:test") (room "!a:test") (room "!b:test")))))))

(ert-deftest matrix-directory-preview-does-not-confuse-remote-identity-with-self ()
  (dolist (sender '("@me:other" "@me:test"))
    (let* ((body "Actual\nmessage 界")
           (preview (matrix-root--preview (list :preview body :preview_sender sender)
                                          "@me:test"))
           (text (appkit-ui-render-one-line-preview preview 80)))
      (should-not (string-match-p "\n" text))
      (should (string-match-p "Actual message 界" text))
      (when (equal sender "@me:other")
        (should (string-match-p (regexp-quote sender) text)))))
  (should (equal "" (appkit-ui-render-one-line-preview
                      (matrix-root--preview '(:preview_sender "@me:test") "@me:test")
                      80))))

(ert-deftest matrix-directory-refresh-and-resize-preserve-anchored-room ()
  (matrix-directory-test--with-root
    (goto-char (point-min))
    (search-forward "Encrypted room")
    (beginning-of-line)
    (let ((key (appkit-directory-key-at-point))
          (state (matrix-ui-test--state "Latest")))
      (push '(:room_id "!new:test" :name "New destination" :membership "join"
              :timestamp 1700000000000 :encrypted t)
            (plist-get state :rooms))
      (appkit-app-send app state)
      (appkit-surface-send root '(matrix-update rooms nil))
      (should (equal key (appkit-directory-key-at-point)))
      (dolist (width '(24 72))
        (cl-letf (((symbol-function 'matrix--width) (lambda (_surface) width)))
          (appkit-surface-send root '(matrix-geometry)))
        (should (equal key (appkit-directory-key-at-point))))
      (should-error (insert "change destination")))))

(ert-deftest matrix-directory-invitation-consent-cannot-target-replacement ()
  (matrix-directory-test--with-root
    (let ((entry (appkit-directory-entry-create
                  :key '(room "!invite:test") :role 'item
                  :payload '(room "!invite:test" "invite")))
          replacement requested)
      (unwind-protect
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _)
                       (appkit-surface-stop root)
                       (setq replacement
                             (appkit-open-generated-surface
                              matrix-root--type :app app :identity '(root) :input '(root)
                              :buffer root-buffer))
                       (setq-local matrix--registered-surface replacement)
                       t))
                    ((symbol-function 'matrix--request)
                     (lambda (&rest _) (setq requested t))))
            (should-error (matrix-root--activate nil entry) :type 'user-error)
            (should-not requested))
        (when replacement (appkit-surface-stop replacement))))))

(ert-deftest matrix-directory-invitation-consent-cannot-cross-session ()
  (matrix-directory-test--with-root
    (let ((entry (appkit-directory-entry-create
                  :key '(room "!invite:test") :role 'item
                  :payload '(room "!invite:test" "invite")))
          requested)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _)
                   (let ((state (matrix-ui-test--state "Changed account")))
                     (setf (plist-get state :session)
                           '(:user_id "@other:test" :device_id "OTHER"))
                     (appkit-app-send app state))
                   t))
                ((symbol-function 'matrix--request)
                 (lambda (&rest _) (setq requested t))))
        (should-error (matrix-root--activate nil entry) :type 'user-error)
        (should-not requested)))))

(ert-deftest matrix-directory-invitation-consent-rechecks-current-membership ()
  (matrix-directory-test--with-root
    (let ((state (matrix-ui-test--state "Invitation"))
          requested)
      (push '(:room_id "!invite:test" :name "Invited room" :membership "invite")
            (plist-get state :rooms))
      (appkit-app-send app state)
      (appkit-surface-send root '(matrix-update rooms nil))
      (let ((entry (appkit-directory-entry-for-key
                    (appkit-directory-surface) '(room "!invite:test"))))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _)
                     (appkit-app-send app (matrix-ui-test--state "Invitation withdrawn"))
                     (appkit-surface-send root '(matrix-update rooms nil))
                     t))
                  ((symbol-function 'matrix--request)
                   (lambda (&rest _) (setq requested t))))
          (should-error (matrix-root--activate nil entry) :type 'user-error)
          (should-not requested))))))

(provide 'matrix-directory-test)
;;; matrix-directory-test.el ends here
