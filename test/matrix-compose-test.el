;;; matrix-compose-test.el --- Consumer draft and private-source invariants -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'matrix-compose)
(require 'matrix-ui-test)

(defconst matrix-compose-test--png
  (base64-decode-string
   "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+jRZkAAAAASUVORK5CYII="))

(defvar matrix-compose-test--token 0)

(defmacro matrix-compose-test--with-files (bindings &rest body)
  "Create user-owned local file BINDINGS around BODY, then remove the fixture."
  (declare (indent 1) (debug t))
  `(let ((directory (make-temp-file "matrix-compose-test-" t)))
     (unwind-protect
         (let ,(mapcar (lambda (binding)
                        `(,(car binding) (expand-file-name ,(cadr binding) directory))) bindings)
           ,@(mapcar (lambda (binding)
                       `(progn
                          (make-directory (file-name-directory ,(car binding)) t)
                          (let ((coding-system-for-write 'no-conversion))
                            (write-region "user-file" nil ,(car binding) nil 'silent)))) bindings)
           ,@body)
       (delete-directory directory t))))

(defun matrix-compose-test--context (surface &optional capture)
  "Retain CAPTURE and enter the real existing Matrix operation on SURFACE."
  (let ((context (append
                  (list :target (plist-get (appkit-surface-model surface) :address)
                        :token (cl-incf matrix-compose-test--token) :method "message.send")
                  (matrix-compose-retain-capture surface
                                                 (or capture (matrix-compose-capture surface))))))
    (appkit-surface-send surface (list 'matrix-operation context))
    context))

(defun matrix-compose-test--settle (surface context result &optional error)
  "Deliver the canonical SDK admission RESULT to exact SURFACE."
  (appkit-surface-send surface
                       (list 'matrix-result 1 "message.send" result error context)))

(defun matrix-compose-test--detach ()
  "Detach the first actual object without opening a selection minibuffer."
  (goto-char (appkit-chatbuf-input-start-position))
  (matrix-compose-detach))

(defun matrix-compose-test--clipboard-image ()
  "Stage a genuine small PNG through the real clipboard staging boundary."
  (cl-letf (((symbol-function 'appkit-media-clipboard-source)
             (lambda () (list :mime-type "image/png" :suffix ".png"
                              :data matrix-compose-test--png))))
    (matrix-compose-attach-clipboard)))

(defun matrix-compose-test--begin-reply (surface)
  "Stage a reply to one authoritative SDK-shaped fixture row on SURFACE."
  (let* ((item '(:id "reply-target" :kind "message" :sender "@other:test"
                :body "Reply target" :event_id "$reply-target:test" :can_reply t))
         (state (plist-put (matrix-ui-test--items-state (list item)) :backend-active t)))
    (appkit-app-send (appkit-surface-app surface) state)
    (appkit-surface-send surface '(matrix-update timeline "!room:test"))
    (matrix-room-begin-reply surface item)))

(ert-deftest matrix-compose-file-only-and-caption-never-serialize-labels ()
  (matrix-compose-test--with-files ((file "report.txt"))
    (matrix-ui-test--with-room
      (setq-local matrix--registered-surface surface)
      (let* ((attachment (matrix-compose-attach-file file))
             (capture (matrix-compose-capture surface))
             (wire (aref (plist-get (plist-get capture :params) :attachments) 0)))
        (should (equal (plist-get capture :body) ""))
        (should (equal (plist-get wire :path) file))
        (should (equal (plist-get wire :filename) "report.txt"))
        (should-not (plist-member wire :temporary-handle))
        (should-not (plist-get (appkit-surface-model surface) :operation))
        (goto-char (point-max))
        (insert "中文 caption")
        (setq capture (matrix-compose-capture surface))
        (should (equal (plist-get capture :body) "中文 caption"))
        (should (= 1 (length (plist-get capture :attachments))))
        (should (equal attachment (car (plist-get capture :attachments))))
        (should-not (get-text-property (1- (point-max)) appkit-chatbuf-input-object-property))
        (matrix-compose-test--detach)
        (should (equal (appkit-chatbuf-input-string) "中文 caption"))
        (should (file-exists-p file))))))

(ert-deftest matrix-compose-same-label-different-path-is-a-newer-object ()
  (matrix-compose-test--with-files ((first "first/same.txt") (second "second/same.txt"))
    (matrix-ui-test--with-room
      (setq-local matrix--registered-surface surface)
      (matrix-compose-attach-file first)
      (let* ((capture (matrix-compose-capture surface))
             (presentation (substring-no-properties (plist-get capture :input))))
        ;; Replacement while confirmation is open: equal visible labels are
        ;; different local objects, not the draft whose send was accepted.
        (appkit-chatbuf-input-replace "")
        (matrix-compose-attach-file second)
        (should (equal presentation (substring-no-properties (appkit-chatbuf-input-state))))
        (should-not (equal-including-properties (plist-get capture :input)
                                               (appkit-chatbuf-input-state)))
        (let ((context (matrix-compose-test--context surface capture)))
          (matrix-compose-test--settle surface context '(:accepted 1 :total 1)))
        (should (equal (plist-get (car (matrix-compose-attachments)) :path) second))
        (should (file-exists-p first))
        (should (file-exists-p second))))))

(ert-deftest matrix-compose-full-acceptance-never-clears-newer-aux-target ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (appkit-chatbuf-input-replace "Captured text")
    (let ((capture (matrix-compose-capture surface)))
      ;; Legitimate new reply selection during confirmation precedes request
      ;; admission; accepting the old text must retain this newer target.
      (matrix-compose-test--begin-reply surface)
      (let ((context (matrix-compose-test--context surface capture)))
        (matrix-compose-test--settle surface context '(:accepted 1 :total 1)))
      (should (equal (appkit-chatbuf-input-string) "Captured text"))
      (should (equal (plist-get (appkit-chatbuf-aux-state) :event-id) "$reply-target:test")))))

(ert-deftest matrix-compose-detach-chooser-removes-the-selected-duplicate-label ()
  (matrix-compose-test--with-files ((first "first/same.txt") (second "second/same.txt"))
    (matrix-ui-test--with-room
      (setq-local matrix--registered-surface surface)
      (matrix-compose-attach-file first)
      (matrix-compose-attach-file second)
      (goto-char (point-max))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _) (car (cadr choices)))))
        (matrix-compose-detach))
      (should (equal (mapcar (lambda (object) (plist-get object :path))
                            (matrix-compose-attachments))
                     (list first)))
      (should (file-exists-p first))
      (should (file-exists-p second)))))

(ert-deftest matrix-compose-partial-acceptance-removes-prefix-and-consumed-caption ()
  (matrix-compose-test--with-files ((first "first.txt") (second "second.txt"))
    (matrix-ui-test--with-room
      (setq-local matrix--registered-surface surface)
      (matrix-compose-test--begin-reply surface)
      (appkit-chatbuf-input-replace "Caption before ")
      (matrix-compose-attach-file first)
      (matrix-compose-attach-file second)
      (goto-char (point-max))
      (insert " caption after")
      (let* ((capture (matrix-compose-capture surface))
             (remaining (cadr (plist-get capture :attachments)))
             (context (matrix-compose-test--context surface capture)))
        (matrix-compose-test--settle
         surface context '(:accepted 1 :total 2 :error (:code "enqueue_failed" :message "Queue stopped")))
        (should (equal (matrix-compose-attachments) (list remaining)))
        (should (equal (matrix-compose-serialize (appkit-chatbuf-input-state)) ""))
        (should-not (plist-get (appkit-surface-model surface) :uncertain-send))
        (let ((retry (matrix-compose-capture surface)))
          (should (= 1 (length (plist-get retry :attachments))))
          (should (equal (plist-get retry :body) ""))
          (should (equal (plist-get (plist-get retry :params) :reply_to) "$reply-target:test"))
          (should (equal (plist-get (aref (plist-get (plist-get retry :params) :attachments) 0) :path)
                         second)))
        (let ((input (appkit-chatbuf-input-state)))
          (matrix-compose-cancel)
          (should-not (appkit-chatbuf-aux-state))
          (should (equal-including-properties input (appkit-chatbuf-input-state))))
        (should (file-exists-p first))
        (should (file-exists-p second))))))

(ert-deftest matrix-compose-partial-newer-draft-requires-review-without-mutation ()
  (matrix-compose-test--with-files ((first "first.txt") (second "second.txt"))
    (matrix-ui-test--with-room
      (setq-local matrix--registered-surface surface)
      (matrix-compose-attach-file first)
      (matrix-compose-attach-file second)
      (let ((context (matrix-compose-test--context surface)))
        (goto-char (point-max))
        (insert " newer caption")
        (let ((newer (appkit-chatbuf-input-state)))
          (matrix-compose-test--settle
           surface context '(:accepted 1 :total 2 :error (:code "enqueue_failed" :message "Queue stopped")))
          (should (equal-including-properties newer (appkit-chatbuf-input-state)))
          (should (plist-get (appkit-surface-model surface) :uncertain-send))
          (should-error (matrix-room-send) :type 'user-error))))))

(ert-deftest matrix-compose-clipboard-file-list-validates-entire-selection ()
  (matrix-compose-test--with-files ((first "first.txt") (second "second.txt"))
    (matrix-ui-test--with-room
      (setq-local matrix--registered-surface surface)
      (appkit-chatbuf-input-replace "Keep caption")
      (let ((before (appkit-chatbuf-input-state)))
        (cl-letf (((symbol-function 'appkit-media-clipboard-source)
                   (lambda () (list :files (list first (concat second ".missing"))))))
          (should-error (matrix-compose-attach-clipboard) :type 'user-error))
        (should (equal-including-properties before (appkit-chatbuf-input-state))))
      (cl-letf (((symbol-function 'appkit-media-clipboard-source)
                 (lambda () (list :files (list first second)))))
        (matrix-compose-attach-clipboard t))
      (let ((capture (matrix-compose-capture surface)))
        (should (equal (plist-get capture :body) "Keep caption"))
        (should (equal (mapcar (lambda (object) (plist-get object :path))
                              (plist-get capture :attachments)) (list first second)))
        (should (cl-every (lambda (object) (equal (plist-get object :mime-type)
                                                "application/octet-stream"))
                          (plist-get capture :attachments))))
      (should-not (plist-get (appkit-surface-model surface) :operation))
      (appkit-surface-stop surface)
      (should (file-exists-p first))
      (should (file-exists-p second)))))

(ert-deftest matrix-compose-picker-replacement-cannot-stage-in-successor ()
  (matrix-compose-test--with-files ((file "picked.txt"))
    (matrix-ui-test--with-room
      (setq-local matrix--registered-surface surface)
      (appkit-chatbuf-input-replace "Old draft")
      (let (replacement)
        (unwind-protect
            (cl-letf (((symbol-function 'appkit-media-read-file-name)
                       (lambda (&rest _)
                         (appkit-surface-stop surface)
                         (setq replacement (matrix-ui-test--open app buffer))
                         (setq-local matrix--registered-surface replacement)
                         (appkit-chatbuf-input-replace "Replacement draft")
                         file)))
              (should-error (call-interactively #'matrix-compose-attach-file) :type 'user-error)
              (should (equal (appkit-chatbuf-input-string) "Replacement draft"))
              (should-not (matrix-compose-attachments)))
          (when replacement (appkit-surface-stop replacement)))))))

(ert-deftest matrix-compose-clipboard-acquisition-fences-newer-reply-target ()
  (matrix-compose-test--with-files ((file "selected.txt"))
    (matrix-ui-test--with-room
      (setq-local matrix--registered-surface surface)
      (appkit-chatbuf-input-replace "Unchanged visible text")
      (cl-letf (((symbol-function 'appkit-media-clipboard-source)
                 (lambda ()
                   (appkit-chatbuf-aux-set '(:aux-type reply :event-id "$new:test"))
                   (list :files (list file)))))
        (should-error (matrix-compose-attach-clipboard) :type 'user-error))
      (should-not (matrix-compose-attachments))
      (should (equal (appkit-chatbuf-input-string) "Unchanged visible text"))
      (should (equal (plist-get (appkit-chatbuf-aux-state) :event-id) "$new:test")))))

(ert-deftest matrix-compose-picker-edit-away-and-back-is-not-the-original-draft ()
  (matrix-compose-test--with-files ((file "picked.txt"))
    (matrix-ui-test--with-room
      (setq-local matrix--registered-surface surface)
      (appkit-chatbuf-input-replace "Original")
      (cl-letf (((symbol-function 'appkit-media-read-file-name)
                 (lambda (&rest _)
                   (appkit-chatbuf-input-replace "Intervening edit")
                   (appkit-chatbuf-input-replace "Original")
                   file)))
        (should-error (call-interactively #'matrix-compose-attach-file) :type 'user-error))
      (should (equal (appkit-chatbuf-input-string) "Original"))
      (should-not (matrix-compose-attachments)))))

(ert-deftest matrix-compose-zero-accepted-parts-preserve-caption-and-objects ()
  (matrix-compose-test--with-files ((file "unsent.txt"))
    (matrix-ui-test--with-room
      (setq-local matrix--registered-surface surface)
      (appkit-chatbuf-input-replace "Unconsumed caption")
      (matrix-compose-attach-file file)
      (let ((input (appkit-chatbuf-input-state))
            (context (matrix-compose-test--context surface)))
        (matrix-compose-test--settle
         surface context '(:accepted 0 :total 1 :error (:code "enqueue_failed" :message "Queue stopped")))
        (should (equal-including-properties input (appkit-chatbuf-input-state)))
        (should-not (plist-get (appkit-surface-model surface) :uncertain-send))
        (should (equal (plist-get (matrix-compose-capture surface) :body) "Unconsumed caption"))))))

(ert-deftest matrix-compose-private-source-detach-and-failure-retain-correctly ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (let* ((attachment (matrix-compose-test--clipboard-image))
           (path (plist-get attachment :path))
           (directory (file-name-directory path))
           (context (matrix-compose-test--context surface)))
      (should (zerop (logand #o077 (file-modes path))))
      (should (zerop (logand #o077 (file-modes directory))))
      (matrix-compose-test--settle surface context nil '(:message "Not admitted"))
      (should (file-exists-p path))
      (should (matrix-compose-attachments))
      (matrix-compose-release-capture context)
      (matrix-compose-release-capture context)
      (should (file-exists-p path))
      (matrix-compose-test--detach)
      (should-not (file-exists-p path))
      (should-not (file-exists-p directory)))))

(ert-deftest matrix-compose-private-source-survives-hidden-saved-edit-draft ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (let* ((attachment (matrix-compose-test--clipboard-image))
           (path (plist-get attachment :path))
           (saved (appkit-chatbuf-input-state)))
      ;; The edit's ordinary draft is still an owner while its file objects
      ;; are hidden behind editable message text.
      (appkit-chatbuf-aux-set (list :aux-type 'edit :saved-input saved))
      (appkit-chatbuf-input-replace "Editing existing message")
      (matrix-compose-prune surface)
      (should (file-exists-p path))
      (matrix-compose-cancel)
      (should (equal-including-properties saved (appkit-chatbuf-input-state)))
      (matrix-compose-prune surface)
      (should (file-exists-p path))
      (matrix-compose-test--detach)
      (should-not (file-exists-p path)))))

(ert-deftest matrix-compose-private-accepted-prefix-cleans-only-consumed-source ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (matrix-compose-test--clipboard-image)
    (matrix-compose-test--clipboard-image)
    (let* ((capture (matrix-compose-capture surface))
           (first (plist-get (car (plist-get capture :attachments)) :path))
           (second (plist-get (cadr (plist-get capture :attachments)) :path))
           (context (matrix-compose-test--context surface capture)))
      (matrix-compose-test--settle
       surface context '(:accepted 1 :total 2 :error (:code "enqueue_failed" :message "Queue stopped")))
      (should-not (file-exists-p first))
      (should (file-exists-p second))
      (let ((remaining (matrix-compose-test--context surface)))
        (matrix-compose-test--settle surface remaining '(:accepted 1 :total 1)))
      (should-not (file-exists-p second))
      (should (equal (appkit-chatbuf-input-string) "")))))

(ert-deftest matrix-compose-account-completion-releases-source-after-room-closes ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (let* ((attachment (matrix-compose-test--clipboard-image))
           (path (plist-get attachment :path))
           (context (matrix-compose-test--context surface))
           (state (copy-sequence (matrix-runtime-state app)))
           (pending (make-hash-table :test #'eql)))
      (appkit-chatbuf-input-replace "Newer ordinary draft")
      (matrix-compose-prune surface)
      (should (file-exists-p path))
      (appkit-surface-stop surface)
      (should (file-exists-p path))
      ;; Feed a terminal backend response through the actual account reducer
      ;; and Appkit post-commit effect, with no live UI completion target.
      (puthash 1 (list :method "message.send" :context context) pending)
      (setq state (plist-put state :pending pending)
            state (plist-put state :next-id 1))
      (appkit-app-send app state)
      (setf (appkit-app-type-update (appkit-app-type app)) #'matrix-runtime--update)
      (appkit-app-send app '(matrix-source (result 1 (:accepted 1 :total 1) nil)))
      (should-not (file-exists-p path))
      (should (zerop (hash-table-count (plist-get (matrix-runtime-state app) :pending))))
      (matrix-compose-release-capture context)
      (should-not (file-exists-p path)))))

(ert-deftest matrix-compose-account-close-releases-pending-private-source ()
  (matrix-ui-test--with-room
    (setq-local matrix--registered-surface surface)
    (let* ((attachment (matrix-compose-test--clipboard-image))
           (path (plist-get attachment :path))
           (context (matrix-compose-test--context surface)))
      (appkit-surface-stop surface)
      (should (file-exists-p path))
      (appkit-app-close app)
      (should-not (file-exists-p path))
      (matrix-compose-release-capture context)
      (should-not (file-exists-p path)))))

(provide 'matrix-compose-test)
;;; matrix-compose-test.el ends here
