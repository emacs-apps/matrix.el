;;; matrix-avatar-test.el --- Managed avatar boundaries -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'appkit-projection)
(require 'matrix-avatar)

(defconst matrix-avatar-test--png
  "iVBORw0KGgoAAAANSUhEUgAAAAIAAAABCAIAAAB7QOjdAAAADUlEQVR4nGP4zwAE/wEHAAH/4iOeWQAAAABJRU5ErkJggg==")

(defun matrix-avatar-test--image-file (store)
  "Write a real PNG to STORE's private backend-shaped avatar cache."
  (let* ((cache (expand-file-name "avatars/fixture/" store))
         (file (expand-file-name "96-crop-fixture.image" cache)))
    (make-directory cache t)
    (set-file-modes (expand-file-name "avatars/" store) #o700)
    (set-file-modes cache #o700)
    (let ((coding-system-for-write 'no-conversion))
      (write-region (base64-decode-string matrix-avatar-test--png)
                    nil file nil 'silent))
    (set-file-modes file #o600)
    file))

(defconst matrix-avatar-test--surface-type
  (appkit-surface-type-create
   :name 'matrix-avatar-test :mode #'special-mode
   :init (lambda (_context uri)
           (appkit-next :model uri :render 'avatar))
   :update (lambda (_context _model uri)
             (appkit-next :model uri :render 'avatar))
   :renderer-factory
   (lambda (_surface)
     (appkit-generated-renderer-create
      :mount #'ignore :merge (lambda (_left right) right)
      :render
      (lambda (surface _app-read-view uri _request)
        (let* ((app (appkit-surface-app surface))
               (key (matrix-avatar-resource-key app uri))
               (demand (matrix-avatar-demand app uri))
               (image (matrix-avatar-cached-image app uri 24)))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (if image (propertize "avatar" 'display image) "waiting")))
          (appkit-render-result-create
           :resource-demands (and demand (list demand))
           :resource-interest-update
           (appkit-resource-interest-update-create
            :mode 'replace
            :entries (and key (list (appkit-resource-interest-create
                                    :key key :row-keys '(avatar))))))))
      :resource-request (lambda (_keys) 'avatar)
      :unmount #'ignore))))

(defmacro matrix-avatar-test--with-accounts (&rest body)
  "Run BODY with actual AppKit owners and controlled transport completions.
The runtime bridge is the only substituted boundary; files and resources are real."
  (declare (indent 0) (debug t))
  `(let ((root (make-temp-file "matrix-avatar-test-" t))
         apps surfaces requests)
     (unwind-protect
         (cl-letf (((symbol-function 'matrix-runtime-resource-request)
                    (lambda (app method params resolve reject)
                      (let ((request (list :app app :method method :params params
                                           :resolve resolve :reject reject
                                           :cancelled nil)))
                        (push request requests)
                        (appkit-cancellation-create
                         :kind 'logical
                         :cancel (lambda ()
                                   (setf (plist-get request :cancelled) t)))))))
           (cl-labels
               ((account (name &optional rooms)
                  (let ((store (expand-file-name name root)))
                    (make-directory store t)
                    (set-file-modes store #o700)
                    (let ((app
                           (appkit-app-start
                            (appkit-app-type-create
                             :name 'matrix-avatar-test
                             :init (lambda (_context model)
                                     (appkit-next :model model :render appkit-render-none))
                             :update (lambda (_context model message)
                                       (appkit-next
                                        :model (if (eq (car-safe message) 'matrix-test-snapshot)
                                                   (cadr message) model)
                                        :render appkit-render-none))
                             :shutdown #'matrix-avatar-clear-cache)
                            :identity (list 'matrix-account store)
                            :input (list :transport (matrix-rpc-transport-create :store store)
                                         :rooms rooms :watches (make-hash-table :test #'eq)))))
                      (push app apps)
                      app)))
                (open-avatar (app uri)
                  (let ((surface (appkit-open-generated-surface
                                  matrix-avatar-test--surface-type
                                  :app app :identity (make-symbol "avatar") :input uri)))
                    (push surface surfaces)
                    surface))
                (finish (request result)
                  (funcall (plist-get request :resolve) result)
                  (when (appkit-app-live-p (plist-get request :app))
                    (appkit-loop-run-pass (appkit-app-loop (plist-get request :app))))
                  (dolist (surface surfaces)
                    (when (appkit-surface-live-p surface)
                      (appkit-loop-run-pass (appkit-surface-loop surface)))))
                (resource (surface uri)
                  (appkit-resource-state
                   surface (matrix-avatar-resource-key (appkit-surface-app surface) uri))))
             ,@body))
       (dolist (surface surfaces)
         (appkit-surface-stop surface)
         (when (buffer-live-p (appkit-surface-buffer surface))
           (kill-buffer (appkit-surface-buffer surface))))
       (dolist (app apps) (appkit-app-close app))
       (delete-directory root t))))

(ert-deftest matrix-avatar-deduplicates-only-while-interested-and-fences-late-files ()
  (matrix-avatar-test--with-accounts
    (let* ((app (account "one"))
           (store (matrix-rpc-transport-store (plist-get (appkit-app-model app) :transport)))
           (file (matrix-avatar-test--image-file store))
           (uri "mxc://example.test/avatar")
           (first (open-avatar app uri))
           (second (open-avatar app uri))
           (old (car requests)))
      (should (= 1 (length requests)))
      (should (eq 'pending (appkit-resource-state-status (resource first uri))))
      (appkit-surface-stop first)
      (should-not (plist-get old :cancelled))
      (appkit-surface-stop second)
      (should (plist-get old :cancelled))
      (let ((successor (open-avatar app uri)))
        (should (= 2 (length requests)))
        (finish old (list :path file))
        (should (eq 'pending (appkit-resource-state-status (resource successor uri))))
        (with-current-buffer (appkit-surface-buffer successor)
          (should-not (matrix-avatar-cached-image app uri 24)))
        (finish (car requests) (list :path file))
        (let ((state (resource successor uri)))
          (should (eq 'ready (appkit-resource-state-status state)))
          (should (equal file (car (appkit-resource-state-value state)))))
        (should (= 0 (appkit-loop-revision (appkit-surface-loop successor))))))))

(ert-deftest matrix-avatar-isolates-accounts-and-rejects-cross-account-files ()
  (matrix-avatar-test--with-accounts
    (let* ((one (account "one")) (two (account "two"))
           (uri "mxc://example.test/shared")
           (file (matrix-avatar-test--image-file
                  (matrix-rpc-transport-store (plist-get (appkit-app-model one) :transport))))
           (first (open-avatar one uri))
           (request-one (car requests))
           (second (open-avatar two uri))
           (request-two (car requests)))
      (should (= 2 (length requests)))
      (should-not (equal (matrix-avatar-resource-key one uri)
                         (matrix-avatar-resource-key two uri)))
      (finish request-one (list :path file))
      (finish request-two (list :path file))
      (should (eq 'ready (appkit-resource-state-status (resource first uri))))
      (should (eq 'failed (appkit-resource-state-status (resource second uri))))
      (with-current-buffer (appkit-surface-buffer second)
        (should-not (matrix-avatar-cached-image one uri 24))
        (should-not (matrix-avatar-cached-image two uri 24))))))

(ert-deftest matrix-avatar-missing-and-malformed-uris-do-not-start-media ()
  (matrix-avatar-test--with-accounts
    (let ((app (account "one")))
      (dolist (uri '(nil "" "https://example.test/image.png" "mxc://example.test/"
                        "mxc:///image" "mxc://example.test/image?token=secret"
                        "mxc://example.test/image#fragment" "mxc://user@example.test/image"
                        "mxc://example.test/a/b" "mxc://example.test/line\nbreak"))
        (let ((surface (open-avatar app uri)))
          (should-not (matrix-avatar-resource-key app uri))
          (with-current-buffer (appkit-surface-buffer surface)
            (should-not (matrix-avatar-cached-image app uri 24)))))
      (should-not requests))))

(ert-deftest matrix-avatar-invalid-local-sources-never-become-ready ()
  (matrix-avatar-test--with-accounts
    (let* ((app (account "one"))
           (store (matrix-rpc-transport-store (plist-get (appkit-app-model app) :transport)))
           (file (matrix-avatar-test--image-file store))
           (link (expand-file-name "avatars/fixture/link.image" store)))
      (make-symbolic-link file link)
      (dolist (payload (list nil '(:path "relative.image")
                            '(:path "/ssh:other:/private/image.png")
                            (list :path link) (list :path store)))
        (let* ((uri (format "mxc://example.test/invalid-%d" (length requests)))
               (surface (open-avatar app uri)))
          (finish (car requests) payload)
          (should (eq 'failed (appkit-resource-state-status (resource surface uri))))))
      (dolist (failure '(permissions content directory))
        (matrix-avatar-test--image-file store)
        (pcase failure
          ('permissions (set-file-modes file #o644))
          ('content (write-region "not an image" nil file nil 'silent))
          ('directory (set-file-modes (file-name-directory file) #o755)))
        (let* ((uri (format "mxc://example.test/unsafe-%s" failure))
               (surface (open-avatar app uri)))
          (finish (car requests) (list :path file))
          (should (eq 'failed (appkit-resource-state-status (resource surface uri)))))))))

(ert-deftest matrix-avatar-real-images-are-versioned-and-clear-only-one-account ()
  (skip-unless (and (appkit-media-inline-image-rendering-available-p)
                    (image-type-available-p 'png)))
  (matrix-avatar-test--with-accounts
    (let* ((one (account "one")) (two (account "two"))
           (uri "mxc://example.test/avatar")
           (store-one (matrix-rpc-transport-store (plist-get (appkit-app-model one) :transport)))
           (store-two (matrix-rpc-transport-store (plist-get (appkit-app-model two) :transport)))
           (file-one (matrix-avatar-test--image-file store-one))
           (file-two (matrix-avatar-test--image-file store-two))
           (first (open-avatar one uri))
           (request-one (car requests))
           (second (open-avatar two uri)))
      (finish request-one (list :path file-one))
      (finish (car requests) (list :path file-two))
      (let ((image-one (with-current-buffer (appkit-surface-buffer first)
                         (matrix-avatar-cached-image one uri 24)))
            (image-two (with-current-buffer (appkit-surface-buffer second)
                         (matrix-avatar-cached-image two uri 24))))
        (should (appkit-media-image-object-valid-p image-one))
        (should (appkit-media-image-object-valid-p image-two))
        (with-current-buffer (appkit-surface-buffer first)
          (should (eq image-one (get-text-property (point-min) 'display)))
          (should (eq image-one (matrix-avatar-cached-image one uri 24)))
          (should-not (eq image-one (matrix-avatar-cached-image one uri 32))))
        (let* ((mtime (file-attribute-modification-time (file-attributes file-one)))
               (replacement (make-temp-file (concat file-one "-"))))
          (let ((coding-system-for-write 'no-conversion))
            (write-region (base64-decode-string matrix-avatar-test--png)
                          nil replacement nil 'silent))
          (set-file-modes replacement #o600)
          (set-file-times replacement mtime)
          (rename-file replacement file-one t)
          (appkit-surface-send first nil)
          (appkit-surface-send first uri)
          (finish (car requests) (list :path file-one))
          (with-current-buffer (appkit-surface-buffer first)
            (let ((new-image (matrix-avatar-cached-image one uri 24)))
              (should (appkit-media-image-object-valid-p new-image))
              (should-not (eq image-one new-image)))))
        (with-current-buffer (appkit-surface-buffer first)
          (let ((before-clear (matrix-avatar-cached-image one uri 24)))
            (matrix-avatar-clear-cache one)
            (let ((after-clear (matrix-avatar-cached-image one uri 24)))
              (should (appkit-media-image-object-valid-p after-clear))
              (should-not (eq before-clear after-clear)))))
        (with-current-buffer (appkit-surface-buffer second)
          (should (eq image-two (matrix-avatar-cached-image two uri 24))))))))

(ert-deftest matrix-avatar-real-images-do-not-shift-directory-destinations ()
  (skip-unless (and (display-graphic-p) (image-type-available-p 'png)))
  (require 'matrix-root)
  (matrix-avatar-test--with-accounts
    (dolist (width '(24 72))
      (let* ((uri "mxc://example.test/avatar")
             (app (account (format "directory-%s" width)
                           (list (list :room_id "!avatar:test" :name "Avatar destination"
                                       :membership "join" :encrypted t :avatar_url uri))))
             (store (matrix-rpc-transport-store (plist-get (appkit-app-model app) :transport)))
             (file (matrix-avatar-test--image-file store))
             (surface
              (cl-letf (((symbol-function 'matrix--width) (lambda (_surface) width)))
                (appkit-open-generated-surface
                 matrix-root--type :app app :identity '(root) :input '(root))))
             (request (car requests)))
        (push surface surfaces)
        (with-current-buffer (appkit-surface-buffer surface)
          (goto-char (point-min))
          (search-forward "Avatar destination")
          (let ((column (current-column))
                (key (appkit-directory-key-at-point)))
            (cl-letf (((symbol-function 'matrix--width) (lambda (_surface) width)))
              (finish request (list :path file)))
            (should (equal key (appkit-directory-key-at-point)))
            (should (= column (current-column)))
            (should
             (cl-loop for pos from (line-beginning-position) below (line-end-position)
                      for display = (get-text-property pos 'display)
                      thereis (or (appkit-media-image-object-valid-p display)
                                  (and (listp display)
                                       (cl-some #'appkit-media-image-object-valid-p
                                                display)))))))))))

(ert-deftest matrix-avatar-retired-account-cannot-publish-into-reopened-store ()
  (matrix-avatar-test--with-accounts
    (let* ((old-app (account "one"))
           (store (matrix-rpc-transport-store
                   (plist-get (appkit-app-model old-app) :transport)))
           (file (matrix-avatar-test--image-file store))
           (uri "mxc://example.test/avatar")
           (_old-surface (open-avatar old-app uri))
           (old-request (car requests)))
      (appkit-app-close old-app)
      (should (plist-get old-request :cancelled))
      (let* ((new-app (account "one"))
             (surface (open-avatar new-app uri)))
        (finish old-request (list :path file))
        (should (eq 'pending (appkit-resource-state-status (resource surface uri))))
        (with-current-buffer (appkit-surface-buffer surface)
          (should-not (matrix-avatar-cached-image new-app uri 24)))
        (finish (car requests) (list :path file))
        (should (eq 'ready (appkit-resource-state-status (resource surface uri))))))))

(provide 'matrix-avatar-test)
;;; matrix-avatar-test.el ends here
