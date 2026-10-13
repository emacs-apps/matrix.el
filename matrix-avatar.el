;;; matrix-avatar.el --- Account-private managed avatars -*- lexical-binding: t; -*-

;;; Commentary:

;; Renderers declare AppKit resource demands.  The authenticated backend owns
;; image acquisition and its private disk cache; only committed ready resource
;; states can be decoded by the current Surface.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-app)
(require 'appkit-resource)
(require 'appkit-surface)
(require 'appkit-media-image)
(require 'matrix-rpc)

(declare-function matrix-runtime-resource-request "matrix-runtime"
                  (app method params resolve reject))

(defvar matrix-avatar--images (make-hash-table :test #'eq :weakness 'key)
  "Exact App to decoded images keyed by resource, size and source version.")

(defun matrix-avatar--mxc-p (uri)
  "Whether URI has the server/media shape of a nonempty MXC URI.
The SDK remains responsible for authoritative URI validation."
  (and (stringp uri)
       (string-match-p
        (concat "\\`mxc://\\(?:[[:alnum:]][[:alnum:].-]*"
                "\\|\\[[[:xdigit:]:.]+\\]\\)\\(?::[0-9]+\\)?/"
                "[^/[:space:][:cntrl:]?#\\\\]+\\'")
        uri)))

(defun matrix-avatar-resource-key (app uri)
  "Return an account-private, URI-versioned key for APP's MXC URI, or nil."
  (when (and (appkit-app-p app) (matrix-avatar--mxc-p uri))
    (list :matrix-avatar
          (secure-hash 'sha256 (prin1-to-string (appkit-app-identity app)))
          (secure-hash 'sha256 uri))))

(defun matrix-avatar-demand (app uri)
  "Describe APP's avatar acquisition for URI without performing I/O."
  (when-let* ((key (matrix-avatar-resource-key app uri))
              (transport (plist-get (appkit-app-model app) :transport))
              ((matrix-rpc-transport-p transport))
              (store (matrix-rpc-transport-store transport)))
    (appkit-resource-demand-create
     :key key
     :input (list app (substring-no-properties uri)
                  (substring-no-properties store))
     :loader #'matrix-avatar--load
     :acquisition-identity key
     :sharing-policy 'app-private
     :cache-policy 'while-interested)))

(defun matrix-avatar-fetch (app uri resolve &optional reject)
  "Fetch URI's avatar through APP to a private local file.
RESOLVE receives the source file path; REJECT receives an error value.  The
backend performs authenticated acquisition and only a path inside APP's
private store is published.  Unlike `matrix-avatar-demand', this is usable
outside a Surface.  Return a cancellation."
  (let* ((transport (plist-get (appkit-app-model app) :transport))
         (store (and (matrix-rpc-transport-p transport)
                     (matrix-rpc-transport-store transport))))
    (unless (and store (matrix-avatar--mxc-p uri))
      (user-error "Matrix avatar requires a connected account and an MXC URI"))
    (matrix-runtime-resource-request
     app "media.avatar" (list :uri uri)
     (lambda (result)
       (let ((source (condition-case nil
                         (matrix-avatar--source store result)
                       (error nil))))
         (funcall (if source resolve (or reject #'ignore))
                  (if source (car source) 'invalid-avatar-source))))
     (or reject #'ignore))))

(defun matrix-avatar--private-p (path directory-p)
  "Whether PATH is owned by this user, private and of the required kind."
  (let ((attributes (file-attributes path 'integer))
        (modes (file-modes path)))
    (and attributes modes
         (eq (file-attribute-type attributes) (and directory-p t))
         (equal (file-attribute-user-id attributes) (user-uid))
         (zerop (logand modes #o077))
         attributes)))

(defun matrix-avatar--source (store result)
  "Validate RESULT as a private image in STORE and return its source version."
  (let ((file (and (listp result) (plist-get result :path))))
    (when (and (stringp file) (file-name-absolute-p file)
               (not (file-remote-p file)))
      (let* ((file (substring-no-properties (expand-file-name file)))
             (store (file-name-as-directory (expand-file-name store)))
             (cache (expand-file-name "avatars/" store))
             (directory (file-name-directory file))
             (attributes (matrix-avatar--private-p file nil)))
        (when (and (string-prefix-p cache file) attributes
                   (> (file-attribute-size attributes) 0)
                   (file-readable-p file))
          (while (and directory (string-prefix-p store directory)
                      (matrix-avatar--private-p
                       (directory-file-name directory) t))
            (setq directory
                  (unless (equal directory store)
                    (file-name-directory (directory-file-name directory)))))
          (when (and (null directory)
                     (appkit-media-image-mime-type file)
                     ;; A terminal can retain a typed local source, but only an
                     ;; image-capable display can validate a decoded descriptor.
                     (or (not (appkit-media-inline-image-rendering-available-p))
                         (appkit-media-image-object-valid-p
                          (create-image file nil nil :ascent 'center))))
            (list file
                  (list (file-attribute-modification-time attributes)
                        (file-attribute-status-change-time attributes)
                        (file-attribute-size attributes)
                        (file-attribute-inode-number attributes)
                        (file-attribute-device-number attributes)))))))))

(defun matrix-avatar--load (_context input resolve reject)
  "Adapt one authenticated backend response to a managed local avatar source."
  (pcase-let ((`(,app ,uri ,store) input))
    (matrix-runtime-resource-request
     app "media.avatar" (list :uri uri)
     (lambda (result)
       (if-let* ((source (condition-case nil
                            (matrix-avatar--source store result)
                          (error nil))))
           (funcall resolve source)
         (funcall reject 'invalid-avatar-source)))
     reject)))

(defun matrix-avatar--decode (file size)
  "Return a valid circular SIZE image of FILE, with a square fallback."
  (or (appkit-media-circular-image-from-file file size)
      (condition-case nil
          (let ((image (create-image file nil nil
                                     :width size :height size :ascent 'center)))
            (and (appkit-media-image-object-valid-p image) image))
        (error nil))))

(defun matrix-avatar-cached-image (app uri pixel-size)
  "Return APP's committed ready avatar for URI at PIXEL-SIZE, or nil.
Call from APP's generated Surface buffer.  This lookup never discovers files
or requests media; only decoding a ready local source may perform I/O."
  (when-let* ((key (matrix-avatar-resource-key app uri))
              ((and (numberp pixel-size) (> pixel-size 0)))
              (surface (appkit-current-surface))
              ((eq app (appkit-surface-app surface)))
              (state (appkit-resource-state surface key))
              ((eq (appkit-resource-state-status state) 'ready))
              (source (appkit-resource-state-value state))
              ((appkit-media-inline-image-rendering-available-p)))
    (let* ((size (max 1 (round pixel-size)))
           (images (or (gethash app matrix-avatar--images)
                       (puthash app (make-hash-table :test #'equal)
                                matrix-avatar--images)))
           (cache-key (list key size source))
           (cached (gethash cache-key images)))
      (if cached
          (unless (eq cached 'missing) cached)
        (let ((image (matrix-avatar--decode (car source) size)))
          (puthash cache-key (or image 'missing) images)
          image)))))

(defun matrix-avatar-clear-cache (app)
  "Discard only APP's decoded avatar descriptors at account shutdown."
  (remhash app matrix-avatar--images))

(provide 'matrix-avatar)
;;; matrix-avatar.el ends here
