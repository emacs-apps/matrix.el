;;; matrix-media.el --- Private Matrix timeline image Resources -*- lexical-binding: t; -*-

;;; Commentary:
;; The SDK resolves and decrypts exact event sources.  Resources own acquisition;
;; committed rows own immutable Canvas posters and video.el playback occurrences.

;;; Code:

(require 'matrix-avatar)
(require 'appkit-media-image)

(declare-function appkit-media-resource-poster "appkit-media-resource"
                  (resource width height &rest args))
(declare-function matrix-runtime-state "matrix-runtime" (app))

(defconst matrix-media--byte-limit (* 32 1024 1024))
(defvar matrix-media--posters (make-hash-table :test #'eq :weakness 'key)
  "App-private latest geometry poster per exact event Resource.")
(defvar-local matrix-media--poster-owner nil
  "Exact Surface whose teardown releases this room's cached posters.")

(defun matrix-media-rendering-available-p ()
  "Whether the current render target supports native Canvas images."
  (let ((frame (or (appkit-geometry-display-frame) (selected-frame))))
    (and (display-graphic-p frame) (display-images-p frame)
         (image-type-available-p 'canvas))))

(defun matrix-media-resource-key (app room subscription item)
  "Identify APP's exact ROOM/SUBSCRIPTION image event in ITEM."
  (let ((source (plist-get (plist-get item :image) :source_id))
        (event (plist-get item :event_id)))
    (when (and (appkit-app-p app) (equal (plist-get item :kind) "message")
               (stringp room) (not (string-empty-p room))
               (stringp subscription) (not (string-empty-p subscription))
               (stringp event) (not (string-empty-p event))
               (stringp source) (string-match-p "\\`[0-9a-f]\\{64\\}\\'" source))
      (list :matrix-image
            (secure-hash 'sha256 (prin1-to-string (appkit-app-identity app)))
            room subscription event source))))

(defun matrix-media-demand (app room subscription item)
  "Describe exact ITEM acquisition without starting protocol work."
  (when-let* ((key (matrix-media-resource-key app room subscription item))
              ((matrix-media-rendering-available-p))
              (transport (plist-get (appkit-app-model app) :transport))
              ((matrix-rpc-transport-p transport)))
    (appkit-resource-demand-create
     :key key :input (list app (matrix-rpc-transport-store transport)
                           (list :room_id room :subscription_id subscription
                                 :event_id (plist-get item :event_id)
                                 :source_id (plist-get (plist-get item :image) :source_id)))
     :loader #'matrix-media--load :acquisition-identity key
     :sharing-policy 'app-private :cache-policy 'while-interested)))

(defun matrix-media--current-p (app params)
  "Whether PARAMS still names the current committed SDK event source."
  (let* ((state (matrix-runtime-state app))
         (timelines (plist-get state :timelines))
         (timeline (and timelines (gethash (plist-get params :room_id) timelines))))
    (and (appkit-app-live-p app)
         (plist-get state :backend-active)
         (equal (plist-get timeline :subscription_id) (plist-get params :subscription_id))
         (seq-some (lambda (item)
                     (and (equal (plist-get item :kind) "message")
                          (equal (plist-get item :event_id) (plist-get params :event_id))
                          (equal (plist-get (plist-get item :image) :source_id)
                                 (plist-get params :source_id))))
                   (plist-get timeline :items)))))

(defun matrix-media--source (store source-id result)
  "Admit RESULT only as SOURCE-ID's private immutable raster in STORE."
  (let ((path (and (listp result) (plist-get result :path))))
    (when (and (stringp path) (file-name-absolute-p path) (not (file-remote-p path)))
      (let* ((file (expand-file-name (substring-no-properties path)))
             (store (file-name-as-directory (expand-file-name store)))
             (cache (expand-file-name "media/" store))
             (relative (and (string-prefix-p cache file)
                            (substring file (length cache))))
             (attributes (matrix-avatar--private-p file nil))
             (directory (file-name-directory file)))
        (when (and relative
                   (string-match-p
                    (concat "\\`[^/]+/" (regexp-quote source-id) "\\.image\\'") relative)
                   attributes (< 0 (file-attribute-size attributes))
                   (<= (file-attribute-size attributes) matrix-media--byte-limit)
                   (file-readable-p file))
          (while (and directory (string-prefix-p store directory)
                      (matrix-avatar--private-p (directory-file-name directory) t))
            (setq directory
                  (unless (equal directory store)
                    (file-name-directory (directory-file-name directory)))))
          (when (and (null directory)
                     (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally file nil 0 16)
                       (appkit-media--known-image-signature-at-p (buffer-string) 0)))
            (list file (list (file-attribute-modification-time attributes)
                             (file-attribute-status-change-time attributes)
                             (file-attribute-size attributes)
                             (file-attribute-inode-number attributes)
                             (file-attribute-device-number attributes)))))))))

(defun matrix-media--load (_context input resolve reject)
  "Acquire INPUT through the account's cancellation-gated Resource bridge."
  (pcase-let ((`(,app ,store ,params) input))
    (matrix-runtime-resource-request
     app "media.image" params
     (lambda (result)
       (if-let* (((matrix-media--current-p app params))
                  (source (condition-case nil
                              (matrix-media--source store (plist-get params :source_id) result)
                            (error nil))))
           (funcall resolve source)
         (funcall reject 'invalid-or-retired-image-source)))
     reject)))

(defun matrix-media--poster (source max-width)
  "Draw SOURCE within a default-scale budget; display slices follow font zoom."
  (require 'appkit-media-resource)
  (let* ((file (car source))
         (size (appkit-media--image-file-size-pixels file))
         (line-height (appkit-media--base-char-pixel-height))
         (height-limit (max 1 appkit-media-preview-max-height)))
    (unless (and (consp size) (> (car size) 0) (> (cdr size) 0))
      (error "Image dimensions unavailable"))
    (let* ((rows (min (appkit-media-preview-height-chars size max-width height-limit)
                      (max 1 (floor (/ height-limit (float line-height))))
                      (max 1 (floor (/ (* max-width (/ (float (cdr size)) (car size)))
                                       line-height)))))
           (display-height (appkit-media--char-pixel-height))
           (scale (/ (float display-height) line-height))
           ;; Keep the line count font-relative, but decode the original at
           ;; its actual display size rather than magnifying a small poster.
           (height (max 1 (* rows display-height)))
           (width (max 1 (round
                          (min (* max-width scale)
                               (* height (/ (float (car size)) (cdr size)))))))
           (poster (appkit-media-resource-poster
                    `((file . ,file)) width height :fit 'contain)))
      (plist-put (cdr poster) :appkit-media-nslices rows)
      (appkit-media-image-with-resource poster `((file . ,file)) :interaction 'media))))

(defun matrix-media-cached-poster (app key max-width)
  "Return ready KEY's immutable Canvas poster, or a visible failure string.
Cache only the latest geometry for each event; insertion copies occurrence
geometry, and video.el owns independent mutable playback targets."
  (when-let* ((surface (appkit-current-surface))
              ((eq app (appkit-surface-app surface)))
              (state (appkit-resource-state surface key))
              ((eq (appkit-resource-state-status state) 'ready))
              ((matrix-media-rendering-available-p)))
    (unless (eq surface matrix-media--poster-owner)
      (setq matrix-media--poster-owner surface)
      (let ((room (cadr (appkit-surface-identity surface))))
        (appkit-register-handle
         surface 'function (lambda () (matrix-media-retain app room nil)))))
    (let* ((source (appkit-resource-state-value state))
           (cache (or (gethash app matrix-media--posters)
                      (puthash app (make-hash-table :test #'equal) matrix-media--posters)))
           (geometry (list source max-width appkit-media-preview-max-height
                           (appkit-media--base-char-pixel-height)
                           (appkit-media--char-pixel-height)
                           (face-background 'default (appkit-geometry-display-frame) t)))
           (entry (gethash key cache)))
      (unless (equal (car entry) geometry)
        (setq entry
              (list geometry
                    (condition-case nil (matrix-media--poster source max-width)
                      (error "[Image unavailable: native decoder failed]"))))
        (puthash key entry cache))
      (cadr entry))))

(defun matrix-media-retain (app room keys)
  "Drop retired ROOM event posters from APP's cache, retaining exact KEYS."
  (when-let* ((cache (gethash app matrix-media--posters)))
    (let (retired)
      (maphash (lambda (key _value)
                 (when (and (equal room (nth 2 key)) (not (member key keys)))
                   (push key retired))) cache)
      (dolist (key retired) (remhash key cache)))))

(defun matrix-media-clear-cache (app)
  "Discard APP's immutable posters on account shutdown."
  (remhash app matrix-media--posters))

(provide 'matrix-media)
;;; matrix-media.el ends here
