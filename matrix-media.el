;;; matrix-media.el --- Private Matrix timeline image Resources -*- lexical-binding: t; -*-

;;; Commentary:
;; The SDK resolves and decrypts exact event sources.  Resources own acquisition;
;; committed rows own immutable Canvas posters and video.el playback occurrences.
;; Attachment opening is two keyed Appkit Effects: acquisition publishes one
;; private local file, then presentation selects the media owner from its kind.

;;; Code:

(require 'matrix-avatar)
(require 'appkit-media-image)
(require 'appkit-media-playback)
(require 'appkit-media-effect)
(require 'appkit-media-resource)
(require 'appkit-command)

(declare-function appkit-media-resource-poster "appkit-media-resource"
                  (resource width height &rest args))
(declare-function matrix-runtime-state "matrix-runtime" (app))
(declare-function matrix-runtime-resource-request "matrix-runtime"
                  (app method params resolve reject))

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

(defun matrix-media-poster (item)
  "Return ITEM's inline poster plist, or nil.
It has the poster's :source_id; a video's also has :video t, :own, :width
and :height.  An image is its own poster.  A video's poster is its
thumbnail or, when :own, its own first frame, of declared size if any."
  (if-let* ((image (plist-get item :image)))
      (list :source_id (plist-get image :source_id))
    (when-let* ((poster (plist-get (plist-get item :attachment) :poster)))
      (list :source_id (plist-get poster :source_id) :video t
            :own (eq (plist-get poster :own) t)
            :width (plist-get poster :width) :height (plist-get poster :height)))))

(defun matrix-media-resource-key (app room subscription item)
  "Identify APP's exact ROOM/SUBSCRIPTION poster source in ITEM."
  (let ((source (plist-get (matrix-media-poster item) :source_id))
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
                                 :source_id (plist-get (matrix-media-poster item) :source_id)))
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
                          (equal (plist-get (matrix-media-poster item) :source_id)
                                 (plist-get params :source_id))))
                   (plist-get timeline :items)))))

(defun matrix-media--poster-bytes-p (bytes)
  "Whether leading BYTES start an image, or a video whose first frame posters it."
  (or (appkit-media--known-image-signature-at-p bytes 0)
      (string-prefix-p "\x1a\x45\xdf\xa3" bytes)
      (and (>= (length bytes) 8) (equal (substring bytes 4 8) "ftyp"))))

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
                       (matrix-media--poster-bytes-p (buffer-string))))
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

(defun matrix-media--poster (source max-width &optional video slot-key)
  "Draw SOURCE within a default-scale budget; display slices follow font zoom.
VIDEO, for a video's poster, is ITEM's `matrix-media-poster' plus :resolve,
a function acquiring the playable video when the poster is only a thumbnail.
SLOT-KEY identifies the event's media across geometry changes."
  (require 'appkit-media-resource)
  (let* ((file (car source))
         (size (or (appkit-media--image-file-size-pixels file)
                   ;; A video posting its own first frame has no image header.
                   (and video (cons (or (plist-get video :width) 16)
                                    (or (plist-get video :height) 9)))))
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
      (cond
       ((not video)
        (appkit-media-image-with-resource poster `((file . ,file))
                                          :slot-key slot-key :interaction 'media))
       ((plist-get video :own)
        (appkit-media-image-with-resource poster `((file . ,file)) :kind 'video
                                          :slot-key slot-key :label "matrix" :interaction 'media))
       (t
        (appkit-media-image-with-resource poster nil :kind 'video :label "matrix"
                                          :slot-key slot-key :cache-key slot-key
                                          :interaction 'media
                                          :resolve-function (plist-get video :resolve)))))))

(defun matrix-media-cached-poster (app key max-width &optional video)
  "Return ready KEY's immutable Canvas poster, or a visible failure string.
VIDEO describes a video's poster as for `matrix-media--poster'.
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
                    (condition-case nil (matrix-media--poster source max-width video key)
                      (error (format "[%s unavailable: native decoder failed]"
                                     (if video "Video" "Image"))))))
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

(defcustom matrix-download-directory "~/Downloads/"
  "Default directory for saving Matrix attachments."
  :type 'directory
  :group 'matrix)

(defcustom matrix-attachment-open-function #'matrix-media-open-file
  "Function opening a downloaded attachment, called with its file and kind.
The kind is \"file\", \"audio\", \"video\" or \"image\".
The default routes media through Appkit; replacing it opts out of the managed
video playback plan while retaining the keyed acquisition Effect."
  :type 'function
  :group 'matrix)

(defvar matrix-media--open-directory nil
  "Private directory of this session's opened attachment copies.")

(defun matrix-media-attachment (item)
  "Return ITEM's downloadable attachment as (KIND SOURCE-ID FILENAME), or nil."
  (when-let* (((equal (plist-get item :kind) "message"))
              (event (plist-get item :event_id))
              ((stringp event))
              (media (or (plist-get item :attachment) (plist-get item :image)))
              (source (plist-get media :source_id))
              ((stringp source)))
    (list (or (plist-get media :kind) "image") source
          (matrix-media--safe-filename (plist-get media :filename)))))

(defun matrix-media--safe-filename (name)
  "Return sender-chosen NAME as a plain file name in one directory."
  (let ((name (replace-regexp-in-string
               "\\`[.[:space:]]+" ""
               (replace-regexp-in-string
                "[\\[:cntrl:]]" "_"
                (file-name-nondirectory (if (stringp name) name ""))))))
    (if (string-empty-p name) "attachment" name)))

(defun matrix-media--delete-open-directory ()
  "Delete this session's opened attachment copies."
  (when matrix-media--open-directory
    (ignore-errors (delete-directory matrix-media--open-directory t))
    (setq matrix-media--open-directory nil)))

(defun matrix-media--open-path (filename)
  "Return a new private path named FILENAME for an opened attachment copy."
  (unless (and matrix-media--open-directory
               (file-directory-p matrix-media--open-directory))
    (setq matrix-media--open-directory (make-temp-file "matrix-open-" t))
    (set-file-modes matrix-media--open-directory #o700)
    (add-hook 'kill-emacs-hook #'matrix-media--delete-open-directory))
  (expand-file-name filename (make-temp-file
                              (expand-file-name "a" matrix-media--open-directory) t)))

(defun matrix-media-open-file (file kind)
  "Open downloaded FILE of KIND through Appkit's media owner.
Audio and video use an Appkit-owned video.el session; everything else uses
`appkit-media-open-file'.  Managed attachment opening uses a playback plan for
video and never calls this function unless the user has replaced
`matrix-attachment-open-function'."
  (if (member kind '("audio" "video"))
      (appkit-media-play-video-file file "Matrix attachment")
    (appkit-media-open-file file)))

(defun matrix-media--download-effect (input)
  "Return the keyed acquisition Effect for attachment INPUT."
  (appkit-effect-create
   :key 'matrix-media-download
   :input input
   :start #'matrix-media--download-start
   :success (lambda (input path)
              (list 'matrix-media-downloaded input path))
   :failure (lambda (_input reason)
              (list 'matrix-media-failed reason))))

(defun matrix-media--download-start (_context input _observe resolve reject)
  "Download INPUT's exact attachment, then settle RESOLVE with its path."
  (let ((cancellation
         (matrix-media-save
          (plist-get input :app)
          (plist-get input :room)
          (plist-get input :subscription)
          (plist-get input :item)
          (plist-get input :path)
          (lambda (path) (funcall resolve path))
          reject)))
    ;; Synchronous completion legitimately leaves no cancellation; never
    ;; forward an arbitrary mapper return value as Effect authority.
    (and (appkit-cancellation-p cancellation) cancellation)))

(defun matrix-media--present-effect (input path)
  "Return the keyed presentation Effect for downloaded INPUT at PATH."
  (appkit-effect-create
   :key 'matrix-media-present
   :input (plist-put (copy-sequence input) :path path)
   :start #'matrix-media--present-start
   :success (lambda (_input _value) '(matrix-media-presented))
   :failure (lambda (_input reason)
              (list 'matrix-media-failed reason))))

(defun matrix-media--present-start (context input _observe resolve reject)
  "Present INPUT through the selected Appkit media owner.
A user-replaced `matrix-attachment-open-function' keeps its explicit escape
hatch.  Otherwise video uses the common playback plan and presentation
starter; audio uses Appkit's local-file session; images and files use
`appkit-media-open-file'.  Video settlement follows the viewer, so closing it
or replacing the presentation Effect owns the complete lifecycle."
  (let* ((path (plist-get input :path))
         (kind (plist-get input :kind))
         (name (or (plist-get input :name)
                   (file-name-nondirectory path)))
         (open-function matrix-attachment-open-function))
    (cond
     ((not (eq open-function #'matrix-media-open-file))
      (condition-case condition
          (progn (funcall open-function path kind)
                 (funcall resolve 'presented)
                 nil)
        ((error quit)
         (funcall reject (error-message-string condition))
         nil)))
     ((equal kind "video")
      (appkit-media-video-presentation-start
       context
       (appkit-media-playback-presentation-create
        (appkit-media-playback-create
         :resource (appkit-media-resource-create :file path :name name)
         :kind 'video
         :label "Matrix attachment"
         :cache-policy 'none)
        :start t)
       nil resolve reject))
     ((equal kind "audio")
      (condition-case condition
          (progn
            ;; Appkit owns the session and closes it with its viewer.
            (appkit-media-play-video-file
             path "Matrix attachment" :owner (plist-get input :surface))
            (funcall resolve 'presented)
            nil)
        ((error quit)
         (funcall reject (error-message-string condition))
         nil)))
     (t
      (condition-case condition
          (progn (appkit-media-open-file path)
                 (funcall resolve 'presented)
                 nil)
        ((error quit)
         (funcall reject (error-message-string condition))
         nil))))))

(defun matrix-media-open-command (input)
  "Return a post-commit command opening exact attachment INPUT."
  (appkit-command-start-effect (matrix-media--download-effect input)))

(defun matrix-media-present-command (input path)
  "Return a post-commit command presenting downloaded INPUT at PATH."
  (appkit-command-start-effect (matrix-media--present-effect input path)))

(defun matrix-media-save (app room subscription item path on-saved &optional on-failed)
  "Download APP's ROOM/SUBSCRIPTION ITEM attachment to new PATH.
Call ON-SAVED with PATH once the decrypted file is in place, or ON-FAILED
with a message; without ON-FAILED, the message is shown.  Return the private
transport cancellation capability, or nil when the backend settles
synchronously."
  (let ((source (nth 1 (or (matrix-media-attachment item)
                           (user-error "This message has no downloadable attachment")))))
    (unless (and (stringp subscription) (not (string-empty-p subscription)))
      (user-error "This room's timeline is not ready"))
    (message "Matrix: downloading %s..." (file-name-nondirectory path))
    (matrix-runtime-resource-request
     app "media.save"
     (list :room_id room :subscription_id subscription
           :event_id (plist-get item :event_id) :source_id source
           :path (expand-file-name path))
     (lambda (_result) (funcall on-saved path))
     (lambda (error)
       (let ((text (or (and (listp error) (plist-get error :message))
                       "Attachment download was not sent")))
         (if on-failed (funcall on-failed text) (message "Matrix: %s" text)))))))

(defun matrix-media-clear-cache (app)
  "Discard APP's immutable posters on account shutdown."
  (remhash app matrix-media--posters))

(provide 'matrix-media)
;;; matrix-media.el ends here
