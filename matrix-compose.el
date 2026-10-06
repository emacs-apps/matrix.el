;;; matrix-compose.el --- Structured Matrix drafts and source ownership -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Matrix contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Appkit input objects are presentation, not captions.  Private clipboard
;; sources have App-owned handles, a Surface draft reference, and captured-send
;; references.  Only the existing Matrix request operation admits a send.
;; Reducers settle the draft; post-commit effects release source ownership.

;;; Code:

(require 'cl-lib)
(require 'mailcap)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-chatbuf)
(require 'appkit-command)
(require 'appkit-effect)
(require 'appkit-media-resource)
(require 'appkit-media-image)
(require 'matrix-runtime)
(require 'matrix)

(declare-function matrix--current-surface "matrix" (&optional kind))
(declare-function matrix--error-text "matrix" (error))
(declare-function matrix-room-cancel-edit "matrix-room" ())
(declare-function matrix-reply-validate "matrix-reply" (surface aux))
(declare-function matrix-reply-cancel "matrix-reply" ())

(defvar matrix-compose--serial 0
  "Process-local serial for immutable attachment identities.")

(cl-defstruct (matrix-compose--source (:constructor matrix-compose--source-create))
  "A private source; user-selected files never have this record."
  path directory handle draft (pending 0))

(cl-defstruct (matrix-compose--draft (:constructor matrix-compose--draft-create))
  "Exact Surface ownership of private sources in input or saved edit input."
  surface handle sources)

(defvar-local matrix-compose--draft nil
  "This host's exact private-source draft owner, created lazily.")

(defun matrix-compose--attachment-p (object)
  "Whether OBJECT is a Matrix local attachment."
  (and (listp object) (eq (plist-get object :type) 'file)))

(defun matrix-compose-attachments (&optional input)
  "Return ordered attachment objects from property-rich INPUT."
  (let* ((value (or input (appkit-chatbuf-input-state)))
         (finish (length value)) (position 0) attachments)
    (while (< position finish)
      (let ((object (get-text-property
                     position appkit-chatbuf-input-object-property value)))
        (when (matrix-compose--attachment-p object)
          (push object attachments)))
      (setq position (appkit-chatbuf-next-input-object-change
                      position value finish)))
    (nreverse attachments)))

(defun matrix-compose-serialize (input)
  "Return plain caption text from INPUT, omitting complete attachment spans."
  (let ((position 0) (finish (length input)) pieces)
    (while (< position finish)
      (let* ((object (get-text-property
                      position appkit-chatbuf-input-object-property input))
             (next (appkit-chatbuf-next-input-object-change position input finish)))
        (cond
         ((matrix-compose--attachment-p object) nil)
         (object (user-error "This Matrix draft contains an unsupported input object"))
         (t (push (substring-no-properties input position next) pieces)))
        (setq position next)))
    (apply #'concat (nreverse pieces))))

(defun matrix-compose--exact-surface-p (surface)
  "Whether SURFACE still owns its exact host and account registration."
  (and (appkit-surface-live-p surface)
       (eq surface (appkit-app-surface
                    (appkit-surface-app surface) (appkit-surface-identity surface)))
       (buffer-live-p (appkit-surface-buffer surface))
       (with-current-buffer (appkit-surface-buffer surface)
         (eq surface (appkit-current-surface)))))

(defun matrix-compose--ensure-attachable (surface)
  "Reject states of SURFACE that cannot acquire or remove attachment objects."
  (unless (matrix-compose--exact-surface-p surface)
    (user-error "The originating Matrix composer is no longer live"))
  (when (plist-get (appkit-surface-model surface) :operation)
    (user-error "Wait for the current Matrix operation"))
  (when (eq (appkit-chatbuf-aux-type) 'edit)
    (user-error "File attachments cannot be changed while editing a message")))

(defun matrix-compose--fence (surface)
  "Capture exact composer and account identity before reentrant acquisition."
  (matrix-compose--ensure-attachable surface)
  (appkit-chatbuf-input-state-sync)
  (list :surface surface :buffer (current-buffer)
        :session (copy-tree (plist-get (matrix-runtime-state
                                      (appkit-surface-app surface)) :session))
        :revision (appkit-chatbuf-composer-revision)
        :input (appkit-chatbuf-input-state)
        :aux (copy-tree (appkit-chatbuf-aux-state))))

(defun matrix-compose--check-fence (fence)
  "Reject a replaced host, session, or changed composer captured in FENCE."
  (let ((surface (plist-get fence :surface)) (buffer (plist-get fence :buffer)))
    (unless (and (eq (current-buffer) buffer)
                 (matrix-compose--exact-surface-p surface)
                 (equal (plist-get fence :session)
                        (plist-get (matrix-runtime-state
                                    (appkit-surface-app surface)) :session)))
      (user-error "The room or account changed while choosing an attachment"))
    (appkit-chatbuf-input-state-sync)
    (unless (and (= (plist-get fence :revision) (appkit-chatbuf-composer-revision))
                 (equal-including-properties (plist-get fence :input)
                                             (appkit-chatbuf-input-state))
                 (equal-including-properties (plist-get fence :aux)
                                             (appkit-chatbuf-aux-state)))
      (user-error "The originating composer changed while choosing an attachment"))
    (matrix-compose--ensure-attachable surface)
    surface))

(defun matrix-compose--attachment (file &optional as-file mime-type)
  "Validate local FILE and return its immutable attachment object.
AS-FILE forces generic Matrix file MIME; MIME-TYPE may come from the clipboard."
  (let* ((path (expand-file-name file))
         (name (file-name-nondirectory path))
         mime)
    (unless (and (not (file-remote-p path)) (appkit-media-readable-file-p path))
      (user-error "Attachment is not a readable local regular file: %s" name))
    (when (or (string-empty-p name) (string-match-p "[[:cntrl:]]" name))
      (user-error "Attachment filename is invalid"))
    (setq mime (if as-file "application/octet-stream"
                 (or mime-type (appkit-media-image-mime-type path)
                     (mailcap-extension-to-mime
                      (downcase (or (file-name-extension name) "")))
                     "application/octet-stream")))
    (unless (string-match-p "\\`[[:alnum:]!#$%&'*+.^_`|~-]+/[[:alnum:]!#$%&'*+.^_`|~-]+\\'" mime)
      (user-error "Attachment MIME type is invalid"))
    (let* ((kind (cond ((string-prefix-p "image/" mime) 'image)
                       ((string-prefix-p "audio/" mime) 'audio)
                       ((string-prefix-p "video/" mime) 'video)
                       (t 'file)))
           (object (list :type 'file
                         :id (format "matrix-attachment-%x" (cl-incf matrix-compose--serial))
                         :path path :filename name :mime-type mime :kind kind
                         :size (file-attribute-size (file-attributes path 'integer))))
           (dimensions (and (eq kind 'image)
                            (appkit-media--image-header-size path))))
      (when (and (consp dimensions)
                 (integerp (car dimensions)) (> (car dimensions) 0)
                 (integerp (cdr dimensions)) (> (cdr dimensions) 0))
        (setq object (plist-put object :width (car dimensions))
              object (plist-put object :height (cdr dimensions))))
      object)))

(defun matrix-compose--label (attachment)
  "Return a safe presentation-only label for ATTACHMENT."
  (format "[%s] %s · %s"
          (pcase (plist-get attachment :kind)
            ('image "Image") ('audio "Audio") ('video "Video") (_ "File"))
          (plist-get attachment :filename)
          (file-size-human-readable (plist-get attachment :size))))

(defun matrix-compose--insert (attachments)
  "Stage fully validated ATTACHMENTS atomically, without sending."
  (appkit-chatbuf-focus-input)
  (atomic-change-group
    (dolist (attachment attachments)
      (appkit-chatbuf-input-insert (matrix-compose--label attachment) :object attachment)))
  attachments)

(defun matrix-compose-attach-file (file &optional as-file)
  "Stage local FILE, or choose one when FILE is nil.
With universal prefix AS-FILE, use generic m.file rather than inferred media."
  (interactive (list nil current-prefix-arg))
  (let* ((surface (matrix--current-surface 'room))
         (fence (matrix-compose--fence surface))
         (file (or file (appkit-media-read-file-name "Attach media/file: ")))
         (attachment (matrix-compose--attachment file as-file)))
    (matrix-compose--check-fence fence)
    (matrix-compose--insert (list attachment))
    attachment))

(defun matrix-compose--delete-source (source)
  "Delete only the private clipboard file and directory in SOURCE."
  (ignore-errors (delete-file (matrix-compose--source-path source)))
  (ignore-errors (delete-directory (matrix-compose--source-directory source))))

(defun matrix-compose--release-unused (source)
  "Release SOURCE only when neither its draft nor a pending capture owns it."
  (when (and (not (matrix-compose--source-draft source))
             (zerop (matrix-compose--source-pending source)))
    (appkit-cancel-handle (matrix-compose--source-handle source))))

(defun matrix-compose--close-draft (draft)
  "Drop exact DRAFT ownership, retaining sources owned by pending captures."
  (dolist (handle (matrix-compose--draft-sources draft))
    (let ((source (appkit-handle-object handle)))
      (when (eq draft (matrix-compose--source-draft source))
        (setf (matrix-compose--source-draft source) nil)
        (matrix-compose--release-unused source))))
  (setf (matrix-compose--draft-sources draft) nil)
  (when-let* ((buffer (appkit-surface-buffer (matrix-compose--draft-surface draft)))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when (eq draft matrix-compose--draft) (setq matrix-compose--draft nil)))))

(defun matrix-compose--draft-owner (surface)
  "Return or create the private-source draft owner for exact SURFACE."
  (unless (and matrix-compose--draft
               (eq surface (matrix-compose--draft-surface matrix-compose--draft)))
    (setq matrix-compose--draft (matrix-compose--draft-create :surface surface))
    (setf (matrix-compose--draft-handle matrix-compose--draft)
          (appkit-register-handle surface 'matrix-compose-draft matrix-compose--draft
                                  #'matrix-compose--close-draft)))
  matrix-compose--draft)

(defun matrix-compose--stage-image (surface source as-file fence)
  "Stage clipboard SOURCE bytes privately after rechecking acquisition FENCE."
  (let* ((suffix (plist-get source :suffix))
         (bytes (plist-get source :data))
         directory path handle succeeded)
    (unless (and (member suffix '(".png" ".jpg" ".bmp"))
                 (stringp bytes) (> (length bytes) 0))
      (user-error "Clipboard has no supported image bytes"))
    (unwind-protect
        (progn
          (setq directory (make-temp-file "matrix-compose-" t)
                path (expand-file-name (concat "clipboard" suffix) directory))
          (set-file-modes directory #o700)
          (let ((coding-system-for-write 'no-conversion))
            (write-region bytes nil path nil 'silent))
          (set-file-modes path #o600)
          (let* ((attachment (matrix-compose--attachment path as-file (plist-get source :mime-type)))
                 (draft (progn (matrix-compose--check-fence fence)
                               (matrix-compose--draft-owner surface)))
                 (state (matrix-compose--source-create :path path :directory directory :draft draft)))
            (setq handle (appkit-register-handle
                          (appkit-surface-app surface) 'matrix-compose-source state
                          #'matrix-compose--delete-source))
            (setf (matrix-compose--source-handle state) handle)
            (setq attachment (plist-put attachment :temporary-handle handle))
            (matrix-compose--insert (list attachment))
            (push handle (matrix-compose--draft-sources draft))
            (setq succeeded t)
            attachment))
      (unless succeeded
        (if handle (appkit-cancel-handle handle)
          (when path (ignore-errors (delete-file path)))
          (when directory (ignore-errors (delete-directory directory))))))))

(defun matrix-compose-attach-clipboard (&optional as-file)
  "Stage real clipboard local files or private image bytes without sending.
With universal prefix AS-FILE, use generic m.file for every staged object."
  (interactive "P")
  (let* ((surface (matrix--current-surface 'room))
         (fence (matrix-compose--fence surface))
         (source (appkit-media-clipboard-source)))
    (matrix-compose--check-fence fence)
    (if-let* ((files (plist-get source :files)))
        (let ((attachments (mapcar (lambda (file) (matrix-compose--attachment file as-file)) files)))
          (matrix-compose--check-fence fence)
          (matrix-compose--insert attachments))
      (matrix-compose--stage-image surface source as-file fence))))

(defun matrix-compose-prune (&optional surface)
  "Release private files absent from exact SURFACE's input and saved edit draft.
Call only outside reducers; captured references independently retain sources."
  (let ((surface (or surface (appkit-current-surface))))
    (when (matrix-compose--exact-surface-p surface)
      (with-current-buffer (appkit-surface-buffer surface)
        (when (and matrix-compose--draft
                   (eq surface (matrix-compose--draft-surface matrix-compose--draft)))
          (appkit-chatbuf-input-state-sync)
          (let* ((draft matrix-compose--draft)
                 (objects (append (matrix-compose-attachments)
                                  (matrix-compose-attachments
                                   (or (plist-get (appkit-chatbuf-aux-state) :saved-input) ""))))
                 (retained (mapcar (lambda (object) (plist-get object :temporary-handle)) objects))
                 (sources (matrix-compose--draft-sources draft)))
            (setf (matrix-compose--draft-sources draft)
                  (cl-remove-if-not (lambda (handle) (memq handle retained)) sources))
            (dolist (handle sources)
              (unless (memq handle retained)
                (let ((source (appkit-handle-object handle)))
                  (setf (matrix-compose--source-draft source) nil)
                  (matrix-compose--release-unused source))))))))))

(defun matrix-compose-setup ()
  "Install private-source draft pruning in the canonical Appkit room input."
  (add-hook 'post-command-hook #'matrix-compose-prune nil t))

(defun matrix-compose-detach ()
  "Detach the attachment at point, or select an exact object by safe label."
  (interactive)
  (let* ((surface (matrix--current-surface 'room))
         (fence (matrix-compose--fence surface))
         (at-point (appkit-chatbuf-input-object-at-point))
         (attachments (matrix-compose-attachments))
         (target (if (matrix-compose--attachment-p at-point) at-point
                   (let ((index 0) choices)
                     (dolist (attachment attachments)
                       (push (cons (format "%s (%d)" (plist-get attachment :filename)
                                           (cl-incf index)) attachment) choices))
                     (unless choices (user-error "This composer has no attachment"))
                     (setq choices (nreverse choices))
                     (cdr (assoc (completing-read "Detach attachment: " choices nil t)
                                 choices))))))
    (matrix-compose--check-fence fence)
    (let* ((bounds (appkit-chatbuf-input-region-bounds))
           (position (car bounds)) (finish (cdr bounds)) found)
      (while (and (< position finish) (not found))
        (let* ((object (get-text-property position appkit-chatbuf-input-object-property))
               (next (appkit-chatbuf-next-input-object-change position nil finish)))
          (when (and (matrix-compose--attachment-p object)
                     (equal (plist-get object :id) (plist-get target :id)))
            (setq found (cons position next)))
          (setq position next)))
      (unless found (user-error "The selected attachment is no longer in this draft"))
      (delete-region (car found) (cdr found))
      (appkit-chatbuf-input-state-sync)
      (matrix-compose-prune surface)
      target)))

(defun matrix-compose-cancel ()
  "Cancel an idle edit or reply without deleting the ordinary draft."
  (interactive)
  (matrix--current-surface 'room)
  (pcase (appkit-chatbuf-aux-type)
    ('edit (matrix-room-cancel-edit))
    ('reply (matrix-reply-cancel))
    (_ (user-error "There is no edit or reply to cancel"))))

(defun matrix-compose--wire-attachment (attachment)
  "Return the backend descriptor of ATTACHMENT, with no ownership metadata."
  (let ((wire (list :path (plist-get attachment :path)
                    :filename (plist-get attachment :filename)
                    :mime_type (plist-get attachment :mime-type))))
    (dolist (key '(:width :height))
      (when-let* ((value (plist-get attachment key)))
        (setq wire (plist-put wire key value))))
    wire))

(defun matrix-compose-capture (surface)
  "Snapshot exact SURFACE input, aux, caption and ordered wire descriptors.
This function retains no files and starts no request."
  (unless (matrix-compose--exact-surface-p surface)
    (user-error "The originating Matrix composer is no longer live"))
  (with-current-buffer (appkit-surface-buffer surface)
    (appkit-chatbuf-input-state-sync)
    (let* ((input (appkit-chatbuf-input-state))
           (aux (copy-tree (appkit-chatbuf-aux-state)))
           (attachments (matrix-compose-attachments input))
           (body (matrix-compose-serialize input))
           (app (appkit-surface-app surface))
           (state (matrix-runtime-state app))
           (room-id (cadr (appkit-surface-identity surface)))
           (subscription (plist-get (gethash room-id (plist-get state :timelines)) :subscription_id))
           (params (list :room_id room-id :subscription_id subscription :body body
                         :attachments (vconcat (mapcar #'matrix-compose--wire-attachment attachments)))))
      (unless (and (stringp subscription) (not (string-empty-p subscription)))
        (user-error "Wait for the room's SDK timeline subscription"))
      (when (and (not attachments) (string-empty-p (string-trim body)))
        (user-error "The draft is empty"))
      (when (eq (plist-get aux :aux-type) 'reply)
        (setq params (plist-put params :reply_to (matrix-reply-validate surface aux))))
      (list :input input :revision (appkit-chatbuf-composer-revision) :aux aux
            :body body :attachments attachments :params params
            :compose-surface surface
            :compose-session (copy-tree (plist-get state :session))))))

(defun matrix-compose--release-capture-sources (sources)
  "Drop one captured reference to every private source handle in SOURCES."
  (dolist (handle sources)
    (let ((source (appkit-handle-object handle)))
      (cl-decf (matrix-compose--source-pending source))
      (matrix-compose--release-unused source))))

(defun matrix-compose-retain-capture (surface capture)
  "Return CAPTURE context retaining private files through terminal completion.
Only account-owned handles are retained; an originating room may close."
  (unless (and (matrix-compose--exact-surface-p surface)
               (eq surface (plist-get capture :compose-surface))
               (equal (plist-get capture :compose-session)
                      (plist-get (matrix-runtime-state (appkit-surface-app surface)) :session)))
    (user-error "The room or account changed before send admission"))
  (let ((app (appkit-surface-app surface)) sources)
    ;; Validate all sources before taking any references.
    (dolist (attachment (plist-get capture :attachments))
      (unless (appkit-media-readable-file-p (plist-get attachment :path))
        (user-error "A captured attachment is no longer readable"))
      (when-let* ((handle (plist-get attachment :temporary-handle)))
        (unless (and (appkit-handle-alive-p handle) (eq app (appkit-handle-owner handle)))
          (user-error "A captured clipboard source is no longer owned by this account"))
        (cl-pushnew handle sources :test #'eq)))
    (let* ((context (copy-sequence capture))
           (owner (and sources
                       (appkit-register-handle app 'matrix-compose-capture sources
                                               #'matrix-compose--release-capture-sources))))
      (dolist (handle sources)
        (cl-incf (matrix-compose--source-pending (appkit-handle-object handle))))
      (setq context (plist-put context :compose-release-key
                               (make-symbol "matrix-compose-capture")))
      (plist-put context :compose-owner owner))))

(defun matrix-compose-release-capture (context)
  "Release CONTEXT's captured files once, outside any reducer."
  (when-let* ((owner (plist-get context :compose-owner)))
    (appkit-cancel-handle owner))
  (when-let* ((surface (plist-get context :compose-surface)))
    (matrix-compose-prune surface)))

(defun matrix-compose--complete-effect (_context input _observe resolve _reject)
  "Release committed capture INPUT outside the transition."
  (matrix-compose-release-capture input)
  (funcall resolve nil)
  nil)

(defun matrix-compose-complete-command (context &optional id)
  "Return an idempotent post-commit capture release command for CONTEXT.
Both the account and exact live Surface complete independently: account
completion handles dead targets, and Surface completion prunes settled input."
  (when (or (plist-get context :compose-owner) (plist-get context :compose-surface))
    (appkit-command-start-effect
     (appkit-effect-create
      :key (list 'matrix-compose-complete (or (plist-get context :compose-release-key) id
                                             (plist-get context :target)))
      :input context :start #'matrix-compose--complete-effect
      :success (lambda (_input _value) '(matrix-compose-released))
      :failure (lambda (_input _error) '(matrix-compose-release-failed))))))

(defun matrix-compose--prune-effect (_context surface _observe resolve _reject)
  "Prune SURFACE's committed draft outside the transition."
  (matrix-compose-prune surface)
  (funcall resolve nil)
  nil)

(defun matrix-compose-prune-command (surface)
  "Return a post-commit exact-SURFACE private-source pruning command."
  (appkit-command-start-effect
   (appkit-effect-create
    :key 'matrix-compose-prune :input surface :start #'matrix-compose--prune-effect
    :success (lambda (_input _value) '(matrix-compose-pruned))
    :failure (lambda (_input _error) '(matrix-compose-prune-failed)))))

(defun matrix-compose--same-capture-p (context)
  "Whether the current rich input and aux are exactly CONTEXT's revision."
  (appkit-chatbuf-input-state-sync)
  (and (equal (appkit-chatbuf-composer-revision) (plist-get context :revision))
       (equal-including-properties (appkit-chatbuf-input-state) (plist-get context :input))
       (equal-including-properties (appkit-chatbuf-aux-state) (plist-get context :aux))))

(defun matrix-compose--remaining-input (context accepted)
  "Keep only objects not among CONTEXT's ACCEPTED attachment prefix.
The caption belongs to the first accepted event and must not be retried."
  (let* ((input (plist-get context :input))
         (ids (mapcar (lambda (object) (plist-get object :id))
                      (cl-subseq (plist-get context :attachments) 0 accepted)))
         (position 0) (finish (length input)) pieces)
    (while (< position finish)
      (let* ((object (get-text-property position appkit-chatbuf-input-object-property input))
             (next (appkit-chatbuf-next-input-object-change position input finish)))
        (when (and (matrix-compose--attachment-p object)
                   (not (member (plist-get object :id) ids)))
          (push (substring input position next) pieces))
        (setq position next)))
    (apply #'concat (nreverse pieces))))

(defun matrix-compose-result (model result error context)
  "Return a transition settling confirmed SDK RESULT for MODEL's operation.
Draft changes commit before echo feedback and private-source release effects."
  (let* ((total (max 1 (length (plist-get context :attachments))))
         (accepted (if error 0 (plist-get result :accepted)))
         (failure (or error (plist-get result :error)))
         (valid (or error (and (integerp accepted) (<= 0 accepted total)
                              (equal total (plist-get result :total)))))
         (same (matrix-compose--same-capture-p context))
         (feedback
          (cond
           ((not valid)
            (setq model (plist-put model :uncertain-send t))
            "SDK acceptance count is invalid; draft retained. Inspect the timeline before sending again.")
           ((= accepted total)
            (when same
              (unless (string-empty-p (string-trim (plist-get context :body)))
                (appkit-chatbuf-input-history-push (plist-get context :body)))
              (appkit-chatbuf-input-replace "")
              (appkit-chatbuf-aux-reset))
            (format "Accepted %d/%d into SDK send queue; delivery is not yet confirmed.%s"
                    accepted total (if same "" " Newer draft retained.")))
           (t
            (when (or (and (> accepted 0) (not same))
                      (equal (plist-get (plist-get error :data) :code) "transport_uncertain"))
              (setq model (plist-put model :uncertain-send t)))
            (when (and same (> accepted 0))
              (appkit-chatbuf-input-replace (matrix-compose--remaining-input context accepted)))
            (concat (format "Accepted %d/%d into SDK send queue. " accepted total)
                    (if failure (concat (matrix--error-text failure) " ") "")
                    (cond
                     ((and (> accepted 0) same) "Unaccepted attachments retained; consumed caption removed.")
                     ((> accepted 0) "Newer draft retained; inspect accepted events and review before resending.")
                     (t "Draft retained; inspect the timeline if acceptance is uncertain.")))))))
    (matrix--feedback-next model feedback)))

(provide 'matrix-compose)
;;; matrix-compose.el ends here
