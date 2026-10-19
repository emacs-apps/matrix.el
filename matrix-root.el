;;; matrix-root.el --- Matrix account and room directory -*- lexical-binding: t; -*-

;;; Commentary:
;; Stable-key native directory rows for one account.  Rooms do not own the
;; account's sync process; closing this directory is also harmless to sync.

;;; Code:

(require 'matrix)
(require 'appkit-directory)
(require 'appkit-presentation)
(require 'appkit-chat-avatar)

(declare-function matrix-room-open "matrix-room" (app room-id &optional select))

(defvar matrix-root-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map appkit-directory-mode-map)
    (define-key map (kbd "j") #'matrix-join-room)
    (define-key map (kbd "c") #'matrix-create-room)
    (define-key map (kbd "v") #'matrix-devices)
    (define-key map (kbd "D") #'matrix-disconnect)
    map)
  "Keymap for `matrix-root-mode'.")

(defvar-keymap matrix-root-navigation-map
  :doc "Fast navigation between rooms, after telega's root `M-g' map."
  "u" #'matrix-root-next-unread
  "m" #'matrix-root-next-mention
  "@" #'matrix-root-next-mention)

(appkit-bindings-define 'matrix-root-mode-map 'read-only
  'app-menu #'matrix-root-transient
  'navigation matrix-root-navigation-map
  'refresh #'matrix-root-refresh
  'search #'matrix-root-filter)

(define-derived-mode matrix-root-mode appkit-directory-mode "Matrix"
  "Matrix room directory.  RET opens a room; j joins and c creates a room.")

(defun matrix-root--unread (room)
  "Return ROOM's unread trail and its face: @mentions before the count."
  (let ((unread (or (plist-get room :unread) 0))
        (mentions (or (plist-get room :mentions) 0)))
    (cons (string-join (delq nil (list (when (> mentions 0) (format "@%d" mentions))
                                       (when (> unread 0) (number-to-string unread))))
                       " ")
          (if (> mentions 0) 'matrix-mention 'bold))))


(defun matrix-root--entries (state model)
  "Project room destinations and actionable account state, not a command manual."
  (let ((rooms (plist-get state :rooms))
        (filter (downcase (or (plist-get model :filter) "")))
        (active 0)
        notices entries)
    (when-let* ((error (plist-get state :last-error)))
      (push (matrix--directory-note '(error) error 'error) notices))
    (when-let* ((notice (plist-get model :notice)))
      (push (matrix--directory-note '(operation) notice) notices))
    (when (plist-get model :operation)
      (push (matrix--directory-note '(pending) "Waiting for account operation…" 'shadow)
            notices))
    (when-let* ((flows (plist-get state :verifications)))
      (maphash (lambda (_key flow)
                 (unless (member (plist-get flow :state) '("done" "cancelled"))
                   (cl-incf active)))
               flows))
    (when (> active 0)
      (push (matrix--directory-action
             '(verification) (format "%d active device verification(s)" active)
             '(command matrix-devices))
            notices))
    (when-let* ((room-id (plist-get model :ready-room))
                ((not (seq-find (lambda (room) (equal room-id (plist-get room :room_id)))
                                rooms))))
      (push (matrix--directory-action
             '(ready-room) (format "Open joined room: %s" room-id)
             (list 'room room-id "join"))
            notices))
    (when notices
      (setq entries
            (cons (appkit-directory-entry-create
                   :key '(account) :role 'section :label "Account" :face 'bold)
                  (nreverse notices))))
    (dolist (section '(("join" . "Joined rooms") ("invite" . "Invitations")))
      (let* ((members
              (sort (seq-filter
                     (lambda (room)
                       (and (equal (plist-get room :membership) (car section))
                            (string-match-p
                             (regexp-quote filter)
                             (downcase (format "%s %s" (plist-get room :name)
                                               (plist-get room :room_id))))))
                     rooms)
                    (lambda (a b)
                      (let ((at (or (plist-get a :timestamp) 0))
                            (bt (or (plist-get b :timestamp) 0)))
                        (if (= at bt)
                            (let ((an (or (plist-get a :name) (plist-get a :room_id)))
                                  (bn (or (plist-get b :name) (plist-get b :room_id))))
                              (if (equal an bn)
                                  (string-lessp (plist-get a :room_id)
                                                (plist-get b :room_id))
                                (string-lessp an bn)))
                          (> at bt))))))
             (section-key (list 'section (car section)))
             (rows
              (list (appkit-directory-entry-create
                     :key section-key :role 'section :face 'bold
                     :label (format "%s (%d)" (cdr section) (length members))
                     :stamp (length members)))))
        (dolist (room members)
          (let ((id (plist-get room :room_id)))
            (push (appkit-directory-entry-create
                   :key (list 'room id) :role 'item :section-key section-key
                   :label (matrix--one-line (or (plist-get room :name) id))
                   :item-p t
                   :unread-p (not (string-empty-p (car (matrix-root--unread room))))
                   :payload (list 'room id (plist-get room :membership))
                   :stamp (list room (plist-get (plist-get state :session) :user_id))
                   :help-echo (matrix-root--room-help room))
                  rows)))
        (setq entries (append entries (nreverse rows)))))
    entries))

(defun matrix-root--room-help (room)
  "Return ROOM's exact destination and available activity identity."
  (string-join
   (delq nil
         (list (plist-get room :room_id)
               (if (eq (plist-get room :encrypted) t)
                   "Encrypted" "Encryption not known")
               (when-let* ((sender (plist-get room :preview_sender)))
                 (concat "Sender: " (appkit-ui-one-line-text sender)))))
   " · "))

(defun matrix-root--preview (room user-id)
  "Project actual ROOM activity, preserving exact sender identity."
  (let* ((body (appkit-ui-one-line-text (plist-get room :preview)))
         (sender (plist-get room :preview_sender))
         (self-p (and (stringp user-id)
                      (equal user-id sender))))
    (appkit-ui-one-line-preview-create
     :text (if (or (string-empty-p body) self-p (not sender))
               body
             (concat body " — " (appkit-ui-one-line-text sender)))
     :label (when (and self-p (not (string-empty-p body))) "You")
     :separator ":" :label-face 'shadow)))

(defun matrix-root--insert-compact-context (title trail width face)
  "Insert TITLE and unread TRAIL in FACE within WIDTH without activity slots."
  (let* ((inner (max 0 (- width 2)))
         (trail-width (min inner (string-width (or trail ""))))
         (title-width (max 0 (- inner trail-width (if trail 1 0)))))
    (when (> width 0) (insert "["))
    (when (> title-width 0)
      (insert (appkit-presentation-elide-string-for-columns title title-width)))
    (when (> trail-width 0)
      (when (> title-width 0) (insert " "))
      (insert (propertize
               (appkit-presentation-elide-string-for-columns trail trail-width face)
               'face face)))
    (when (> width 1) (insert "]"))))

(defun matrix-root--insert-item (_directory entry)
  "Insert one bounded activity row from ENTRY's actual SDK summary."
  (let* ((stamp (appkit-directory-entry-stamp entry))
         (room (and (listp (car-safe stamp)) (car stamp)))
         (user-id (cadr stamp)))
    (if (not (plist-get room :room_id))
        (insert (appkit-ui-one-line-text (appkit-directory-entry-label entry)) "\n")
      (let* ((surface (appkit-current-surface))
             (width (matrix--width surface))
             (timestamp (plist-get room :timestamp))
             (unread (matrix-root--unread room))
             (trail (unless (string-empty-p (car unread)) (car unread)))
             (pixel-size (when (appkit-media-inline-image-rendering-available-p)
                           (appkit-chat-avatar-line-pixel-height)))
             (avatar-width
              (when pixel-size
                (ceiling (/ (float pixel-size)
                            (appkit-chat-avatar-column-pixel-width)))))
             (image (when pixel-size
                      (matrix-avatar-cached-image
                       (appkit-surface-app surface) (plist-get room :avatar_url)
                       pixel-size)))
             (avatar (when pixel-size
                       (appkit-chat-avatar-one-line-string image "@")))
             (icon (concat
                    (when avatar
                      (concat avatar
                              (make-string
                               (max 1 (- (1+ avatar-width) (string-width avatar)))
                               ?\s)))
                    (propertize (if (eq (plist-get room :encrypted) t) "+" "?")
                                'face 'shadow)))
             (icon-width (max 2 (1+ (string-width icon))))
             (title (appkit-directory-entry-label entry))
             (help (matrix-root--room-help room))
             (start (point)))
        ;; The shared activity inserter reserves at least twenty content columns.
        (if (< width 36)
            (progn
              (when (>= width 8) (insert icon " "))
              (matrix-root--insert-compact-context
               title trail (max 0 (- width (if (>= width 8) icon-width 0)))
               (cdr unread))
              (insert "\n")
              (add-text-properties start (point) (list 'help-echo help)))
          (appkit-presentation-insert-one-line-row
           (appkit-presentation-one-line-row-create
            :icon-inserter (lambda () (insert icon))
            :context title :context-trail trail :context-trail-face (cdr unread)
            :preview (matrix-root--preview room user-id)
            :time (when (and (>= width 60) (numberp timestamp) (> timestamp 0))
                    (format-time-string "%m-%d %H:%M"
                                        (seconds-to-time (/ timestamp 1000.0))))
            :time-face 'shadow :help-echo help)
           :width width :icon-slot-width icon-width :context-width-spec '(0.55 16 36)))))))

(defun matrix-root--activate (_directory entry)
  "Activate ENTRY; invitation consent authorizes only its originating Surface."
  (let* ((surface (matrix--current-surface 'root))
         (app (appkit-surface-app surface))
         (session (copy-tree (plist-get (matrix-runtime-state app) :session))))
    (pcase (appkit-directory-entry-payload entry)
      (`(command ,command) (call-interactively command))
      (`(room ,room-id "invite")
       (when (yes-or-no-p (format "Join invited room %s? " room-id))
         (unless (and (appkit-surface-live-p surface)
                      (eq surface (matrix--current-surface 'root))
                      (eq surface (appkit-app-surface app '(root)))
                      (equal session (plist-get (matrix-runtime-state app) :session))
                      (equal "invite" (plist-get (matrix-runtime-room app room-id) :membership)))
           (user-error "Invitation origin changed; activate the invitation again"))
         (matrix--request surface "room.join" (list :room_id room-id))))
      (`(room ,room-id ,_membership)
       (require 'matrix-room)
       (matrix-room-open app room-id t)))))

(defun matrix-root--mount (_surface _app-view _model)
  "Install native account directory activation."
  (appkit-directory-configure (appkit-directory-surface)
                              :activate-function #'matrix-root--activate
                              :item-inserter #'matrix-root--insert-item
                              :action-rows-p t))

(defun matrix-root--render (surface state model request)
  "Render pass-scoped STATE with compact room rows and native chrome."
  (matrix--frame state "Rooms"
                 (when-let* ((filter (plist-get model :filter))
                             ((not (string-empty-p filter))))
                   (format "filter: %s" filter)))
  (let* ((entries (matrix-root--entries state model))
         (avatars (cl-loop for entry in entries
                           for room = (car-safe (appkit-directory-entry-stamp entry))
                           when (and (listp room) (plist-get room :room_id))
                           collect (cons (appkit-directory-entry-key entry)
                                         (plist-get room :avatar_url)))))
    (appkit-directory-reconcile
     (appkit-directory-surface) entries
     :force-keys (if (appkit-projection-change-geometry-p request)
                     (mapcar #'appkit-directory-entry-key entries)
                   (matrix--avatar-changed-rows surface avatars request))
     :preserve-position-p t)
    (matrix--avatar-resource-result surface avatars)))

(defun matrix-root--result (model method result error _context)
  "Accept directed METHOD RESULT or ERROR into presentation MODEL."
  (if (and (not error) (member method '("room.join" "room.create")))
      (plist-put model :ready-room (plist-get result :room_id))
    model))

(defvar matrix-root--type
  (matrix--surface-type 'matrix-root #'matrix-root-mode
                        #'matrix-root--render #'matrix-root--mount)
  "Generated account directory Surface type.")

(defun matrix-root-open (app &optional select)
  "Open APP's account directory, optionally SELECT it."
  (matrix--open app matrix-root--type '(root)
                (format "*Matrix:%s*" (plist-get (matrix-runtime-state app) :account))
                select))

(defun matrix-root-filter ()
  "Filter visible destinations by room name or ID; empty input clears the filter."
  (interactive)
  (let* ((surface (matrix--current-surface 'root))
         (filter (read-string "Filter rooms: "
                              (plist-get (appkit-surface-model surface) :filter))))
    (appkit-surface-send surface (list 'matrix-filter (string-trim filter)))))

;;;###autoload
(defun matrix-switch-room ()
  "Select a joined room from the current account using native completion."
  (interactive)
  (let* ((app (matrix--current-app))
         (choices
          (mapcar (lambda (room)
                    (cons (format "%s  (%s)" (matrix--one-line (plist-get room :name))
                                  (plist-get room :room_id))
                          (plist-get room :room_id)))
                  (seq-filter (lambda (room) (equal (plist-get room :membership) "join"))
                              (plist-get (matrix-runtime-state app) :rooms)))))
    (unless choices (user-error "No joined rooms in this account"))
    (let ((room-id (cdr (assoc (completing-read "Room: " choices nil t) choices))))
      (require 'matrix-room)
      (matrix-room-open app room-id t))))

(defun matrix-root--command-surface ()
  "Return a root Surface for the current account, opening it if necessary."
  (matrix-root-open (matrix--current-app) t))

(defun matrix-root-refresh ()
  "Refresh current account's room summaries without replacing any draft."
  (interactive)
  (matrix-runtime-refresh (matrix--current-app)))

;;;###autoload
(defun matrix-join-room (&optional room-id)
  "Join ROOM-ID or alias, or accept an invitation to that room."
  (interactive)
  (let ((surface (matrix-root--command-surface))
        (id (or room-id (read-string "Room ID or alias: "))))
    (when (string-empty-p (string-trim id)) (user-error "A room ID is required"))
    (matrix--request surface "room.join" (list :room_id id))))

;;;###autoload
(defun matrix-create-room ()
  "Create a private, encrypted room, optionally inviting Matrix users."
  (interactive)
  (let* ((surface (matrix-root--command-surface))
         (name (read-string "Private encrypted room name: "))
         (invite (split-string (read-string "Invite user IDs (space separated, optional): ")
                               "[ \t\n,]+" t)))
    (matrix--request surface "room.create"
                     (list :name name :invite (vconcat invite)))))

(defun matrix-root--goto-room (predicate what)
  "Move to the next room row whose summary satisfies PREDICATE, wrapping.
WHAT names the rooms sought when there is none."
  (let ((app (appkit-surface-app (matrix--current-surface 'root)))
        (after (line-end-position))
        positions)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (pcase (appkit-directory-entry-payload (appkit-directory-entry-at-point))
          (`(room ,room-id ,_)
           (when (funcall predicate (matrix-runtime-room app room-id))
             (push (point) positions))))
        (forward-line 1)))
    (setq positions (nreverse positions))
    (goto-char (or (seq-find (lambda (position) (> position after)) positions)
                   (car positions)
                   (user-error "No %s" what)))))

(defun matrix-root-next-unread ()
  "Move to the next room with unread messages."
  (interactive)
  (matrix-root--goto-room (lambda (room) (> (or (plist-get room :unread) 0) 0))
                          "room with unread messages"))

(defun matrix-root-next-mention ()
  "Move to the next room with unread mentions."
  (interactive)
  (matrix-root--goto-room (lambda (room) (> (or (plist-get room :mentions) 0) 0))
                          "room with unread mentions"))

(defun matrix-root-decline-invitation ()
  "Decline the invitation at point after confirming."
  (interactive)
  (let ((surface (matrix--current-surface 'root)))
    (pcase (appkit-directory-entry-payload (appkit-directory-entry-at-point))
      (`(room ,room-id "invite")
       (when (yes-or-no-p (format "Decline the invitation to %s? "
                                  (or (plist-get (matrix-runtime-room
                                                  (appkit-surface-app surface) room-id)
                                                 :name)
                                      room-id)))
         (matrix--request surface "room.leave" (list :room_id room-id))))
      (_ (user-error "Point is not on an invitation")))))

;;;###autoload
(defun matrix-direct-chat (user-id)
  "Open the direct chat with USER-ID, starting an encrypted one if none exists."
  (interactive (list (matrix--read-user-id "Direct chat with user ID: ")))
  (let ((app (matrix--current-app)))
    (message "Matrix: opening a direct chat with %s..." user-id)
    (matrix-runtime-resource-request
     app "dm.open" (list :user_id user-id)
     (lambda (result)
       (require 'matrix-room)
       (matrix-room-open app (plist-get result :room_id) t))
     (lambda (error)
       (message "Matrix: %s" (or (and (listp error) (plist-get error :message))
                                 "The direct chat request was not sent"))))))

(provide 'matrix-root)
;;; matrix-root.el ends here
