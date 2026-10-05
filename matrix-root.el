;;; matrix-root.el --- Matrix account and room directory -*- lexical-binding: t; -*-

;;; Commentary:
;; Stable-key native directory rows for one account.  Rooms do not own the
;; account's sync process; closing this directory is also harmless to sync.

;;; Code:

(require 'matrix)
(require 'appkit-directory)

(declare-function matrix-room-open "matrix-room" (app room-id &optional select))

(defvar matrix-root-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map appkit-directory-mode-map)
    (define-key map (kbd "g") #'matrix-root-refresh)
    (define-key map (kbd "j") #'matrix-join-room)
    (define-key map (kbd "c") #'matrix-create-room)
    (define-key map (kbd "v") #'matrix-devices)
    (define-key map (kbd "D") #'matrix-disconnect)
    map)
  "Keymap for `matrix-root-mode'.")

(define-derived-mode matrix-root-mode appkit-directory-mode "Matrix"
  "Matrix room directory.  RET opens a room; j joins and c creates a room.")

(defun matrix-root--note (key label &optional face)
  "Make a stable KEY directory note with LABEL and optional FACE."
  (appkit-directory-entry-create
   :key key :role 'note :label (matrix--one-line label)
   :face face :stamp (list label face)))

(defun matrix-root--action (key label payload)
  "Make a native action row with stable KEY, LABEL and PAYLOAD."
  (appkit-directory-entry-create
   :key key :role 'item :label (matrix--one-line label)
   :item-p t :payload payload :stamp (list label payload)
   :help-echo "RET or mouse-2 activates this action"))

(defun matrix-root--entries (state model)
  "Project account STATE and Surface MODEL into stable read-only rows."
  (let* ((session (plist-get state :session))
         (rooms (plist-get state :rooms))
         (flows (plist-get state :verifications))
         (active 0)
         (entries
          (list
           (appkit-directory-entry-create
            :key '(account) :role 'section :face 'bold
            :label (format "Matrix — %s" (matrix--one-line (plist-get state :account)))
            :stamp (plist-get state :account))
           (matrix-root--note '(status)
                              (format "Connection: %s" (plist-get state :connection)))
           (matrix-root--note '(identity)
                              (format "User: %s   Device: %s"
                                      (or (plist-get session :user_id) "not authenticated")
                                      (or (plist-get session :device_id) "—")))
           (matrix-root--note '(homeserver)
                              (format "Homeserver: %s"
                                      (or (plist-get session :homeserver) "—"))))))
    (when-let* ((error (plist-get state :last-error)))
      (setq entries (append entries (list (matrix-root--note '(error) error 'error)))))
    (when-let* ((notice (plist-get model :notice)))
      (setq entries (append entries (list (matrix-root--note '(operation) notice)))))
    (when (hash-table-p flows)
      (maphash (lambda (_key flow)
                 (unless (member (plist-get flow :state) '("done" "cancelled"))
                   (cl-incf active)))
               flows))
    (setq entries
          (append entries
                  (list
                   (matrix-root--action '(refresh) "Refresh rooms [g]" '(command matrix-root-refresh))
                   (matrix-root--action '(join) "Join room / accept invitation [j]" '(command matrix-join-room))
                   (matrix-root--action '(create) "Create private encrypted room [c]" '(command matrix-create-room))
                   (matrix-root--action '(devices)
                                        (format "Devices and SAS verification [v]%s"
                                                (if (> active 0)
                                                    (format " — %d active" active) ""))
                                        '(command matrix-devices))
                   (matrix-root--action '(export) "Export encrypted room keys" '(command matrix-keys-export))
                   (matrix-root--action '(import) "Import encrypted room keys" '(command matrix-keys-import))
                   (matrix-root--action '(recover) "Recover encryption keys" '(command matrix-keys-recover))
                   (matrix-root--action '(disconnect) "Disconnect locally (keep this device) [D]" '(command matrix-disconnect)))))
    (when-let* ((room-id (plist-get model :ready-room)))
      (setq entries
            (append entries
                    (list (matrix-root--action '(ready-room)
                                               (format "Open joined room: %s" room-id)
                                               (list 'room room-id "join"))))))
    (dolist (section '(("join" . "Joined rooms") ("invite" . "Invitations")))
      (let ((section-entries
             (list (appkit-directory-entry-create
                    :key (list 'section (car section)) :role 'section
                    :label (cdr section) :face 'bold :stamp (cdr section)))))
        (dolist (room (sort (seq-filter
                            (lambda (room) (equal (plist-get room :membership) (car section)))
                            rooms)
                           (lambda (a b)
                             (string-lessp (or (plist-get a :name) (plist-get a :room_id))
                                           (or (plist-get b :name) (plist-get b :room_id))))))
          (let ((id (plist-get room :room_id)))
            (push
             (appkit-directory-entry-create
              :key (list 'room id) :role 'item :section-key (car section)
              :label (format "%s  %s"
                             (if (eq (plist-get room :encrypted) t)
                                 "[encrypted]" "[NOT encrypted]")
                             (matrix--one-line (or (plist-get room :name) id)))
              :trailing (format "  (%s)%s" (matrix--one-line id)
                                (if (> (or (plist-get room :unread) 0) 0)
                                    (format "  unread: %d" (plist-get room :unread)) ""))
              :indent 2 :item-p t
              :unread-p (> (or (plist-get room :unread) 0) 0)
              :payload (list 'room id (plist-get room :membership))
              :stamp room)
             section-entries)))
        (setq entries (append entries (nreverse section-entries)))))
    entries))

(defun matrix-root--activate (_directory entry)
  "Activate a native account directory ENTRY."
  (pcase (appkit-directory-entry-payload entry)
    (`(command ,command) (call-interactively command))
    (`(room ,room-id "invite")
     (when (yes-or-no-p (format "Join invited room %s? " room-id))
       (matrix-join-room room-id)))
    (`(room ,room-id ,_membership)
     (require 'matrix-room)
     (matrix-room-open (appkit-surface-app (matrix--current-surface 'root)) room-id t))))

(defun matrix-root--mount (_surface _app-view _model)
  "Install native account directory activation."
  (appkit-directory-configure (appkit-directory-surface)
                              :activate-function #'matrix-root--activate
                              :action-rows-p t))

(defun matrix-root--render (_surface state model)
  "Render pass-scoped STATE and Surface MODEL."
  (appkit-directory-reconcile (appkit-directory-surface)
                              (matrix-root--entries state model)
                              :preserve-position-p t))

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
  (let ((surface (or (appkit-app-surface app '(root))
                     (matrix--register
                      app (appkit-open-generated-surface
                           matrix-root--type :app app :identity '(root) :input '(root)
                           :buffer-name (format "*Matrix:%s*"
                                                (plist-get (matrix-runtime-state app) :account)))))))
    (when select (pop-to-buffer (appkit-surface-buffer surface)))
    surface))

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

(provide 'matrix-root)
;;; matrix-root.el ends here
