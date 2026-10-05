;;; matrix-root.el --- Matrix account and room directory -*- lexical-binding: t; -*-

;;; Commentary:
;; Stable-key native directory rows for one account.  Rooms do not own the
;; account's sync process; closing this directory is also harmless to sync.

;;; Code:

(require 'matrix)
(require 'appkit-directory)
(require 'appkit-presentation)

(declare-function matrix-room-open "matrix-room" (app room-id &optional select))

(defvar matrix-root-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map appkit-directory-mode-map)
    (define-key map (kbd "g") #'matrix-root-refresh)
    (define-key map (kbd "j") #'matrix-join-room)
    (define-key map (kbd "c") #'matrix-create-room)
    (define-key map (kbd "v") #'matrix-devices)
    (define-key map (kbd "D") #'matrix-disconnect)
    (define-key map (kbd "/") #'matrix-root-filter)
    (define-key map (kbd "?") #'matrix-root-transient)
    (define-key map (kbd "M-g") matrix-navigation-map)
    map)
  "Keymap for `matrix-root-mode'.")

(define-derived-mode matrix-root-mode appkit-directory-mode "Matrix"
  "Matrix room directory.  RET opens a room; j joins and c creates a room.")


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
                            (string-lessp (or (plist-get a :name) (plist-get a :room_id))
                                          (or (plist-get b :name) (plist-get b :room_id)))
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
                   :item-p t :unread-p (> (or (plist-get room :unread) 0) 0)
                   :payload (list 'room id (plist-get room :membership))
                   :stamp room :help-echo id)
                  rows)))
        (setq entries (append entries (nreverse rows)))))
    entries))

(defun matrix-root--insert-item (_directory entry)
  "Insert one bounded activity row from ENTRY's actual SDK summary."
  (let ((room (appkit-directory-entry-stamp entry)))
    (if (not (plist-get room :room_id))
        (insert (appkit-directory-entry-label entry) "\n")
      (let ((timestamp (plist-get room :timestamp))
            (unread (or (plist-get room :unread) 0)))
        (appkit-presentation-insert-one-line-row
         (appkit-presentation-one-line-row-create
          :context (appkit-directory-entry-label entry)
          :context-trail (when (> unread 0) (format "%d" unread))
          :context-trail-face 'bold
          :preview (appkit-ui-one-line-preview-create
                    :text (or (plist-get room :preview) "")
                    :label (plist-get room :preview_sender)
                    :separator ": " :label-face 'shadow)
          :time (when (numberp timestamp)
                  (format-time-string "%m-%d %H:%M" (seconds-to-time (/ timestamp 1000.0))))
          :time-face 'shadow)
         :width (matrix--width (appkit-current-surface))
         :context-width-spec '(0.32 12 36) :time-slot-width 12)))))

(defun matrix-root--activate (_directory entry)
  "Activate a directory ENTRY, keeping invitation consent on its exact account."
  (let ((surface (matrix--current-surface 'root)))
    (pcase (appkit-directory-entry-payload entry)
      (`(command ,command) (call-interactively command))
      (`(room ,room-id "invite")
       (when (yes-or-no-p (format "Join invited room %s? " room-id))
         (matrix--request surface "room.join" (list :room_id room-id))))
      (`(room ,room-id ,_membership)
       (require 'matrix-room)
       (matrix-room-open (appkit-surface-app surface) room-id t)))))

(defun matrix-root--mount (_surface _app-view _model)
  "Install native account directory activation."
  (appkit-directory-configure (appkit-directory-surface)
                              :activate-function #'matrix-root--activate
                              :item-inserter #'matrix-root--insert-item
                              :action-rows-p t))

(defun matrix-root--render (_surface state model request)
  "Render pass-scoped STATE with compact room rows and native chrome."
  (matrix--frame state "Rooms"
                 (when-let* ((filter (plist-get model :filter))
                             ((not (string-empty-p filter))))
                   (format "filter: %s" filter)))
  (let ((entries (matrix-root--entries state model)))
    (appkit-directory-reconcile
     (appkit-directory-surface) entries
     :force-keys (when (appkit-projection-change-geometry-p request)
                   (mapcar #'appkit-directory-entry-key entries))
     :preserve-position-p t)))

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

(provide 'matrix-root)
;;; matrix-root.el ends here
