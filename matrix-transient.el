;;; matrix-transient.el --- Context-safe Matrix command menus -*- lexical-binding: t; -*-

;;; Commentary:
;; Menus capture one exact Surface generation.  Message actions additionally
;; capture the SDK row key and resolve its current value, never a nearby row.

;;; Code:

(require 'transient)
(require 'url-util)
(require 'appkit-chat-emoji)
(require 'matrix-root)
(require 'matrix-room)
(require 'matrix-verify)

(cl-defstruct (matrix-menu-scope (:constructor matrix-menu-scope-create))
  surface item-key input-p)

(defconst matrix-menu--prefixes
  '(matrix-root-transient matrix-account-transient matrix-room-transient
    matrix-message-transient matrix-composer-transient matrix-attachment-transient
    matrix-verify-transient))

(defun matrix-menu--scope (&optional prefixes)
  "Return the Matrix scope of an active prefix, without initializing another."
  (when (or transient-current-prefix transient--prefix)
    (let ((scope (transient-scope prefixes)))
      (and (matrix-menu-scope-p scope) scope))))

(defun matrix-menu--capture (&optional kind message-p inherited)
  "Use INHERITED or the active menu target, otherwise capture this exact view."
  (let* ((scope
          (or inherited (matrix-menu--scope matrix-menu--prefixes)
              (let* ((surface (matrix--current-surface kind))
                     (identity (appkit-surface-identity surface))
                     (key
                      (pcase (car identity)
                        ('root (appkit-directory-key-at-point))
                        ('room
                         (unless (appkit-chatbuf-point-in-input-p)
                           (get-text-property (point) matrix-room-message-key-property))))))
                (matrix-menu-scope-create
                 :surface surface :item-key (copy-tree key)
                 :input-p (and (eq (car identity) 'room)
                               (appkit-chatbuf-point-in-input-p))))))
         (surface (matrix-menu--surface scope)))
    (when (and kind (not (eq kind (car (appkit-surface-identity surface)))))
      (user-error "This menu requires a Matrix %s view" kind))
    (when message-p (matrix-menu--message scope))
    scope))

(defun matrix-menu--surface (scope)
  "Require SCOPE's still-live exact registered Surface, never its replacement."
  (let ((surface (and (matrix-menu-scope-p scope) (matrix-menu-scope-surface scope))))
    (unless (and (appkit-surface-live-p surface)
                 (eq surface (appkit-app-surface
                              (appkit-surface-app surface) (appkit-surface-identity surface)))
                 (with-current-buffer (appkit-surface-buffer surface)
                   (eq surface (appkit-current-surface))))
      (user-error "The view that opened this menu is no longer live"))
    surface))

(defun matrix-menu--command (function &rest arguments)
  "Invoke FUNCTION in the captured view, including its interactive arguments."
  (let ((surface (matrix-menu--surface (matrix-menu--scope matrix-menu--prefixes))))
    (with-current-buffer (appkit-surface-buffer surface)
      (apply function arguments))))

(defun matrix-menu--message (scope)
  "Resolve SCOPE's exact SDK row against current account state."
  (let* ((surface (matrix-menu--surface scope))
         (identity (appkit-surface-identity surface))
         (item (and (eq (car identity) 'room)
                    (seq-find
                     (lambda (item) (equal (plist-get item :id) (matrix-menu-scope-item-key scope)))
                     (matrix-runtime-room-items (appkit-surface-app surface) (cadr identity))))))
    (unless (and item (matrix-state-event-row-p item))
      (user-error "The captured message is no longer available"))
    item))

(defun matrix-message-copy-text (scope)
  "Copy the current plaintext of the message captured by SCOPE."
  (interactive (list (transient-scope 'matrix-message-transient)))
  (let ((item (matrix-menu--message scope)))
    (unless (matrix-menu--plaintext-p item)
      (user-error "This event has no available message plaintext"))
    (kill-new (substring-no-properties (plist-get item :body)))
    (message "Message text copied")))

(defun matrix-message-copy-id (scope)
  "Copy SCOPE's actual server event ID, never a local queue identifier."
  (interactive (list (transient-scope 'matrix-message-transient)))
  (let* ((item (matrix-menu--message scope))
         (event-id (plist-get item :event_id)))
    (unless (matrix-menu--server-id-p item)
      (user-error "This message has no server event ID yet"))
    (kill-new event-id)
    (message "Event ID copied")))

(defun matrix-message-open-sender (scope)
  "Open the page of the sender of SCOPE's exact message."
  (interactive (list (matrix-menu--capture 'room t)))
  (let ((surface (matrix-menu--surface scope)))
    (matrix-user-open (appkit-surface-app surface)
                      (plist-get (matrix-menu--message scope) :sender) t)))

(defun matrix-message-copy-link (scope)
  "Copy a Matrix permalink to SCOPE's exact server event."
  (interactive (list (transient-scope 'matrix-message-transient)))
  (let* ((surface (matrix-menu--surface scope))
         (item (matrix-menu--message scope))
         (event-id (plist-get item :event_id)))
    (unless (matrix-menu--server-id-p item)
      (user-error "This message has no server event ID yet"))
    (kill-new (format "https://matrix.to/#/%s/%s"
                      (url-hexify-string (cadr (appkit-surface-identity surface)))
                      (url-hexify-string event-id)))
    (message "Message link copied")))

(defun matrix-menu--plaintext-p (item)
  "Return non-nil when ITEM has decrypted message text."
  (and (equal (plist-get item :kind) "message")
       (stringp (plist-get item :body))))

(defun matrix-menu--server-id-p (item)
  "Return non-nil when ITEM has an actual server event ID."
  (let ((id (plist-get item :event_id)))
    (and (stringp id) (not (string-empty-p id)))))

(defun matrix-message-edit (scope)
  "Edit SCOPE's exact message, never the draft or a nearby timeline row."
  (interactive (list (matrix-menu--capture 'room t)))
  (matrix-room-begin-edit (matrix-menu--surface scope) (matrix-menu--message scope)))

(defun matrix-message-reply (scope)
  "Reply to SCOPE's exact SDK message while preserving the current draft."
  (interactive (list (matrix-menu--capture 'room t)))
  (matrix-room-begin-reply (matrix-menu--surface scope) (matrix-menu--message scope)))

(defun matrix-message-delete (scope)
  "Confirm and redact SCOPE's exact server event through the Matrix SDK."
  (interactive (list (matrix-menu--capture 'room t)))
  (let* ((surface (matrix-menu--surface scope))
         (app (appkit-surface-app surface))
         (session (copy-tree (plist-get (matrix-runtime-state app) :session)))
         (params (matrix-room--mutation-params surface (matrix-menu--message scope) :can_redact)))
    (when (yes-or-no-p "Permanently delete the selected Matrix message? ")
      (unless (and (eq surface (matrix-menu--surface scope))
                   (equal session (plist-get (matrix-runtime-state app) :session))
                   (equal params (matrix-room--mutation-params
                                  surface (matrix-menu--message scope) :can_redact)))
        (user-error "The selected message or account changed during confirmation"))
      (matrix--request surface "message.redact" params
                       (list :event-id (plist-get params :event_id))))))

(defun matrix-message-react (scope)
  "Toggle a reaction on SCOPE's exact message, offering its reactions first."
  (interactive (list (matrix-menu--capture 'room t)))
  (let* ((surface (matrix-menu--surface scope))
         (item (matrix-menu--message scope)))
    (matrix-room--mutation-params surface item :can_react)
    (let ((key (appkit-chat-emoji-read
                "React with: "
                (mapcar (lambda (reaction) (plist-get reaction :key))
                        (plist-get item :reactions)))))
      (with-current-buffer (appkit-surface-buffer (matrix-menu--surface scope))
        (matrix-room-toggle-reaction (plist-get item :id) key)))))

(defun matrix-message-open-attachment (scope)
  "Download and open a private copy of SCOPE's exact message attachment."
  (interactive (list (matrix-menu--capture 'room t)))
  (let ((surface (matrix-menu--surface scope)))
    (with-current-buffer (appkit-surface-buffer surface)
      (matrix-room-open-attachment (plist-get (matrix-menu--message scope) :id)))))

(defun matrix-message-save-attachment (scope)
  "Download SCOPE's exact message attachment to a new file."
  (interactive (list (matrix-menu--capture 'room t)))
  (with-current-buffer (appkit-surface-buffer (matrix-menu--surface scope))
    (matrix-room-save-attachment (plist-get (matrix-menu--message scope) :id))))

(defun matrix-message-retry-send (scope)
  "Send SCOPE's exact failed message again."
  (interactive (list (matrix-menu--capture 'room t)))
  (with-current-buffer (appkit-surface-buffer (matrix-menu--surface scope))
    (matrix-room-retry-send (plist-get (matrix-menu--message scope) :id))))

(defun matrix-message-discard-send (scope)
  "Withdraw SCOPE's exact unsent message after confirming."
  (interactive (list (matrix-menu--capture 'room t)))
  (with-current-buffer (appkit-surface-buffer (matrix-menu--surface scope))
    (matrix-room-discard-send (plist-get (matrix-menu--message scope) :id))))

(defun matrix-menu--capability-inapt-p (capability)
  "Return whether the captured message lacks CAPABILITY or the room is busy."
  (condition-case nil
      (or (matrix-menu--request-inapt-p)
          (not (eq (plist-get (matrix-menu--message (matrix-menu--scope)) capability) t)))
    (user-error t)))

(defun matrix-menu--download-inapt-p ()
  "Return whether the captured message has no downloadable attachment."
  (condition-case nil
      (or (matrix-menu--request-inapt-p)
          (not (matrix-media-attachment (matrix-menu--message (matrix-menu--scope)))))
    (user-error t)))

(defun matrix-menu--mutation-inapt-p (capability)
  "Return non-nil when the captured server event cannot perform CAPABILITY."
  (or (matrix-menu--capability-inapt-p capability)
      (condition-case nil
          (not (matrix-menu--server-id-p (matrix-menu--message (matrix-menu--scope))))
        (user-error t))))

(defun matrix-menu--edit-inapt-p ()
  "Return whether editing the captured message is unavailable."
  (or (matrix-menu--mutation-inapt-p :can_edit)
      (condition-case nil
          (with-current-buffer (appkit-surface-buffer (matrix-menu--surface (matrix-menu--scope)))
            (not (null (matrix-room--edit-state))))
        (user-error t))))

(defun matrix-menu--delete-inapt-p ()
  "Return whether redacting the captured message is unavailable."
  (matrix-menu--mutation-inapt-p :can_redact))

(defun matrix-menu--react-inapt-p ()
  "Return whether reacting to the captured message is unavailable."
  (matrix-menu--mutation-inapt-p :can_react))

(defun matrix-menu--reply-inapt-p ()
  "Return whether replying to the captured SDK message is unavailable."
  (or (matrix-menu--mutation-inapt-p :can_reply)
      (condition-case nil
          (with-current-buffer (appkit-surface-buffer (matrix-menu--surface (matrix-menu--scope)))
            (not (null (matrix-room--edit-state))))
        (user-error t))))

(defun matrix-menu--root-entry (scope)
  "Resolve SCOPE's captured actionable directory entry by its public key."
  (let ((surface (matrix-menu--surface scope)))
    (unless (eq (car (appkit-surface-identity surface)) 'root)
      (user-error "This action requires the captured room directory"))
    (with-current-buffer (appkit-surface-buffer surface)
      (let ((entry (appkit-directory-entry-for-key
                    (appkit-directory-surface) (matrix-menu-scope-item-key scope))))
        (unless (and entry
                     (pcase (appkit-directory-entry-payload entry)
                       (`(command ,command) (commandp command))
                       (`(room ,id ,membership)
                        (and (stringp id) (not (string-empty-p id))
                             (member membership '("join" "invite"))))))
          (user-error "The captured directory item is no longer actionable"))
        entry))))

(defun matrix-menu-root-open (scope)
  "Open or accept the current entry for SCOPE's exact captured directory key."
  (interactive (list (matrix-menu--scope matrix-menu--prefixes)))
  (let ((entry (matrix-menu--root-entry scope))
        (surface (matrix-menu--surface scope)))
    (with-current-buffer (appkit-surface-buffer surface)
      (matrix-root--activate (appkit-directory-surface) entry))))

(defun matrix-menu--focus-command (function &rest arguments)
  "Select the captured view before invoking a focus or latest command."
  (let* ((surface (matrix-menu--surface (matrix-menu--scope matrix-menu--prefixes)))
         (buffer (appkit-surface-buffer surface))
         (window (get-buffer-window buffer t)))
    (if (window-live-p window)
        (select-window window)
      (pop-to-buffer buffer))
    (with-current-buffer buffer (apply function arguments))))

(defun matrix-menu--view-inapt-p ()
  "Return non-nil when the originating exact view is no longer live."
  (condition-case nil
      (progn (matrix-menu--surface (matrix-menu--scope)) nil)
    (user-error t)))

(defun matrix-menu--ready-p (surface)
  "Return whether SURFACE's account can admit an ordinary runtime request."
  (let* ((app (appkit-surface-app surface))
         (state (matrix-runtime-state app)))
    (and (appkit-app-live-p app) (plist-get state :backend-active)
         (plist-get state :session))))

(defun matrix-menu--request-inapt-p ()
  "Return non-nil when a captured-view operation cannot be started."
  (condition-case nil
      (let ((surface (matrix-menu--surface (matrix-menu--scope))))
        (or (not (matrix-menu--ready-p surface))
            (plist-get (appkit-surface-model surface) :operation)))
    (user-error t)))

(defun matrix-menu--refresh-inapt-p ()
  "Return non-nil when the captured account cannot refresh."
  (condition-case nil
      (not (matrix-menu--ready-p (matrix-menu--surface (matrix-menu--scope))))
    (user-error t)))

(defun matrix-menu--root-entry-inapt-p ()
  "Return non-nil when the captured item cannot be opened or accepted."
  (condition-case nil
      (let ((entry (matrix-menu--root-entry (matrix-menu--scope))))
        (and (equal (nth 2 (appkit-directory-entry-payload entry)) "invite")
             (matrix-menu--request-inapt-p)))
    (user-error t)))

(defun matrix-menu--message-inapt-p ()
  "Return non-nil when the captured SDK row is no longer a message."
  (condition-case nil
      (progn (matrix-menu--message (matrix-menu--scope)) nil)
    (user-error t)))

(defun matrix-menu--text-inapt-p ()
  "Return non-nil when the captured message has no decrypted text."
  (condition-case nil
      (not (matrix-menu--plaintext-p (matrix-menu--message (matrix-menu--scope))))
    (user-error t)))

(defun matrix-menu--id-inapt-p ()
  "Return non-nil when the captured message has no server ID or permalink."
  (condition-case nil
      (not (matrix-menu--server-id-p (matrix-menu--message (matrix-menu--scope))))
    (user-error t)))

(defun matrix-menu--history-inapt-p ()
  "Return non-nil when older history is loading, exhausted or not ready."
  (condition-case nil
      (let* ((surface (matrix-menu--surface (matrix-menu--scope)))
             (identity (appkit-surface-identity surface))
             (timeline (matrix-room--timeline
                        (matrix-runtime-state (appkit-surface-app surface))
                        (cadr identity))))
        (or (not (eq (car identity) 'room))
            (not (matrix-menu--ready-p surface))
            (plist-get timeline :loading)
            (with-current-buffer (appkit-surface-buffer surface)
              (or (not (appkit-chat-history-window-known-p))
                  (appkit-chat-history-loading-p)
                  (appkit-chat-history-older-loaded-p)))))
    (user-error t)))

(defun matrix-menu--send-inapt-p ()
  "Return non-nil when the captured room's current draft cannot be sent."
  (condition-case nil
      (let* ((surface (matrix-menu--surface (matrix-menu--scope)))
             (model (appkit-surface-model surface)))
        (or (not (eq (car (appkit-surface-identity surface)) 'room))
            (matrix-menu--request-inapt-p)
            (plist-get model :uncertain-send)
            (with-current-buffer (appkit-surface-buffer surface)
              (let ((capture (matrix-compose-capture surface)))
                (and (string-empty-p (string-trim (plist-get capture :body)))
                     (not (plist-get capture :attachments)))))))
    (user-error t)))

(defun matrix-menu--review-inapt-p ()
  "Return non-nil unless the captured room has an uncertain submission."
  (condition-case nil
      (let ((surface (matrix-menu--surface (matrix-menu--scope))))
        (or (not (eq (car (appkit-surface-identity surface)) 'room))
            (not (plist-get (appkit-surface-model surface) :uncertain-send))))
    (user-error t)))

(defun matrix-menu--attachment-inapt-p ()
  "Return whether the captured composer cannot stage attachments."
  (condition-case nil
      (let ((surface (matrix-menu--surface (matrix-menu--scope))))
        (or (not (eq (car (appkit-surface-identity surface)) 'room))
            (plist-get (appkit-surface-model surface) :operation)
            (with-current-buffer (appkit-surface-buffer surface)
              (matrix-room--edit-state))))
    (user-error t)))

(defun matrix-menu--detach-inapt-p ()
  "Return whether there is no removable attachment in this composer."
  (or (matrix-menu--attachment-inapt-p)
      (condition-case nil
          (with-current-buffer (appkit-surface-buffer (matrix-menu--surface (matrix-menu--scope)))
            (not (matrix-compose-attachments)))
        (user-error t))))

(defun matrix-menu--cancel-inapt-p ()
  "Return whether this composer has no idle reply or edit context."
  (condition-case nil
      (let ((surface (matrix-menu--surface (matrix-menu--scope))))
        (or (plist-get (appkit-surface-model surface) :operation)
            (with-current-buffer (appkit-surface-buffer surface)
              (not (memq (appkit-chatbuf-aux-type) '(reply edit))))))
    (user-error t)))

(defun matrix-menu--label (text)
  "Return bounded, plain, single-line contextual TEXT."
  (truncate-string-to-width
   (replace-regexp-in-string "[[:cntrl:]]" " " (matrix--one-line text))
   72 nil nil t))

(defun matrix-menu--description ()
  "Describe the current canonical account and room."
  (condition-case nil
      (let* ((scope (matrix-menu--scope))
             (surface (matrix-menu--surface scope))
             (app (appkit-surface-app surface))
             (state (matrix-runtime-state app))
             (identity (appkit-surface-identity surface))
             (account (matrix-menu--label (plist-get state :account)))
             (room (and (eq (car identity) 'room)
                        (matrix-runtime-room app (cadr identity)))))
        (format "Matrix %s%s" account
                (if (eq (car identity) 'room)
                    (format " / %s" (matrix-menu--label
                                    (or (plist-get room :name) (cadr identity))))
                  "")))
    (user-error "Matrix / unavailable view")))

(defun matrix-menu--message-description ()
  "Describe the captured message without exposing any composer draft."
  (condition-case nil
      (let* ((item (matrix-menu--message (matrix-menu--scope)))
             (sender (matrix-menu--label (plist-get item :sender))))
        (format "%s / %s (%s)" (matrix-menu--description) sender
                (if (matrix-menu--server-id-p item) "server event" "not server accepted")))
    (user-error "Matrix / unavailable message")))

(transient-define-prefix matrix-root-transient (&optional scope)
  "Room directory commands for the captured account."
  [:description matrix-menu--description
   ["Captured item"
    ("RET" "Open / accept item" matrix-menu-root-open
     :inapt-if matrix-menu--root-entry-inapt-p)]
   ["Rooms" :advice* matrix-menu--command :inapt-if matrix-menu--view-inapt-p
    ("s" "Switch room" matrix-switch-room)
    ("/" "Filter directory" matrix-root-filter)
    ("g" "Refresh summaries" matrix-root-refresh :inapt-if matrix-menu--refresh-inapt-p)
    ("j" "Join room / alias" matrix-join-room :inapt-if matrix-menu--request-inapt-p)
    ("c" "Create encrypted room" matrix-create-room :inapt-if matrix-menu--request-inapt-p)
    ("d" "Direct chat" matrix-direct-chat :inapt-if matrix-menu--request-inapt-p)
    ("x" "Decline invitation" matrix-root-decline-invitation
     :inapt-if matrix-menu--request-inapt-p)]
   ["Account" :advice* matrix-menu--command :inapt-if matrix-menu--view-inapt-p
    ("v" "Devices and trust" matrix-devices)
    ("a" "Account / keys" matrix-account-transient)]]
  (interactive)
  (setq scope (matrix-menu--capture 'root nil scope))
  (transient-setup 'matrix-root-transient nil nil :scope scope))

(transient-define-prefix matrix-account-transient (&optional scope)
  "Device, key and connection operations for one exact account view."
  [:description matrix-menu--description
   ["Trust" :advice* matrix-menu--command :inapt-if matrix-menu--view-inapt-p
    ("v" "Devices and SAS" matrix-devices)
    ("r" "Request device verification" matrix-verification-request
     :inapt-if matrix-menu--request-inapt-p)]
   ["Room keys" :advice* matrix-menu--command :inapt-if matrix-menu--request-inapt-p
    ("e" "Export encrypted keys" matrix-keys-export)
    ("i" "Import encrypted keys" matrix-keys-import)
    ("k" "Recover with recovery key" matrix-keys-recover)]
   ["Account" :advice* matrix-menu--command :inapt-if matrix-menu--view-inapt-p
    ("h" "Room directory" matrix-accounts)
    ("D" "Disconnect locally; retain device" matrix-disconnect)]]
  (interactive)
  (setq scope (matrix-menu--capture nil nil scope))
  (transient-setup 'matrix-account-transient nil nil :scope scope))

(transient-define-prefix matrix-room-transient (&optional scope)
  "Room navigation, or the composer menu when invoked from editable input."
  [:description matrix-menu--description
   ["Message"
    ("m" "Captured message" matrix-message-transient :inapt-if matrix-menu--message-inapt-p)]
   ["History" :advice* matrix-menu--command :inapt-if matrix-menu--view-inapt-p
    ("<" "Load older" matrix-room-load-older :inapt-if matrix-menu--history-inapt-p)
    (">" "Latest / input" matrix-room-latest :advice* matrix-menu--focus-command)
    ("g" "Refresh room summary" matrix-room-refresh :inapt-if matrix-menu--refresh-inapt-p)]
   ["Room" :advice* matrix-menu--command :inapt-if matrix-menu--request-inapt-p
    ("i" "Invite user" matrix-room-invite)
    ("n" "Rename" matrix-room-rename)
    ("t" "Change topic" matrix-room-set-topic)
    ("L" "Leave" matrix-room-leave)]
   ["Navigate" :advice* matrix-menu--command :inapt-if matrix-menu--view-inapt-p
    ("r" "Room directory" matrix-accounts)
    ("s" "Switch room" matrix-switch-room)
    ("c" "Composer" matrix-composer-transient)
    ("a" "Account / keys" matrix-account-transient)]]
  (interactive)
  (setq scope (matrix-menu--capture 'room nil scope))
  (if (matrix-menu-scope-input-p scope)
      (transient-setup 'matrix-composer-transient nil nil :scope scope)
    (transient-setup 'matrix-room-transient nil nil :scope scope)))

(transient-define-prefix matrix-message-transient (&optional scope)
  "Actions on the captured SDK message, independent of subsequent point motion."
  [:description matrix-menu--message-description
   ["Message"
    ("r" "Reply to message" matrix-message-reply :inapt-if matrix-menu--reply-inapt-p)
    ("e" "Edit message" matrix-message-edit :inapt-if matrix-menu--edit-inapt-p)
    ("d" "Delete message" matrix-message-delete :inapt-if matrix-menu--delete-inapt-p)
    ("!" "React / toggle reaction" matrix-message-react :inapt-if matrix-menu--react-inapt-p)
    ("o" "Open attachment" matrix-message-open-attachment
     :inapt-if matrix-menu--download-inapt-p)
    ("s" "Save attachment as" matrix-message-save-attachment
     :inapt-if matrix-menu--download-inapt-p)
    ("R" "Retry sending" matrix-message-retry-send
     :inapt-if (lambda () (matrix-menu--capability-inapt-p :can_retry)))
    ("x" "Discard unsent message" matrix-message-discard-send
     :inapt-if (lambda () (matrix-menu--capability-inapt-p :can_discard)))
    ("w" "Copy text" matrix-message-copy-text :inapt-if matrix-menu--text-inapt-p)
    ("i" "User page" matrix-message-open-sender)
    ("I" "Copy event ID" matrix-message-copy-id :inapt-if matrix-menu--id-inapt-p)
    ("l" "Copy link" matrix-message-copy-link :inapt-if matrix-menu--id-inapt-p)]]
  (interactive)
  (setq scope (matrix-menu--capture 'room t scope))
  (transient-setup 'matrix-message-transient nil nil :scope scope))

(transient-define-prefix matrix-composer-transient (&optional scope)
  "Commands for the canonical composer of one exact room Surface."
  [:description matrix-menu--description
   ["Composer" :advice* matrix-menu--command :inapt-if matrix-menu--view-inapt-p
    ("c" "Send current draft" matrix-room-send :inapt-if matrix-menu--send-inapt-p)
    ("i" "Focus input" matrix-room-edit-draft :advice* matrix-menu--focus-command)
    ("u" "Review uncertain submission" matrix-room-review-send
     :inapt-if matrix-menu--review-inapt-p)
    ("k" "Cancel reply / edit" matrix-compose-cancel :inapt-if matrix-menu--cancel-inapt-p)]
   ["Attachments" :advice* matrix-menu--command :inapt-if matrix-menu--attachment-inapt-p
    ("f" "Local media / file" matrix-compose-attach-file)
    ("v" "Clipboard files / image" matrix-compose-attach-clipboard)
    ("d" "Detach attachment" matrix-compose-detach :inapt-if matrix-menu--detach-inapt-p)]
   ["Navigate" :advice* matrix-menu--command :inapt-if matrix-menu--view-inapt-p
    ("r" "Room directory" matrix-accounts)
    ("s" "Switch room" matrix-switch-room)
    ("a" "Account / keys" matrix-account-transient)]]
  (interactive)
  (setq scope (matrix-menu--capture 'room nil scope))
  (transient-setup 'matrix-composer-transient nil nil :scope scope))

(transient-define-prefix matrix-attachment-transient (&optional scope)
  "Stage local media or clipboard content in the captured room composer."
  [:description matrix-menu--description
   ["Attachments" :advice* matrix-menu--command :inapt-if matrix-menu--attachment-inapt-p
    ("f" "Local media / file" matrix-compose-attach-file)
    ("v" "Clipboard files / image" matrix-compose-attach-clipboard)
    ("d" "Detach attachment" matrix-compose-detach :inapt-if matrix-menu--detach-inapt-p)]]
  (interactive)
  (setq scope (matrix-menu--capture 'room nil scope))
  (transient-setup 'matrix-attachment-transient nil nil :scope scope))

(transient-define-prefix matrix-verify-transient (&optional scope)
  "Trust-directory commands; SAS confirmation remains an explicit per-flow action."
  [:description matrix-menu--description
   ["Devices" :advice* matrix-menu--command :inapt-if matrix-menu--view-inapt-p
    ("g" "Refresh devices" matrix-verify-refresh :inapt-if matrix-menu--request-inapt-p)
    ("v" "Request verification" matrix-verification-request
     :inapt-if matrix-menu--request-inapt-p)
    ("a" "Account / keys" matrix-account-transient)
    ("r" "Room directory" matrix-accounts)]]
  (interactive)
  (setq scope (matrix-menu--capture 'verify nil scope))
  (transient-setup 'matrix-verify-transient nil nil :scope scope))

(provide 'matrix-transient)
;;; matrix-transient.el ends here
