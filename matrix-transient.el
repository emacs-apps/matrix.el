;;; matrix-transient.el --- Context-safe Matrix command menus -*- lexical-binding: t; -*-

;;; Commentary:
;; Menus capture one exact Surface generation.  Message actions additionally
;; capture the SDK row key and resolve its current value, never a nearby row.

;;; Code:

(require 'transient)
(require 'url-util)
(require 'matrix-root)
(require 'matrix-room)
(require 'matrix-verify)

(cl-defstruct (matrix-menu-scope (:constructor matrix-menu-scope-create))
  surface item-key)

(defconst matrix-menu--prefixes
  '(matrix-root-transient matrix-account-transient matrix-room-transient
    matrix-message-transient matrix-composer-transient matrix-verify-transient))

(defun matrix-menu--capture (&optional kind message-p)
  "Capture the exact current Surface of KIND and optionally its message key."
  (let* ((surface (matrix--current-surface kind))
         (key (when message-p
                (unless (appkit-chatbuf-point-in-input-p)
                  (get-text-property (point) matrix-room-message-key-property)))))
    (when (and message-p (not key)) (user-error "No message at point"))
    (matrix-menu-scope-create :surface surface :item-key key)))

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
  (let ((surface (matrix-menu--surface (transient-scope matrix-menu--prefixes))))
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
    (unless (and item (not (member (plist-get item :kind) '("state" "day" "read_marker"))))
      (user-error "The captured message is no longer available"))
    item))

(defun matrix-message-copy-text (scope)
  "Copy the current plaintext of the message captured by SCOPE."
  (interactive (list (transient-scope 'matrix-message-transient)))
  (let ((item (matrix-menu--message scope)))
    (unless (equal (plist-get item :kind) "message")
      (user-error "This event has no available message plaintext"))
    (kill-new (substring-no-properties (plist-get item :body)))
    (message "Message text copied")))

(defun matrix-message-copy-id (scope)
  "Copy SCOPE's actual server event ID, never a local queue identifier."
  (interactive (list (transient-scope 'matrix-message-transient)))
  (let ((event-id (plist-get (matrix-menu--message scope) :event_id)))
    (unless event-id (user-error "This message has no server event ID yet"))
    (kill-new event-id)
    (message "Event ID copied")))

(defun matrix-message-copy-link (scope)
  "Copy a Matrix permalink to SCOPE's exact server event."
  (interactive (list (transient-scope 'matrix-message-transient)))
  (let* ((surface (matrix-menu--surface scope))
         (event-id (plist-get (matrix-menu--message scope) :event_id)))
    (unless event-id (user-error "This message has no server event ID yet"))
    (kill-new (format "https://matrix.to/#/%s/%s"
                      (url-hexify-string (cadr (appkit-surface-identity surface)))
                      (url-hexify-string event-id)))
    (message "Message link copied")))

(transient-define-prefix matrix-root-transient (scope)
  "Room directory commands for the captured account."
  [["Rooms" :advice* matrix-menu--command
    ("s" "Switch room" matrix-switch-room)
    ("/" "Filter directory" matrix-root-filter)
    ("g" "Refresh summaries" matrix-root-refresh)
    ("j" "Join room / alias" matrix-join-room)
    ("c" "Create encrypted room" matrix-create-room)]
   ["Account" :advice* matrix-menu--command
    ("v" "Devices and trust" matrix-devices)
    ("a" "Account / keys" matrix-account-transient)]]
  (interactive (list (matrix-menu--capture 'root)))
  (matrix-menu--surface scope)
  (transient-setup 'matrix-root-transient nil nil :scope scope))

(transient-define-prefix matrix-account-transient (scope)
  "Device, key and connection operations for one exact account view."
  [["Trust" :advice* matrix-menu--command
    ("v" "Devices and SAS" matrix-devices)
    ("r" "Request device verification" matrix-verification-request)]
   ["Room keys" :advice* matrix-menu--command
    ("e" "Export encrypted keys" matrix-keys-export)
    ("i" "Import encrypted keys" matrix-keys-import)
    ("k" "Recover with recovery key" matrix-keys-recover)]
   ["Account" :advice* matrix-menu--command
    ("h" "Room directory" matrix-accounts)
    ("D" "Disconnect locally; retain device" matrix-disconnect)]]
  (interactive (list (matrix-menu--capture)))
  (matrix-menu--surface scope)
  (transient-setup 'matrix-account-transient nil nil :scope scope))

(transient-define-prefix matrix-room-transient (scope)
  "Room navigation, or the composer menu when invoked from editable input."
  [["History" :advice* matrix-menu--command
    ("<" "Load older" matrix-room-load-older)
    (">" "Latest / input" matrix-room-latest)
    ("g" "Refresh room summary" matrix-room-refresh)]
   ["Navigate" :advice* matrix-menu--command
    ("r" "Room directory" matrix-accounts)
    ("s" "Switch room" matrix-switch-room)
    ("c" "Composer" matrix-composer-transient)
    ("a" "Account / keys" matrix-account-transient)]]
  (interactive (list (matrix-menu--capture 'room)))
  (let ((surface (matrix-menu--surface scope)))
    (if (with-current-buffer (appkit-surface-buffer surface) (appkit-chatbuf-point-in-input-p))
        (matrix-composer-transient scope)
      (transient-setup 'matrix-room-transient nil nil :scope scope))))

(transient-define-prefix matrix-message-transient (scope)
  "Actions on the captured SDK message, independent of subsequent point motion."
  [["Message"
    ("w" "Copy text" matrix-message-copy-text)
    ("i" "Copy event ID" matrix-message-copy-id)
    ("l" "Copy link" matrix-message-copy-link)]]
  (interactive (list (matrix-menu--capture 'room t)))
  (matrix-menu--message scope)
  (transient-setup 'matrix-message-transient nil nil :scope scope))

(transient-define-prefix matrix-composer-transient (scope)
  "Commands for the canonical composer of one exact room Surface."
  [["Composer" :advice* matrix-menu--command
    ("c" "Send captured draft" matrix-room-send)
    ("i" "Focus input" matrix-room-latest)
    ("u" "Review uncertain submission" matrix-room-review-send)]
   ["Navigate" :advice* matrix-menu--command
    ("r" "Room directory" matrix-accounts)
    ("s" "Switch room" matrix-switch-room)
    ("a" "Account / keys" matrix-account-transient)]]
  (interactive (list (matrix-menu--capture 'room)))
  (matrix-menu--surface scope)
  (transient-setup 'matrix-composer-transient nil nil :scope scope))

(transient-define-prefix matrix-verify-transient (scope)
  "Trust-directory commands; SAS confirmation remains an explicit per-flow action."
  [["Devices" :advice* matrix-menu--command
    ("g" "Refresh devices" matrix-verify-refresh)
    ("v" "Request verification" matrix-verification-request)
    ("a" "Account / keys" matrix-account-transient)
    ("r" "Room directory" matrix-accounts)]]
  (interactive (list (matrix-menu--capture 'verify)))
  (matrix-menu--surface scope)
  (transient-setup 'matrix-verify-transient nil nil :scope scope))

(provide 'matrix-transient)
;;; matrix-transient.el ends here
