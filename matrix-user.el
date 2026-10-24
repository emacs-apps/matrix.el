;;; matrix-user.el --- Matrix user pages -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Matrix contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A telega-style card for one Matrix user as the account sees them: their
;; profile, our direct chat with them, the rooms we share and how far their
;; keys are trusted.  It is a Surface of the account, like rooms are.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-bindings)
(require 'appkit-chat-avatar)
(require 'appkit-name-color)
(require 'appkit-position)
(require 'appkit-presentation)
(require 'appkit-ui)
(require 'matrix)

(declare-function matrix-room-open "matrix-room" (app room-id &optional select))

(defvar-keymap matrix-user-mode-map
  :doc "Keymap for `matrix-user-mode'."
  "m" #'matrix-user-message
  "w" #'matrix-user-copy-id)

(appkit-bindings-define 'matrix-user-mode-map 'read-only
  'refresh #'matrix-user-refresh
  'next-button #'forward-button
  'previous-button #'backward-button
  'quit #'quit-window
  'help #'describe-mode)

(define-derived-mode matrix-user-mode special-mode "Matrix-User"
  "One Matrix user as this account sees them.
\\<matrix-user-mode-map>\\[matrix-user-message] opens your direct chat, \
\\[matrix-user-copy-id] copies their user ID."
  (setq-local truncate-lines nil))

(defun matrix-user--id (&optional surface)
  "Return the user ID of SURFACE, or of the current user page."
  (cadr (appkit-surface-identity (or surface (matrix--current-surface 'user)))))

(defun matrix-user--request (surface)
  "Ask the backend for SURFACE's user profile."
  (matrix--request surface "user.profile" (list :user_id (matrix-user--id surface))))

(defun matrix-user--result (model method result _error _context)
  "Keep a settled profile RESULT of METHOD in MODEL."
  (if (and (equal method "user.profile") result)
      (plist-put (plist-put model :profile result) :notice nil)
    model))

(defun matrix-user-refresh ()
  "Fetch this user's profile again."
  (interactive)
  (matrix-user--request (matrix--current-surface 'user)))

(defun matrix-user-message ()
  "Open your direct chat with this user, starting one if there is none."
  (interactive)
  (let* ((surface (matrix--current-surface 'user))
         (profile (plist-get (appkit-surface-model surface) :profile))
         (room (plist-get profile :direct_room)))
    (when (eq (plist-get profile :self) t)
      (user-error "This is you"))
    (if (stringp room)
        (progn (require 'matrix-room)
               (matrix-room-open (appkit-surface-app surface) room t))
      (matrix-direct-chat (matrix-user--id surface)))))

(defun matrix-user-copy-id ()
  "Copy this user's Matrix ID."
  (interactive)
  (kill-new (matrix-user--id))
  (message "Copied %s" (matrix-user--id)))

(defun matrix-user--insert-rooms (app rooms)
  "Insert APP's ROOMS in common as buttons opening them."
  (when rooms
    (insert "\n")
    (appkit-presentation-insert-heading-line
     (format "Rooms in common (%d)" (length rooms)) :face 'bold)
    (dolist (room rooms)
      (let ((room-id (plist-get room :room_id)))
        (insert "  ")
        (appkit-ui-insert-action-button
         (matrix--one-line (or (plist-get room :name) room-id))
         (lambda () (require 'matrix-room) (matrix-room-open app room-id t))
         :help-echo room-id)
        (insert "\n")))))

(defun matrix-user--render (surface state model _request)
  "Render SURFACE's user card from MODEL's profile within account STATE."
  (let* ((app (appkit-surface-app surface))
         (user-id (matrix-user--id surface))
         (profile (plist-get model :profile))
         (name (matrix--one-line (or (plist-get profile :display_name) user-id)))
         (uri (plist-get profile :avatar_url))
         (pixel-size (when (and (matrix-avatar-resource-key app uri)
                                (appkit-media-inline-image-rendering-available-p))
                       (appkit-chat-avatar-two-line-pixel-size)))
         (prefixes (appkit-chat-avatar-prefixes
                    (and pixel-size (matrix-avatar-cached-image app uri pixel-size))
                    (propertize "@" 'face (appkit-name-color-face user-id))
                    :pixel-size pixel-size)))
    (matrix--frame state (concat "User · " name)
                   (when (plist-get model :operation) "loading"))
    (appkit-position-render-preserving
     (lambda ()
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert (plist-get prefixes :header)
                 (propertize name 'face (list 'appkit-presentation-card-title
                                              (appkit-name-color-face user-id)))
                 "\n" (plist-get prefixes :first-body)
                 (propertize user-id 'face 'shadow) "\n\n  ")
         (unless (eq (plist-get profile :self) t)
           (appkit-ui-insert-action-button
            " Message " #'matrix-user-message
            :face 'appkit-presentation-action-button
            :help-echo "Open your direct chat (m)")
           (insert "  "))
         (appkit-ui-insert-action-button
          " Copy ID " #'matrix-user-copy-id
          :face 'appkit-presentation-action-button
          :help-echo "Copy the Matrix user ID (w)")
         (insert "\n")
         (cond
          ((plist-get model :notice)
           (appkit-presentation-insert-note-line (plist-get model :notice) :face 'error))
          ((not profile)
           (appkit-presentation-insert-note-line "Loading user profile…")))
         (appkit-presentation-insert-note-line "g refresh · m message · w copy ID · q quit")
         (insert "\n")
         (when profile
           (appkit-presentation-insert-field "User ID" user-id)
           (appkit-presentation-insert-field "Time zone" (plist-get profile :time_zone))
           (appkit-presentation-insert-field
            "Identity"
            (pcase (plist-get profile :verified)
              ('t "Verified") (:json-false "Not verified"))
            (if (eq (plist-get profile :verified) t) 'success 'warning))
           (appkit-presentation-insert-field "Devices" (plist-get profile :devices))
           (matrix-user--insert-rooms app (append (plist-get profile :rooms) nil)))
         (goto-char (point-min))))
     :preserve-window-start t)
    (matrix--avatar-resource-result surface (list (cons 'card uri)))))

(defvar matrix-user--type
  (matrix--surface-type 'matrix-user #'matrix-user-mode #'matrix-user--render nil)
  "Generated Matrix user page Surface type.")

(defun matrix-user-open (app user-id &optional select)
  "Open APP's page for USER-ID, optionally SELECT it, and load its profile."
  (let ((surface (matrix--open
                  app matrix-user--type (list 'user user-id)
                  (format "*Matrix:%s:%s*" (plist-get (matrix-runtime-state app) :account)
                          user-id)
                  select)))
    (unless (plist-get (appkit-surface-model surface) :operation)
      (with-current-buffer (appkit-surface-buffer surface)
        (matrix-user--request surface)))
    surface))

;;;###autoload
(defun matrix-user (user-id)
  "Open the page of Matrix USER-ID for the current account."
  (interactive (list (matrix--read-user-id "User ID: ")))
  (matrix-user-open (matrix--current-app) user-id t))

(provide 'matrix-user)
;;; matrix-user.el ends here
