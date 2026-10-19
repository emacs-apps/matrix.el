;;; matrix-notify.el --- Matrix desktop notifications -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Matrix contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; After each sync the backend reports, coalesced per room, the new events the
;; account's own push rules notify for; the first sync after start is history
;; and reports nothing.  This presents them on the desktop notification bus
;; after the account commits, skips a room already shown in the focused frame,
;; and closes a room's notification once the room is read.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-app)
(require 'appkit-surface)
(require 'appkit-ui)
(require 'matrix-runtime)

(autoload 'notifications-notify "notifications")
(autoload 'notifications-close-notification "notifications")
(declare-function matrix-room-open "matrix-room" (app room-id &optional select))
(declare-function matrix-root-open "matrix-root" (app &optional select))

(defcustom matrix-notifications 'all
  "Which Matrix push notifications become desktop notifications.
`all' shows every new event the account's push rules notify for,
`mentions' only highlights (mentions and keywords), and nil none."
  :type '(choice (const :tag "All notifying events" all)
                 (const :tag "Mentions and keywords only" mentions)
                 (const :tag "None" nil))
  :group 'matrix)

(defcustom matrix-notification-content t
  "Whether desktop notifications show the sender and message text.
Text from encrypted rooms is shown decrypted; nil shows only room names."
  :type 'boolean
  :group 'matrix)

(defconst matrix-notify--room-limit 3
  "Rooms one sync may show individually; more are summarized as one.")

(defvar matrix-notify--ids (make-hash-table :test #'equal)
  "Shown notification id per (STORE . ROOM-ID); ROOM-ID nil is a summary.")

(defun matrix-notify--escape (text)
  "Return plain TEXT safe for a body the notification server may parse."
  (replace-regexp-in-string
   "[&<>]" (lambda (char) (pcase char ("&" "&amp;") ("<" "&lt;") (_ "&gt;")))
   text t t))

(defun matrix-notify--watching-p (app room-id)
  "Whether APP's ROOM-ID is shown in a window of the focused frame."
  (when-let* ((surface (and app (appkit-app-surface app (list 'room room-id))))
              ((appkit-surface-live-p surface)))
    (and (frame-focus-state)
         (get-buffer-window (appkit-surface-buffer surface) (selected-frame)))))

(defun matrix-notify--open (store room-id)
  "Show STORE's ROOM-ID, or its room directory when ROOM-ID is nil."
  (when-let* ((app (matrix-runtime-account store)))
    (if room-id
        (progn (require 'matrix-room) (matrix-room-open app room-id t))
      (require 'matrix-root)
      (matrix-root-open app t))
    (select-frame-set-input-focus (selected-frame))))

(defun matrix-notify--show (key title body &optional urgency target)
  "Show TITLE and plain BODY as KEY's notification, replacing its last one.
Its action opens TARGET, a room ID of KEY's store, or else the room directory."
  (let ((store (car key)))
    (condition-case nil
        (puthash key
                 (notifications-notify
                  :app-name "Matrix" :title (appkit-ui-one-line-text title)
                  :body (matrix-notify--escape body) :urgency (or urgency 'normal)
                  :replaces-id (gethash key matrix-notify--ids)
                  :actions '("default" "Open")
                  :on-action (lambda (_id _action) (matrix-notify--open store target))
                  :on-close (lambda (id _reason)
                              (when (eql id (gethash key matrix-notify--ids))
                                (remhash key matrix-notify--ids))))
                 matrix-notify--ids)
      (error (message "Matrix: %s: %s" title body)))))

(defun matrix-notify--body (room)
  "Describe ROOM's latest notifying event and how many arrived."
  (let* ((count (plist-get room :count))
         (sender (or (plist-get room :sender_name) (plist-get room :sender)))
         (text (pcase (plist-get room :kind)
                 ("invite" "Invited you")
                 ("encrypted" "Encrypted message")
                 (_ (or (plist-get room :body) "New event"))))
         (more (when (> count 1) (format " (+%d more)" (1- count)))))
    (if matrix-notification-content
        (concat (when (and sender (not (equal sender (plist-get room :room_name))))
                  (concat (appkit-ui-one-line-text sender) ": "))
                text more)
      (if (= count 1) "New event" (format "%d new events" count)))))

(defun matrix-notify--room (store room)
  "Show STORE's notification for ROOM."
  (let ((id (plist-get room :room_id))
        (name (plist-get room :room_name))
        (highlight (eq (plist-get room :highlight) t)))
    (matrix-notify--show
     (cons store id)
     (if highlight (concat "Mention · " name) name)
     (matrix-notify--body room)
     ;; Mentions stay until read; the account retires them then.
     (when highlight 'critical)
     (unless (equal (plist-get room :kind) "invite") id))))

(defun matrix-notify--present (store account rooms omitted)
  "Show STORE's ROOMS, plus OMITTED others, for local profile ACCOUNT."
  (when matrix-notifications
    (let* ((app (matrix-runtime-account store))
           (rooms (seq-filter
                   (lambda (room)
                     (and (or (eq matrix-notifications 'all)
                              (eq (plist-get room :highlight) t))
                          (not (matrix-notify--watching-p
                                app (plist-get room :room_id)))))
                   rooms))
           (omitted (if (eq matrix-notifications 'all) omitted 0))
           (total (+ (length rooms) omitted)))
      (cond
       ((null rooms))
       ((<= total matrix-notify--room-limit)
        (dolist (room (reverse rooms))
          (matrix-notify--room store room)))
       (t
        (matrix-notify--show
         (cons store nil) (format "Matrix · %s" account)
         (format "New activity in %d rooms: %s" total
                 (string-join (mapcar (lambda (room) (plist-get room :room_name))
                                      (seq-take rooms 5))
                              ", "))
         (when (seq-some (lambda (room) (eq (plist-get room :highlight) t)) rooms)
           'critical)))))))

(defun matrix-notify--close (key)
  "Close KEY's shown notification, if any."
  (when-let* ((id (gethash key matrix-notify--ids)))
    (remhash key matrix-notify--ids)
    (ignore-errors (notifications-close-notification id))))

(defun matrix-notify-start (_context input _observe resolve _reject)
  "Present or retire committed notification INPUT outside the reducer."
  (pcase-let ((`(,store ,account (,operation . ,arguments)) input))
    (pcase operation
      ('present (apply #'matrix-notify--present store account arguments))
      ('retire (dolist (room-id (car arguments))
                 (matrix-notify--close (cons store room-id))))))
  (funcall resolve nil)
  nil)

(defun matrix-notify-forget (store)
  "Close every notification STORE's account has shown."
  (dolist (key (hash-table-keys matrix-notify--ids))
    (when (equal (car key) store)
      (matrix-notify--close key))))

(provide 'matrix-notify)
;;; matrix-notify.el ends here
