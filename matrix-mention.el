;;; matrix-mention.el --- Matrix @-mention completion -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Matrix contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Completing `@' in a room composer inserts one atomic mention object for a
;; joined member.  The backend turns it into a pill and an intentional
;; m.mentions entry, which is what modern push rules notify on.  Members come
;; from a per-room cache refreshed in the background, plus loaded senders.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-chatbuf)
(require 'appkit-chat-completion)
(require 'appkit-chat-emoji)
(require 'matrix-runtime)

(defconst matrix-mention--ttl 300
  "Seconds a room's cached member list stays fresh.")

(defvar-local matrix-mention--members nil
  "This room's joined members as (FETCHED-AT . MEMBERS), or nil.")

(defvar-local matrix-mention--fetching nil
  "Non-nil while this room's member list is being fetched.")

(defun matrix-mention-p (object)
  "Whether composer OBJECT is a Matrix user mention."
  (and (listp object) (eq (plist-get object :type) 'mention)
       (stringp (plist-get object :user-id)) (stringp (plist-get object :name))))

(defun matrix-mention--refresh (surface)
  "Fetch SURFACE's joined members unless a fresh list or fetch exists."
  (unless (or matrix-mention--fetching
              (and matrix-mention--members
                   (< (- (float-time) (car matrix-mention--members))
                      matrix-mention--ttl)))
    (let ((buffer (current-buffer)))
      (setq matrix-mention--fetching t)
      (condition-case nil
          (matrix-room-members
           (appkit-surface-app surface) (cadr (appkit-surface-identity surface))
           (lambda (members)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq matrix-mention--members (cons (float-time) members)
                       matrix-mention--fetching nil))))
           (lambda (_error)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (setq matrix-mention--fetching nil)))))
        (error (setq matrix-mention--fetching nil))))))

(defun matrix-mention--people (surface)
  "Return SURFACE's mentionable (USER-ID . NAME) pairs, excluding ourselves."
  (let* ((app (appkit-surface-app surface))
         (own (plist-get (plist-get (matrix-runtime-state app) :session) :user_id))
         (people (make-hash-table :test #'equal))
         order)
    (cl-flet ((add (id name)
                (when (and (stringp id) (not (equal id own)) (not (gethash id people)))
                  (puthash id (if (and (stringp name) (not (string-empty-p name))) name id)
                           people)
                  (push id order))))
      (dolist (member (cdr matrix-mention--members))
        (add (plist-get member :user_id) (plist-get member :display_name)))
      ;; Recent senders are mentionable before the member list arrives.
      (dolist (item (reverse (matrix-runtime-room-items
                              app (cadr (appkit-surface-identity surface)))))
        (add (plist-get item :sender) (plist-get item :sender_name))))
    (mapcar (lambda (id) (cons id (gethash id people))) (nreverse order))))

(defun matrix-mention--candidates (surface)
  "Return completion candidates for SURFACE's mentionable people."
  (let* ((people (matrix-mention--people surface))
         (counts (make-hash-table :test #'equal)))
    (dolist (person people)
      (cl-incf (gethash (cdr person) counts 0)))
    (mapcar
     (lambda (person)
       (pcase-let ((`(,id . ,name) person))
         (appkit-chat-completion-candidate-create
          ;; Duplicate display names stay distinguishable by their user ID.
          :label (let ((label (if (string-prefix-p "@" name) name (concat "@" name))))
                   (if (> (gethash name counts) 1) (format "%s (%s)" label id) label))
          :annotation (concat "  " id)
          :search-terms (list name id)
          :value (list :user-id id :name name))))
     people)))

(defun matrix-mention--insert (candidate)
  "Insert CANDIDATE as one atomic mention object."
  (let ((value (appkit-chat-completion-candidate-value candidate)))
    (appkit-chatbuf-input-insert
     (plist-get value :name)
     :object (list :type 'mention :user-id (plist-get value :user-id)
                   :name (plist-get value :name))
     :properties (list 'face 'font-lock-keyword-face
                       'help-echo (plist-get value :user-id)))))

(defun matrix-mention-capf ()
  "Complete an @-mention of a room member at point."
  (when-let* ((surface (appkit-current-surface))
              ((eq (car-safe (appkit-surface-identity surface)) 'room))
              (token (appkit-chat-completion-token-bounds ?@)))
    (matrix-mention--refresh surface)
    (when-let* ((candidates (matrix-mention--candidates surface)))
      (appkit-chat-completion-capf
       (plist-get token :start) (plist-get token :end) candidates
       :insert-function #'matrix-mention--insert :suffix " "))))

(defun matrix-mention-setup ()
  "Install mention and Unicode emoji completion in this room composer."
  ;; Idempotent across mode re-entry, keeping unrelated CAPFs and dispatchers.
  (setq-local completion-at-point-functions
              (cl-remove-if (lambda (function)
                              (memq function '(matrix-mention-capf appkit-chat-emoji-capf)))
                            completion-at-point-functions))
  (setq-local appkit-chat-completion-functions
              (delq #'appkit-chat-completion-at-point appkit-chat-completion-functions))
  (appkit-chat-completion-setup
   :capf-functions '(matrix-mention-capf appkit-chat-emoji-capf) :append t))

(provide 'matrix-mention)
;;; matrix-mention.el ends here
