;;; matrix-state.el --- Matrix presentation snapshots -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Matrix contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This is a presentation mirror, not a Matrix protocol reducer.  The SDK owns
;; message relationships, local echoes and encryption.  A diff batch either
;; replaces one entire committed snapshot or changes nothing.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(define-error 'matrix-protocol-error "Invalid Matrix backend presentation data")
(define-error 'matrix-timeline-gap "Matrix timeline needs a fresh snapshot"
              'matrix-protocol-error)

(defun matrix-state--array (value)
  "Return an owned list spine for wire array VALUE."
  (unless (or (vectorp value) (proper-list-p value))
    (signal 'matrix-protocol-error '("Expected an array")))
  (append value nil))

(defun matrix-state--items (items)
  "Validate unique row identities in ITEMS and return ITEMS."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (item items)
      (let ((id (plist-get item :id)))
        (unless (and (stringp id) (not (string-empty-p id))
                     (stringp (plist-get item :body))
                     (not (gethash id seen)))
          (signal 'matrix-protocol-error '("Invalid or duplicate timeline row")))
        (puthash id t seen))))
  items)

(defun matrix-state--index (index length &optional insertion)
  "Validate INDEX against LENGTH; INSERTION permits the final gap."
  (unless (and (integerp index) (>= index 0)
               (if insertion (<= index length) (< index length)))
    (signal 'matrix-protocol-error '("Timeline index outside snapshot")))
  index)

(defun matrix-state-apply-changes (items changes)
  "Apply ordered wire CHANGES atomically to the presentation ITEMS.
Only the list spine is copied; unchanged immutable row values are retained."
  (let ((rows (copy-sequence items))
        (size (length items)))
    (dolist (change (matrix-state--array changes))
      (let ((index (plist-get change :index))
            (item (plist-get change :item)))
        (pcase (plist-get change :op)
          ("append"
           (let ((more (matrix-state--array (plist-get change :items))))
             (setq rows (nconc rows more) size (+ size (length more)))))
          ("clear" (setq rows nil size 0))
          ("push_front" (push item rows) (cl-incf size))
          ("push_back" (setq rows (nconc rows (list item))) (cl-incf size))
          ("pop_front"
           (matrix-state--index 0 size)
           (setq rows (cdr rows)) (cl-decf size))
          ("pop_back"
           (matrix-state--index 0 size)
           (if (= size 1) (setq rows nil)
             (setcdr (nthcdr (- size 2) rows) nil))
           (cl-decf size))
          ("insert"
           (matrix-state--index index size t)
           (if (zerop index) (push item rows)
             (let ((previous (nthcdr (1- index) rows)))
               (setcdr previous (cons item (cdr previous)))))
           (cl-incf size))
          ("set"
           (matrix-state--index index size)
           (setcar (nthcdr index rows) item))
          ("remove"
           (matrix-state--index index size)
           (if (zerop index) (setq rows (cdr rows))
             (let ((previous (nthcdr (1- index) rows)))
               (setcdr previous (cddr previous))))
           (cl-decf size))
          ("truncate"
           (let ((length (plist-get change :length)))
             (matrix-state--index length size t)
             (if (zerop length) (setq rows nil)
               (setcdr (nthcdr (1- length) rows) nil))
             (setq size length)))
          ("reset"
           (setq rows (matrix-state--array (plist-get change :items))
                 size (length rows)))
          (_ (signal 'matrix-protocol-error '("Unknown timeline change"))))))
    (matrix-state--items rows)))

(defun matrix-state-timeline-update (timeline event)
  "Return a new TIMELINE for EVENT, or `stale' for obsolete delivery.
A skipped revision signals `matrix-timeline-gap'.  Neither input is mutated."
  (if (not (equal (plist-get timeline :subscription_id)
                  (plist-get event :subscription_id)))
      'stale
    (let ((revision (plist-get event :revision))
          (previous (plist-get timeline :revision)))
      (unless (and (integerp revision) (>= revision 0))
        (signal 'matrix-protocol-error '("Invalid timeline revision")))
      (cond
       ((and previous (<= revision previous)) 'stale)
       ((equal (plist-get event :type) "timeline_reset")
        (unless (and (null previous) (zerop revision))
          (signal 'matrix-timeline-gap nil))
        (list :subscription_id (plist-get timeline :subscription_id)
              :revision revision :loading nil :error nil
              :read_state (plist-get event :read_state)
              :items (matrix-state--items
                      (matrix-state--array (plist-get event :items)))))
       ((equal (plist-get event :type) "timeline_diff")
        (unless (and previous (= revision (1+ previous)))
          (signal 'matrix-timeline-gap nil))
        (list :subscription_id (plist-get timeline :subscription_id)
              :revision revision :loading nil :error nil
              :read_state (plist-get event :read_state)
              :items (matrix-state-apply-changes
                      (plist-get timeline :items) (plist-get event :changes))))
       (t (signal 'matrix-protocol-error '("Unknown timeline notification")))))))

(defun matrix-state-rooms (value)
  "Return a room list with unique, nonempty room identities from VALUE."
  (let ((rooms (matrix-state--array value))
        (seen (make-hash-table :test #'equal)))
    (dolist (room rooms)
      (let ((id (plist-get room :room_id)))
        (unless (and (stringp id) (not (string-empty-p id))
                     (stringp (plist-get room :name))
                     (not (gethash id seen)))
          (signal 'matrix-protocol-error '("Invalid or duplicate room")))
        (puthash id t seen)))
    rooms))

(defun matrix-state-notifications (value)
  "Return per-room desktop notification candidates from VALUE.
Each names a room, its notifying event :count and :highlight, and describes
the latest such event; sender and text fields are optional strings."
  (let ((rooms (matrix-state--array value)))
    (dolist (room rooms)
      (let ((id (plist-get room :room_id))
            (count (plist-get room :count)))
        (unless (and (stringp id) (not (string-empty-p id))
                     (stringp (plist-get room :room_name))
                     (natnump count) (> count 0)
                     (memq (plist-get room :highlight) '(t :json-false))
                     (member (plist-get room :kind)
                             '("message" "encrypted" "invite" "other"))
                     (cl-every (lambda (key)
                                 (let ((text (plist-get room key)))
                                   (or (null text) (stringp text))))
                               '(:event_id :sender :sender_name :body)))
          (signal 'matrix-protocol-error '("Invalid room notification")))))
    rooms))

(defun matrix-state-members (value)
  "Return joined members with unique, nonempty user identities from VALUE.
A member's `:display_name' and `:avatar_url' are optional strings."
  (let ((members (matrix-state--array value))
        (seen (make-hash-table :test #'equal)))
    (dolist (member members)
      (let ((id (plist-get member :user_id))
            (name (plist-get member :display_name))
            (avatar (plist-get member :avatar_url)))
        (unless (and (stringp id) (not (string-empty-p id))
                     (or (null name) (stringp name))
                     (or (null avatar) (stringp avatar))
                     (not (gethash id seen)))
          (signal 'matrix-protocol-error '("Invalid or duplicate room member")))
        (puthash id t seen)))
    members))

(defun matrix-state-member-by-display-name (members display-name)
  "Return the first MEMBER named DISPLAY-NAME, or nil.
Duplicate display names keep backend order; this lookup is best-effort, as
in the bridge bots that only relay a Matrix display name."
  (seq-find (lambda (member)
              (equal display-name (plist-get member :display_name)))
            members))

(provide 'matrix-state)
;;; matrix-state.el ends here
