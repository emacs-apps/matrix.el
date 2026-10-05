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
              :items (matrix-state--items
                      (matrix-state--array (plist-get event :items)))))
       ((equal (plist-get event :type) "timeline_diff")
        (unless (and previous (= revision (1+ previous)))
          (signal 'matrix-timeline-gap nil))
        (list :subscription_id (plist-get timeline :subscription_id)
              :revision revision :loading nil :error nil
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

(provide 'matrix-state)
;;; matrix-state.el ends here
