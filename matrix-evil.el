;;; matrix-evil.el --- Optional native Matrix Evil bindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Ordinary mode maps remain the Emacs-state interface.  Timeline-only modal
;; actions disappear in input; bare `?' and visual `o' retain Evil semantics.

;;; Code:

(require 'appkit-evil)



(declare-function matrix-root-transient "matrix-transient" (&optional scope))
(declare-function matrix-room-transient "matrix-transient" (&optional scope))
(declare-function matrix-message-transient "matrix-transient" (&optional scope))
(declare-function matrix-verify-transient "matrix-transient" (&optional scope))
(declare-function matrix-root-refresh "matrix-root" ())
(declare-function matrix-verify-refresh "matrix-verify" ())
(declare-function matrix-room-refresh "matrix-room" ())
(declare-function matrix-room-next-message "matrix-room" ())
(declare-function matrix-room-previous-message "matrix-room" ())
(declare-function matrix-room-return "matrix-room" ())

(defvar matrix-navigation-map)
(defvar matrix-room-navigation-map)

(defcustom matrix-evil-enable-integration t
  "Whether to install Matrix bindings when Evil is available."
  :type 'boolean :group 'matrix)

(defcustom matrix-evil-initial-state 'normal
  "Initial Evil state for Matrix views, or nil to leave it unchanged."
  :type '(choice (const nil) (const normal) (const motion) (const emacs) symbol)
  :group 'matrix)

(defconst matrix-evil--modes '(matrix-root-mode matrix-room-mode matrix-verify-mode))

(defun matrix-evil-setup ()
  "Project Matrix's semantic capabilities without promoting native letters."
  (interactive)
  (when (and (featurep 'matrix) (featurep 'evil) matrix-evil-enable-integration)
    (appkit-evil-set-initial-states matrix-evil--modes matrix-evil-initial-state)
    (dolist (map '(matrix-root-mode-map matrix-verify-mode-map
                  matrix-room-mode-map matrix-room-timeline-mode-map))
      (appkit-bindings-enable-evil map))
    (appkit-evil-normalize-buffers matrix-evil--modes)))

(with-eval-after-load 'matrix (matrix-evil-setup))
(with-eval-after-load 'matrix-room (matrix-evil-setup))
(with-eval-after-load 'evil (matrix-evil-setup))



(provide 'matrix-evil)
;;; matrix-evil.el ends here
