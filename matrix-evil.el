;;; matrix-evil.el --- Optional native Matrix Evil bindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Ordinary mode maps remain the Emacs-state interface.  Timeline-only modal
;; actions disappear in input; bare `?' and visual `o' retain Evil semantics.

;;; Code:

(require 'appkit-evil)

(declare-function turn-off-evil-snipe-mode "evil-snipe" ())
(declare-function turn-off-evil-snipe-override-mode "evil-snipe" ())
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
  "Install deliberate Matrix actions without promoting native single-letter maps."
  (interactive)
  (when (and (featurep 'matrix) (featurep 'evil) matrix-evil-enable-integration)
    (appkit-evil-set-initial-states matrix-evil--modes matrix-evil-initial-state)
    (dolist (map '(matrix-root-mode-map matrix-verify-mode-map))
      (appkit-evil-define-readonly-keys map)
      (appkit-evil-define-keys '(normal motion) map
        "g A" matrix-navigation-map
        "g j" #'appkit-directory-next-item
        "g k" #'appkit-directory-previous-item))
    (appkit-evil-map
      :map matrix-root-mode-map :nm
      "g a" #'matrix-root-transient
      "g r" #'matrix-root-refresh
      :map matrix-verify-mode-map :nm
      "g a" #'matrix-verify-transient
      "g r" #'matrix-verify-refresh
      :map matrix-room-mode-map :nm
      "g a" #'matrix-room-transient
      "g r" #'matrix-room-refresh
      "g j" #'matrix-room-next-message
      "g k" #'matrix-room-previous-message
      "RET" #'matrix-room-return
      "<return>" #'matrix-room-return
      :i "RET" #'newline "<return>" #'newline
      :map matrix-room-timeline-mode-map :nm
      "q" #'quit-window
      "o" #'matrix-message-transient
      "i" #'appkit-evil-chatbuf-enter-input
      "RET" #'matrix-room-return
      "<return>" #'matrix-room-return)
    (when (boundp 'matrix-room-navigation-map)
      (appkit-evil-map :map matrix-room-mode-map :nm "g A" matrix-room-navigation-map))
    (appkit-evil-normalize-buffers matrix-evil--modes)))

(with-eval-after-load 'matrix (matrix-evil-setup))
(with-eval-after-load 'matrix-room (matrix-evil-setup))
(with-eval-after-load 'evil (matrix-evil-setup))

(with-eval-after-load 'evil-snipe
  (dolist (mode matrix-evil--modes)
    (add-hook (intern (format "%s-hook" mode)) #'turn-off-evil-snipe-mode)
    (add-hook (intern (format "%s-hook" mode)) #'turn-off-evil-snipe-override-mode))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (memq major-mode matrix-evil--modes)
        (turn-off-evil-snipe-mode)
        (turn-off-evil-snipe-override-mode)))))

(provide 'matrix-evil)
;;; matrix-evil.el ends here
