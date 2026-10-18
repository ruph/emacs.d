;;; -*- lexical-binding: t; -*-

(require 'vterm)

(declare-function projectile-project-root "projectile" (&optional dir))

;; Set the shell to zsh, the modern default on macOS
(setq vterm-shell "/bin/zsh")

;; Disable yasnippet in vterm to avoid conflicts
(add-hook 'vterm-mode-hook (lambda () (yas-minor-mode -1)))

;; Function to open vterm in the project root
(defun ruph/vterm-project ()
  (interactive)
  (let ((project-root (projectile-project-root)))
    (vterm (concat project-root "/"))))

(defun ruph/vterm-copy-mark-line ()
  "Select the current line for copying.

Example:
  (ruph/vterm-copy-mark-line)"
  (interactive)
  (beginning-of-line)
  (push-mark (point) nil t)
  (forward-line 1))

;; --- Keybindings ---
(define-key vterm-mode-map (kbd "C-c c") #'vterm-copy-mode)

(define-key vterm-copy-mode-map (kbd "j") #'next-line)
(define-key vterm-copy-mode-map (kbd "k") #'previous-line)
(define-key vterm-copy-mode-map (kbd "l") #'forward-char)
(define-key vterm-copy-mode-map (kbd "h") #'backward-char)
(define-key vterm-copy-mode-map (kbd "w") #'forward-word)
(define-key vterm-copy-mode-map (kbd "b") #'backward-word)
(define-key vterm-copy-mode-map (kbd "C-v") #'ruph/vterm-copy-mark-line)
(define-key vterm-copy-mode-map (kbd "v") #'set-mark-command)
(define-key vterm-copy-mode-map (kbd "y") #'vterm-copy-mode-done)

(provide 'init-vterm)
