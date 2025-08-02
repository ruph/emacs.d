;;; -*- lexical-binding: t; -*-

(require 'vterm)

;; Set the shell to zsh, the modern default on macOS
(setq vterm-shell "/bin/zsh")

;; Pass arguments to the shell for a clean startup.
;; This sets a prompt, enables colors, clears the screen, and starts a clean shell session.
(setq vterm-shell-args '("-c" "PROMPT='%n@%m:%~%# ' exec /bin/zsh"))

;; Disable yasnippet in vterm to avoid conflicts
(add-hook 'vterm-mode-hook (lambda () (yas-minor-mode -1)))

;; Function to open vterm in the project root
(defun ruph/vterm-project ()
  (interactive)
  (let ((project-root (projectile-project-root)))
    (vterm (concat project-root "/"))))

;; Global keybinding to open a new terminal
(define-key global-map (kbd "C-c t") #'ruph/vterm-project)

;; --- Keybindings ---
(define-key vterm-mode-map (kbd "C-c c") #'vterm-copy-mode)

(define-key vterm-copy-mode-map (kbd "j") #'vterm-copy-mode-down)
(define-key vterm-copy-mode-map (kbd "k") #'vterm-copy-mode-up)
(define-key vterm-copy-mode-map (kbd "l") #'vterm-copy-mode-forward-char)
(define-key vterm-copy-mode-map (kbd "h") #'vterm-copy-mode-backward-char)
(define-key vterm-copy-mode-map (kbd "w") #'vterm-copy-mode-forward-word)
(define-key vterm-copy-mode-map (kbd "b") #'vterm-copy-mode-backward-word)
(define-key vterm-copy-mode-map (kbd "C-v") #'vterm-copy-mode-visual-line)
(define-key vterm-copy-mode-map (kbd "v") #'vterm-copy-mode-visual-char)
(define-key vterm-copy-mode-map (kbd "y") #'vterm-copy-mode-copy)

(provide 'init-vterm)