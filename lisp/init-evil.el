;; EVIL
(require 'evil)
(evil-mode 0)
(global-set-key (kbd "C-z") 'turn-on-evil-mode)

;; last cha in selection
(setq evil-want-visual-char-semi-exclusive t)

(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
(define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
(define-key evil-normal-state-map "\C-f" 'evil-forward-char)
(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
(define-key evil-normal-state-map "\C-b" 'evil-backward-char)
(define-key evil-insert-state-map "\C-b" 'evil-backward-char)
(define-key evil-visual-state-map "\C-b" 'evil-backward-char)
(define-key evil-normal-state-map "\C-d" 'evil-delete-char)
(define-key evil-insert-state-map "\C-d" 'evil-delete-char)
(define-key evil-visual-state-map "\C-d" 'evil-delete-char)
(define-key evil-normal-state-map "\C-n" 'evil-next-line)
(define-key evil-insert-state-map "\C-n" 'evil-next-line)
(define-key evil-visual-state-map "\C-n" 'evil-next-line)
(define-key evil-normal-state-map "\C-p" 'evil-previous-line)
(define-key evil-insert-state-map "\C-p" 'evil-previous-line)
(define-key evil-visual-state-map "\C-p" 'evil-previous-line)
(define-key evil-normal-state-map "\C-w" 'evil-delete)
(define-key evil-insert-state-map "\C-w" 'evil-delete)
(define-key evil-visual-state-map "\C-w" 'evil-delete)
(define-key evil-normal-state-map "\C-y" 'yank)
(define-key evil-insert-state-map "\C-y" 'yank)
(define-key evil-visual-state-map "\C-y" 'yank)
(define-key evil-normal-state-map "\C-k" 'kill-line)
(define-key evil-insert-state-map "\C-k" 'kill-line)
(define-key evil-visual-state-map "\C-k" 'kill-line)

(defun evil-undefine ()
  (interactive)
  (let (evil-mode-map-alist)
    (call-interactively (key-binding (this-command-keys)))))

(define-key evil-normal-state-map (kbd "TAB") 'evil-undefine)
(define-key evil-normal-state-map (kbd "S-x") 'evil-undefine)
(define-key evil-normal-state-map (kbd "S-c") 'evil-undefine)
(define-key evil-normal-state-map (kbd "S-v") 'evil-undefine)
(define-key evil-normal-state-map (kbd "S-u") 'evil-undefine)
(define-key evil-normal-state-map (kbd "C-.") 'evil-undefine)
(define-key evil-normal-state-map (kbd "M-.") 'evil-undefine)
(define-key evil-normal-state-map (kbd "M-:") 'evil-undefine)

(loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
                              (compilation-mode . emacs)
                              (cider-mode      . emacs)
                              (cider-repl-mode . emacs)
                              (cider-stacktrace-mode . emacs)
                              (neotree-mode    . emacs)
                              (pylookup-mode   . emacs)
                              (read-only-mode  . emacs)
                              (git-rebase-mode . emacs)
                              (term-mode       . emacs)
                              (eshell-mode     . emacs)
                              (eww-mode        . emacs)
                              (help-mode       . emacs)
                              (helm-grep-mode  . emacs)
                              (grep-mode       . emacs)
                              (bc-menu-mode    . emacs)
                              (comint-mode     . emacs)
                              (sql-interactive-mode . emacs)
                              (magit-branch-manager-mode . emacs)
                              (rdictcc-buffer-mode . emacs)
                              (dired-mode . emacs)
                              (deft-mode  . emacs))
      do (evil-set-initial-state mode state))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; evil comint
(defun kill-comint ()
  (interactive)
  (comint-interrupt-subjob)
  (popwin:close-popup-window))
(evil-define-key 'normal comint-mode-map (kbd "C-q") 'kill-comint)
(evil-define-key 'normal comint-mode-map (kbd "q") 'popwin:close-popup-window)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; C-q as general purpose escape key sequence.
(defun my-esc (prompt)
"Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding. "
  (cond
   ((or (evil-insert-state-p)
        (evil-normal-state-p)
        (evil-replace-state-p)
        (evil-visual-state-p)) [escape])
   (t (kbd "C-q"))))
(define-key key-translation-map (kbd "C-q") 'my-esc)
(define-key evil-operator-state-map (kbd "<C-q>") 'keyboard-quit)
(set-quit-char "C-q")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; evil ace jump
(define-key evil-normal-state-map "  " 'ace-jump-mode)
(define-key evil-normal-state-map " c" 'ace-jump-char-mode)
(define-key evil-normal-state-map " l" 'ace-jump-line-mode)
(define-key evil-normal-state-map " j" 'evil-jump-item)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-evil)
