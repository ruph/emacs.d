;; Python Tab Hook
(add-hook 'python-mode-hook
          (lambda ()
            (setenv "LANG" "en_GB.UTF-8")
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (setq python-indent 4)
            (flycheck-select-checker 'python-flake8)
            (ggtags-mode 1)
            (helm-gtags-mode 1)
            (add-hook 'before-save-hook 'delete-trailing-whitespace)))

;; Multi-version environment
(require 'pyenv)
(setq pyenv-installation-dir "/usr/local/pyenv")
(add-hook 'python-mode-hook 'pyenv-use-corresponding)

;; FLYCHECK
(setq-default flycheck-flake8rc
              (expand-file-name "~/.emacs.d/dotfiles/.flake8rc"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; YAPF code formatting
(defun yapf-buffer ()
  (interactive)
  (let ((cursor-relative-pos (/ (point-max) (float (point)))))
    (shell-command-on-region (point-min) (point-max)
                             "python -m yapf --style=google" t t)
    (goto-char (round (/ (point-max) cursor-relative-pos)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; DEBUGGING
;; prominent ipdb breakpoint
(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import ipdb")
  (highlight-lines-matching-regexp "ipdb.set_trace()"))
(add-hook 'python-mode-hook 'annotate-pdb)

;; add ipdb breakpoint
(defun python-add-breakpoint ()
  (interactive)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "C-c C-b") 'python-add-breakpoint)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'lang-python)
