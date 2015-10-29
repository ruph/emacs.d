;; Python Tab Hook
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (setq python-indent 4)
            (flycheck-select-checker 'python-flake8)
            (add-hook 'before-save-hook 'delete-trailing-whitespace)))

;; Multi-version environment
(require 'pyenv)
(setq pyenv-installation-dir "/usr/local/pyenv")
(add-hook 'python-mode-hook 'pyenv-use-corresponding)

;; FLYCHECK
(setq-default flycheck-flake8rc
              (expand-file-name "~/.emacs.d/dotfiles/.flake8rc"))
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
