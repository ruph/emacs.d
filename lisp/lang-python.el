;; Python Tab Hook
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (setq python-indent 4)
            (flycheck-select-checker 'python-flake8)
            (add-hook 'before-save-hook 'delete-trailing-whitespace)))

;; FLYCHECK
(setq-default flycheck-flake8rc
              (expand-file-name "~/.emacs.d/dotfiles/.flake8rc"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'lang-python)
