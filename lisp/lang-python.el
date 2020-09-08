;; Python Tab Hook
(add-hook 'python-mode-hook
          (lambda ()
            (setenv "LANG" "en_GB.UTF-8")
            ;; (setq indent-tabs-mode t) - use .editorconfig
            (setq tab-width 4)
            (setq python-indent 4)
            (flycheck-select-checker 'python-flake8)
            (ggtags-mode 0)
            (helm-gtags-mode 0)
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


;; LSP
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-prefer-capf t)
(setq lsp-idle-delay 0.500)

(use-package lsp-mode
  :hook ((python-mode . lsp))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp-deferred))))  ; or lsp
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


;; Emacs IPython Notebook
(use-package ein
  :ensure t
  :defer t
  :commands (ein:notebooklist-login)
  :config
  (setq ein:polymode t)
  :init
  (progn
    ;; fix smartparens not wanting to write closing parenthises when highlighting a region
    (defun insert-open-parens-or-wrap (&optional arg)
      (interactive "P")
      (if (region-active-p)
          (insert-parentheses arg)
        (insert "()")
        (backward-char)))
    (defun setup-key-hack ()
      (define-key ein:notebook-mode-map (kbd "(") #'insert-open-parens-or-wrap)
      (keymap-unset-key (kbd "M-<up>") "ein:notebook-mode")
      (keymap-unset-key (kbd "M-<down>") "ein:notebook-mode")
      (keymap-unset-key (kbd "C-<up>") "ein:notebook-mode")
      (keymap-unset-key (kbd "C-<down>") "ein:notebook-mode")
      )
    (add-hook 'ein:notebooklist-mode-hook #'setup-key-hack)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'lang-python)
