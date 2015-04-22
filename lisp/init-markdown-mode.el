;; MARKDOWN
(require 'markdown-mode)

(defun init-markdown ()
  (progn
    (setq markdown-enable-math t)
    (setq markdown-coding-system "UTF-8")
    (setq markdown-indent-on-enter t)
    (visual-line-mode t)
    (flyspell-mode)
    (flycheck-mode -1)
    (company-mode -1)
    (yas-minor-mode -1)
    (remove-hook 'before-save-hook 'delete-trailing-whitespace)
    (define-key markdown-mode-map (kbd "M-<left>")  nil)
    (define-key markdown-mode-map (kbd "C-<left>")  'markdown-promote)
    (define-key markdown-mode-map (kbd "M-<right>") nil)
    (define-key markdown-mode-map (kbd "C-<right>") 'markdown-demote)
    ))

(add-hook 'markdown-mode-hook 'init-markdown)
(add-hook 'gfm-mode-hook 'init-markdown)

(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-markdown-mode)
