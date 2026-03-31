;;; -*- lexical-binding: t; -*-
;; MARKDOWN
(declare-function ruph/backtab-dwim nil)

(require 'markdown-mode)

(defun init-markdown ()
  (progn
    (setq markdown-enable-math t)
    (setq markdown-coding-system 'utf-8)
    (setq markdown-indent-on-enter t)
    (visual-line-mode t)
    (flyspell-mode)
    (flycheck-mode -1)
    (yas-minor-mode -1)
    
    (remove-hook 'before-save-hook 'delete-trailing-whitespace)
    (define-key markdown-mode-map (kbd "M-<left>")  nil)
    (define-key markdown-mode-map (kbd "C-<left>")  'markdown-promote)
    (define-key markdown-mode-map (kbd "M-<right>") nil)
    (define-key markdown-mode-map (kbd "C-<right>") 'markdown-demote)
	(define-key markdown-mode-map (kbd "M-S-<left>") nil)   ; markdown-promote-subtree
	(define-key markdown-mode-map (kbd "M-S-<right>") nil)  ; markdown-demote-subtree
    ))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-c v") #'markdown-preview)
  (dolist (map (list markdown-mode-map gfm-mode-map))
    (define-key map [backtab] #'ruph/backtab-dwim)
    (define-key map (kbd "<backtab>") #'ruph/backtab-dwim)
    (define-key map (kbd "<S-tab>") #'ruph/backtab-dwim)
    (define-key map (kbd "<S-iso-lefttab>") #'ruph/backtab-dwim))) ; keep S-TAB outdent in Markdown/GFM

(add-hook 'markdown-mode-hook 'init-markdown)
(add-hook 'gfm-mode-hook 'init-markdown)

(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-markdown-mode)
