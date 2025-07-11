;;; -*- lexical-binding: t; -*-
;; CSS
(add-hook 'css-mode-hook
          (lambda ()
            (setq tab-width 4)
            (set (make-local-variable 'company-backends)
                 '((company-css company-dabbrev-code company-files company-yasnippet)))
            (add-hook 'before-save-hook 'delete-trailing-whitespace)))

;; CSS colors
(require 'rainbow-mode)
(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-mode))

;; CSS and SCSS
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))

;; LESS
(require 'less-css-mode)
(when (featurep 'js2-mode)
  (require 'skewer-less))

;; Use eldoc for syntax hints
(require 'css-eldoc)
(autoload 'turn-on-css-eldoc "css-eldoc")
(add-hook 'css-mode-hook 'turn-on-css-eldoc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-css)
