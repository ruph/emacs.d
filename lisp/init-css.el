;;; -*- lexical-binding: t; -*-
;; CSS
(add-hook 'css-mode-hook
          (lambda ()
            (setq tab-width 4)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-css)
