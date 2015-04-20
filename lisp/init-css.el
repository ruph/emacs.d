;; CSS
(add-hook 'css-mode-hook
          (lambda ()
            (setq tab-width 4)
            (local-set-key (kbd "RET") 'newline-and-indent)
            (add-hook 'before-save-hook 'delete-trailing-whitespace)))

;; CSS colors
(require 'rainbow-mode)
(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-mode))

;; SASS and SCSS
(require 'sass-mode)
(require 'scss-mode)
(setq-default scss-compile-at-save nil)

;; LESS
(require 'less-css-mode)
(when (featurep 'js2-mode)
  (require 'skewer-less))

;; ;; Auto-complete CSS keywords
;; (with-eval-after-load 'auto-complete
;;   (dolist (hook '(css-mode-hook sass-mode-hook scss-mode-hook))
;;     (add-hook hook 'ac-css-mode-setup)))

;; Use eldoc for syntax hints
(require 'css-eldoc)
(autoload 'turn-on-css-eldoc "css-eldoc")
(add-hook 'css-mode-hook 'turn-on-css-eldoc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-css)
