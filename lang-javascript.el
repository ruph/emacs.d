;; Javascript mode from  https://github.com/mooz/js2-mode/
;; ~ mooz's community fork of js2-mode - actively maintained
(add-to-list 'load-path "~/.emacs.d/el-get/js2-mode-mooz/")
(require 'js2-mode)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;; Js2-mode
(add-hook 'js2-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (setq js2-indent-level 4)
            (local-set-key (kbd "RET") 'newline-and-indent)
            (setq js2-missing-semi-one-line-override t)
            ))
(add-hook 'js2-mode-hook 'esk-paredit-nonlisp)

;; default js-mode
(add-hook 'js-mode-hook
          (lambda ()
            (setq tab-width 4)
            (local-set-key (kbd "RET") 'newline-and-indent)
            (add-hook 'before-save-hook 'delete-trailing-whitespace)))

(provide 'lang-javascript)
