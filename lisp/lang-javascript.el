(require 'js2-mode)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;; tern
(require 'tern)

;; mode settings
(defun js-mode-common ()
  (setq tab-width 4)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (tern-mode t)
  (set (make-local-variable 'company-backends)
       '((company-dabbrev-code company-tern company-keywords company-yasnippet))))

(add-hook 'js-mode-hook 'js-mode-common)

(add-hook 'js2-mode-hook
          (lambda ()
            (js-mode-common)
            (setq js2-indent-level 4)
            (setq js2-missing-semi-one-line-override t)
            (local-unset-key (kbd "M-j")) ;; jump to window below conflict
            (yas-activate-extra-mode 'js-mode)
            ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'lang-javascript)
