(require 'js2-mode)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))

;; tern
(require 'tern)

;; Js2-mode
(add-hook 'js2-mode-hook
          (lambda ()
            ;; (setq indent-tabs-mode t) ;; .editorconfig
            (setq tab-width 4)
            (setq js2-indent-level 4)
            (setq js2-missing-semi-one-line-override t)
            (local-unset-key (kbd "M-j")) ;; jump to window below conflict
            (yas-activate-extra-mode 'js-mode)
            (tern-mode t)
            (set (make-local-variable 'company-backends)
                 '((company-dabbrev-code company-tern company-keywords company-yasnippet)))
            ))

;; some jsx support
(defun modify-syntax-table-for-jsx ()
  (modify-syntax-entry ?< "(>")
  (modify-syntax-entry ?> ")<"))
(add-hook 'js2-mode-hook 'modify-syntax-table-for-jsx)

;; default js-mode
(add-hook 'js-mode-hook
          (lambda ()
            (setq tab-width 4)
            (add-hook 'before-save-hook 'delete-trailing-whitespace)
            (yas-activate-extra-mode 'js2-mode)
            (tern-mode t)
            (set (make-local-variable 'company-backends)
                 '((company-dabbrev-code company-tern company-keywords company-yasnippet)))
            ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'lang-javascript)
