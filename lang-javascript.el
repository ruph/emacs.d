;; Javascript mode from
;; https://github.com/mooz/js2-mode/
;; ~ mooz's community fork of js2-mode - actively maintained
(add-to-list 'load-path "~/.emacs.d/js2-mode/")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;; Js-mode Tab Hook
(add-hook 'js2-mode-hook
      (lambda ()
        (setq indent-tabs-mode t)
        (setq tab-width 2)
        (setq js-indent-level 2)
	(setq js2-missing-semi-one-line-override t)))

(provide 'lang-javascript)
