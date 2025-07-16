;;; -*- lexical-binding: t; -*-
;; GO
(require 'go-mode)
(add-hook 'go-mode-hook
		  (lambda ()
            (flycheck-select-checker 'go-gofmt)
			))

(provide 'lang-go)
