;; GO
(require 'go-mode)
(add-hook 'go-mode-hook
		  (lambda ()
            (flycheck-select-checker 'go-gofmt)
			(ggtags-mode 1)))

(provide 'lang-go)
