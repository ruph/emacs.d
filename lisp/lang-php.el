;; PHP mode
(require 'php-mode)

;; Code style
(setq-default php-mode-coding-style 'psr2)
(setq-default flycheck-phpcs-standard "PSR2")
(setq-default flycheck-php-phpcs-executable
			  (concat (getenv "HOME") "/bin/phpcs"))

;; # comments
(modify-syntax-entry ?# "< b" php-mode-syntax-table)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Autocompletion
(defun php-completion ()
  (ggtags-mode 1)
  (helm-gtags-mode 1)
  (set (make-local-variable 'company-backends)
       '((company-gtags company-dabbrev-code company-files company-yasnippet)))
  )

;; Hooks
(add-hook 'php-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (local-unset-key (kbd "M-j"))
            (add-hook 'before-save-hook 'delete-trailing-whitespace)
            (flycheck-select-checker 'php)
            (php-completion)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Snippets for standard PHP functions
(use-package php-auto-yasnippets
  :defer t
  :config
  (setq php-auto-yasnippet-php-program
		"~/.emacs.d/el-get/yasnippets/php-auto-yasnippets/Create-PHP-YASnippet.php")
  (define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'lang-php)
