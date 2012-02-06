;; PHP mode
(add-to-list 'load-path "~/.emacs.d/elpa/php-mode-1.5.0/")
(require 'php-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-hook 'php-mode-hook
      (lambda ()
        (setq indent-tabs-mode t)
        (setq tab-width 4)
	(local-set-key (kbd "RET") 'newline-and-indent)
	(local-unset-key (kbd "M-j"))
        (add-hook 'before-save-hook 'delete-trailing-whitespace)))

(add-hook 'php-mode-hook 'flymake-find-file-hook)

(add-to-list 'load-path "~/.emacs.d/elpa/flymake-cursor-1.0/")
(load-library "flymake-cursor")
(add-hook 'php-mode-hook
	  (lambda ()
	    (local-set-key [S-f5] 'flymake-goto-prev-error)
	    (local-set-key [f5] 'flymake-goto-next-error)))

;; MMM Mode
(add-to-list 'load-path "~/.emacs.d/el-get/mmm-mode")
(require 'mmm-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'lang-php)
