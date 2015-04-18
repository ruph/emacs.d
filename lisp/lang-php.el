;; PHP mode
(add-to-list 'load-path "~/.emacs.d/el-get/php-mode")
(require 'php-mode)
;; # comments
(modify-syntax-entry ?# "< b" php-mode-syntax-table)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Hooks
(add-hook 'php-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (local-set-key (kbd "RET") 'newline-and-indent)
            (local-unset-key (kbd "M-j"))
            (add-hook 'before-save-hook 'delete-trailing-whitespace)
            (setq flymake-gui-warnings-enabled nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Snippets for standard PHP functions
(require 'php-auto-yasnippets)
(setq php-auto-yasnippet-php-program
      "~/.emacs.d/el-get/yasnippets/php-auto-yasnippets/Create-PHP-YASnippet.php")
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Flymake for php
(add-to-list 'flymake-allowed-file-name-masks '("\\.php$" flymake-php-init))

(add-to-list 'load-path "~/.emacs.d/el-get/flymake-cursor")
(load-library "flymake-cursor")
(add-hook 'php-mode-hook
          (lambda ()
            (local-set-key [S-f5] 'flymake-goto-prev-error)
            (local-set-key [f5] 'flymake-goto-next-error)))

;; Nope, I want my copies in the system temp dir.
(setq flymake-run-in-place nil)
;; This lets me say where my temp dir is.
(setq temporary-file-directory "~/.emacs.d/tmp/")
(if (eq nil (file-exists-p temporary-file-directory))
    (make-directory temporary-file-directory))

;; I want to see all errors for the line.
(setq flymake-number-of-errors-to-display nil)

(add-hook 'php-mode-hook (lambda () (flymake-mode 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; HTML
(add-hook 'html-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (local-set-key (kbd "RET") 'newline-and-indent)
            ))
;; some issues with flymake
(delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'lang-php)
