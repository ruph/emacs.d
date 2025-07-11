;;; -*- lexical-binding: t; -*-
;; RUST MODE
(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook
			(lambda ()
			  (progn
				(flycheck-select-checker 'rust)
				(racer-mode)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Code completion
(use-package racer
  :init
  (unless (getenv "RUST_SRC_PATH")
	(let ((rust-src-shell-path
		   (expand-file-name  ;; racer needs an absolute path to src
			(replace-regexp-in-string
			 "[ \t\n]*$" ""
			 (shell-command-to-string "$SHELL --login -i -c 'echo $RUST_SRC_PATH'")))))
	  (setenv "RUST_SRC_PATH" rust-src-shell-path)
	  (setq racer-rust-src-path rust-src-shell-path)))
  :config
  (add-hook 'racer-mode-hook
			(lambda ()
			  (progn
				(eldoc-mode)
				(company-mode)
				(add-to-list 'company-backends 'company-racer)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'lang-rust)
