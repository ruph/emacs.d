;;; -*- lexical-binding: t; -*-
;; RUST MODE
(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (lsp))))

(provide 'lang-rust)