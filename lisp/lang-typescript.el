;;; -*- lexical-binding: t; -*-

(use-package typescript-mode
  :ensure t
  :mode ("\.ts\'" . typescript-mode)
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook
            (lambda ()
              (lsp))))

(provide 'lang-typescript)
