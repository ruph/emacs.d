;;; -*- lexical-binding: t; -*-

(defvar ruph/beam-debug nil
  "When non-nil, emit Erlang and Elixir setup logs.

Example:
  (setq ruph/beam-debug t)")

(defun ruph/beam-setup-buffer (indent-width)
  "Configure the current BEAM buffer with INDENT-WIDTH indentation.

Example:
  (ruph/beam-setup-buffer 2)"
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width indent-width)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
  (when ruph/beam-debug
    (message "[Beam] INFO BufferSetup mode={%s} file={%s} tab_width={%s}"
             major-mode
             (or buffer-file-name "")
             indent-width)))

(defun ruph/elixir-mode-setup ()
  "Configure Elixir buffers.

Example:
  (add-hook (quote elixir-mode-hook) (function ruph/elixir-mode-setup))"
  (ruph/beam-setup-buffer 2))

(defun ruph/erlang-mode-setup ()
  "Configure Erlang buffers.

Example:
  (add-hook (quote erlang-mode-hook) (function ruph/erlang-mode-setup))"
  (ruph/beam-setup-buffer 4))

(add-to-list 'interpreter-mode-alist '("elixir" . elixir-mode))
(add-to-list 'interpreter-mode-alist '("escript" . erlang-mode))

(add-hook 'elixir-mode-hook #'ruph/elixir-mode-setup)
(add-hook 'erlang-mode-hook #'ruph/erlang-mode-setup)

(provide 'lang-beam)
