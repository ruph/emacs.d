;;; -*- lexical-binding: t; -*-

(defvar ruph/beam-debug nil
  "When non-nil, emit Erlang and Elixir setup logs.

Example:
  (setq ruph/beam-debug t)")

(defvar ruph/beam-enable-lsp t
  "When non-nil, start LSP automatically in Erlang and Elixir buffers.

Example:
  (setq ruph/beam-enable-lsp nil)")

(defun ruph/beam-maybe-start-lsp ()
  "Start LSP in the current BEAM buffer when enabled.

Example:
  (add-hook 'elixir-mode-hook #'ruph/beam-maybe-start-lsp)"
  (when ruph/beam-debug
    (message "[Beam] INFO LspSetup mode={%s} file={%s} lsp_enabled={%s}"
             major-mode
             (or buffer-file-name "")
             ruph/beam-enable-lsp))
  (when (and ruph/beam-enable-lsp
             (fboundp 'lsp-deferred))
    (lsp-deferred)))

(defun ruph/beam-setup-buffer (tab-width)
  "Configure the current BEAM buffer with TAB-WIDTH indentation.

Example:
  (ruph/beam-setup-buffer 2)"
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width tab-width)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
  (when ruph/beam-debug
    (message "[Beam] INFO BufferSetup mode={%s} file={%s} tab_width={%s}"
             major-mode
             (or buffer-file-name "")
             tab-width))
  (ruph/beam-maybe-start-lsp))

(defun ruph/elixir-mode-setup ()
  "Configure Elixir buffers.

Example:
  (add-hook 'elixir-mode-hook #'ruph/elixir-mode-setup)"
  (ruph/beam-setup-buffer 2))

(defun ruph/erlang-mode-setup ()
  "Configure Erlang buffers.

Example:
  (add-hook 'erlang-mode-hook #'ruph/erlang-mode-setup)"
  (ruph/beam-setup-buffer 4))

(add-to-list 'interpreter-mode-alist '("elixir" . elixir-mode))
(add-to-list 'interpreter-mode-alist '("escript" . erlang-mode))

(add-hook 'elixir-mode-hook #'ruph/elixir-mode-setup)
(add-hook 'erlang-mode-hook #'ruph/erlang-mode-setup)

(provide 'lang-beam)
