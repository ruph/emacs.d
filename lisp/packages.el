;;; -*- lexical-binding: t; -*-

;; Initialize package management
(require 'package)
(require 'cl-lib)

;; Set TLS settings for macOS compatibility
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(setq package-archive-priorities '((gnu . 10) (nongnu . 8) (melpa . 5)))

(package-initialize)

;; Auto-install essential packages if not present, using a delayed approach to ensure TLS settings are applied
(defun ruph/ensure-packages-installed ()
  "Ensure essential packages are installed."
  (let ((essential-packages '(diminish))) ; use-package is built-in now
    (dolist (pkg essential-packages)
      (unless (package-installed-p pkg)
        (condition-case err
            (progn
              (message "[Packages] Auto-installing %s..." pkg)
              (package-install pkg)
              (message "[Packages] %s auto-installed successfully" pkg))
          (error
            (message "[Packages] ERROR: Failed to install %s: %s" pkg (error-message-string err))))))))

;; Use idle timer to ensure Emacs is fully initialized before installing packages
(run-with-idle-timer 2.0 nil 
  (lambda ()
    (condition-case err
        (progn
          (message "[Packages] Starting auto-installation process...")
          (unless package-archive-contents
            (message "[Packages] Refreshing package contents...")
            (package-refresh-contents))
          (ruph/ensure-packages-installed)
          (message "[Packages] Auto-installation process completed"))
      (error
        (message "[Packages] ERROR in auto-install: %s" (error-message-string err))))))

;; Ensure archive list is current for key packages (handles 404s from stale indices)
(defun ruph/ensure-archives-for (&rest _pkgs)
  ;; Intentionally no-op by default to avoid network errors on startup.
  ;; Use M-x ruph/refresh-package-archives to update archives manually.
  nil)

(defun ruph/refresh-package-archives ()
  "Refresh package archives with basic error handling."
  (interactive)
  (condition-case err
      (progn
        (message "[Packages] INFO RefreshStart")
        (package-refresh-contents)
        (message "[Packages] INFO RefreshDone"))
    (error (message "[Packages] ERROR RefreshFailed msg=\"%s\"" (error-message-string err)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI & THEME

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t)

;; Built-in dired with hidden details
(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(use-package writeroom-mode
  :ensure t
  :init
  (setq writeroom-extra-line-spacing 0.4
        writeroom-maximize-window t)
  :config
  (add-hook 'writeroom-mode-hook
            (lambda () (progn (text-scale-increase 1)))))

(use-package volatile-highlights
  :ensure t
  :init
  (volatile-highlights-mode t))

(use-package highlight-symbol
  :ensure t
  :bind (("M-<f3>" . highlight-symbol-at-point)
         ("<f3>"   . highlight-symbol-next)
         ("S-<f3>" . highlight-symbol-prev)
         ("C-<f3>" . highlight-symbol-prev)))

(use-package markdown-mode
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/init-markdown-mode.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDITING & COMPLETION

;; Lightweight, fast completion UI based on CAPF
(use-package corfu
  :ensure t
  :init
  (setq corfu-auto t
        corfu-auto-prefix 1
        corfu-auto-delay 0.02
        corfu-quit-no-match t)

  (defvar ruph/completion-debug nil
    "When non-nil, emit completion-related debug logs.

Example:
  (setq ruph/completion-debug t)")

  (defun ruph/text-mode-completion-setup ()
    "Make completion less intrusive in text buffers.

Example:
  (add-hook (quote text-mode-hook) (function ruph/text-mode-completion-setup))"
    (setq-local corfu-auto nil)
    ;; Avoid TAB-triggered `completion-at-point' in prose buffers.
    (setq-local tab-always-indent t)
    (when ruph/completion-debug
      (message
       "[Completion] INFO TextModeSetup mode={%s} corfu_auto={%s} tab_always_indent={%s}"
       major-mode corfu-auto tab-always-indent)))

  (add-hook 'text-mode-hook #'ruph/text-mode-completion-setup)
  :config
  (global-corfu-mode)
  (global-set-key (kbd "C-c SPC") #'completion-at-point))

;; Completion sources for CAPF
(use-package cape
  :ensure t
  :init
  ;; Order: keywords > file > dabbrev (tunable)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; Better matching style for responsiveness
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; Keybinding hints for discoverability (lightweight)
(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.4
        which-key-max-description-length 40))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode)
  :config
  (progn
    (setq yas-snippet-dirs
          '(
            "~/.emacs.d/snippets/clojure-snippets"
            "~/.emacs.d/snippets/minimal-yasnippet-php-mode"
            "~/.emacs.d/snippets/php-auto-yasnippets"
            ))
    (yas-global-mode 1)
    (setq yas-wrap-around-region t)
    (setq yas-prompt-functions
          '(yas/x-prompt yas/ido-prompt))))

(use-package yasnippet-snippets
    :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-c c" . mc/edit-lines)
   ("C-c e" . mc/edit-ends-of-lines)
   ("C-c a" . mc/edit-beginnings-of-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<return>" . set-rectangular-region-anchor)))

(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  :config
  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-mode)  ; This will trigger when markdown-mode loads
  (add-hook 'nodejs-repl-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'inferior-python-mode-hook 'turn-on-smartparens-mode)
  ;; Avoid `sp-with-modes` here: when user config is byte-compiled without
  ;; Smartparens loaded, macro expansion can be skipped and the resulting .elc
  ;; may mis-evaluate these forms at startup.
  (sp-local-pair '(php-mode js2-mode rust-mode)
                 "/**" "*/"
                 :post-handlers '(("| " "SPC") (my-php-handle-docstring "RET")))
  (sp-local-pair '(php-mode js2-mode rust-mode)
                 "/*." ".*/"
                 :post-handlers '(("| " "SPC")))
  (sp-local-pair '(php-mode js2-mode rust-mode)
                 "{" nil
                 :post-handlers '(("|\\|\n[i]" "RET")))
  (sp-local-pair '(php-mode js2-mode rust-mode)
                 "(" nil
                 :prefix "\\(\\sw\\|\\s_\\)*")
  :bind
  (("C-<right>" . sp-slurp-hybrid-sexp)
   ("C-<left>" . sp-forward-barf-sexp)))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . (lambda () (turn-off-smartparens-mode) (paredit-mode +1)))
         (lisp-mode . (lambda () (turn-off-smartparens-mode) (paredit-mode +1)))
         (lisp-interaction-mode . (lambda () (turn-off-smartparens-mode) (paredit-mode +1)))
         (scheme-mode . (lambda () (turn-off-smartparens-mode) (paredit-mode +1)))
         (slime-repl-mode . (lambda () (turn-off-smartparens-mode) (paredit-mode +1)))
         (clojure-mode . (lambda () (turn-off-smartparens-mode) (paredit-mode +1)))))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist
        '(("." . "~/.emacs.d/undo-tree-history/"))))

;; Avy for fast navigation (replacement for ace-jump)
(use-package avy
  :ensure t
  :bind (("C-0" . avy-goto-word-1)
         ("C-c C-0" . avy-pop-mark)))

(use-package ace-window
  :ensure t
  :bind (("M-h" . ace-window)
         ("M-o" . ace-swap-window))
  :config
  (setq aw-keys '(?g ?h ?j ?k ?l ?b ?n ?m)))

(use-package visual-regexp-steroids
  :ensure t
  :config
  (setq vr/engine 'python)
  (let ((vr-path (locate-library "visual-regexp-steroids")))
    (when vr-path
      (setq vr/command-python (concat "python3 " (file-name-directory vr-path) "regexp.py"))))
  :bind (("C-M-%" . vr/replace)
         ("M-%"   . vr/query-replace)
         ("C-r"   . vr/isearch-backward)
         ("C-s"   . vr/isearch-forward)
         ("C-M-s" . isearch-forward)
         ("C-M-r" . isearch-backward)
         ("C-c m" . vr/mc-mark)))

(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

(use-package syntactic-close
  :ensure t
  :bind ("C-<" . syntactic-close))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TOOLS & INTEGRATIONS

(use-package projectile
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/init-project.el"))

(use-package helm-projectile
  :ensure t)

(use-package helm
  :ensure t
  :bind (("C-:" . helm-occur))
  :config
  (load-file "~/.emacs.d/lisp/init-helm.el"))

;; Ripgrep integration for Helm
(use-package helm-rg
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/init-flycheck.el"))

(use-package quickrun
  :ensure t
  :commands quickrun
  :bind ("C-c q" . quickrun))

(use-package diff-hl
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/init-vc.el"))

(use-package vterm
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/init-vterm.el"))

(use-package deft
  :ensure t
  :init
  (setq deft-directories '("~/Dropbox/Notes")
        deft-extension "txt"
        deft-use-filename-as-title t
        deft-text-mode 'gfm-mode)
  :bind
  ("C-c n" . deft))

(use-package eproject
  :ensure t
  :defer t)

(use-package recentf
  :ensure t
  :config
  (recentf-mode 1)
  (setq
   recentf-max-saved-items 256
   recentf-max-menu-items 32)
  :bind ("C-x f" . helm-recentf))

(use-package shackle
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/init-shackle.el"))

(use-package tree-sitter
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/init-treesitter.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LANGUAGES

(use-package js2-mode
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/lang-javascript.el"))

(use-package typescript-mode
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/lang-typescript.el"))

(use-package web-mode
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/init-web-mode.el"))

(use-package emmet-mode
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/init-emmet-mode.el"))

(use-package less-css-mode
  :ensure t
  :mode "\\.less\\'")

(use-package skewer-mode
  :ensure t)

(use-package nodejs-repl
  :ensure t)

(use-package php-mode
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/lang-php.el"))

(use-package go-mode
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/lang-go.el"))

(use-package rust-mode
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/lang-rust.el"))

(use-package evil
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/init-evil.el"))

(use-package circe
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/init-irc.el"))

(use-package swift-mode
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/lang-clojure.el"))

(use-package cider
  :ensure t
  :config
  (require 'widget))

(use-package inf-clojure
  :ensure t)

(use-package ein
  :ensure t)

(use-package pyenv-mode
  :ensure t
  :commands (pyenv-use-corresponding)
  :config
  (setq pyenv-installation-dir "/usr/local/pyenv")
  (add-hook 'python-mode-hook 'pyenv-use-corresponding))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package csv-mode
  :ensure t
  :mode ("\\.[CcTt][Ss][Vv]\\'" . csv-mode))

(use-package android-mode
  :ensure t
  :config
  (defun set-android-path-from-shell-PATH ()
    (let ((android-path
           (replace-regexp-in-string
            "[ \t\n]*$"
            ""
            (shell-command-to-string "$SHELL --login -i -c 'echo $ANDROID_HOME'"))))
      (setenv "ANDROID_HOME" android-path)))
  (when (and window-system (eq system-type 'darwin))
    (set-android-path-from-shell-PATH)))

(use-package lsp-mode
  :ensure t
  :hook ((js2-mode . lsp)
         (typescript-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :ensure t)

(use-package gptel
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/init-gptel.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC

(defvar org-cua-dwim-shift-translated nil)
(defvar cua--last-region-shifted nil)
(defvar cua--explicit-region-start nil)
(use-package org-cua-dwim
  :ensure t)

;; Removed popup/pos-tip to avoid legacy deps; keep UI simple

(use-package visual-fill-column
  :ensure t)


(use-package lsp-pyright
  :ensure t)

;; Dependencies (good to have them explicit)
(use-package dash :ensure t)

(use-package s :ensure t)
(use-package f :ensure t)
(use-package pkg-info :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LEGACY CONFIG (to be reviewed/migrated)

;; Auto indent
(electric-indent-mode nil)
(setq clean-aindent-is-simple-indent t)

;; SQL
(defun sql-connect-bookmark (product connection)
  (setq sql-product product)
  (sql-connect connection))
(setq sql-mysql-options (list "-A"))
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; the modeline
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(if (require 'sml-modeline nil 'noerror)
    (progn
      (if (fboundp 'scroll-bar-mode)
          (scroll-bar-mode -1)))
  (if (fboundp 'scroll-bar-mode)
      (progn
        (scroll-bar-mode 1)
        (set-scroll-bar-mode 'right))))

;; Compatibility shim: this config historically used `keymap-unset-key`, but
;; Emacs ships `keymap-unset` (and friends). Keep the existing call sites.
(unless (fboundp 'keymap-unset-key)
  (defvar ruph/keymap-debug nil
    "When non-nil, emit keymap-related debug logs.

Example:
  (setq ruph/keymap-debug t)")

  (defun keymap-unset-key (key mode)
    "Unset KEY in MODE's keymap after the relevant feature is loaded.

MODE is a string or symbol like `paredit-mode`.

Example:
  (keymap-unset-key (kbd \"M-<up>\") \"paredit-mode\")"
    (let* ((mode-name (if (symbolp mode) (symbol-name mode) mode))
           (mode-base (if (string-suffix-p "-mode" mode-name)
                          (substring mode-name 0 (- (length mode-name) (length "-mode")))
                        mode-name))
           (feature-candidates (delq nil (list (intern mode-base) (intern mode-name))))
           (map-sym (intern (format "%s-map" mode-name))))
      (dolist (feature feature-candidates)
        (with-eval-after-load feature
          (when (boundp map-sym)
            (define-key (symbol-value map-sym) key nil)
            (when ruph/keymap-debug
              (message "[Keymap] INFO UnsetKey mode={%s} key={%s}" mode-name key))))))))

;; these are used for moving lines/regions
(keymap-unset-key (kbd "M-<up>") "paredit-mode")
(keymap-unset-key (kbd "M-<down>") "paredit-mode")
(keymap-unset-key [?\e up] "paredit-mode")
(keymap-unset-key [?\e down] "paredit-mode")
(keymap-unset-key (kbd "M-J") "paredit-mode")

;; from emacs-starter-kit
(defun esk-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

;; Colored ()
(require 'paren)
(setq show-paren-delay 0)
(show-paren-mode 1)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "red")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'"
  (interactive)
  (if show-paren-mode
      (let* ((cb (char-before (point)))
             (matching-text (and cb
                                 (char-equal (char-syntax cb) ?\) )
                                 (blink-matching-open))))
        (when matching-text (message matching-text)))))

;; Colors in *compilation*
(defun colorize-buffer ()
  (progn
    (require 'ansi-color)
    (read-only-mode)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode)))
(add-hook 'compilation-filter-hook 'colorize-buffer)

;; Flyspell (auto-spelling)
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; ORG-MODE
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; ARFF WEKA
(require 'generic)
(define-generic-mode 'arff-file-mode
  (list ?%)
  (list "attribute" "relation" "end" "data")
  '(
    ("\\('.*'\\)" 1 'font-lock-string-face)
    ("^\\@\\S-*\\s-\\(\\S-*\\)" 1 'font-lock-string-face)
    ("^\\@.*\\(real\\)" 1 'font-lock-type-face)
    ("^\\@.*\\(integer\\)" 1 'font-lock-type-face)
    ("^\\@.*\\(numeric\\)" 1 'font-lock-type-face)
    ("^\\@.*\\(string\\)" 1 'font-lock-type-face)
    ("^\\@.*\\(date\\)" 1 'font-lock-type-face)
    ("^\\@.*\\({.*}\\)" 1 'font-lock-type-face)
    ("^\\({\\).*\\(}\\)$" (1 'font-lock-reference-face) (2
                                                         'font-lock-reference-face))
    ("\\(\\?\\)" 1 'font-lock-reference-face)
    ("\\(\\,\\)" 1 'font-lock-keyword-face)
    ("\\(-?[0-9]+?.?[0-9]+\\)" 1 'font-lock-constant-face)
    ("\\(\\@\\)" 1 'font-lock-preprocessor-face)
    )
  (list "\\.arff?")
  (list
   (function
    (lambda ()
      (setq font-lock-defaults (list 'generic-font-lock-defaults nil t ; case
                                     insensitive
                                     (list (cons ?* "w") (cons ?- "w"))))
      (turn-on-font-lock))))
  "Mode for arff-files.")

(global-set-key (kbd "M-.") 'xref-find-definitions)
(provide 'packages)
