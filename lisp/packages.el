;;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE MANAGEMENT

;; Initialize package management
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(use-package diminish :ensure t)

(use-package quelpa
  :ensure t)

(use-package quelpa-use-package
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI & THEME

(use-package sml-modeline
  :ensure t
  :init (sml-modeline-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t)

(use-package dired+
  :quelpa (dired+ :fetcher github :repo "emacsmirror/dired-plus")
  :init
  (setq diredp-hide-details-initially-flag nil)
  :config
  (setq diredp-hide-details-propagate-flag nil))



(use-package neotree
  :ensure t)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDITING & COMPLETION

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (add-hook 'company-mode-hook
            (lambda ()
              (define-key company-mode-map (kbd "C-c SPC") 'company-complete)
              (set (make-local-variable 'company-backends)
                   '((company-dabbrev-code company-yasnippet company-keywords))))))

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
   ("C-<return>" . set-rectangular-region-anchor)))

(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  :config
  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'nodejs-repl-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'inferior-python-mode-hook 'turn-on-smartparens-mode)
  (sp-with-modes '(php-mode js2-mode rust-mode)
    (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC") (my-php-handle-docstring "RET")))
    (sp-local-pair "/*." ".*/" :post-handlers '(("| " "SPC")))
    (sp-local-pair "{" nil :post-handlers '(("|\|\n[i]" "RET")))
    (sp-local-pair "(" nil :prefix "\\(\\sw\\|\\s_\\)*"))
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
  (global-undo-tree-mode))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-0" . ace-jump-mode)
         ("C-c C-0" . ace-jump-mode-pop-mark))
  :config
  (ace-jump-mode-enable-mark-sync))

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

(use-package origami
  :ensure t
  :init (global-origami-mode t)
  :config
  (define-key origami-mode-map (kbd "C-c RET") 'origami-recursively-toggle-node)
  (define-key origami-mode-map (kbd "C-c o") 'origami-toggle-all-nodes))

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





(use-package flycheck
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/init-flycheck.el"))

(use-package flycheck-pos-tip
  :ensure t)

(use-package quickrun
  :ensure t
  :commands quickrun
  :bind ("C-c q" . quickrun))

(use-package diff-hl
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/init-vc.el"))

(use-package multi-term
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/init-term.el"))

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
  :ensure t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LANGUAGES

(use-package js2-mode
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/lang-javascript.el"))

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

(use-package racer
  :ensure t)

(use-package company-racer
  :ensure t)

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
  :mode ("\\.[CcTt][Ss][Vv]\\\'" . csv-mode))

(use-package markdown-mode
  :ensure t
  :config
  (load-file "~/.emacs.d/lisp/init-markdown-mode.el"))

(use-package android-mode
  :ensure t
  :config
  (defun set-android-path-from-shell-PATH ()
    (let ((android-path
           (replace-regexp-in-string
            "[ \t\n]*$" ""
            (shell-command-to-string "$SHELL --login -i -c 'echo $ANDROID_HOME'"))))
      (setenv "ANDROID_HOME" android-path)))
  (when (and window-system (eq system-type 'darwin))
    (set-android-path-from-shell-PATH)))

(use-package lsp-mode
  :ensure t)

(use-package lsp-ui
  :ensure t)

(use-package tern
  :ensure t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC

(defvar org-cua-dwim-shift-translated nil)
(defvar cua--last-region-shifted nil)
(defvar cua--explicit-region-start nil)
(use-package org-cua-dwim
  :ensure t)

(use-package popup
  :ensure t)

(use-package pos-tip
  :ensure t)

(use-package visual-fill-column
  :ensure t)

(use-package css-eldoc
  :ensure t)

(use-package lsp-python-ms
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
