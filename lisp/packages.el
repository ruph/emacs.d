;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional PACKAGES

;; making sure installation doesn't break
(setq url-http-attempt-keepalives nil)

;; sources for elpa
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "http://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (emacs-lisp-mode)
       (goto-char (point-min))
       (down-list)
       (eval-defun nil)))))

;; Extra recipes
(setq el-get-sources
      '((:name paredit          :type elpa)
        (:name dash             :type elpa)  ; dependency smartparens, origami, lsp, racer, ...
        (:name dash-functional  :type elpa)  ; dependency lsp
        (:name popup            :type elpa)  ; dependency
        (:name s                :type elpa)  ; dependency racer, origami, f, ...
        (:name f                :type elpa)  ; dependency lsp-mode
        (:name pkg-info         :type elpa)  ; dependency flycheck, cider
        (:name pos-tip          :type elpa)  ; dependency racer
        (:name sml-modeline     :type elpa)
        (:name org-cua-dwim     :type elpa)
        (:name flycheck         :type elpa)
        (:name css-eldoc        :type elpa)
        (:name origami          :type elpa)
        (:name undo-tree        :type elpa)
        (:name rust-mode        :type elpa)
        (:name racer            :type elpa)
        (:name company-racer    :type elpa)
        (:name smartparens      :type elpa)
        (:name swift-mode       :type elpa)
        (:name helm             :type elpa)
        (:name helm-swoop       :type elpa)
        (:name shackle          :type elpa)
        (:name inf-clojure      :type elpa)
        (:name clojure-mode     :type elpa)
        (:name cider            :type elpa)
        (:name ein              :type elpa)
        (:name visual-fill-column :type elpa)
        (:name markdown-mode    :type elpa)
        (:name lsp-mode         :type elpa)
        (:name lsp-ui           :type elpa)
        (:name lsp-python-ms    :type elpa)
        (:name tern             :type elpa)
        (:name csv-mode
               :website "http://www.emacswiki.org/emacs/CsvMode"
               :description "This package implements CSV mode, a major mode for editing records in a generalized CSV (character-separated values) format."
               :type github
               :pkgname "emacsmirror/csv-mode"
               :compile ("csv-mode.el")
               :features csv-mode)
        (:name yasnippets/clojure-mode
               :website "https://github.com/mpenet/clojure-snippets"
               :description "Clojure-mode yasnippets"
               :type github
               :pkgname "mpenet/clojure-snippets"
               :features nil)
        (:name yasnippets/minimal-yasnippet-php-mode
               :website "https://github.com/ruph/minimal-yasnippet-php-mode"
               :description ""
               :type github
               :pkgname "ruph/minimal-yasnippet-php-mode"
               :features nil)
        (:name yasnippets/php-auto-yasnippets
               :website "https://github.com/ejmr/php-auto-yasnippets"
               :description ""
               :type github
               :pkgname "ejmr/php-auto-yasnippets"
               :features nil)
        (:name eproject
               :type git
               :url "git://github.com/gabrielelanaro/eproject.git"
               :autoloads nil  ;; autoload problem with eproject
               :compile ("eproject.el" "eproject-config.el")
               :features eproject)
        (:name deft-multidir
               :type git
               :url "git://github.com/dsevilla/deft-multidir.git"
               :load "deft.el"
               :compile ("deft.el")
               :features deft)
        (:name syntactic-close
               :type git
               :url "git://github.com/emacs-berlin/syntactic-close.git"
               :load "syntactic-close.el"
               :compile ("syntactic-close.el")
               :features syntactic-close)
        (:name company-tern
               :type git
               :url "git://github.com/kevinushey/company-tern.git"
               :load "company-tern.el"
               :compile ("company-tern.el")
               :features company-tern)
        ))

;; All packages for installation
(setq my-el-get-packages
      (append '(helm-ag rainbow-delimiters highlight-symbol projectile
                        ace-jump-mode pyenv yaml-mode js2-mode
                        use-package php-mode yasnippet android-mode popup
                        diminish company-mode multi-term volatile-highlights
                        multiple-cursors quickrun diff-hl
                        web-mode emmet-mode rainbow-mode less-css-mode nodejs-repl
                        skewer-less clean-aindent ggtags helm-gtags
                        emacs-neotree dired+
                        go-mode writeroom-mode helm-projectile ace-window
                        visual-regexp visual-regexp-steroids yasnippet-snippets
                        comment-dwim-2 pos-tip flycheck-pos-tip)
              (mapcar 'el-get-source-name el-get-sources)))

;; Install packages
(el-get 'sync my-el-get-packages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Use-package
(add-to-list 'load-path "~/.emacs.d/el-get/use-package")
(eval-when-compile (require 'use-package))
(add-to-list 'load-path "~/.emacs.d/el-get/diminish")
(require 'diminish)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; dired+
(use-package dired+
  :init
  (setq diredp-hide-details-initially-flag nil)
  :config
  (setq diredp-hide-details-propagate-flag nil)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Quickrun
(use-package quickrun
  :commands quickrun
  :bind ("C-c q" . quickrun))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Recent files
(use-package recentf
  :init
  (recentf-mode 1)
  :config
  (setq
   recentf-max-saved-items 256
   recentf-max-menu-items 32)
  :bind ("C-x f" . helm-recentf))


;; Auto indent
(electric-indent-mode nil)
(setq clean-aindent-is-simple-indent t)


;; Visual regex replace + python regex engine
(use-package visual-regexp-steroids
  :bind (("C-M-%" . vr/replace)
         ("M-%"   . vr/query-replace)
         ("C-r"   . vr/isearch-backward)
         ("C-s"   . vr/isearch-forward)
         ("C-M-s" . isearch-forward)  ; ordinary forward search
         ("C-M-r" . isearch-backward) ; ordinary backward search
         ("C-c m" . vr/mc-mark)  ; for multiple-cursors
         ))


;; Commenting
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))


;; Syntactic close
(use-package syntactic-close
  :bind ("C-<" . syntactic-close))

;; Window navigation
(use-package ace-window
  :bind (("M-h" . ace-window)
         ("M-o" . ace-swap-window))
  :config
  (setq aw-keys '(?g ?h ?j ?k ?l ?b ?n ?m)))

;; Code folding
(use-package origami
  :init
  (global-origami-mode t)
  :config
  (define-key origami-mode-map (kbd "C-c RET") 'origami-recursively-toggle-node)
  (define-key origami-mode-map (kbd "C-c o") 'origami-toggle-all-nodes))

;; SQL
(defun sql-connect-bookmark (product connection)
  ;; remember to set the sql-product, otherwise, it will fail for the first time
  ;; you call the function -> http://lists.gnu.org/archive/html/bug-gnu-emacs/2014-12/msg00980.html
  ;; bookmarks in private.el -> https://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client/
  (setq sql-product product)
  (sql-connect connection))

;; Disable completion in mysql
(setq sql-mysql-options (list "-A"))

;; Don't break the lines
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; YML
(add-to-list 'load-path "~/.emacs.d/el-get/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; CSV
(use-package csv-mode
  :mode ("\.[CcTt][Ss][Vv]\'' . csv-mode))

;; Better undo
(use-package undo-tree
  :init
  (global-undo-tree-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; the modeline
(line-number-mode t)                     ;; show line numbers
(column-number-mode t)                   ;; show column numbers
(size-indication-mode t)                 ;; show file size (emacs 22+)

(if (require 'sml-modeline nil 'noerror) ;; use sml-modeline if available
    (progn
      (sml-modeline-mode 1)              ;; show buffer pos in the mode line
      (if (fboundp 'scroll-bar-mode)
          (scroll-bar-mode -1)))         ;; turn off the scrollbar
  (if (fboundp 'scroll-bar-mode)
      (progn
        (scroll-bar-mode 1)               ;; otherwise, show a scrollbar...
        (set-scroll-bar-mode 'right))))   ;; ... on the right
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Yasnippet
(use-package yasnippet
  :if (not noninteractive)
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode)
  :config
  (progn
    (setq yas-snippet-dirs
          '("~/.emacs.d/el-get/yasnippet-snippets/snippets"
            "~/.emacs.d/el-get/yasnippets"
            "~/.emacs.d/el-get/yasnippets/minimal-yasnippet-php-mode"
            ))
    (yas-global-mode 1)
    (setq yas-wrap-around-region t)
    (setq yas-prompt-functions
          '(yas/x-prompt yas/ido-prompt))
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Company-mode for autocompletion
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'company-mode-hook
          (lambda ()
            (define-key company-mode-map (kbd "C-c SPC") 'company-complete)
            (set (make-local-variable 'company-backends)
                 '((company-dabbrev-code company-yasnippet company-keywords)))
            ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Android mode
(add-to-list 'load-path "~/.emacs.d/el-get/android-mode")
(require 'android-mode)
;; .bashrc << export ANDROID_HOME=/usr/local/opt/android-sdk
(defun set-android-path-from-shell-PATH ()
  (let ((android-path
         (replace-regexp-in-string
          "[ \t\n]*$" ""
          (shell-command-to-string "$SHELL --login -i -c 'echo $ANDROID_HOME'"))))
    (setenv "ANDROID_HOME" android-path)))
(when (and window-system (eq system-type 'darwin))
  (set-android-path-from-shell-PATH))
(setq android-mode-sdk-dir (getenv "ANDROID_HOME"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Multiple-cursors
(use-package multiple-cursors
  :bind
  (("C-c c" . mc/edit-lines)
   ("C-c e" . mc/edit-ends-of-lines)
   ("C-c a" . mc/edit-beginnings-of-lines)
   ;; Rectangular region mode
   ("C-<return>" . set-rectangular-region-anchor)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Smartparens
(use-package smartparens
  :init
  (use-package smartparens-config)
  :config
  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'nodejs-repl-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'inferior-python-mode-hook 'turn-on-smartparens-mode)

  ;;; {}, comments in C-like-modes
  (sp-with-modes '(php-mode js2-mode rust-mode)
    (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                               (my-php-handle-docstring "RET")))
    (sp-local-pair "/*." ".*/" :post-handlers '(("| " "SPC")))
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "(" nil :prefix "\\(\\sw\\|\\s_\\)*"))

  (defun my-php-handle-docstring (&rest _ignored)
    (-when-let (line (save-excursion
                       (forward-line)
                       (thing-at-point 'line)))
      (cond
       ((string-match-p "function" line)
        (save-excursion
          (insert "\n")
          (let ((args (save-excursion
                        (forward-line)
                        (my-php-get-function-args))))
            (--each args
              (insert (format "* @param %s\n" it)))))
        (insert "* "))
       ((string-match-p ".*class\\|interface" line)
        (save-excursion (insert "\n*\n* @author\n"))
        (insert "* ")))
      (let ((o (sp--get-active-overlay)))
        (indent-region (overlay-start o) (overlay-end o)))))
  :bind
  (("C-<right>" . sp-slurp-hybrid-sexp)
   ("C-<left>" . sp-forward-barf-sexp)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Paredit () for lisps
(use-package paredit
  :hook ((emacs-lisp-mode . (lambda () (turn-off-smartparens-mode) (paredit-mode +1)))
         (lisp-mode . (lambda () (turn-off-smartparens-mode) (paredit-mode +1)))
         (lisp-interaction-mode . (lambda () (turn-off-smartparens-mode) (paredit-mode +1)))
         (scheme-mode . (lambda () (turn-off-smartparens-mode) (paredit-mode +1)))
         (slime-repl-mode . (lambda () (turn-off-smartparens-mode) (paredit-mode +1)))
         (clojure-mode . (lambda () (turn-off-smartparens-mode) (paredit-mode +1)))))

;; these are used for moving lines/regions
(keymap-unset-key (kbd "M-<up>") "paredit-mode")
(keymap-unset-key (kbd "M-<down>") "paredit-mode")
(keymap-unset-key [?\e up] "paredit-mode")
(keymap-unset-key [?\e down] "paredit-mode")
(keymap-unset-key (kbd "M-J") "paredit-mode") ;; resize window conflict

;; from emacs-starter-kit
(defun esk-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Colored ()
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Show Paren Mode - Highlight ()
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Highlight symbol
(add-to-list 'load-path "~/.emacs.d/el-get/highlight-symbol")
(require 'highlight-symbol)
(global-set-key (kbd "M-<f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "<f3>")   'highlight-symbol-next)
(global-set-key (kbd "S-<f3>") 'highlight-symbol-prev)
(global-set-key (kbd "C-<f3>") 'highlight-symbol-prev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Fast/direct cursor location minor mode.
(use-package ace-jump-mode
  :bind (("C-0" . ace-jump-mode)
         ("C-c C-0" . ace-jump-mode-pop-mark))
  :config
  (eval-after-load "ace-jump-mode"
    '(ace-jump-mode-enable-mark-sync)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Colors in *compilation*
(defun colorize-buffer ()
  (progn
    (require 'ansi-color)
    (read-only-mode)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode)))
(add-hook 'compilation-filter-hook 'colorize-buffer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Flyspell (auto-spelling)
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Deft NOTES (markdown)
(use-package deft
  :init
  (setq deft-directories '("~/Dropbox/Notes")
        deft-extension "txt"
        deft-use-filename-as-title t
        deft-text-mode 'gfm-mode)
  :bind
  ("C-c n" . deft))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ORG-MODE
;; Word-wrapping
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; Cua compatibility
(setq org-cua-dwim-shift-translated nil) ; suppressing free variable warning
(setq cua--last-region-shifted nil)      ; suppressing free variable warning
(setq cua--explicit-region-start nil)    ; suppressing free variable warning
(use-package org-cua-dwim)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Highlighting copy/paste actions
(use-package volatile-highlights
  :init
  (volatile-highlights-mode t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
  (list "\.arff?")
  (list
   (function
    (lambda ()
      (setq font-lock-defaults (list 'generic-font-lock-defaults nil t ; case
                                     insensitive
                                     (list (cons ?* "w") (cons ?- "w"))))
      (turn-on-font-lock))))
  "Mode for arff-files.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Writeroom
(use-package writeroom-mode
  :init
  (setq writeroom-extra-line-spacing 0.4
        writeroom-maximize-window t)
  :config
  (add-hook 'writeroom-mode-hook
            (lambda () (progn (text-scale-increase 1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'packages)
