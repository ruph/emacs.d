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
      '((:name clojure-mode     :type elpa)
        (:name paredit          :type elpa)
        (:name sml-modeline     :type elpa)
        (:name org-cua-dwim     :type elpa)
        (:name flycheck         :type elpa)
        (:name css-eldoc        :type elpa)
        (:name projectile       :type elpa)
        (:name helm-projectile  :type elpa)
        (:name dired+           :type elpa)
        (:name swiper           :type elpa)
        (:name swiper-helm      :type elpa)
        (:name undo-tree        :type elpa)
        (:name evil             :type elpa)
        (:name csv-mode
               :website "http://www.emacswiki.org/emacs/CsvMode"
               :description "This package implements CSV mode, a major mode for editing records in a generalized CSV (character-separated values) format."
               :type github
               :pkgname "emacsmirror/csv-mode"
               :compile ("csv-mode.el")
               :features csv-mode)
        (:name flymake
               :website "https://github.com/illusori/emacs-flymake"
               :description "This project is a fork of Pavel Kobyakov's excellent flymake.el."
               :type github
               :pkgname "illusori/emacs-flymake"
               :load "flymake.el"
               :compile ("flymake.el")
               :features flymake)
        (:name yasnippets/clojure-mode
               :website "https://github.com/swannodette/clojure-snippets"
               :description "Clojure-mode yasnippets"
               :type github
               :pkgname "swannodette/clojure-snippets"
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
        ))

;; All packages for installation
(setq my-el-get-packages
      (append '(helm helm-ag highlight-parentheses highlight-symbol cider
                     ace-jump-mode psvn pymacs yaml-mode js2-mode
                     php-mode yasnippet android-mode smarttabs popup
                     company-mode multi-term flymake-cursor volatile-highlights
                     markdown-mode multiple-cursors quickrun diff-hl
                     web-mode emmet-mode rainbow-mode less-css-mode
                     skewer-less helm-dash clean-aindent ggtags helm-gtags
                     editorconfig tern company-tern emacs-neotree
                     go-mode rust-mode)
              (mapcar 'el-get-source-name el-get-sources)))

;; Install packages
(el-get 'sync my-el-get-packages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; editorconfig
(require 'editorconfig)
(load "editorconfig")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; quickrun
(require 'quickrun)
(global-set-key (kbd "C-c q") 'quickrun)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; clean-aindent
(electric-indent-mode nil)
(clean-aindent-mode t)
(setq clean-aindent-is-simple-indent t)
(define-key global-map (kbd "RET") 'newline-and-indent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; YML
(add-to-list 'load-path "~/.emacs.d/el-get/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; CSV
(add-to-list 'load-path "~/.emacs.d/el-get/csv-mode/")
(add-to-list 'auto-mode-alist '("\\.[CcTt][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; Better undo
(require 'undo-tree)
(global-undo-tree-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")
(setq multi-term-switch-after-close nil)
(multi-term-keystroke-setup)
(add-hook 'term-mode-hook
          (lambda ()
            (yas-minor-mode 0)))

;; from https://github.com/tavisrudd/emacs.d/blob/master/dss-term.el
(defun term-setup-tramp ()
  "Setup ansi-term/tramp remote directory tracking
   NOTE:  this appears to have some sort of timing bug in it and doesn't always work"
  (interactive)
  (term-send-raw-string
   (concat "
function eterm_set_variables {
    local emacs_host=\"" (car (split-string (system-name) "\\.")) "\"
    local tramp_hostname=${TRAMP_HOSTNAME-$(hostname)}
    if [[ $TERM == \"eterm-color\" ]]; then
        if [[ $tramp_hostname != \"$emacs_host\" ]]; then
            echo -e \"\\033AnSiTu\" ${TRAMP_USERNAME-$(whoami)}
            echo -e \"\\033AnSiTh\" $tramp_hostname
        fi
        echo -e \"\\033AnSiTc\" $(pwd)
    elif [[ $TERM == \"screen\" || $TERM  == \"screen-256color\" ]]; then
        if [[ $tramp_hostname != \"$emacs_host\" ]]; then
            echo -e \"\\033P\\033AnSiTu\\033\\\\\" ${TRAMP_USERNAME-$(whoami)}
            echo -e \"\\033P\\033AnSiTh\\033\\\\\" $tramp_hostname
        fi
        echo -e \"\\033P\\033AnSiTc\\033\\\\\" $(pwd)
    fi
}
function eterm_tramp_init {
    for temp in cd pushd popd; do
        alias $temp=\"eterm_set_cwd $temp\"
    done

    # set hostname, user, and cwd now
    eterm_set_variables
}
function eterm_set_cwd {
    $@
    eterm_set_variables
}
eterm_tramp_init
export -f eterm_tramp_init
export -f eterm_set_variables
export -f eterm_set_cwd
clear
echo \"tramp initialized\"
")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; the modeline
(line-number-mode t)                     ;; show line numbers
(column-number-mode t)                   ;; show column numbers
(size-indication-mode t)                 ;; show file size (emacs 22+)

(if (require 'sml-modeline nil 'noerror) ;; use sml-modeline if available
    (progn
      (sml-modeline-mode 1)              ;; show buffer pos in the mode line
      (scroll-bar-mode -1))              ;; turn off the scrollbar
  (scroll-bar-mode 1)                    ;; otherwise, show a scrollbar...
  (set-scroll-bar-mode 'right))          ;; ... on the right
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Yasnippet
(add-to-list 'load-path "~/.emacs.d/el-get/yasnippet")
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/el-get/yasnippet/snippets"
        "~/.emacs.d/el-get/yasnippets"
        "~/.emacs.d/el-get/yasnippets/minimal-yasnippet-php-mode"
        ))
(yas/global-mode 1)
(setq yas/wrap-around-region t)
(setq yas/prompt-functions
      '(yas/x-prompt yas/ido-prompt))
(setq yas/trigger-key "C-<tab>")
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
(add-to-list 'load-path "~/.emacs.d/el-get/multiple-cursors")
(require 'multiple-cursors)
(global-set-key (kbd "C-c c") 'mc/edit-lines)
(global-set-key (kbd "C-c e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c a") 'mc/edit-beginnings-of-lines)
;; Rectangular region mode
(global-set-key (kbd "C-<return>") 'set-rectangular-region-anchor)
;; Mark more like this
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c <") 'mc/mark-all-like-this)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Paredit () for lisps
(add-to-list 'load-path "~/.emacs.d/el-get/paredit")
(require 'paredit)
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
(add-hook 'slime-repl-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))

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


;; Highlight ()
(add-to-list 'load-path "~/.emacs.d/el-get/highlight-parentheses")
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
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
(add-to-list 'load-path "~/.emacs.d/el-get/ace-jump-mode")
(require 'ace-jump-mode)
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back:-)"t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-0") 'ace-jump-mode)
(define-key global-map (kbd "C-c C-0") 'ace-jump-mode-pop-mark)
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
(add-to-list 'load-path "~/.emacs.d/el-get/deft-multidir/")
(require 'deft)
(setq deft-directories '("~/Dropbox/Notes"))
(setq deft-extension "txt")
(setq deft-use-filename-as-title t)
(setq deft-text-mode 'gfm-mode)
(global-set-key (kbd "C-c n") 'deft)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ORG-MODE
;; Word-wrapping
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; Cua compatibility
(add-to-list 'load-path "~/.emacs.d/el-get/org-cua-dwim")
(require 'org-cua-dwim)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Highlighting copy/paste actions
(require 'volatile-highlights)
(volatile-highlights-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; SmartTabs
(require 'smart-tabs-mode)
(smart-tabs-insinuate 'python 'javascript)
(smart-tabs-advice js2-indent-line js2-basic-offset)
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


(provide 'packages)
