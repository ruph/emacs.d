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
        (:name ac-slime         :type elpa)
        (:name paredit          :type elpa)
        (:name flymake-cursor   :type elpa)
        (:name sml-modeline     :type elpa)
        (:name markdown-mode    :type elpa)
        (:name org-cua-dwim     :type elpa)
        (:name multiple-cursors :type elpa)
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
               :website "https://github.com/nishimura/minimal-yasnippet-php-mode"
               :description ""
               :type github
               :pkgname "nishimura/minimal-yasnippet-php-mode"
               :features nil)
        (:name js2-mode-mooz
               :type git
               :url "git://github.com/mooz/js2-mode.git"
               :load "js2-mode.el"
               :compile ("js2-mode.el" "js2-imenu-extras.el")
               :features js2-mode)
        (:name eproject
               :type git
               :url "git://github.com/gabrielelanaro/eproject.git"
               :load nil
               :compile ("eproject.el" "eproject-config.el")
               :features eproject)
        ))

;; All packages for installation
(setq my-el-get-packages
      (append '(autopair highlight-parentheses highlight-symbol
                         ace-jump-mode mmm-mode psvn pymacs yaml-mode
                         php-mode yasnippet helm deft android-mode
                         popup auto-complete auto-complete-etags)
              (mapcar 'el-get-source-name el-get-sources)))

;; Install packages
(el-get 'sync my-el-get-packages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; SVN
(add-to-list 'load-path "~/.emacs.d/el-get/psvn")
(require 'psvn)

;; GIT ~ http://files.taesoo.org/git-emacs/git-emacs.html
;; ~ slows down emacs
;;(add-to-list 'load-path "~/.emacs.d/el-get/git-emacs")
;;(require 'git-emacs)

;; YML
(add-to-list 'load-path "~/.emacs.d/el-get/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; CSS
(add-hook 'css-mode-hook
          (lambda ()
            (setq tab-width 4)
            (local-set-key (kbd "RET") 'newline-and-indent)
            (add-hook 'before-save-hook 'delete-trailing-whitespace)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; the modeline
(add-to-list 'load-path "~/.emacs.d/el-get/sml-modeline")
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


;; Eproject
(require 'eproject)

;; remove pesky M-left/right
(defun prj-setup-all ()
  (progn
    (prj-setkeys)
    (prj-setmenu)
    (prj-settitle)
    (prj-config-print)
    (message ">> EPROJECT setup" )
    (keymap-unset-key [M-left] "eproject-mode")
    (keymap-unset-key [M-right] "eproject-mode")
    (keymap-unset-key [f5] "eproject-mode")
    ))
(global-set-key (kbd "C-S-<f5>") 'eproject-setup-toggle)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; HELM
(require 'helm-config)
(helm-mode 1)

;; set helm for find files
(global-unset-key (kbd "C-x C-f"))
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; a little different buffer finder
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;; search kill-ring
(global-set-key (kbd "S-C-v") 'helm-show-kill-ring)
(global-set-key (kbd "s-V") 'helm-show-kill-ring)

;; recursive helm-do-grep
(global-set-key (kbd "S-<f7>")
                (lambda () (interactive)
                  (let ((current-prefix-arg '(4))) ; C-u
                    (call-interactively 'helm-do-grep))))

;; eproject integration
(defun helm-eproject-resource ()
  "Enumerate files belonging to the eproject"
  (interactive)
  (helm
   '((
      (name . "Files in eproject:")
      (init . (lambda ()
                (with-current-buffer (helm-candidate-buffer 'local)
                  (mapcar
                   (lambda (item)
                     (insert (format "%s/%s\n" (cadr prj-current) (car item))))
                   prj-files))))
      (candidates-in-buffer)
      (type . file)
      ))
   nil "Switch to file: " nil nil))
(global-set-key (kbd "S-C-t") 'helm-eproject-resource)

;; find files recursively from project dir
(defun helm-eproject-recursive-resources ()
  "Enumerate files belonging to the eproject"
  (interactive)
  (helm
   '(((name . "All files under eproject root dir:")
      (init . (lambda ()
                (with-current-buffer (helm-candidate-buffer 'local)
                  (insert
                   (shell-command-to-string
                    (format "find %s -type d \\( -name .svn -o -name .git -o -name .hg \\) -prune -o -type f -print" (cadr prj-current))))
                  )))
      (candidates-in-buffer)
      (type . file)
      ))
   nil "Switch to file: " nil nil))
(global-set-key (kbd "S-C-r") 'helm-eproject-recursive-resources)

;; for eshell: complete and history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map
                [remap pcomplete]
                'helm-esh-pcomplete)
              (define-key eshell-mode-map
                (kbd "C-r")
                'helm-eshell-history)))
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


;; AUTO-COMPLETE
;; http://cx4a.org/software/auto-complete/manual.html
(add-to-list 'load-path "~/.emacs.d/el-get/popup/")
(add-to-list 'load-path "~/.emacs.d/el-get/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/dict/")
(ac-config-default)

;; Invoke auto-completion with TAB
(setq ac-auto-start t)
(ac-set-trigger-key "TAB")
;; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

;; showing the menu
(setq ac-auto-show-menu nil)
(setq ac-show-menu-immediately-on-auto-complete t)

;; TAB completes the word, ENTER exists
(define-key ac-completing-map [tab] 'ac-complete)
(define-key ac-completing-map [return] nil)

;; Just M-n, M-p for ac-next/previous
(define-key ac-completing-map (kbd "<down>") nil)
(define-key ac-completing-map (kbd "<up>") nil)

;; + etags
(add-to-list 'load-path "~/.emacs.d/el-get/auto-complete-etags/")
(require 'auto-complete-etags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Android mode
(add-to-list 'load-path "~/.emacs.d/el-get/android-mode")
(require 'android-mode)
(setq android-mode-sdk-dir "/usr/local/opt/android-sdk")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Automatic "" ()
(add-to-list 'load-path "~/.emacs.d/el-get/autopair")
(require 'autopair)
(autopair-global-mode)
(setq autopair-blink nil)
(set-default 'autopair-dont-activate #'(lambda ()
                                         (or
                                          (eq major-mode 'clojure-mode)
                                          (eq major-mode 'emacs-lisp-mode)
                                          (eq major-mode 'lisp-mode)
                                          (eq major-mode 'emacs-interaction-mode)
                                          (eq major-mode 'scheme-mode)
                                          (eq major-mode 'slime-repl-mode)
                                          (eq major-mode 'sldb-mode))))
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
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Markdown
(add-to-list 'load-path "~/.emacs.d/el-get/markdown-mode")
(require 'markdown-mode)
(add-hook 'markdown-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)
            (visual-line-mode t)
            (remove-hook 'before-save-hook 'delete-trailing-whitespace)
            ))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Deft NOTES (markdown)
(add-to-list 'load-path "~/.emacs.d/el-get/deft/")
(require 'deft)
(if (eq system-type 'windows-nt)
    (setq deft-directory "d:\\My Dropbox\\Notes")
  (setq deft-directory "~/Dropbox/Notes"))
(setq deft-extension "txt")
(setq deft-use-filename-as-title t)
(setq deft-text-mode 'markdown-mode)
(global-set-key (kbd "C-c n") 'deft)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ORG-MODE
;; Word-wrapping
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; Cua compatibility
(add-to-list 'load-path "~/.emacs.d/el-get/org-cua-dwim")
(require 'org-cua-dwim)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'packages)
