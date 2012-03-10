;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional PACKAGES
;; sources for elpa
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")))
;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
				(lambda (s) (end-of-buffer) (eval-print-last-sexp))))
;; main load path
(add-to-list 'load-path "~/.emacs.d/")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; SVN, YML
(add-to-list 'load-path "~/.emacs.d/el-get/psvn")
(require 'psvn)

(add-to-list 'load-path "~/.emacs.d/el-get/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; the modeline
(add-to-list 'load-path "~/.emacs.d/elpa/sml-modeline-0.5/")
(line-number-mode t)                     ;; show line numbers
(column-number-mode t)                   ;; show column numbers
(size-indication-mode t)                 ;; show file size (emacs 22+)

(if (require 'sml-modeline nil 'noerror)    ;; use sml-modeline if available
  (progn
    (sml-modeline-mode 1)                   ;; show buffer pos in the mode line
    (scroll-bar-mode -1))                   ;; turn off the scrollbar
  (scroll-bar-mode 1)                       ;; otherwise, show a scrollbar...
  (set-scroll-bar-mode 'right))             ;; ... on the right
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Eproject
(load-file "~/.emacs.d/elpa/eproject-0.4/eproject.el")
(setq prj-autotracking nil) ; Disable automatic add/remove files from project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Yasnippet
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-0.6.1/")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/global-mode 1)
(yas/load-directory "~/.emacs.d/elpa/yasnippet-0.6.1/snippets")
(setq yas/wrap-around-region t)
(setq yas/prompt-functions
    '(yas/x-prompt yas/ido-prompt))
(setq yas/trigger-key "C-<tab>")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; AUTO-COMPLETE
;; http://cx4a.org/software/auto-complete/manual.html
(add-to-list 'load-path "~/.emacs.d/el-get/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/dict/")
(ac-config-default)

; Invoke auto-completion with TAB
(setq ac-auto-start t)
(ac-set-trigger-key "TAB")
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

; showing the menu
(setq ac-auto-show-menu nil)
(setq ac-show-menu-immediately-on-auto-complete t)

; TAB completes the word, ENTER exists
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)
(define-key ac-complete-mode-map "ESC" nil)
; Just M-n, M-p for ac-next/previous
(define-key ac-completing-map (kbd "<down>") nil)
(define-key ac-completing-map (kbd "<up>") nil)

;; + etags
(add-to-list 'load-path "~/.emacs.d/el-get/auto-complete-etags/")
(require 'auto-complete-etags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;; Paredit () for lisps
(add-to-list 'load-path "~/.emacs.d/elpa/paredit-22")
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
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-prev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; EVERNOTE
(add-to-list 'load-path "~/.emacs.d/elpa/evernote-mode-0.41/")
(require 'evernote-mode)
(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; option
(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)
(global-set-key "\C-cep" 'evernote-post-region)
(global-set-key "\C-ceb" 'evernote-browser)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'packages)
