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
(add-to-list 'load-path "~/.emacs.d/elpa/sml-modeline-0.5/")
(line-number-mode t)                     ;; show line numbers
(column-number-mode t)                   ;; show column numbers
(size-indication-mode t)                 ;; show file size (emacs 22+)

(if (require 'sml-modeline nil 'noerror) ;; use sml-modeline if available
    (progn
      (sml-modeline-mode 1)              ;; show buffer pos in the mode line
      (scroll-bar-mode -1))		 ;; turn off the scrollbar
  (scroll-bar-mode 1)			 ;; otherwise, show a scrollbar...
  (set-scroll-bar-mode 'right))		 ;; ... on the right
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Eproject
(load-file "~/.emacs.d/eproject/eproject.el")

;; change pesky M-left/right to C-,/."
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
(global-set-key (kbd "C->") 'eproject-nextfile)
(global-set-key (kbd "C-<") 'eproject-prevfile)
(global-set-key (kbd "C-S-<f5>") 'eproject-setup-toggle)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ANYTHING
(add-to-list 'load-path "~/.emacs.d/elpa/anything-1.287")
(require 'anything)
(add-to-list 'load-path "~/.emacs.d/elpa/anything-config-0.4.1")
(require 'anything-config)

;; recursive anything-do-grep
(global-set-key (kbd "S-<f7>")
                (lambda () (interactive)
                  (let ((current-prefix-arg '(4))) ; C-u
                    (call-interactively 'anything-do-grep))))

;; all files from current directory
(load-file "~/.emacs.d/emacs-anything-fpr/anything-find-project-resources.el")
(global-set-key (kbd "S-C-r") 'anything-find-resource)

;; eproject integration
(defun anything-eproject-resource ()
  "Enumerate files belonging to the eproject"
  (interactive)
  (anything
   '((
      (name . "Files in eproject:")
      (init . (lambda ()
		(with-current-buffer (anything-candidate-buffer 'local)
		  (mapcar
		   (lambda (item)
		     (insert (format "%s/%s\n" (cadr prj-current) (car item))))
		   prj-files))))
      (candidates-in-buffer)
      (type . file)
      ))
   nil "resource files: " nil nil))
(global-set-key (kbd "S-C-t") 'anything-eproject-resource)

;; Better buffer finder
(global-set-key (kbd "C-x C-b") 'anything-buffers+)
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
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)
(define-key ac-complete-mode-map "ESC" nil)
;; Just M-n, M-p for ac-next/previous
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
(global-set-key (kbd "M-<f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "<f3>") 'highlight-symbol-next)
(global-set-key (kbd "S-<f3>") 'highlight-symbol-prev)
(global-set-key (kbd "C-<f3>") 'highlight-symbol-prev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Fast/direct cursor location minor mode.
(add-to-list 'load-path "~/.emacs.d/elpa/ace-jump-mode-1.0")
(require 'ace-jump-mode)
(define-key global-map (kbd "C-0") 'ace-jump-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Colors in *compilation*
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
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
