;; Start a server
(require 'server)
(or (server-running-p)
	    (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings
;;
(menu-bar-mode  t)                       ;; show the menu...
(mouse-avoidance-mode 'jump)             ;; mouse ptr when cursor is too close
(tool-bar-mode -1)                       ;; turn-off toolbar

(setq ;; scrolling
  scroll-margin 0                        ;; do smooth scrolling, ...
  scroll-conservatively 100000           ;; ... the defaults ...
  scroll-up-aggressively 0               ;; ... are very ...
  scroll-down-aggressively 0             ;; ... annoying
  scroll-preserve-screen-position t)     ;; preserve screen pos with C-v/M-v

(setq fringe-mode '(1 . 0))              ;; emacs 22+
(delete-selection-mode 1)                ;; delete the sel with a keyp

(setq search-highlight t                 ;; highlight when searching...
  query-replace-highlight t)             ;; ...and replacing
(fset 'yes-or-no-p 'y-or-n-p)            ;; enable y/n answers to yes/no

(setq completion-ignore-case t           ;; ignore case when completing...
  read-file-name-completion-ignore-case t) ;; ...filenames too

(setq initial-scratch-message ";; scratch pad\n\n")

(setq-default
 frame-title-format
 '(:eval
   (format "%s@%s:%s"
           (or (file-remote-p default-directory 'user) user-login-name)
           (or (file-remote-p default-directory 'host) system-name)
           (file-name-nondirectory (or (buffer-file-name) default-directory)))))

(put 'narrow-to-region 'disabled nil)    ;; enable...
(put 'erase-buffer 'disabled nil)        ;; ... useful things
(file-name-shadow-mode t)                ;; be smart about filenames in mbuf

(setq inhibit-startup-message t          ;; don't show ...
  inhibit-startup-echo-area-message t)   ;; ... startup messages
(setq require-final-newline t)           ;; end files with a newline

(put 'downcase-region 'disabled nil)     ;; Enable C-x C-l and C-x C-u
(put 'upcase-region 'disabled nil)       ;; for down/up-case conversions

(cua-mode t)

;; especially for osx
(if (eq system-type 'darwin)
    (progn
    (setq ns-right-alternate-modifier nil)   ;; unbind right alt
    (global-set-key (kbd "<f6>") 'ns-toggle-fullscreen) ;; Full screen mode
    (setq cua-enable-cua-keys nil)))           ;; only for rectangles

;; especially for windows
(if (eq system-type 'windows-nt)
    (progn
    (set-face-attribute 'default nil :font "ProggyCleanTT CE 12")
    (setq cua-auto-tabify-rectangles nil ;; Don't tabify after rectangle commands
           cua-keep-region-after-copy t) ;; Standard Windows behaviour
    (transient-mark-mode 1)              ;; No region when it is not highlighted
    (setq cygwin-bin "c:\\cygwin\\bin")  ;; Find & Grep on windows
    (setenv "PATH"
	    (concat cygwin-bin ";" (getenv "PATH")))
    ))            

;; theme
(load-theme 'wombat)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; keyboard / input method settings
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")       ; prefer utf-8 for language settings
(set-input-method nil)                   ; no funky input for normal editing;
(setq read-quoted-char-radix 10)         ; use decimal, not octal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; backup
(setq-default make-backup-files         nil) ; Don't want any backup files
(setq-default auto-save-list-file-name  nil) ; Don't want any .saves files
(setq-default auto-save-default         nil) ; Don't want any auto saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Needed elsewhere
(global-unset-key (kbd "<f12>"))

;; Goto-line short-cut key
(global-set-key (kbd "C-l") 'goto-line)

;; File search & in-file search
(global-set-key (kbd "<f7>") 'find-name-dired)
(global-set-key (kbd "S-<f7>") 'rgrep)

;; Delete to start of the line
(defun kill-start-of-line ()
  "kill from point to start of line"
  (interactive)
  (kill-line 0))
(global-set-key (kbd "C-;") 'kill-start-of-line)

;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(global-set-key (kbd "C-x f") 'recentf-open-files)


;; OSX has problems with PATH when running Emacs.app
;; = flymake doesn't work :-/
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when (and window-system (eq system-type 'darwin))
  ;; When started from Emacs.app or similar, ensure $PATH
  ;; is the same the user would see in Terminal.app
  (set-exec-path-from-shell-PATH))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; IDO mode
;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
(require 'ido)
(ido-mode 'both) ;; for buffers and files
(setq
  ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
  ;; ido-ignore-buffers ;; ignore these guys
  ;; '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
  ;;    "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
  ;; ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
  ido-case-fold  t                 ; be case-insensitive
  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)
  ;; ido-enable-flex-matching nil     ; don't try to be too smart
  ido-max-prospects 16             ; don't spam my minibuffer
  ido-confirm-unique-completion t) ; wait for RET, even with unique completion

;; when using ido, the confirmation is rather annoying...
 (setq confirm-nonexistent-file-or-buffer nil)

;; increase minibuffer size when ido completion is active
(add-hook 'ido-minibuffer-setup-hook
  (function
    (lambda ()
      (make-local-variable 'resize-minibuffer-window-max-height)
      (setq resize-minibuffer-window-max-height 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CEDET mode
(require 'semantic)
(require 'semantic/sb)
(require 'srecode)
;(global-ede-mode 1)
(semantic-mode 1)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-completions-mode 1)
;(global-semantic-decoration-mode 1)
(global-semantic-highlight-func-mode 1)
(global-semantic-stickyfunc-mode -1)
;(global-semantic-idle-summary-mode 1)
(global-semantic-mru-bookmark-mode 1)
;; gnu global support
;(require 'semanticdb-global)
;(semanticdb-enable-gnu-global-databases 'c-mode)
;(semanticdb-enable-gnu-global-databases 'c++-mode)
(set-default 'semantic-case-fold t)

(if (eq system-type 'darwin)
  (setq semantic-python-dependency-system-include-path
      '("/usr/lib/python2.7/")))
(if (eq system-type 'windows-nt)
  (setq semantic-python-dependency-system-include-path
      '("C:/Python26/")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; SPEEDBAR
(setq speedbar-default-position 'right)
(setq speedbar-load-hook nil)
(setq speedbar-show-unknown-files t)
(setq speedbar-use-images t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; C/C++ mode
(defun my-c-mode-common-hook ()
  ;; customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  ;; other customizations can go here
  (setq c-default-style "bsd")

  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)
  (setq c-indent-level 4)

  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode t)  ; use spaces only if nil

  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; EMACS-LISP indenting
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (local-set-key (kbd "RET") 'newline-and-indent)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Indenting - It's kind of sad this doesn't exist normally...
(defun indent-rigidly-n (n)
  "Indent the region, or otherwise the current line, by N spaces."
  (let* ((use-region (and transient-mark-mode mark-active))
		 (rstart (if use-region (region-beginning) (point-at-bol)))
		 (rend   (if use-region (region-end)       (point-at-eol)))
		 (deactivate-mark "irrelevant")) ; avoid deactivating mark
	(indent-rigidly rstart rend n)))
(defun indent-rigidly-tab ()
  "Indent the region, or otherwise the current line, by 'tab' spaces."
  (interactive)
  (indent-rigidly-n tab-width))
(defun outdent-rigidly-tab ()
  "Indent the region, or otherwise the current line, by 'tab' spaces."
  (interactive)
  (indent-rigidly-n (- tab-width)))
(global-set-key [C-S-right] 'indent-rigidly-tab)
(global-set-key [C-S-left] 'outdent-rigidly-tab)
(global-set-key [backtab] 'outdent-rigidly-tab)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; MOVE LINE
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
(global-set-key [?\e up] 'move-text-up)              ; moving lines
(global-set-key [?\e down] 'move-text-down)          ; in console
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; WINDOW MANAGEMENT
(defun swap-with (dir)
  (interactive)
  (let ((other-window (windmove-find-other-window dir)))
    (when other-window
      (let* ((this-window  (selected-window))
             (this-buffer  (window-buffer this-window))
             (other-buffer (window-buffer other-window))
             (this-start   (window-start this-window))
             (other-start  (window-start other-window)))
        (set-window-buffer this-window  other-buffer)
        (set-window-buffer other-window this-buffer)
        (set-window-start  this-window  other-start)
        (set-window-start  other-window this-start)))))

(global-set-key (kbd "C-M-J") (lambda () (interactive) (swap-with 'down)))
(global-set-key (kbd "C-M-K") (lambda () (interactive) (swap-with 'up)))
(global-set-key (kbd "C-M-H") (lambda () (interactive) (swap-with 'left)))
(global-set-key (kbd "C-M-L") (lambda () (interactive) (swap-with 'right)))

(global-set-key (kbd "M-J") (lambda () (interactive) (enlarge-window 1)))
(global-set-key (kbd "M-K") (lambda () (interactive) (enlarge-window -1)))
(global-set-key (kbd "M-H") (lambda () (interactive) (enlarge-window -1 t)))
(global-set-key (kbd "M-L") (lambda () (interactive) (enlarge-window 1 t)))

(global-unset-key (kbd "M-j"))
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; INITIAL WINDOW SIZE
(setq initial-frame-alist
        (append '((width . 117) (height . 57) (left . 59) (top . 27))
                initial-frame-alist))
(setq default-frame-alist
        (append '((width . 117) (height . 57) (left . 59) (top . 27))
                default-frame-alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; UNSETTING KEYS
(defun get-key-combo (key)
  "Just return the key combo entered by the user"
  (interactive "kKey combo: ")
  key)
(defun keymap-unset-key (key keymap)
  "Remove binding of KEY in a keymap
    KEY is a string or vector representing a sequence of keystrokes."
  (interactive
   (list (call-interactively #'get-key-combo)
	 (completing-read "Which map: " minor-mode-map-alist nil t)))
  (let ((map (rest (assoc (intern keymap) minor-mode-map-alist))))
    (when map
      (define-key map key nil)
      (message  "%s unbound for %s" key keymap))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; IMPORTS
(add-to-list 'load-path "~/.emacs.d/")
(require 'packages)
(require 'lang-python)
(require 'lang-clojure)
(require 'lang-javascript)
(require 'lang-php)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
