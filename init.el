;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; Faster start by disabling special processing temporarily,
(setq bkp-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist '())

;; Faster start by limiting GC
(setq gc-cons-threshold (* 100 1024 1024))

;; Start a server
(require 'server)
(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
(unless (server-running-p)
  (server-start))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General settings
;;
(setq disabled-command-function nil)    ;; enable all disabled commands
(menu-bar-mode  t)                      ;; show the menu...
(mouse-avoidance-mode 'jump)            ;; mouse ptr when cursor is too close
(if (fboundp 'tool-bar-mode)
	(tool-bar-mode -1))                 ;; turn-off toolbar

(setq                                   ;; scrolling
 scroll-margin 3                        ;; do smooth scrolling, ...
 scroll-conservatively 100000           ;; ... the defaults ...
 scroll-up-aggressively nil             ;; ... are very ...
 scroll-down-aggressively nil           ;; ... annoying
 scroll-preserve-screen-position t)     ;; preserve screen pos with C-v/M-v

(setq fringe-mode '(1 . 0))             ;; emacs 22+
(delete-selection-mode 1)               ;; delete the sel with a keyp

(setq search-highlight t                ;; highlight when searching...
      query-replace-highlight t)        ;; ...and replacing
(fset 'yes-or-no-p 'y-or-n-p)           ;; enable y/n answers to yes/no

(setq completion-ignore-case t          ;; ignore case when completing...
      read-file-name-completion-ignore-case t) ;; ...filenames too

(setq initial-scratch-message "# scratch pad #\n\n")
(setq initial-major-mode 'gfm-mode)     ;;  scratch pad in github markdown mode

(setq-default
 frame-title-format
 '(:eval
   (format "%s@%s:%s"
           (or (file-remote-p default-directory 'user) user-login-name)
           (or (file-remote-p default-directory 'host) system-name)
           (file-name-nondirectory (or (buffer-file-name) default-directory)))))

(put 'narrow-to-region 'disabled nil)   ;; enable...
(put 'erase-buffer 'disabled nil)       ;; ... useful things
(file-name-shadow-mode t)               ;; be smart about filenames in mbuf

(setq inhibit-startup-message t         ;; don't show ...
      inhibit-startup-echo-area-message t)   ;; ... startup messages
(setq require-final-newline t)          ;; end files with a newline

(put 'downcase-region 'disabled nil)    ;; Enable C-x C-l and C-x C-u
(put 'upcase-region 'disabled nil)      ;; for down/up-case conversions

(global-unset-key (kbd "C-z"))          ;; needed elsewhere

(setq savehist-file "~/.emacs.d/savehist"  ;; save command history
      history-length 150)

(setq-default save-place t)             ;; remember place in file from last time
(setq save-place-file "~/.emacs.d/saveplace")

(with-eval-after-load 'dired            ;; single buffer for dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

(setq save-interprogram-paste-before-kill t)  ;; save clipboard data in ring

(setq visible-bell 1)                   ;; visual bell on
(setq bell-volume 0)                    ;; no sound bell

;; Disable the Ctrl+Mouse Wheel zoom 
;; Common on Linux/Windows
(global-unset-key [C-mouse-4])
(global-unset-key [C-mouse-5])
;; Common on macOS GUI builds
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))
;; In some builds, these are the actual events:
(global-unset-key (kbd "<C-wheel-up>"))
(global-unset-key (kbd "<C-wheel-down>"))
;; Extra (defensive)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Theme Dark
(load-theme 'wombat)
(set-face-attribute 'region nil :background "#666")
;; Theme Light
;;(load-theme 'adwaita)
;; Theme Common
(set-cursor-color "red")
(setq cursor-type 'box)

;; Spellchecker
(setq-default ispell-program-name "aspell")


;; Keyboard / input method settings
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")       ;; prefer utf-8 for language settings
(set-input-method nil)                   ;; no funky input for normal editing;
(setq read-quoted-char-radix 10)         ;; use decimal, not octal
(setq-default buffer-file-coding-system 'utf-8-unix)  ;; utf-8 & unix EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Backup
(setq-default make-backup-files         nil)  ;; don't want any backup files
(setq-default auto-save-list-file-name  nil)  ;; don't want any .saves files
(setq-default auto-save-default         nil)  ;; don't want any auto saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; File search
(global-set-key (kbd "<f7>") 'find-name-dired)

;; Whitespaces
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; Delete to start of the line
(defun kill-start-of-line ()
  "kill from point to start of line"
  (interactive)
  (kill-line 0))
(global-set-key (kbd "S-C-k") 'kill-start-of-line)

;; Indent the whole buffer
(defun indent-buffer ()
  "Indent the buffer"
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    ))
(global-set-key (kbd "S-C-f") 'indent-buffer)


;; Default tab width
(setq tab-width 4)
(setq-default tab-width 4)

(message (getenv "LANG"))

(defun get-env-from-shell (variable)
  (substring
   (format
	"%s" (last
		  (delete
		   "" (split-string
			   (shell-command-to-string (concat "$SHELL --login -i -c 'echo $" variable "'"))
			   "\n"))))
   1 -1))

(setenv "LANG" (get-env-from-shell "LANG"))

;; OSX has problems with PATH when running Emacs.app
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (get-env-from-shell "PATH"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when (and window-system (eq system-type 'darwin))
  ;; When started from Emacs.app or similar, ensure $PATH
  ;; is the same the user would see in Terminal.app
  (set-exec-path-from-shell-PATH))

;; If switch to buffer that's already open in another frame,
;; don't switch to that window, just open it again in current window
(setq switch-to-buffer-preserve-window-point t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Uniquify buffers
(when (require 'uniquify nil 'noerror)  ;; make buffer names more unique
  (setq
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"
   uniquify-after-kill-buffer-p t       ;; rename after killing uniquified
   uniquify-ignore-buffers-re "^\\*"))  ;; don't muck with special buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Remove ^M
(defun strip-^m ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil nil)
    (replace-match "")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Reload all open files
(defun revert-all-buffers ()
  "Replaces text in all buffers with the text of the visited file on disk."
  (interactive)
  (dolist (buf (buffer-list))
	(with-current-buffer buf
	  (when (and (buffer-file-name) (file-exists-p (buffer-file-name))
				 (not (buffer-modified-p)))
		(revert-buffer t t t) )))
  (message "All open files / buffers reverted."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CUA
(setq cua-rectangle-mark-key (kbd "S-C-<return>"))
(cua-mode t)


;; especially for osx
(if (eq system-type 'darwin)
    (progn
      (setq ns-right-alternate-modifier nil)             ;; unbind right alt
      (setq cua-enable-cua-keys nil)))                   ;; only for rectangles


;; especially for windows
(if (eq system-type 'windows-nt)
    (progn
      (setq cua-auto-tabify-rectangles nil ;; don't tabify after rectangle commands
            cua-keep-region-after-copy t)  ;; standard Windows behaviour
      (transient-mark-mode 1)              ;; no region when it is not highlighted
      (setq cygwin-bin "c:\\cygwin\\bin")  ;; find & Grep on windows
      (setenv "PATH"
              (concat cygwin-bin ";" (getenv "PATH")))
      ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; IDO mode
(require 'ido)
(ido-mode 'both) ; for buffers and files
(setq
 ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
 ido-case-fold  t                 ; be case-insensitive
 ido-enable-last-directory-history t ; remember last used dirs
 ido-max-work-directory-list 30   ; should be enough
 ido-max-work-file-list      50   ; remember many
 ido-use-filename-at-point nil    ; don't use filename at point (annoying)
 ido-use-url-at-point nil         ; don't use url at point (annoying)
 ido-enable-flex-matching t       ; be smart
 ido-max-prospects 16             ; don't spam my minibuffer
 ido-confirm-unique-completion t) ; wait for RET, even with unique completion

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)

;; Increase minibuffer size when ido completion is active
(add-hook 'ido-minibuffer-setup-hook
          (function
           (lambda ()
             (make-local-variable 'resize-minibuffer-window-max-height)
             (setq resize-minibuffer-window-max-height 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; SPEEDBAR
(setq speedbar-default-position 'right)
(setq speedbar-load-hook nil)
(setq speedbar-show-unknown-files t)
(setq speedbar-use-images t)
(setq speedbar-update-flag-disable t)
(setq speedbar-update-flag nil)
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
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; INDENTING
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

(defun toggle-indent-tabs-mode ()
  "Toggle `indent-tabs-mode'."
  (interactive)
  (setq-local indent-tabs-mode (not indent-tabs-mode)))
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
(global-set-key [?\e up] 'move-text-up)              ;; moving lines
(global-set-key [?\e down] 'move-text-down)          ;; in console
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; INITIAL WINDOW SIZE
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
        ;; use 120 char wide window for largeish displays
        ;; and smaller 80 column windows for smaller displays
        ;; pick whatever numbers make sense for you
        (if (> (x-display-pixel-width) 1280)
            (add-to-list 'default-frame-alist (cons 'width 100))
          (add-to-list 'default-frame-alist (cons 'width 80)))
        ;; for the height, take the 60% of the screen height (for panels,
		;; menubars and  whatnot), then divide by the height of a char to
        ;; get the height we want
        (add-to-list 'default-frame-alist
                     (cons 'height (/ (floor (* (x-display-pixel-height) 0.6))
                                      (frame-char-height)))))))
(set-frame-size-according-to-resolution)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; UNSETTING KEYS
(defun get-key-combo (key)
  "Just return the key combo entered by the user"
  (interactive "Key combo: ")
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


;; RANDOM
(defun which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ESHELL
(add-hook 'eshell-first-time-mode-hook
          (lambda ()
            (progn
              (setq eshell-history-size 5000)
              (setq eshell-save-history-on-exit t)
              (setq eshell-where-to-jump 'begin)
              (setq eshell-review-quick-commands nil)
              (setq eshell-smart-space-goes-to-end t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; IMPORTS
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'packages)

;; PRIVATE SETTINGS
(if (file-exists-p "~/.emacs.d/lisp/private.el")
    (require 'private))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Restore original values
(setq file-name-handler-alist bkp-file-name-handler-alist)
(setq bkp-file-name-handler-alist nil)
;; Just keep that bigger memory limit.
;; (run-with-idle-timer
;;  5 nil (lambda ()
;; 		 (setq gc-cons-threshold (* 1024 1024))
;; 		 (message "gc-cons-threshold restored to %S" gc-cons-threshold)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
 '(helm-ag-command-option "--all-text")
 '(helm-ag-insert-at-point 'symbol)
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style 'root)
 '(helm-gtags-prefix-key "\3g")
 '(helm-gtags-pulse-at-cursor t)
 '(helm-gtags-suggested-key-mapping t)
 '(helm-gtags-use-input-at-cursor t)
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-button ((t (:underline "dark gray" :weight bold))))
 '(erc-input-face ((t (:foreground "indian red"))))
 '(erc-my-nick-face ((t (:foreground "indian red" :weight bold))))
 '(erc-nick-default-face ((t (:foreground "dark gray" :weight bold))))
 '(erc-timestamp-face ((t (:foreground "dim gray" :weight normal)))))
