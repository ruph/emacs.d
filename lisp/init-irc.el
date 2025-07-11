;;; -*- lexical-binding: t; -*-
;; ERC
(use-package erc
  :commands erc
  :init
  (setq erc-track-exclude-types '("JOIN" "KICK" "NICK" "PART" "QUIT" "333" "353")
	    erc-insert-away-timestamp-function (quote erc-insert-timestamp-left)
		erc-insert-timestamp-function (quote erc-insert-timestamp-left)
		erc-stamp-mode t
		erc-timestamp-only-if-changed-flag nil
		erc-timestamp-use-align-to t)
  ;; colors
  (custom-set-faces
   '(erc-button ((t (:underline "dark gray" :weight bold))))
   '(erc-input-face ((t (:foreground "indian red"))))
   '(erc-my-nick-face ((t (:foreground "indian red" :weight bold))))
   '(erc-nick-default-face ((t (:foreground "dark gray" :weight bold))))
   '(erc-nick-default-face ((t (:foreground "dark gray" :weight bold))))
   '(erc-timestamp-face ((t (:foreground "dim gray" :weight normal)))))
  :config
  ;; fluid filling works best
  (add-hook 'erc-mode-hook
			(lambda ()
			  (erc-fill-mode nil)
			  (erc-fill-disable)
			  (visual-line-mode t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-irc)
