;; ERC
(setq erc-track-exclude-types '("JOIN" "KICK" "NICK" "PART" "QUIT" "333" "353"))

;; colors
(custom-set-faces
 '(erc-button ((t (:underline "dark gray" :weight bold))))
 '(erc-input-face ((t (:foreground "indian red"))))
 '(erc-my-nick-face ((t (:foreground "indian red" :weight bold))))
 '(erc-nick-default-face ((t (:foreground "dark gray" :weight bold)))))

;; fluid filling works best
(add-hook 'erc-mode-hook
          (lambda ()
			(erc-fill-mode nil)
			(erc-fill-disable)
			(visual-line-mode t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-irc)
