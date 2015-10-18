;; ERC
(setq erc-track-exclude-types '("JOIN" "KICK" "NICK" "PART" "QUIT" "333" "353"))

;; colors
(custom-set-faces
 '(erc-button ((t (:underline "dark gray" :weight bold))))
 '(erc-input-face ((t (:foreground "indian red"))))
 '(erc-my-nick-face ((t (:foreground "indian red" :weight bold))))
 '(erc-nick-default-face ((t (:foreground "dark gray" :weight bold)))))

;; http://www.emacswiki.org/emacs/ErcFilling
(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook
		  '(lambda ()
			 (save-excursion
			   (walk-windows
				(lambda (w)
				  (let ((buffer (window-buffer w)))
					(set-buffer buffer)
					(when (eq major-mode 'erc-mode)
					  (setq erc-fill-column (- (window-width w) 3)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-irc)
