;; SVN
(require 'psvn)

;; Colors in diff
(use-package ansi-color
  :init
  (add-hook 'diff-mode-hook
			(lambda () (ansi-color-apply-on-region (point-min) (point-max))))
  )

;; Highlight uncommitted changes
(use-package diff-hl
  :init
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  ;; (add-hook 'prog-mode-hook
  ;; 			(lambda () (diff-hl-mode) (diff-hl-flydiff-mode)))
  ;; (add-hook 'vc-dir-mode-hook
  ;; 			(lambda () (diff-hl-mode) (diff-hl-flydiff-mode)))
  ;; (add-hook 'dired-mode-hook
  ;; 			(lambda() (diff-hl-dired-mode)))
  :config
  (setq svn-status-hide-unmodified nil)  ; Hide unmodified by default
  (setq svn-status-ediff-delete-temporary-files t)  ; Cleanup the ~BASE~ files on ediff
  )

(defadvice svn-status-update-modeline
	(after svn-update-diff-hl activate) (diff-hl-update))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-vc)
