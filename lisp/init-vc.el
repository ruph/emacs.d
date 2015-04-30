;; SVN
(require 'psvn)

;; Colors in diff
(defadvice vc-diff-finish (after handle-color-in-diff-output)
  "Run `ansi-color-apply-on-region'." 
  (progn
    (require 'ansi-color)
    (toggle-read-only)
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only)))

(require 'diff-hl)
(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-vc)
