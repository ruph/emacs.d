;; SVN
(require 'psvn)

;; GIT ~ http://files.taesoo.org/git-emacs/git-emacs.html
;; ~ slows down emacs
;;(require 'git-emacs)

;; Colors in diff
(defadvice vc-diff-finish (after handle-color-in-diff-output)
  "Run `ansi-color-apply-on-region'."
  (colorize-buffer))

(require 'diff-hl)
(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-vc)