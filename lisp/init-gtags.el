;; GTAGS
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'php-mode)
              (ggtags-mode 1))))
(define-key ggtags-mode-map (kbd "C-.") 'ggtags-show-definition)

;; Helm GTAGS
(require 'helm-gtags)

;; Enable helm-gtags-mode
(add-hook 'php-mode-hook 'helm-gtags-mode)

;; customize
(custom-set-variables
 '(helm-gtags-path-style 'root)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t)
 '(helm-gtags-use-input-at-cursor t)
 '(helm-gtags-pulse-at-cursor t)
 '(helm-gtags-prefix-key "\C-cg")
 '(helm-gtags-suggested-key-mapping t)
 )

;; key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "C-:") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-select)
     (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-gtags)
