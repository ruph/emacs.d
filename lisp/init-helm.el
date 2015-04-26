;; HELM
(require 'helm-config)
(helm-mode 1)

;; set helm for find files
(global-unset-key (kbd "C-x C-f"))
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; better command executor
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

;; a little different buffer finder
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(setq helm-buffer-max-length 42)

;; display full file path toggle
(define-key helm-map (kbd "C-M-g") 'helm-ff-run-toggle-basename)

;; search kill-ring
(global-set-key (kbd "S-C-v") 'helm-show-kill-ring)
(global-set-key (kbd "s-V") 'helm-show-kill-ring)

;; recursive helm-do-ag
(global-set-key (kbd "S-<f7>")
                (lambda () (interactive)
                  (let ((current-prefix-arg '(4))) ; C-u
                    (call-interactively 'helm-do-ag))))

;; for eshell: complete and history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map
                [remap pcomplete]
                'helm-esh-pcomplete)
              (define-key eshell-mode-map
                (kbd "C-r")
                'helm-eshell-history)))

;; Helm dash documentation
(require 'helm-dash)
(setq helm-dash-docsets-path "~/.docsets")
(setq helm-dash-browser-func 'eww)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-helm)
