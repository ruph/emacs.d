;; PROJECTILE
(require 'projectile)
(add-hook 'emacs-lisp-mode-hook 'projectile-mode)
(add-hook 'php-mode-hook 'projectile-mode)

;; PROJECTILE doing HELM
(require 'helm-projectile)
(helm-projectile-on)

;; PROJECTILE doing AG
(global-set-key (kbd "S-C-<f7>") 'helm-projectile-ag)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; EPROJECT
(require 'eproject)

;; remove pesky M-left/right
(defun prj-setup-all ()
  (progn
    (prj-setkeys)
    (prj-setmenu)
    (prj-settitle)
    (prj-config-print)
    (message ">> EPROJECT setup" )
    (keymap-unset-key [M-left] "eproject-mode")
    (keymap-unset-key [M-right] "eproject-mode")
    (keymap-unset-key [f5] "eproject-mode")
    ))
(global-set-key (kbd "C-S-<f5>") 'eproject-setup-toggle)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; EPROJECT doing HELM
(defun helm-eproject-resource ()
  "Enumerate files belonging to the eproject"
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm
     '((
        (name . "Files in eproject:")
        (init . (lambda ()
                  (with-current-buffer (helm-candidate-buffer 'local)
                    (mapcar
                     (lambda (item)
                       (insert (format "%s/%s\n" (cadr prj-current) (car item))))
                     prj-files))))
        (candidates-in-buffer)
        (mode-line . helm-generic-file-mode-line-string)
        (help-message . helm-generic-file-help-message)
        (type . file)))
     nil "Switch to file: " nil nil)))
(global-set-key (kbd "S-C-t") 'helm-eproject-resource)

;; find files recursively from project dir
(defun helm-eproject-recursive-resources ()
  "Enumerate files belonging to the eproject"
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm
     '(((name . "All files under eproject root dir:")
        (init . (lambda ()
                  (with-current-buffer (helm-candidate-buffer 'local)
                    (insert
                     (shell-command-to-string
                      (format "find %s -type d \\( -name .svn -o -name .git -o -name .hg \\) -prune -o -type f -print" (cadr prj-current))))
                    )))
        (candidates-in-buffer)
        (mode-line . helm-generic-file-mode-line-string)
        (help-message . helm-generic-file-help-message)
        (type . file)))
     nil "Switch to file: " nil nil)))
(global-set-key (kbd "S-C-r") 'helm-eproject-recursive-resources)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-project)
