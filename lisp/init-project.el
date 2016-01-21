;; PROJECTILE
(require 'projectile)
(add-hook 'emacs-lisp-mode-hook 'projectile-mode)
(add-hook 'php-mode-hook 'projectile-mode)

;; PROJECTILE doing HELM
(require 'helm-projectile)
(helm-projectile-on)

;; PROJECTILE doing AG
(global-set-key (kbd "S-C-<f7>") 'helm-projectile-ag)
(custom-set-variables
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
 '(helm-ag-command-option "--all-text")
 '(helm-ag-insert-at-point 'symbol))

;; PROJECTILE doing EPROJECT
(setq projectile-project-root-files-bottom-up
      (push "eproject.cfg" projectile-project-root-files-bottom-up))

;; Find file in project
(global-set-key (kbd "S-C-r") 'helm-projectile-find-file)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; EPROJECT
(require 'eproject)

(setq prj-set-compilation-frame t)

;; remove pesky M-left/right
(setq prj-keybindings
      '(([S-C-f5] eproject-setup-toggle  always)
        ([C-f5]   eproject-dired)))

;; advising eproject setup to have no evil
(defun no-evil-in-eproject-setup (orig-fun &rest args)
  (progn
    (apply orig-fun args)
    (turn-off-evil-mode)))
(advice-add 'eproject-setup :around #'no-evil-in-eproject-setup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; EPROJECT doing HELM for opened projects
(defclass helm-source-eproject-files (helm-type-file)
  ((candidates :initform
			   (lambda ()
				 (mapcar (lambda (item)
						   (format "%s/%s" (cadr prj-current) (car item)))
						 prj-files)))))

(defun helm-eproject-files (&optional candidate)
  "List files add to the eproject."
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
	(helm :sources (helm-make-source "Files in eproject:" 'helm-source-eproject-files)
		  :buffer "*eproject files*")))

(global-set-key (kbd "S-C-t") 'helm-eproject-files)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-project)
