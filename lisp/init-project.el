;;; -*- lexical-binding: t; -*-
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

;; PROJECTILE doing MULTI-TERM root
(defun multi-term-projectile-root ()
  (interactive)
  (multi-term)
  (term-send-raw-string (format "cd %s\n" (projectile-project-root))))

;; Find file in project
(global-set-key (kbd "S-C-r") 'helm-projectile-find-file)
(global-set-key (kbd "M-<f5>") 'multi-term-projectile-root)
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


;; EPROJECT doing HELM for opened project
(defun helm-eproject-files ()
  "List files add to the eproject."
  (interactive)
  (let ((files (mapcar (lambda (item)
						 (file-relative-name
						  (expand-file-name (car item) prj-directory)))
					   prj-files)))
	(helm :sources (helm-build-sync-source "Files in eproject:"
					 :candidates files
					 :fuzzy-match t
					 :action helm-projectile-file-actions)
		  :buffer "*eproject files*"
		  :helm-ff-transformer-show-only-basename nil)))

(global-set-key (kbd "S-C-t") 'helm-eproject-files)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-project)
