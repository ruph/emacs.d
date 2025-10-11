;;; -*- lexical-binding: t; -*-
;; PROJECTILE
(require 'projectile)
(add-hook 'emacs-lisp-mode-hook 'projectile-mode)
(add-hook 'php-mode-hook 'projectile-mode)

;; PROJECTILE doing HELM
(require 'helm-projectile)
(helm-projectile-on)

;; PROJECTILE + ripgrep
(global-set-key (kbd "S-C-<f7>") 'helm-projectile-rg)

;; PROJECTILE doing EPROJECT
(setq projectile-project-root-files-bottom-up
      (push "eproject.cfg" projectile-project-root-files-bottom-up))

;; Find file in project
(global-set-key (kbd "S-C-r") 'helm-projectile-find-file)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; EPROJECT (lazy)
(setq prj-set-compilation-frame t)
(setq prj-keybindings
      '(([S-C-f5] eproject-setup-toggle  always)
        ([C-f5]   eproject-dired)))

(with-eval-after-load 'eproject
  ;; advising eproject setup to have no evil
  (defun no-evil-in-eproject-setup (orig-fun &rest args)
    (progn
      (apply orig-fun args)
      (turn-off-evil-mode)))
  (advice-add 'eproject-setup :around #'no-evil-in-eproject-setup))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; EPROJECT doing HELM for opened project
(defun helm-eproject-files ()
  "List files add to the eproject."
  (interactive)
  (unless (featurep 'eproject)
    (require 'eproject))
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
