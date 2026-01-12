;;; -*- lexical-binding: t; -*-
;; HELM
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

;; project-aware ripgrep via Helm (prefix C-u to set dir)
(global-set-key (kbd "S-<f7>")
                (lambda () (interactive)
                  (if (and (fboundp 'projectile-project-root)
                           (projectile-project-root))
                      (helm-rg nil (projectile-project-root))
                    (helm-rg))))

;; Search including ignored/hidden files (bypass .gitignore) using ripgrep
(defun ruph/helm-rg-all ()
  "Run helm-rg searching all files, including ignored/hidden ones."
  (interactive)
  (let ((helm-rg-default-extra-args '("-uu" "--hidden")))
    (if (and (fboundp 'projectile-project-root)
             (projectile-project-root))
        (helm-rg nil (projectile-project-root))
      (helm-rg))))

;; Recursive file name search using ripgrep + Helm (includes dotfiles).
(defvar ruph/file-search-debug nil
  "When non-nil, emit file-search debug logs.

Example:
  (setq ruph/file-search-debug t)")

(defun ruph/helm-find-file-recursive (arg)
  "Pick a file recursively under a directory using Helm.

Example:
  (ruph/helm-find-file-recursive nil)"
  (interactive "P")
  (let* ((include-ignored (equal arg '(16)))
         (root
          (file-name-as-directory
           (if arg
               (read-directory-name "Find file under dir: " default-directory nil t)
             default-directory))))
    (if (file-remote-p root)
        (progn
          (when ruph/file-search-debug
            (message "[FileSearch] WARN RemoteDirFallback dir={%s}" root))
          (let ((default-directory root))
            (call-interactively #'helm-find-files)))
      (let ((rg (executable-find "rg")))
        (unless rg
          (user-error "[FileSearch] rg not found in PATH (install ripgrep)"))
        (when ruph/file-search-debug
          (message "[FileSearch] INFO Start dir={%s} include_ignored={%s}" root include-ignored))
        (let* ((start-s (float-time))
               (default-directory root)
               (rg-args (append (list "--files" "--hidden")
                                (when include-ignored (list "-uu"))))
               (lines
                (condition-case err
                    (apply #'process-lines rg rg-args)
                  (error
                   (message "[FileSearch] ERROR ListFailed dir={%s} msg=\"%s\""
                            root (error-message-string err))
                   (user-error "File listing failed for %s" root))))
               (candidates
                (mapcar (lambda (rel)
                          (cons rel (expand-file-name rel root)))
                        lines))
               (elapsed-ms (truncate (* 1000 (- (float-time) start-s)))))
          (when ruph/file-search-debug
            (message "[FileSearch] INFO Listed dir={%s} count={%s} ms={%s}"
                     root (length candidates) elapsed-ms))
          (helm :sources
                (helm-build-sync-source (format "Files: %s" root)
                  :candidates candidates
                  :fuzzy-match t
                  :action
                  '(("Open file" . find-file)
                    ("Open file other window" . find-file-other-window)
                    ("Open file other frame" . find-file-other-frame)
                    ("Open dired" . (lambda (path) (dired (file-name-directory path))))))
                :buffer "*helm file search*"))))))

;; for eshell: complete and history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map
                [remap pcomplete]
                'helm-esh-pcomplete)
              (define-key eshell-mode-map
                (kbd "C-r")
                'helm-eshell-history)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-helm)
