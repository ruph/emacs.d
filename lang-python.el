;; Python Tab Hook
(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode t)
        (setq tab-width 4)
        (setq python-indent 4)
	(local-set-key (kbd "RET") 'newline-and-indent)
        (add-hook 'before-save-hook 'delete-trailing-whitespace)))

;; Initialize Pymacs
;; (add-to-list 'load-path "~/.emacs.d/pinard-Pymacs-cebc80b/")
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)

;; Initialize Rope
;; (add-to-list 'load-path "~/.emacs.d/el-get/ropemacs")
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)

;; (defun ac-ropemacs-require ()
;;   (with-no-warnings
;;     (unless ac-ropemacs-loaded
;;       (pymacs-load "ropemacs" "rope-")
;;       (if (boundp 'ropemacs-enable-autoimport)
;;           (setq ropemacs-enable-autoimport t))
;;       (setq ac-ropemacs-loaded t))))

;; (defun ac-ropemacs-setup ()
;;   (ac-ropemacs-require)
;;   (setq ac-sources (append (list 'ac-source-ropemacs) ac-sources))
;;   (setq ac-omni-completion-sources '(("\\." ac-source-ropemacs))))

;; Pyflakes & PEP8 checks
(add-hook 'python-mode-hook 'flymake-find-file-hook)
; define python checker
(if (eq system-type 'windows-nt)
  (defvar pychecker "pycheckers")
  (defvar pychecker "pyflakespep8.py"))
; use python checker
(when (load "flymake" t)
  (defun flymake-pychecker-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pychecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pychecker-init)))

(add-to-list 'load-path "~/.emacs.d/elpa/flymake-cursor-1.0/")
(load-library "flymake-cursor")
(add-hook 'python-mode-hook
      (lambda ()
	    (local-set-key [S-f5] 'flymake-goto-prev-error)
	    (local-set-key [f5] 'flymake-goto-next-error)))

; PYSMELL
;; (add-to-list 'load-path "~/.emacs.d/elpa/pysmell-0.7.2/")
;; (require 'pysmell)
;; (defvar ac-source-pysmell
;;   '((candidates
;;      . (lambda ()
;;          (require 'pysmell)
;;          (pysmell-get-all-completions))))
;;   "Source for PySmell")
;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;              (set (make-local-variable 'ac-sources) (append ac-sources '(ac-source-pysmell)))))

(provide 'lang-python)
