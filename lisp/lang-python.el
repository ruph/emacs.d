;; Python Tab Hook
(smart-tabs-advice py-indent-line py-indent-offset)
(smart-tabs-advice py-newline-and-indent py-indent-offset)
(smart-tabs-advice py-indent-region py-indent-offset)
(smart-tabs-advice python-indent-line-1 python-indent)
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (setq python-indent 4)
            (local-set-key (kbd "RET") 'newline-and-indent)
            (flycheck-mode -1) ;; flymake currently
            (add-hook 'before-save-hook 'delete-trailing-whitespace)))

;; Pyflakes & PEP8 checks
(add-hook 'python-mode-hook 'flymake-find-file-hook)

;; define python checker
(if (eq system-type 'windows-nt)
    (defvar pychecker "pycheckers")
  (defvar pychecker "pyflakespep8.py"))

;; use python checker
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

(add-to-list 'load-path "~/.emacs.d/el-get/flymake-cursor")
(load-library "flymake-cursor")
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key [S-f5] 'flymake-goto-prev-error)
            (local-set-key [f5] 'flymake-goto-next-error)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'lang-python)
