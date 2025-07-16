;;; -*- lexical-binding: t; -*-
;; Clojure
(require 'clojure-mode)
(setq clojure-enable-paredit t)

;; Clojurescript
(setq inf-clojure-program "lumo")
(setq inf-clojure-project-type "lumo")
(setq inf-clojure-generic-cmd "lumo -d")
(add-hook 'clojurescript-mode-hook #'inf-clojure-minor-mode)
(add-hook 'inf-clojure-mode-hook #'paredit-mode)
(add-hook 'inf-clojure-mode-hook #'eldoc-mode)

;; ...
(add-hook 'clojure-mode-hook
          (lambda ()
            
            (set (make-local-variable 'company-backends)
                 '((company-etags company-dabbrev-code company-yasnippet)))
            ))

;; Paredit - curly & square brackets in paredit
(defun setup-mode-paredit (mode-map)
  (define-key mode-map
    (kbd "{") 'paredit-open-curly)
  (define-key mode-map
    (kbd "}") 'paredit-close-curly)
  (modify-syntax-entry ?\{ "(}")
  (modify-syntax-entry ?\} "){")
  (modify-syntax-entry ?\[ "(]")
  (modify-syntax-entry ?\] ")[")
  (modify-syntax-entry ?~ "'   ")
  (modify-syntax-entry ?, "    ")
  (modify-syntax-entry ?^ "'")
  (modify-syntax-entry ?= "'"))

;; add paredit to inferior-lisp
(defun setup-inferior-lisp-paredit ()
  (setup-mode-paredit inferior-lisp-mode-map))
(add-hook 'inferior-lisp-mode-hook 'setup-inferior-lisp-paredit)

;; and other lisp modes
(mapc (lambda (mode)
        (let ((hook (intern (concat (symbol-name mode) "-mode-hook"))))
          (add-hook hook (lambda () (paredit-mode +1)))))
      '(emacs-lisp lisp inferior-lisp slime slime-repl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Cider
(use-package cider
  :init
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook (lambda () (paredit-mode +1)))
  :config
  (setq
   ;; REPL history file
   cider-repl-history-file "~/.emacs.d/cider-history"
   ;; nice pretty printing
   cider-repl-use-pretty-printing t
   ;; nicer font lock in REPL
   cider-repl-use-clojure-font-lock t
   ;; result prefix for the REPL
   cider-repl-result-prefix ";; => "
   ;; never ending REPL history
   cider-repl-wrap-history t
   ;; looong history
   cider-repl-history-size 3000
   ;; error buffer not popping up
   cider-show-error-buffer nil)
  (cider-repl-toggle-pretty-printing))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Recursively generate etags for all *.clj files,
;; creating tags for def* and namespaces
(defun create-clj-tags (dir-name)
  "Create tags file."
  (interactive "Directory: ")
  (shell-command
   (format "find %s -name \'*.clj*\' | xargs etags --regex=\'/[ \\t\\(]*def[a-z]* \\([a-z-!]+\\)/\\1/\' --regex=\'/[ \\t\\(]*ns \\([a-z.]+\\)/\\1/\' -o %s/TAGS" dir-name dir-name)
   ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'lang-clojure)
