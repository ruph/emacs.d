;; Clojure
(require 'clojure-mode)
(setq clojure-enable-paredit t)

;; ... and clojurescript
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

;; ...
(add-hook 'clojure-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-:")) ;; needed for swiper
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
(require 'cider)
(add-hook 'cider-repl-mode-hook
          (lambda ()
            (paredit-mode +1)))

;; REPL history file
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; nice pretty printing
(setq cider-repl-use-pretty-printing t)

;; nicer font lock in REPL
(setq cider-repl-use-clojure-font-lock t)

;; result prefix for the REPL
(setq cider-repl-result-prefix ";; => ")

;; never ending REPL history
(setq cider-repl-wrap-history t)

;; looong history
(setq cider-repl-history-size 3000)

;; eldoc for clojure
(add-hook 'cider-mode-hook #'eldoc-mode)

;; error buffer not popping up
(setq cider-show-error-buffer nil)

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
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
