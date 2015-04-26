;; Clojure
(add-to-list 'load-path "~/.emacs.d/el-get/clojure-require")
(require 'clojure-mode)
(setq clojure-enable-paredit t)

(add-hook 'clojure-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)))


;; Clojurescript
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))


;; Paredit
;; Curly & square brackets in paredit
(defun setup-mode-paredit (mode-map)
  (define-key mode-map
    (kbd "DEL") 'paredit-backward-delete)
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
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(add-hook 'cider-repl-mode-hook
          (lambda ()
            (paredit-mode +1)
            (setq cider-repl-use-pretty-printing t)))
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
