;; Clojure
(add-to-list 'load-path "~/.emacs.d/el-get/clojure-require")
(require 'clojure-mode)
(setq clojure-enable-paredit t)


;; Clojurescript
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))


;; Swank-clojure
;; https://github.com/technomancy/swank-clojure
;; lein plugin install swank-clojure 1.4.0
;; M-x clojure-jack-in


;; Slime autocompletion module
(add-to-list 'load-path "~/.emacs.d/el-get/ac-slime")
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)


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

(defun setup-slime-repl-paredit ()
  (setup-mode-paredit slime-repl-mode-map))
(add-hook 'slime-repl-mode-hook 'setup-slime-repl-paredit)

(defun setup-inferior-lisp-paredit ()
  (setup-mode-paredit inferior-lisp-mode-map))
(add-hook 'inferior-lisp-mode-hook 'setup-inferior-lisp-paredit)

;; add paredit to slime and to all other relevant modes
(mapc (lambda (mode)
        (let ((hook (intern (concat (symbol-name mode) "-mode-hook"))))
          (add-hook hook (lambda () (paredit-mode +1)))))
      '(emacs-lisp lisp inferior-lisp slime slime-repl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Recursively generate tags for all *.clj files,
;; creating tags for def* and namespaces
(defun create-clj-tags (dir-name)
  "Create tags file."
  (interactive "Directory: ")
  (shell-command
   (format "find %s -name \'*.clj*\' | xargs etags --regex=\'/[ \\t\\(]*def[a-z]* \\([a-z-!]+\\)/\\1/\' --regex=\'/[ \\t\\(]*ns \\([a-z.]+\\)/\\1/\' -o %s/TAGS" dir-name dir-name)
   ))

(provide 'lang-clojure)
