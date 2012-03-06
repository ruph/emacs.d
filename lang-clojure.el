;; Clojure
(add-to-list 'load-path "~/.emacs.d/elpa/clojure-mode-1.11.5/")
(require 'clojure-mode)
(setq clojure-enable-paredit t)


;; clojurescript
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))


;; swank-clojure
;; https://github.com/technomancy/swank-clojure
;; lein plugin install swank-clojure 1.4.0
;; M-x clojure-jack-in


;; slime autocompletion module
(add-to-list 'load-path "~/.emacs.d/elpa/ac-slime-0.1/")
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)


;; yasnippet
;; https://github.com/swannodette/clojure-snippets
;; git clone https://github.com/swannodette/clojure-snippets.git clojure-mode
;; ..in snippets dir preferably


;; paredit slime settings
(defun setup-slime-repl-paredit ()
  (define-key slime-repl-mode-map
    (kbd "DEL") 'paredit-backward-delete)
  (define-key slime-repl-mode-map
    (kbd "{") 'paredit-open-curly)
  (define-key slime-repl-mode-map
    (kbd "}") 'paredit-close-curly)
  (modify-syntax-entry ?\{ "(}")
  (modify-syntax-entry ?\} "){")
  (modify-syntax-entry ?\[ "(]")
  (modify-syntax-entry ?\] ")[")
  (modify-syntax-entry ?~ "'   ")
  (modify-syntax-entry ?, "    ")
  (modify-syntax-entry ?^ "'")
  (modify-syntax-entry ?= "'"))
(add-hook 'slime-repl-mode-hook 'setup-slime-repl-paredit)

;; add paredit to slime and to all other relevant modes
(mapc (lambda (mode)
	(let ((hook (intern (concat (symbol-name mode) "-mode-hook"))))
	  (add-hook hook (lambda () (paredit-mode +1)))))
      '(emacs-lisp lisp inferior-lisp slime slime-repl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'lang-clojure)
