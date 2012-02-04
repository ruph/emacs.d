;; Clojure
(add-to-list 'load-path "~/.emacs.d/elpa/clojure-mode-1.11.5/")
(require 'clojure-mode)

(add-hook 'clojure-mode-hook 'flymake-find-file-hook)



(provide 'lang-clojure)
