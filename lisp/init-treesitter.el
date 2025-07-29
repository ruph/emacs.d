;;; -*- lexical-binding: t; -*-

(require 'tree-sitter)

;; Associate major modes with tree-sitter languages
(setq-default tree-sitter-major-mode-language-alist
              '((typescript-mode . typescript)
                (js2-mode . javascript)
                (rust-mode . rust)
                (python-mode . python)
                (php-mode . php)
                (css-mode . css)
                (go-mode . go)
                (clojure-mode . clojure)
                (yaml-mode . yaml)
                (json-mode . json)
                (html-mode . html)
                (web-mode . html)
                (markdown-mode . markdown)
                (sh-mode . bash)))

;; Define where to find language grammars
(setq tree-sitter-language-source-alist
      '( (bash "https://github.com/tree-sitter/tree-sitter-bash")
         (clojure "https://github.com/sogaiu/tree-sitter-clojure")
         (css "https://github.com/tree-sitter/tree-sitter-css")
         (go "https://github.com/tree-sitter/tree-sitter-go")
         (html "https://github.com/tree-sitter/tree-sitter-html")
         (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
         (json "https://github.com/tree-sitter/tree-sitter-json")
         (php "https://github.com/tree-sitter/tree-sitter-php")
         (python "https://github.com/tree-sitter/tree-sitter-python")
         (rust "https://github.com/tree-sitter/tree-sitter-rust")
         (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript")
         (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx")
         (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(global-tree-sitter-mode 1)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(provide 'init-treesitter)