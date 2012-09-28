;; PHP mode
(add-to-list 'load-path "~/.emacs.d/el-get/php-mode")
(require 'php-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Hooks
(add-hook 'php-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (local-set-key (kbd "RET") 'newline-and-indent)
            (local-unset-key (kbd "M-j"))
            (add-hook 'before-save-hook 'delete-trailing-whitespace)
            (setq flymake-gui-warnings-enabled nil)))
(add-hook 'php-mode-hook 'esk-paredit-nonlisp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Flymake for php
(add-to-list 'flymake-allowed-file-name-masks '("\\.php$" flymake-php-init))
(add-hook 'php-mode-hook (lambda () (flymake-mode 1)))

(add-to-list 'load-path "~/.emacs.d/el-get/flymake-cursor")
(load-library "flymake-cursor")
(add-hook 'php-mode-hook
          (lambda ()
            (local-set-key [S-f5] 'flymake-goto-prev-error)
            (local-set-key [f5] 'flymake-goto-next-error)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; HTML
(add-hook 'html-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (local-set-key (kbd "RET") 'newline-and-indent)
            ))
;; some issues with flymake
(delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; MMM Mode
(add-to-list 'load-path "~/.emacs.d/el-get/mmm-mode")
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(set-face-background 'mmm-default-submode-face "gray16")
(setq mmm-submode-decoration-level 2)

;; http://sourceforge.net/tracker/index.php?func=detail&aid=2901780&group_id=8658&atid=108658
(defun mmm-format-string (string arg-pairs)
  "Format STRING by replacing arguments as specified by ARG-PAIRS.
Each element of ARG-PAIRS is \(REGEXP . STR) where each STR is to be
substituted for the corresponding REGEXP wherever it matches."
  (let ((case-fold-search nil))
    (save-match-data
      (dolist (pair arg-pairs)
        (while (string-match (car pair) string)
          (setq string (replace-match (format-mode-line (cdr pair)) t t string))))))
  string)

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; css
(mmm-add-group
 'html-css
 '((embedded-css
    :submode css-mode
    :front "<style[^>]*>"
    :back "</style>"
    )))
(mmm-add-mode-ext-class nil "\\.html?\\'" 'html-css)
(mmm-add-mode-ext-class nil "\\.php\\'" 'html-css)
;; javascript
(mmm-add-group
 'html-js
 '((js-tag
    :submode js-mode
    :delimiter-mode nil
    :front "<script\[^>\]*\\(language=\"javascript\\([0-9.]*\\)\"\\|type=\"text/javascript\"\\)\[^>\]*>"
    :back"</script>"
    :insert ((?j js-tag nil @ "<script language=\"JavaScript\">"
                 @ "\n" _ "\n" @ "</script>" @)))
   (js-inline
    :submode js-mode
    :delimiter-mode nil
    :front "on\\w+=\""
    :back "\"")
   (js-heredoc
    :submode js-mode
    :delimiter-mode nil
    :front "<<<JS"
    :back "JS;")))
(mmm-add-mode-ext-class nil "\\.html?\\'" 'html-js)
(mmm-add-mode-ext-class nil "\\.php\\'" 'html-js)
;; html
(mmm-add-group
 'html-php
 '(
   (html-default
    :submode html-mode
    :front "\\`.\\|?>\\|</script[^>]*>\\|</style[^>]*>" ; flymake kills emacs if only \` is set for start
    :back "\\'\\|<?php\\|<script[^>]*>\\|<style[^>]*>"
    :delimiter-mode nil
    )
   (html-heredoc
    :submode html-mode
    :delimiter-mode nil
    :front "<<<HTML"
    :back "HTML;")
   )
 )
(mmm-add-mode-ext-class nil "\\.php\\'" 'html-php)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'lang-php)
