;;; -*- lexical-binding: t; -*-
;; FLYCHECKING
(require 'flycheck)

(add-hook 'after-init-hook 'global-flycheck-mode)

;; Tooltips
(add-hook 'after-init-hook 'flycheck-pos-tip-mode)

;; Same keybindings as flymake
(define-key flycheck-mode-map [S-f5] 'flycheck-previous-error)
(define-key flycheck-mode-map [f5] 'flycheck-next-error)

;; Override default flycheck triggers
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.8)

(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

;; Dont nag about elisp documentation
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))
(setq flycheck-checkers '(javascript-eslint))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'js-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-flycheck)
