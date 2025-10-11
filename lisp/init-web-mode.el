;;; -*- lexical-binding: t; -*-
;; WEB-MODE
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(setq web-mode-content-types-alist '(("jsx" . "\\.[jt]sx?\\'")
                                     ("javascript" . "\\.[cm]?js\\'")))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(add-hook 'web-mode-hook
          '(lambda ()
             (when (equal web-mode-content-type "html")
               (yas-activate-extra-mode 'html-mode))
             (when (equal web-mode-content-type "php")
               (yas-activate-extra-mode 'php-mode))
             (when (equal web-mode-content-type "javascript")
               (yas-activate-extra-mode 'js-mode)
               )
             (when (equal web-mode-content-type "jsx")
               (yas-activate-extra-mode 'html-mode)
               (yas-activate-extra-mode 'js-mode)
               (yas-activate-extra-mode 'jsx-mode)
               )
             ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-web-mode)
