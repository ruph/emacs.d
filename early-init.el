;;; early-init.el --- Configure startup before package activation -*- lexical-binding: t; -*-

;; Older packages omit a binding cookie.  Keep the notices in *Warnings*
;; without opening that buffer on every Emacs 31 startup.
(require 'warnings)
(add-to-list 'warning-suppress-types '(files missing-lexbind-cookie))

;; Package activation runs before init.el; apply the same GC budget there.
(setq gc-cons-threshold (* 100 1024 1024))

;; Create the first frame without a toolbar instead of removing it afterward.
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))

(message "[Startup] INFO EarlyInit gc_mib=100 toolbar=disabled")

;;; early-init.el ends here
