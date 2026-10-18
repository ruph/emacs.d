;;; early-init.el --- Configure startup before package activation -*- lexical-binding: t; -*-

;; Package activation runs before init.el; apply the same GC budget there.
(setq gc-cons-threshold (* 100 1024 1024))

;; Create the first frame without a toolbar instead of removing it afterward.
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))

(message "[Startup] INFO EarlyInit gc_mib=100 toolbar=disabled")

;;; early-init.el ends here
