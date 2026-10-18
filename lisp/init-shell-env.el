;;; init-shell-env.el --- Cached shell environment -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar ruph/shell-environment-cache nil
  "Cached login-shell values, cleared with (setq ruph/shell-environment-cache nil).")

(defconst ruph/shell-environment-variables '("LANG" "PATH" "ANDROID_HOME")
  "Environment values fetched together during startup.")

(defun ruph/read-shell-environment (variables)
  "Read VARIABLES from one interactive login shell and cache their values.

Example:
  (ruph/read-shell-environment (list \"LANG\" \"PATH\"))"
  (dolist (variable variables)
    (unless (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*\\'" variable)
      (error "Invalid shell environment variable: %s" variable)))
  (let* ((shell (or (getenv "SHELL") shell-file-name))
         (start-marker "\0EMACS_ENV_BEGIN\0")
         (end-marker "EMACS_ENV_END\0")
         (script
          (concat
           "printf '\\000EMACS_ENV_BEGIN\\000'; "
           (mapconcat
            (lambda (variable)
              (format "printf '%%s\\000%%s\\000' %s \"${%s-}\""
                      (shell-quote-argument variable) variable))
            variables "; ")
           "; printf 'EMACS_ENV_END\\000'"))
         (started (float-time))
         values)
    (message "[Startup] INFO ShellEnvironmentStart variables=%s"
             (mapconcat #'identity variables ","))
    (condition-case err
        (with-temp-buffer
          (let* ((default-directory temporary-file-directory)
                 (status (call-process shell nil t nil "--login" "-i" "-c" script)))
            (unless (eq status 0)
              (error "Shell exited with status %s" status)))
          (goto-char (point-min))
          (unless (search-forward start-marker nil t)
            (error "Shell environment start marker missing"))
          (let ((start (point)))
            (unless (search-forward end-marker nil t)
              (error "Shell environment end marker missing"))
            (let ((fields (split-string
                           (buffer-substring-no-properties
                            start (- (point) (length end-marker)))
                           "\0")))
              ;; Each pair has a trailing NUL; retain empty variable values.
              (unless (and (= (length fields) (1+ (* 2 (length variables))))
                           (equal (car (last fields)) ""))
                (error "Malformed shell environment response"))
              (dolist (variable variables)
                (unless (equal variable (pop fields))
                  (error "Unexpected shell environment field"))
                (push (cons variable (pop fields)) values))))
          (message "[Startup] INFO ShellEnvironmentLoaded variables=%d elapsed_ms=%d"
                   (length variables) (* 1000 (- (float-time) started))))
      (error
       (message "[Startup] WARN ShellEnvironmentFallback reason=%s"
                (error-message-string err))
       (setq values (mapcar (lambda (variable) (cons variable (getenv variable)))
                            variables))))
    (setq ruph/shell-environment-cache
          (append (nreverse values)
                  (cl-remove-if (lambda (entry) (member (car entry) variables))
                                ruph/shell-environment-cache)))))

(defun get-env-from-shell (variable)
  "Return cached login-shell VARIABLE, fetching related values together.

Example:
  (get-env-from-shell \"PATH\")"
  (unless (assoc variable ruph/shell-environment-cache)
    (ruph/read-shell-environment
     (delete-dups (cons variable (copy-sequence ruph/shell-environment-variables)))))
  (cdr (assoc variable ruph/shell-environment-cache)))

(defun set-exec-path-from-shell-PATH ()
  "Set PATH and `exec-path' from the cached login-shell environment.

Example:
  (set-exec-path-from-shell-PATH)"
  (let ((path (get-env-from-shell "PATH")))
    (when (and path (not (string-empty-p path)))
      (setenv "PATH" path)
      (setq exec-path (split-string path path-separator t)))))

(provide 'init-shell-env)
;;; init-shell-env.el ends here
