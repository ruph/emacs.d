;;; startup-tests.el --- Startup and responsiveness regression tests -*- lexical-binding: t; -*-
;; Run: emacs -Q --batch -l tests/startup-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'package)
(require 'server)

(defconst ruph/startup-test-root
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(add-to-list 'load-path (expand-file-name "lisp" ruph/startup-test-root))
(require 'init-shell-env)

(defvar ruph/startup-test-initialized nil)
(defvar ruph/startup-test-idle-jobs nil)
(defvar ruph/startup-test-shell-calls 0)
(defvar ruph/startup-test-package-initializations 0)
(defvar ruph/startup-test-initial-features nil)

(defun ruph/startup-test--shell-response (variables)
  "Build a shell protocol response for VARIABLES, an alist."
  (concat "shell startup banner\n\0EMACS_ENV_BEGIN\0"
          (mapconcat (lambda (entry) (concat (car entry) "\0" (or (cdr entry) "") "\0"))
                     variables "")
          "EMACS_ENV_END\0trailing shell message\n"))

(defun ruph/startup-test--initialize ()
  "Load this checkout with isolated state and no maintenance or language servers."
  (unless ruph/startup-test-initialized
    (unless noninteractive (error "Run startup tests in a fresh batch Emacs"))
    (setq package-user-dir (expand-file-name "elpa" ruph/startup-test-root)
          user-emacs-directory (file-name-as-directory (make-temp-file "emacs-startup-test-" t)))
    (load (expand-file-name "early-init.el" ruph/startup-test-root) nil t t)
    ;; Interactive Emacs activates packages before init without initializing
    ;; package management or reading archive metadata.
    (package-activate-all)
    (let ((original-call-process (symbol-function 'call-process))
          (original-package-initialize (symbol-function 'package-initialize))
          (after-init-time nil)
          (window-system 'ns))
      (cl-letf (((symbol-function 'server-running-p) (lambda (&rest _) t))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (seconds repeat function &rest _)
                   (push (list seconds repeat function) ruph/startup-test-idle-jobs)
                   nil))
                ((symbol-function 'package-initialize)
                 (lambda (&rest arguments)
                   (cl-incf ruph/startup-test-package-initializations)
                   (apply original-package-initialize arguments)))
                ((symbol-function 'package-refresh-contents)
                 (lambda (&rest _) (error "Unexpected startup archive refresh")))
                ((symbol-function 'package-install)
                 (lambda (&rest _) (error "Unexpected startup package install")))
                ((symbol-function 'call-process)
                 (lambda (program &rest arguments)
                   (if (member "-c" arguments)
                       (progn
                         (cl-incf ruph/startup-test-shell-calls)
                         (insert (ruph/startup-test--shell-response
                                  (mapcar (lambda (name) (cons name (getenv name)))
                                          ruph/shell-environment-variables)))
                         0)
                     (apply original-call-process program arguments))))
                ((symbol-function 'flyspell-mode) #'ignore))
        (load (expand-file-name "init.el" ruph/startup-test-root) nil t t)
        (run-hooks 'after-init-hook)
        (with-temp-buffer (funcall initial-major-mode))))
    ;; The isolated test must not save histories belonging to a live Emacs.
    (setq kill-emacs-hook nil
          ruph/startup-test-initial-features (copy-sequence features)
          ruph/startup-test-initialized t)))

(ert-deftest startup/early-init-configures-gc-and-toolbar ()
  "Set the startup GC budget and toolbar frame parameter before activation."
  (let ((gc-cons-threshold 800000)
        (default-frame-alist (copy-tree default-frame-alist)))
    (load (expand-file-name "early-init.el" ruph/startup-test-root) nil t t)
    (should (= gc-cons-threshold (* 100 1024 1024)))
    (should (eq (alist-get 'tool-bar-lines default-frame-alist) 0))))

(ert-deftest startup/shell-cache-and-empty-values ()
  "One lookup fetches the three startup values, preserving empty values and spaces."
  (let ((ruph/shell-environment-cache nil) (calls 0))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _)
                 (cl-incf calls)
                 (insert (ruph/startup-test--shell-response
                          '(("LANG" . "en_US.UTF-8") ("PATH" . "/bin:/a path/bin")
                            ("ANDROID_HOME" . ""))))
                 0)))
      (should (equal (get-env-from-shell "LANG") "en_US.UTF-8"))
      (should (equal (get-env-from-shell "PATH") "/bin:/a path/bin"))
      (should (equal (get-env-from-shell "ANDROID_HOME") ""))
      (should (= calls 1))
      (should (equal (get-env-from-shell "PATH") "/bin:/a path/bin"))
      (should (= calls 1)))))

(ert-deftest startup/shell-failure-retains-environment ()
  "A failed shell lookup keeps inherited values and does not retry each getter."
  (let ((ruph/shell-environment-cache nil)
        (process-environment (copy-sequence process-environment))
        (calls 0))
    (setenv "LANG" "test-locale")
    (setenv "PATH" "/bin:/test")
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _) (cl-incf calls) 1)))
      (should (equal (get-env-from-shell "LANG") "test-locale"))
      (should (equal (get-env-from-shell "PATH") "/bin:/test"))
      (should (= calls 1)))))

(ert-deftest startup/shell-rejects-invalid-names ()
  "Reject shell syntax before starting an external process."
  (let ((calls 0))
    (cl-letf (((symbol-function 'call-process) (lambda (&rest _) (cl-incf calls))))
      (should-error (ruph/read-shell-environment '("PATH; exit")))
      (should (= calls 0)))))

(ert-deftest startup/no-idle-maintenance-or-duplicate-initialization ()
  "Installed packages need one shell lookup and no startup maintenance jobs."
  (ruph/startup-test--initialize)
  (should (= ruph/startup-test-shell-calls 1))
  (should (= ruph/startup-test-package-initializations 0))
  (should-not (cl-find 2.0 ruph/startup-test-idle-jobs :key #'car :test #'equal))
  (should-not (cl-find 'ruph/byte-compile-init-files ruph/startup-test-idle-jobs
                       :key #'caddr)))

(ert-deftest startup/deferred-command-entrypoints ()
  "Optional tools stay unloaded while their commands remain discoverable."
  (ruph/startup-test--initialize)
  (dolist (feature '(vterm android-mode circe ein cider lsp-mode lsp-ui js2-mode
                          typescript-mode web-mode php-mode go-mode rust-mode))
    (should-not (memq feature ruph/startup-test-initial-features)))
  (dolist (command '(vterm ruph/vterm-project android-mode circe cider-jack-in
                          ein:notebooklist-open nodejs-repl lsp lsp-ui-mode))
    (should (commandp command)))
  (should (memq 'lsp-ui-mode lsp-mode-hook)))

(ert-deftest startup/manual-completion-only ()
  "Typing has no Corfu auto hook; the configured key still invokes completion."
  (ruph/startup-test--initialize)
  (should-not (default-value 'corfu-auto))
  (with-temp-buffer
    (emacs-lisp-mode)
    (should-not corfu-auto)
    (should-not (memq 'corfu-auto--post-command post-command-hook))
    (insert "alp")
    (setq-local completion-at-point-functions
                (list (lambda () (list (point-min) (point-max) '("alpha" "alpine")))))
    (let (completed)
      (let ((completion-in-region-function
             (lambda (start end collection &optional _)
               (setq completed
                     (list (if (markerp start) (marker-position start) start)
                           (if (markerp end) (marker-position end) end)
                           collection)))))
        (call-interactively (lookup-key global-map (kbd "C-c SPC"))))
      (should (equal completed '(1 4 ("alpha" "alpine")))))))

(ert-deftest startup/colorizes-only-new-output ()
  "Preserve old output and buffer permissions while coloring the appended chunk."
  (ruph/startup-test--initialize)
  (with-temp-buffer
    (insert (propertize "old output\n" 'face 'bold))
    (let ((compilation-filter-start (point-max)))
      (insert "\e[31mnew red output\e[0m\n")
      (setq buffer-read-only t)
      (colorize-buffer)
      (should buffer-read-only)
      (should (eq (get-text-property (point-min) 'face) 'bold))
      (should-not (string-match-p "\e" (buffer-string)))
      (should (cl-some (lambda (overlay) (overlay-get overlay 'face))
                       (overlays-at compilation-filter-start))))))

(ert-deftest startup/split-color-sequences ()
  "Color escapes split across process chunks keep their parsing context."
  (ruph/startup-test--initialize)
  (with-temp-buffer
    (let ((compilation-filter-start (point-min)))
      (insert "\e[3")
      (colorize-buffer))
    (let ((compilation-filter-start (point-max)))
      (insert "1mred\e[0m\n")
      (colorize-buffer))
    (should (equal (buffer-string) "red\n"))
    (should (cl-some (lambda (overlay) (overlay-get overlay 'face))
                     (overlays-at (point-min))))))

(ert-deftest startup/zz-language-autoloads-and-fallback ()
  "File routes load their setup hooks; the vterm fallback preserves C-c t."
  (ruph/startup-test--initialize)
  (let ((global-tree-sitter-mode nil))
    (cl-letf (((symbol-function 'lsp) #'ignore)
              ((symbol-function 'lsp-deferred) #'ignore)
              ((symbol-function 'flycheck-select-checker) #'ignore)
              ((symbol-function 'flyspell-mode) #'ignore))
      (dolist (entry '(("example.js" . js2-mode) ("example.mjs" . js2-mode)
                       ("example.jsonc" . js2-mode) ("example.ts" . typescript-mode)
                       ("example.tsx" . typescript-mode) ("example.html" . web-mode)
                       ("example.jsx" . web-mode) ("example.tpl.php" . web-mode)
                       ("example.php" . php-mode) ("example.go" . go-mode)
                       ("example.rs" . rust-mode) ("example.clj" . clojure-mode)))
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name (car entry) temporary-file-directory))
          (set-auto-mode)
          (should (eq major-mode (cdr entry)))
          (should-not corfu-auto)))))
  (should (featurep 'lang-javascript))
  (should (featurep 'lang-typescript))
  (should (featurep 'init-web-mode))
  (should (featurep 'lang-clojure))
  (autoload-do-load (symbol-function 'ruph/vterm-project) 'ruph/vterm-project)
  (should (featurep 'init-vterm))
  (should (commandp 'ruph/vterm-project))
  (should (eq (lookup-key global-map (kbd "C-c t")) 'ghostel-project)))

;;; startup-tests.el ends here
