;; MULTI-TERM
(require 'multi-term)
(setq multi-term-program "/bin/bash")
(setq multi-term-switch-after-close nil)
(multi-term-keystroke-setup)
(add-hook 'term-mode-hook
          (lambda ()
            (yas-minor-mode 0)))

;; TERM shortcuts
(defun term-toggle-mode ()
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
	(term-line-mode)))

(defun term-send-kill-line ()
  (interactive) (call-interactively 'kill-line) (term-send-raw-string "\C-k"))
(defun term-send-Cright () (interactive) (term-send-raw-string "\ef"))
(defun term-send-Cleft  () (interactive) (term-send-raw-string "\eb"))
(add-hook 'term-mode-hook
          (lambda ()
            (progn
              (define-key term-raw-map (kbd "C-<right>") 'term-send-Cright)
              (define-key term-raw-map (kbd "C-<left>") 'term-send-Cleft)
              (define-key term-raw-map (kbd "M-<right>") 'term-send-Cright)
              (define-key term-raw-map (kbd "M-<left>") 'term-send-Cleft)
              (define-key term-raw-map (kbd "M-<backspace>") 'term-send-backward-kill-word)
              (define-key term-raw-map (kbd "M-d") 'term-send-forward-kill-word)
              (define-key term-raw-map (kbd "C-k") 'term-send-kill-line)
              (define-key term-raw-map (kbd "C-y") 'term-paste)
              (define-key term-raw-map (kbd "s-v") 'term-paste)
              (define-key term-raw-map (kbd "M-w") 'term-copy-old-input)
              (define-key term-raw-map (kbd "s-c") 'term-copy-old-input)
              (define-key term-raw-map (kbd "C-w") 'term-kill-input)
              (define-key term-raw-map (kbd "s-x") 'term-kill-input)
              (define-key term-raw-map (kbd "C-r") 'term-send-raw)
              (define-key term-raw-map (kbd "C-c C-k") 'term-toggle-mode)
              )))

;; from https://github.com/tavisrudd/emacs.d/blob/master/dss-term.el
(defun term-setup-tramp ()
  "Setup ansi-term/tramp remote directory tracking
   NOTE:  this appears to have some sort of timing bug in it and doesn't always work"
  (interactive)
  (term-send-raw-string
   (concat "
function eterm_set_variables {
    local emacs_host=\"" (car (split-string (system-name) "\\.")) "\"
    local tramp_hostname=${TRAMP_HOSTNAME-$(hostname)}
    if [[ $TERM == \"eterm-color\" ]]; then
        if [[ $tramp_hostname != \"$emacs_host\" ]]; then
            echo -e \"\\033AnSiTu\" ${TRAMP_USERNAME-$(whoami)}
            echo -e \"\\033AnSiTh\" $tramp_hostname
        fi
        echo -e \"\\033AnSiTc\" $(pwd)
    elif [[ $TERM == \"screen\" || $TERM  == \"screen-256color\" ]]; then
        if [[ $tramp_hostname != \"$emacs_host\" ]]; then
            echo -e \"\\033P\\033AnSiTu\\033\\\\\" ${TRAMP_USERNAME-$(whoami)}
            echo -e \"\\033P\\033AnSiTh\\033\\\\\" $tramp_hostname
        fi
        echo -e \"\\033P\\033AnSiTc\\033\\\\\" $(pwd)
    fi
}
function eterm_tramp_init {
    for temp in cd pushd popd; do
        alias $temp=\"eterm_set_cwd $temp\"
    done

    # set hostname, user, and cwd now
    eterm_set_variables
}
function eterm_set_cwd {
    $@
    eterm_set_variables
}
eterm_tramp_init
export -f eterm_tramp_init
export -f eterm_set_variables
export -f eterm_set_cwd
clear
echo \"tramp initialized\"
")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-term)
