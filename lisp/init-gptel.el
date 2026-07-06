;;; -*- lexical-binding: t; -*-

(require 'gptel-openai-oauth)

(defvar ruph/gptel-openai-subscription-model 'gpt-5.5
  "Default model for the ChatGPT/Codex subscription backend.

Example:
  (setq ruph/gptel-openai-subscription-model 'gpt-5.5)")

(defvar ruph/gptel-openai-subscription-reasoning-effort "high"
  "Reasoning effort for the ChatGPT/Codex subscription backend.

Example:
  (setq ruph/gptel-openai-subscription-reasoning-effort \"high\")")

(defvar ruph/gptel-openai-subscription-backend-name "OpenAI-sub"
  "Display name for the ChatGPT/Codex subscription backend.

Example:
  (setq ruph/gptel-openai-subscription-backend-name \"OpenAI-sub\")")

(setq gptel-model ruph/gptel-openai-subscription-model
      gptel-backend
      (gptel-make-openai-oauth
       ruph/gptel-openai-subscription-backend-name
       :request-params
       `(:reasoning (:effort ,ruph/gptel-openai-subscription-reasoning-effort))))

(message "[GPTel] INFO BackendSelected backend={openai-subscription} model={%s} reasoning_effort={%s}"
         gptel-model
         ruph/gptel-openai-subscription-reasoning-effort)

(defun ruph/gptel-login-openai-subscription ()
  "Authenticate gptel against the ChatGPT/Codex subscription backend.

Example:
  (ruph/gptel-login-openai-subscription)"
  (interactive)
  (call-interactively #'gptel-openai-oauth-login))

(require 'gptel-integrations)

(defvar gptel-keymap (make-sparse-keymap))
(define-key global-map (kbd "C-c g") gptel-keymap)
(define-key gptel-keymap (kbd "g") 'gptel)
(define-key gptel-keymap (kbd "l") 'ruph/gptel-login-openai-subscription)
(define-key gptel-keymap (kbd "s") 'gptel-send)
(define-key gptel-keymap (kbd "a") 'gptel-abort)
(define-key gptel-keymap (kbd "m") 'gptel-menu)

(provide 'init-gptel)
