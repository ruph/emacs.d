;;; -*- lexical-binding: t; -*-

(defvar gptel-openrouter-api-key nil
  "API key for OpenRouter. This should be set in private.el.")

(setq gptel-backend
      (gptel-make-openai "OpenRouter"
        :host "openrouter.ai"
        :endpoint "/api/v1/chat/completions"
        :stream t
        :key gptel-openrouter-api-key
        :models '("google/gemini-flash-1.5"
                  "meta-llama/llama-3-8b-instruct"
                  "mistralai/mistral-7b-instruct-v0.3"
                  "openai/gpt-4o"
                  "openai/gpt-oss-120b")))

(require 'gptel-integrations)

(provide 'init-gptel)
