;;; init-minuet.el --- Minuet (LLM) -*- lexical-binding: t -*-

;;; Commentary:

;; Minuet: Dance with LLM in Your Code

;;; Code:

(use-package minuet
  :ensure t
  :bind
  (("M-\\" . #'minuet-show-suggestion) ;; use overlay for completion
    :map minuet-active-mode-map
    ("M-}" . #'minuet-previous-suggestion)
    ("M-]" . #'minuet-next-suggestion)
    ("M-a" . #'minuet-accept-suggestion-line)
    ("M-A" . #'minuet-accept-suggestion)
    ("M-e" . #'minuet-dismiss-suggestion))
  :config
  (setq minuet-provider 'openai-fim-compatible)
  (plist-put minuet-openai-fim-compatible-options :name "DeepInfra")
  (plist-put minuet-openai-fim-compatible-options :end-point "https://api.deepinfra.com/v1/inference/")
  ;; api-key configured separately
  (plist-put minuet-openai-fim-compatible-options :api-key 'minuet-deepinfra-api-key)
  ;; (plist-put minuet-openai-fim-compatible-options :model "Qwen/Qwen2.5-Coder-32B-Instruct")
  ;; (plist-put minuet-openai-fim-compatible-options :model "Qwen/Qwen3-Next-80B-A3B-Instruct")
  (plist-put minuet-openai-fim-compatible-options :model "Qwen/Qwen3-Coder-480B-A35B-Instruct-Turbo")
  (plist-put minuet-openai-fim-compatible-options :transform '(minuet-deepinfra-fim-transform))


  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64)
  (minuet-set-optional-options minuet-openai-fim-compatible-options :stop ["\n\n"
                                                                            "<|endoftext|>"
                                                                            "<|fim_prefix|>"
                                                                            "<|fim_middle|>"
                                                                            "<|fim_suffix|>"
                                                                            "<|fim_pad|>"
                                                                            "<|repo_name|>"
                                                                            "<|file_sep|>"
                                                                            "<|im_start|>"
                                                                            "<|im_end|>"])

  ;; DeepInfra FIM does not support the `suffix` option in FIM
  ;; completion.  Therefore, we must disable it and manually
  ;; populate the special tokens required for FIM completion.
  (minuet-set-nested-plist minuet-openai-fim-compatible-options nil :template :suffix)

  ;; Custom prompt formatting for Qwen model
  (minuet-set-nested-plist
    minuet-openai-fim-compatible-options
    (defun minuet-deepinfra-fim-qwen-prompt-function (ctx)
      (format "<|fim_prefix|>%s\n%s<|fim_suffix|>%s<|fim_middle|>"
        (plist-get ctx :language-and-tab)
        (plist-get ctx :before-cursor)
        (plist-get ctx :after-cursor)))
    :template
    :prompt)

  ;; Function to transform requests data according to DeepInfra's API format.
  (defun minuet-deepinfra-fim-transform (data)
    ;; DeepInfra requires the endpoint to be formatted as: https://api.deepinfra.com/v1/inference/$MODEL_NAME
    `(:end-point ,(concat (plist-get data :end-point)
                    (--> data
                      (plist-get it :body)
                      (plist-get it :model)))
       ;; No modifications needed for headers.
       :headers ,(plist-get data :headers)
       ;; DeepInfra uses `input` instead of `prompt`, and does not require :model in the request body.
       :body ,(--> data
                (plist-get it :body)
                (plist-put it :input (plist-get it :prompt))
                (map-delete it :model)
                (map-delete it :prompt))))

  ;; Function to extract generated text from DeepInfra's JSON output.
  (plist-put minuet-openai-fim-compatible-options
    :get-text-fn
    (defun minuet--deepinfra-get-text-fn (json)
      ;; DeepInfra's response format is: `json.token.text`
      (--> json
        (plist-get it :token)
        (plist-get it :text)))))

(provide 'init-minuet)
;;; init-minuet.el ends here
