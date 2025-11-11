;;; init-sops.el --- Config for editing files encrypted with sops -*- lexical-binding: t -*-

;;; Commentary:

;; sops config

;;; Code:

(use-package sops
  :ensure t
  :bind (("C-c C-c" . sops-save-file)
         ("C-c C-k" . sops-cancel)
         ("C-c C-d" . sops-edit-file))
  :init
  ;; (setq sops-before-encrypt-decrypt-hook 'sops-setup-env)
  (global-sops-mode 1))

(provide 'init-sops)
;;; init-sops.el ends here
