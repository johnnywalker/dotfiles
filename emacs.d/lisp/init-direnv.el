;;; init-direnv.el --- direnv -*- lexical-binding: t -*-

;;; Commentary:

;; direnv - load environment variables from .envrc files.

;;; Code:

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(provide 'init-direnv)
;;; init-direnv.el ends here
