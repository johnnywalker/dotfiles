;;; init-wgrep.el --- Wgrep config. -*- lexical-binding: t -*-

;;; Commentary:

;; Wgrep - edit search results en masse.

;;; Code:

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(provide 'init-wgrep)
;;; init-wgrep.el ends here
