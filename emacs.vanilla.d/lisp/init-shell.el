;;; init-shell.el --- Shell/terminal-emulation config. -*- lexical-binding: t -*-

;;; Commentary:

;; Shell/terminal-emulation config

;;; Code:

(use-package eshell
  :init
  (defun bedrock/setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . bedrock/setup-eshell)))

;; Eat: Emulate A Terminal
(use-package eat
  :ensure t
  :custom
  (eat-term-name "xterm")
  :init
  ;; TODO might want to use shell-pop or popper instead
  (johnny/set-leader-keys
    "'" #'eat
    "p'" #'eat-project)
  :config
  (eat-eshell-mode)                     ; use Eat to handle term codes in program output
  (eat-eshell-visual-command-mode))     ; commands like less will be handled by Eat

;; (use-package vterm
;;   :ensure t
;;   :custom
;;   (vterm-max-scrollback 10000)          ; increase scrollback buffer size
;;   (vterm-kill-buffer-on-exit t)         ; kill vterm buffer on exit
;;   :config
;;   ;; Set up keybindings for vterm
;;   (define-key vterm-mode-map (kbd "C-c C-j") 'vterm-send-escape)
;;   (define-key vterm-mode-map (kbd "C-c C-k") 'vterm-send-C-k))

(provide 'init-shell)
;;; init-shell.el ends here
