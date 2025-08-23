;;; init-flycheck.el --- Flycheck -*- lexical-binding: t -*-

;;; Commentary:

;; Flycheck - on-the-fly syntax checking for Emacs.

;;; Code:

(use-package flycheck
  :ensure t
  :init
  (setq
    flycheck-display-errors-delay 0.9)
  ;; flycheck-global-modes '(json-mode json-ts-mode go-mode haskell-mode js2-mode kotlin-mode python-mode
  ;;                          rjsx-mode scala-mode tsx-ts-mode typescript-tsx-mode
  ;;                          typescript-mode-hook typescript-ts-mode web-mode yaml-mode))

  (defun johnny/toggle-flycheck ()
    "Toggle flycheck mode."
    (interactive)
    (if (bound-and-true-p flycheck-mode)
      (flycheck-mode -1)
      (flycheck-mode 1)))

  ;; toggle flycheck window
  (defun johnny/toggle-flycheck-error-list ()
    "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
    (interactive)
    (if-let* ((window (flycheck-get-error-list-window)))
      (save-selected-window (quit-window nil window))
      (flycheck-list-errors)))

  (defun johnny/goto-flycheck-error-list ()
    "Open and go to the error list buffer."
    (interactive)
    (if (flycheck-get-error-list-window)
      (switch-to-buffer flycheck-error-list-buffer)
      (flycheck-list-errors)
      (switch-to-buffer-other-window flycheck-error-list-buffer)))

  (johnny/set-leader-keys
    "eb" #'flycheck-buffer
    "ec" #'flycheck-clear
    "ed" #'flycheck-disable-checker
    "eh" #'flycheck-describe-checker
    "el" #'johnny/toggle-flycheck-error-list
    "eL" #'johnny/goto-flycheck-error-list
    "es" #'flycheck-select-checker
    "eS" #'flycheck-set-checker-executable
    "ev" #'flycheck-verify-setup
    "ey" #'flycheck-copy-errors-as-kill
    "ex" #'flycheck-explain-error-at-point
    "ts" #'johnny/toggle-flycheck)

  (johnny/declare-prefix
    "ts" "flycheck")

  (global-flycheck-mode)
  :config
  ;; add prefix description for which-key
  (which-key-add-keymap-based-replacements flycheck-mode-map "C-c !" "flycheck"))

(use-package consult-flycheck
  :ensure t
  :after (consult flycheck)
  :init
  (johnny/set-leader-keys
    "je" 'consult-flycheck))

(use-package flycheck-eglot
  :ensure t
  :after flycheck
  :config
  (global-flycheck-eglot-mode 1)

  :hook
  ;; disable exclusive behavior when eglot not enabled
  ((rustic-mode yaml-ts-mode) . (lambda ()
                                  (setq flycheck-eglot-exclusive nil))))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
