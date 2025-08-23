;;; init-copilot.el --- Copilot config -*- lexical-binding: t -*-

;;; Commentary:

;; Copilot

;;; Code:

(use-package copilot
  :ensure (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :init

  (setq
    copilot-max-char 1000000 ; increase from 100000
    copilot-idle-delay nil)  ; disable automatic suggestions

  ;;; electric-indent workaround
  ;; see https://github.com/copilot-emacs/copilot.el/issues/250

  (defun copilot/cancel-on-electric-indent-chars (arg)
    "Cancel copilot completion eagerly when electric-indent-mode is triggered."

    (interactive "p")

    ;; clear the overlay if visible and keypress is in electric-indent-chars. Not really a rejection, so maybe let's not notify it as such?
    (when (and (copilot--overlay-visible)
            (memq last-command-event electric-indent-chars))
      (delete-overlay copilot--overlay)
      (setq copilot--real-posn nil))

    ;; continue on to self-insert command. With the copilot overlay cleared, electric-indent-mode will not be misbehave.
    (self-insert-command arg))

  (defun copilot/override-electric-keys ()
    "Override electric keys for copilot."
    (dolist (char electric-indent-chars)
      (message "disable electric-indent-mode for %s" (char-to-string char))
      (define-key copilot-completion-map (vector char) 'copilot/cancel-on-electric-indent-chars)))

  (add-hook 'typescript-mode-hook 'copilot/override-electric-keys)

  :hook
  ((markdown-mode org-mode prog-mode yaml-mode) . copilot-mode)
  :config
  ;; define backup for when copilot-completion-map isn't activating (not sure why, but it happens in TF)
  (define-key copilot-mode-map (kbd "<f8>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
  (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word)
  (define-key copilot-completion-map (kbd "C-M-TAB") 'copilot-accept-completion-by-line)
  (define-key copilot-completion-map (kbd "C-M-<tab>") 'copilot-accept-completion-by-line)
  ;; use M-} instead of M-[ to avoid conflict with xterm focus in/out bindings
  (define-key copilot-mode-map (kbd "M-}") 'copilot-previous-completion)
  (define-key copilot-mode-map (kbd "M-]") 'copilot-next-completion)
  (define-key copilot-mode-map (kbd "M-\\") 'copilot-complete)

  (add-to-list 'copilot-indentation-alist '(markdown-mode 4))
  (add-to-list 'copilot-indentation-alist '(org-mode 2)))

(provide 'init-copilot)
;;; init-copilot.el ends here
