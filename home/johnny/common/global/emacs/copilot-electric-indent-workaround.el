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
