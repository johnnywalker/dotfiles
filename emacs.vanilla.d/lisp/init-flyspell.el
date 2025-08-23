;;; init-flyspell.el --- Flyspell -*- lexical-binding: t -*-

;;; Commentary:

;; Flyspell - on-the-fly spell checking in Emacs.

;;; Code:

(use-package flyspell
  :ensure nil ; built-in
  :demand t
  :custom
  ;; do not overwrite completion-at-point binding
  (flyspell-use-meta-tab nil)
  :init
  (defun johnny/flyspell-toggle ()
    "Toggle flyspell mode."
    (interactive)
    (if (bound-and-true-p flyspell-mode)
      (flyspell-mode -1)
      (flyspell-mode 1)))
  (johnny/set-leader-keys
    "tS" #'johnny/flyspell-toggle)
  (johnny/declare-prefix
    "tS" "flyspell")
  :hook
  ((prog-mode . flyspell-prog-mode)          ; spellcheck comments in programming modes
   (text-mode . flyspell-mode)))               ; spellcheck text modes

(provide 'init-flyspell)
;;; init-flyspell.el ends here
