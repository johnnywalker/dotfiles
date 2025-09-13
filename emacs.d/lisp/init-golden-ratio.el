;;; init-golden-ratio.el --- golden-ratio -*- lexical-binding: t -*-

;;; Commentary:

;; golden-ratio - automatically resize windows to the golden ratio.

;;; Code:

(use-package golden-ratio
  :ensure t
  :init
  (defun johnny/toggle-golden-ratio ()
    "Toggle golden-ratio mode."
    (interactive)
    (if (bound-and-true-p golden-ratio-mode)
        (golden-ratio-mode -1)
      (golden-ratio-mode 1)))
  (johnny/set-leader-keys
    "tg" #'johnny/toggle-golden-ratio)
  (johnny/declare-prefix
    "tg" "golden-ratio")
  :config
  (dolist (m '("bs-mode"
                "calc-mode"
                "ediff-mode"
                "dired-mode"
                "gud-mode"
                "gdb-locals-mode"
                "gdb-registers-mode"
                "gdb-breakpoints-mode"
                "gdb-threads-mode"
                "gdb-frames-mode"
                "gdb-inferior-io-mode"
                "gdb-disassembly-mode"
                "gdb-memory-mode"
                "magit-status-mode"
                "ranger-mode"
                "speedbar-mode"
                "treemacs-mode"))
    (add-to-list 'golden-ratio-exclude-modes m))
  (dolist (n '(" *NeoTree*"
                "*LV*"
                " *transient*"
                " *which-key*"))
    (add-to-list 'golden-ratio-exclude-buffer-names n))

(dolist (f '(ace-window
                 ace-delete-window
                 ace-select-window
                 ace-swap-window
                 ace-maximize-window
                 avy-pop-mark
                 buf-move-left
                 buf-move-right
                 buf-move-up
                 buf-move-down
                 evil-avy-goto-word-or-subword-1
                 evil-avy-goto-line
                 evil-window-delete
                 evil-window-split
                 evil-window-vsplit
                 evil-window-left
                 evil-window-right
                 evil-window-up
                 evil-window-down
                 evil-window-bottom-right
                 evil-window-top-left
                 evil-window-mru
                 evil-window-next
                 evil-window-prev
                 evil-window-new
                 evil-window-vnew
                 evil-window-rotate-upwards
                 evil-window-rotate-downwards
                 evil-window-move-very-top
                 evil-window-move-far-left
                 evil-window-move-far-right
                 evil-window-move-very-bottom
                 next-multiframe-window
                 previous-multiframe-window
                 quit-window
                 windmove-left
                 windmove-right
                 windmove-up
                 windmove-down
                 johnny/alternate-window))
      (add-to-list 'golden-ratio-extra-commands f)))

(provide 'init-golden-ratio)
;;; init-golden-ratio.el ends here
