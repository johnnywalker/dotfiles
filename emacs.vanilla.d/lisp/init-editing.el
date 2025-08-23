;;; init-editing.el --- Editing enhancements -*- lexical-binding: t -*-

;;; Commentary:

;; Editing enhancements

;;; Code:

(use-package emacs
  :init
  ;; bump of the undo limits to avoid issues with premature
  ;; Emacs GC which truncates the undo history very aggressively
  (setq-default
    undo-limit 80000000
    undo-strong-limit 120000000
    undo-outer-limit 360000000)

  (defun johnny/indent-region-or-buffer ()
    "Indent a region if selected, otherwise the whole buffer."
    (interactive)
    (save-excursion
      (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
        (evil-indent (point-min) (point-max))
        (message "Indented buffer."))
      (whitespace-cleanup)))

  (defun johnny/toggle-display-fill-column-indicator ()
    "Toggle display-fill-column-indicator mode."
    (interactive)
    (if (bound-and-true-p display-fill-column-indicator-mode)
      (display-fill-column-indicator-mode -1)
      (display-fill-column-indicator-mode 1)))

  (johnny/set-leader-keys
    "j=" #'johnny/indent-region-or-buffer
    "tf" #'johnny/toggle-display-fill-column-indicator)
  (johnny/declare-prefix
    "j=" "indent-region-or-buffer"
    "tf" "display-fill-column-indicator")

  (electric-indent-mode))

;; Motion aid: jump to visible text using a char-based decision tree
(use-package avy
  :ensure t
  :demand t
  :bind (("C-c j" . avy-goto-line)
          ("s-j"   . avy-goto-char-timer))
  :init
  (johnny/set-leader-keys
    "jb" #'avy-pop-mark
    "jj" #'avy-goto-char-timer
    "jl" #'avy-goto-line))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode t)
  (with-eval-after-load 'diminish
    (diminish 'editorconfig-mode))) ; hide in modeline

;;; Disabled - guides do not work that well, and I gave up on styling
;; (use-package highlight-indent-guides
;;   :ensure t
;;   :init
;;   (setq highlight-indent-guides-method 'bitmap
;;     highlight-indent-guides-responsive 'top)
;;   (defun johnny/toggle-indent-guides ()
;;     "Toggle highlight-indent-guides mode."
;;     (interactive)
;;     (if (bound-and-true-p highlight-indent-guides-mode)
;;       (highlight-indent-guides-mode -1)
;;       (highlight-indent-guides-mode 1)))
;;   (johnny/set-leader-keys
;;     "ti" 'johnny/toggle-indent-guides)
;;   :config
;;   (set-face-background 'highlight-indent-guides-odd-face "darkgray")
;;   (set-face-background 'highlight-indent-guides-even-face "dimgray")
;;   (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
;;   (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package hl-todo
    :ensure t
    :config
    ;; global hook activates hl-todo-mode for prog-mode, text-mode
    ;; mode can be explicitly defined using hl-todo-activate-in-modes variable
    (global-hl-todo-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; NOTE: might not need this - let's use electric-pair-mode for now
;; (use-package smartparens
;;   :ensure smartparens
;;   :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
;;   :config
;;   ;; load default config
;;   (require 'smartparens-config))

(use-package undo-fu
  :ensure t
  :config
  (setq undo-fu-allow-undo-in-region t))

(use-package undo-fu-session
  :ensure t
  :after undo-fu
  :config
  (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")
    undo-fu-session-directory (concat user-emacs-directory "undo")
    undo-fu-session-compression 'zst)
  (undo-fu-session-global-mode))

;;; Support for reopening killed buffers

(defvar johnny--killed-buffer-list nil
  "List of recently killed buffers.")

(defun johnny//add-buffer-to-killed-list ()
  "Record filenames of killed buffers.

This is a `kill-buffer-hook'.  If the buffer is visiting a file,
 add that file to `johnny--killed-buffer-list'."
  (when buffer-file-name
    (push buffer-file-name johnny--killed-buffer-list)))

(defun johnny/reopen-killed-buffer ()
  "Reopen the most recently killed file buffer, if one exists."
  (interactive)
  (when johnny--killed-buffer-list
    (find-file (pop johnny--killed-buffer-list))))

;; Add buffer reference to internal list of killed buffers on `kill-buffer',
;; used for restoring recently killed buffers.
(add-hook 'kill-buffer-hook #'johnny//add-buffer-to-killed-list)

;;; Functions for copying and pasting whole buffers
(defun johnny/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun johnny/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer."
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(provide 'init-editing)
;;; init-editing.el ends here
