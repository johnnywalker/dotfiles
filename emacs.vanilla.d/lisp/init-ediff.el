;;; init-ediff.el --- Ediff config -*- lexical-binding: t -*-

;;; Commentary:

;; Ediff

;;; Code:

(use-package ediff
  :ensure nil
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
    ediff-merge-split-window-function 'split-window-horizontally)

  (defun johnny//ediff-buffer-outline-show-all ()
    "Try `outline-show-all' for ediff buffers."
    (when (fboundp 'outline-show-all)
      (outline-show-all)))

  (defvar johnny//ediff-saved-window-configuration nil)

  (defun johnny//ediff-save-window-configuration ()
    (setq johnny//ediff-saved-window-configuration
      (current-window-configuration)))

  (defun johnny//ediff-restore-window-configuration ()
    (when johnny//ediff-saved-window-configuration
      (set-window-configuration johnny//ediff-saved-window-configuration)))

  (defun johnny//ediff-delete-temp-files ()
    "Delete the temp-files associated with the ediff buffers."
    (let ((inhibit-interaction t))
      (dolist (b ediff-session-registry)
        (ignore-errors
          (with-current-buffer b
            (ediff-delete-temp-files))))))

  :config
  ;; show org ediffs unfolded
  (add-hook 'ediff-prepare-buffer-hook 'johnny//ediff-buffer-outline-show-all)
  ;; save window layout before starting...
  (add-hook 'ediff-before-setup-hook #'johnny//ediff-save-window-configuration)
  ;; ... and restore window layout when done
  ;;
  ;; Append to `ediff-quit-hook' so that this runs after `ediff-cleanup-mess'.
  ;; This avoids interfering with ediff's own cleanup, since it depends on the
  ;; ediff control buffer still being current.
  (add-hook 'ediff-quit-hook #'johnny//ediff-restore-window-configuration 50)
  (add-hook 'kill-emacs-hook #'johnny//ediff-delete-temp-files))

(provide 'init-ediff)
;;; init-ediff.el ends here
