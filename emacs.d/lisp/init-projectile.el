;;; init-projectile.el --- Projectile -*- lexical-binding: t -*-

;;; Commentary:

;; Projectile
;; Some functions adapted from https://github.com/syl20bnr/spacemacs/blob/214de2f3398dd8b7b402ff90802012837b8827a5/core/core-keybindings.el

;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(use-package projectile
  :ensure t
  :init
  (setq projectile-sort-order 'recentf
    projectile-cache-file (concat user-emacs-directory
                            "projectile.cache")
    projectile-known-projects-file (concat user-emacs-directory
                                     "projectile-bookmarks.eld")
    projectile-switch-project-action #'projectile-dired
    projectile-auto-discover t
    projectile-auto-cleanup-known-projects t
    projectile-project-search-path '("~/source/designsforhealth"))


  (defun johnny--projectile-directory-path ()
    "Retrieve the directory path relative to project root.

If the buffer is not visiting a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the
ones created by `magit' and `dired'.

Returns:
  - A string containing the directory path in case of success.
  - `nil' in case the current buffer does not have a directory."
    (when-let* ((directory-name (if-let* ((file-name (buffer-file-name)))
                                  (file-name-directory file-name)
                                  list-buffers-directory)))
      (file-relative-name
        (file-truename directory-name)
        (projectile-project-root))))

  (defun johnny--projectile-file-path ()
    "Retrieve the file path relative to project root.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
    (when-let* ((file-name (buffer-file-name)))
      (file-relative-name (file-truename file-name) (projectile-project-root))))

  (defun johnny--projectile-file-path-with-line ()
    "Retrieve the file path relative to project root, including line number.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
    (when-let* ((file-path (johnny--projectile-file-path)))
      (concat file-path ":" (number-to-string (line-number-at-pos)))))

  (defun johnny/projectile-copy-directory-path ()
    "Copy and show the directory path relative to project root.

If the buffer is not visiting a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the
ones created by `magit' and `dired'."
    (interactive)
    (if-let* ((directory-path (johnny--projectile-directory-path)))
      (progn
        (kill-new directory-path)
        (message "%s" directory-path))
      (message "WARNING: Current buffer does not have a directory!")))

  (defun johnny/projectile-copy-file-path ()
    "Copy and show the file path relative to project root."
    (interactive)
    (if-let* ((file-path (johnny--projectile-file-path)))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
      (message "WARNING: Current buffer is not visiting a file!")))

  (defun johnny/projectile-copy-file-path-with-line ()
    "Copy and show the file path relative to project root, including line number."
    (interactive)
    (if-let* ((file-path (johnny--projectile-file-path-with-line)))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
      (message "WARNING: Current buffer is not visiting a file!")))

  (johnny/set-leader-keys
    ;; File path
    "fyD" 'johnny/projectile-copy-directory-path
    "fyL" 'johnny/projectile-copy-file-path-with-line
    "fyY" 'johnny/projectile-copy-file-path
    ;; Project
    "p!" 'projectile-run-shell-command-in-root
    "p&" 'projectile-run-async-shell-command-in-root
    "p%" 'projectile-replace-regexp
    "pa" 'projectile-toggle-between-implementation-and-test
    "pb" 'projectile-switch-to-buffer
    "pc" 'projectile-compile-project
    "pu" 'projectile-run-project
    "pd" 'projectile-find-dir
    "pD" 'projectile-dired
    "pe" 'projectile-edit-dir-locals
    "pf" 'projectile-find-file
    "pF" 'projectile-find-file-dwim
    "pE" 'projectile-find-references
    "pg" 'projectile-find-tag
    "pG" 'projectile-regenerate-tags
    "pi" 'projectile-install-project
    "pI" 'projectile-invalidate-cache
    "pk" 'projectile-kill-buffers
    "pp" 'projectile-switch-project
    "pr" 'projectile-recentf
    "pR" 'projectile-replace
    "pT" 'projectile-test-project
    "pv" 'projectile-vc)

  (johnny/declare-prefix
    "fyD" "projectile-copy-directory-path"
    "fyL" "projectile-copy-file-path-with-line"
    "fyY" "projectile-copy-file-path")

  :config
  (which-key-add-keymap-based-replacements projectile-mode-map
    "C-x p" "projectile")
  ;; TODO remove if not desired
  (with-eval-after-load 'diminish (diminish 'projectile-mode)) ; hide in modeline
  (projectile-mode))

(provide 'init-projectile)
;;; init-projectile.el ends here
