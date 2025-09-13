;;; init-keybindings.el --- Keybindings and related functions -*- lexical-binding: t -*-

;;; Commentary:

;; Keybindings and related functions
;; Adapted from https://github.com/syl20bnr/spacemacs/blob/214de2f3398dd8b7b402ff90802012837b8827a5/core/core-keybindings.el

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

(defvar johnny-leader-key "SPC"
  "The leader key used for keybindings. Default is 'SPC'.")
(defvar johnny-emacs-leader-key "M-m"
  "The leader key used in Emacs state. Default is 'M-m'.")
(defvar johnny-major-mode-leader-key ","
  "The major mode leader key, which is a shortcut key equivalent to pressing '<leader> m'. Default
  is ','.")
(defvar johnny-major-mode-emacs-leader-key "<M-return>"
  "The major mode leader key accessible in Emacs state. Default is '<M-return>'.")

(defvar johnny-default-map (make-sparse-keymap)
  "Base keymap for all leader key commands.")

(defun johnny/declare-prefix (prefix name &rest more)
  "Declare a prefix PREFIX. PREFIX is a string describing a key
sequence. NAME is a string used as the prefix command."
  (declare (indent defun))
  (with-eval-after-load 'which-key
    (apply #'which-key-add-keymap-based-replacements johnny-default-map
      prefix name more)))

(defun johnny/declare-prefix-for-mode (mode prefix name &optional _)
  "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
be added. PREFIX is a string describing a key sequence. NAME is a symbol name
used as the prefix command."
  (let* ((is-major-mode-prefix (string-prefix-p "m" prefix))
          (is-minor-mode-prefix (not is-major-mode-prefix))
          (smap (intern (format "johnny-%s-map" mode))))
    (when (johnny//init-leader-mode-map mode smap is-minor-mode-prefix)
      (which-key-add-keymap-based-replacements (symbol-value smap)
        (if is-major-mode-prefix (substring prefix 1) prefix) name))))

(defun johnny/set-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key bindings under
`johnny-leader-key' and `johnny-emacs-leader-key'.
KEY should be a string suitable for passing to `kbd', and it
should not include the leaders. DEF is most likely a quoted
command. See `define-key' for more information about the possible
choices for DEF. This function simply uses `define-key' to add
the bindings.

For convenience, this function will accept additional KEY DEF
pairs. For example,

\(johnny/set-leader-keys
   \"a\" 'command1
   \"C-c\" 'command2
   \"bb\" 'command3\)"
  (while key
    (define-key johnny-default-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))

(defun johnny//acceptable-leader-p (key)
  "Return t if key is a string and non-empty."
  (and (stringp key) (not (string= key ""))))

(defun johnny//init-leader-mode-map (mode map &optional minor)
  "Check for MAP-prefix. If it doesn't exist yet, use `bind-map'
to create it and bind it to `johnny-major-mode-leader-key'
and `johnny-major-mode-emacs-leader-key'. If MODE is a
minor-mode, the third argument should be non nil."
  (let* ((prefix (intern (format "%s-prefix" map)))
          (leader1 (when (johnny//acceptable-leader-p
                           johnny-major-mode-leader-key)
                     johnny-major-mode-leader-key))
          (leader2 (when (johnny//acceptable-leader-p
                           johnny-leader-key)
                     (concat johnny-leader-key " m")))
          (emacs-leader1 (when (johnny//acceptable-leader-p
                                 johnny-major-mode-emacs-leader-key)
                           johnny-major-mode-emacs-leader-key))
          (emacs-leader2 (when (johnny//acceptable-leader-p
                                 johnny-emacs-leader-key)
                           (concat johnny-emacs-leader-key " m")))
          (leaders (delq nil (list leader1 leader2)))
          (emacs-leaders (delq nil (list emacs-leader1 emacs-leader2))))
    (or (boundp prefix)
      (progn
        (eval
          `(bind-map ,map
             :prefix-cmd ,prefix
             ,(if minor :minor-modes :major-modes) (,mode)
             :keys ,emacs-leaders
             :evil-keys ,leaders
             :evil-states (normal motion visual)))
        (boundp prefix)))))

(defun johnny/inherit-leader-keys-from-parent-mode (mode &optional parent-mode)
  "Make derived mode MODE inherit leader key bindings from PARENT-MODE.
If omitted, PARENT-MODE defaults to the parent mode of MODE.
Signal an error if MODE is not a derived mode (for example if the
package defining the mode has not yet been loaded)."
  (unless parent-mode
    (setq parent-mode (or (get mode 'derived-mode-parent)
                        (error "Mode %s has no parent" mode))))
  (let ((map (intern (format "johnny-%s-map" mode)))
         (parent-map (intern (format "johnny-%s-map" parent-mode))))
    (when (and (johnny//init-leader-mode-map mode map)
            (johnny//init-leader-mode-map parent-mode parent-map))
      (set-keymap-parent (symbol-value map) (symbol-value parent-map)))))

(defun johnny/set-leader-keys-for-major-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under
`johnny-major-mode-leader-key' and
`johnny-major-mode-emacs-leader-key' for the major-mode
MODE. MODE should be a quoted symbol corresponding to a valid
major mode. The rest of the arguments are treated exactly like
they are in `johnny/set-leader-keys'."
  (let* ((map (intern (format "johnny-%s-map" mode))))
    (when (johnny//init-leader-mode-map mode map)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings) def (pop bindings))))))

(defun johnny/set-leader-keys-for-minor-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under
`johnny-major-mode-leader-key' and
`johnny-major-mode-emacs-leader-key' for the minor-mode
MODE. MODE should be a quoted symbol corresponding to a valid
minor mode. The rest of the arguments are treated exactly like
they are in `johnny/set-leader-keys'. If DEF is string, then
it is treated as a prefix not a command."
  (let* ((map (intern (format "johnny-%s-map" mode))))
    (when (johnny//init-leader-mode-map mode map t)
      (let ((map-value (symbol-value map)))
        (while key
          (if (stringp def)
            (which-key-add-keymap-based-replacements map-value key def)
            (define-key map-value (kbd key) def))
          (setq key (pop bindings) def (pop bindings)))))))

(defun johnny/set-leader-keys-for-minor-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under
`johnny-major-mode-leader-key' and
`johnny-major-mode-emacs-leader-key' for the minor-mode
MODE. MODE should be a quoted symbol corresponding to a valid
minor mode. The rest of the arguments are treated exactly like
they are in `johnny/set-leader-keys'. If DEF is string, then
it is treated as a prefix not a command."
  (let* ((map (intern (format "johnny-%s-map" mode))))
    (when (johnny//init-leader-mode-map mode map t)
      (let ((map-value (symbol-value map)))
        (while key
          (if (stringp def)
            (which-key-add-keymap-based-replacements map-value key def)
            (define-key map-value (kbd key) def))
          (setq key (pop bindings) def (pop bindings)))))))

(defun johnny/declare-prefix-for-minor-mode (mode prefix name)
  "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
be added. PREFIX is a string describing a key sequence. NAME is a symbol name
used as the prefix command.

Example:
  \(johnny/declare-prefix-for-minor-mode 'tide-mode \"E\" \"errors\"\)"

  (let* ((map (intern (format "johnny-%s-map" mode))))
    (when (johnny//init-leader-mode-map mode map t)
      (which-key-add-keymap-based-replacements (symbol-value map) prefix name))))

(defun johnny/alternate-window ()
  "Switch back and forth between current and last window in the
current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
         (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found."))
    (select-window prev-window)))

(johnny/set-leader-keys
  "ap" #'list-processes

  "bd" #'kill-current-buffer
  "bK" #'kill-buffer
  "bn" #'next-buffer
  "bp" #'previous-buffer
  "bR" #'revert-buffer
  "bu" #'johnny/reopen-killed-buffer
  "bx" #'kill-buffer-and-window
  "bP" #'johnny/copy-clipboard-to-whole-buffer
  "bY" #'johnny/copy-whole-buffer-to-clipboard

  "ff" #'find-file
  "fF" #'find-file-other-window
  "fr" #'recentf-open-files
  "fs" #'save-buffer
  "fS" #'save-some-buffers
  "fD" #'delete-file
  "fR" #'rename-file

  "hdT" #'describe-theme
  "hda" #'apropos-command
  "hdb" #'describe-bindings
  "hdB" #'describe-mode-local-bindings
  "hdc" #'describe-char
  "hdf" #'describe-function
  "hdk" #'describe-key
  "hdK" #'describe-keymap
  "hdm" #'describe-mode
  "hdp" #'describe-package
  "hdt" #'describe-text-properties
  "hdv" #'describe-variable
  "hdF" #'describe-face
  "hPk"	'profiler-stop
  "hPr"	'profiler-report
  "hPs"	'profiler-start
  "hPw"	'profiler-report-write-profile

  "jd" #'dired-jump

  ; TODO update if needed to save layout/window configuration
  "qq" #'save-buffers-kill-terminal
  "qr" #'restart-emacs

  "re" #'evil-show-registers

  "w TAB" #'johnny/alternate-window
  "w-"  #'split-window-below
  "w/"  #'split-window-right
  "w="  #'balance-windows-area
  "wd"  #'delete-window
  "wF"  #'make-frame
  "wH"  #'evil-window-move-far-left
  "wh"  #'evil-window-left
  "wJ"  #'evil-window-move-very-bottom
  "wj"  #'evil-window-down
  "wK"  #'evil-window-move-very-top
  "wk"  #'evil-window-up
  "wL"  #'evil-window-move-far-right
  "wl"  #'evil-window-right
  "wo"  #'other-frame
  "ws"  #'split-window-below
  "wv"  #'split-window-right
  "ww"  #'other-window
  "wx"  #'kill-buffer-and-window

  "Db3" #'ediff-buffers3
  "DbB" #'ediff-backup
  "Dbb" #'ediff-buffers
  "Dbp" #'ediff-patch-buffer
  "Dd3" #'ediff-directories3
  "Ddd" #'ediff-directories
  "Ddr" #'ediff-directory-revisions
  "Df3" #'ediff-files3
  "Dff" #'ediff-files
  "Dfp" #'ediff-patch-file
  "Dfv" #'ediff-revision
  "Drl" #'ediff-regions-linewise
  "Drw" #'ediff-regions-wordwise
  "Dwl" #'ediff-windows-linewise
  "Dww" #'ediff-windows-wordwise

  "Fd" #'delete-frame
  "Fn" #'make-frame
  "Fo" #'other-frame)

(johnny/declare-prefix
  "bu" "reopen-killed-buffer"
  "bP" "copy-clipboard-to-whole-buffer"
  "bY" "copy-whole-buffer-to-clipboard")

(provide 'init-keybindings)
;;; init-keybindings.el ends here
