;;; init-which-key.el --- Which-key config. -*- lexical-binding: t -*-

;;; Commentary:

;; Which-key - display available keybindings in a popup.

;;; Code:

(use-package which-key
  :ensure nil ;; this is built-in, no need to fetch it
  :config
  (setq which-key-idle-delay 0.4
    which-key-min-display-lines 6
    which-key-add-column-padding 1)
  (which-key-add-keymap-based-replacements johnny-default-map
    "a" "Applications"
    "b" "Buffers"
    "e" "Errors"
    "f" "Files"
    "fy" "Yank/Copy"
    "g" "Git/Version control"
    "h" "Help"
    "hd" "Describe"
    "hp" "Profiler"
    "j" "Jump/Join/Split"
    "p" "Projects"
    "q" "Quit"
    "r" "Registers/Rings/Resume"
    "s" "Search/Symbol"
    "t" "Toggles"
    "w" "Windows"
    "D" "Diff/Compare"
    "Db" "Buffers"
    "Dd" "Directories"
    "Df" "Files"
    "Dr" "Regions"
    "Dw" "Windows"
    "F" "Frames"
    "S" "Spelling"
    "T" "UI toggles")
  (which-key-add-keymap-based-replacements global-map
    "C-x RET" "coding system"
    "C-x 8" "unicode"
    "C-x a" "abbrev"
    "C-x n" "narrowing"
    "C-x r" "registers"
    "C-x t" "tabs"
    "C-x w" "windows"
    "C-x x" "buffers")
  (which-key-mode)
  (with-eval-after-load 'diminish
    (diminish 'which-key-mode))) ; hide in modeline

(provide 'init-which-key)
;;; init-which-key.el ends here
