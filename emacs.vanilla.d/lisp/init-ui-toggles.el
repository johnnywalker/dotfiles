;;; init-ui-toggles.el --- UI toggles -*- lexical-binding: t -*-

;;; Commentary:

;; UI toggles

;;; Code:

(defun johnny/toggle-menu-bar ()
  "Toggle the menu bar."
  (interactive)
  (if (bound-and-true-p menu-bar-mode)
      (menu-bar-mode -1)
    (menu-bar-mode 1)))

(johnny/set-leader-keys
  "Tm" #'johnny/toggle-menu-bar)
(johnny/declare-prefix
  "Tm" "menu-bar")

(provide 'init-ui-toggles)
;;; init-ui-toggles.el ends here
