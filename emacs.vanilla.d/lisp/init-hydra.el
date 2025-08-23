;;; init-hydra.el --- Hydra package and config -*- lexical-binding: t -*-

;;; Commentary:

;; Hydra - bindings that stick around

;;; Code:

(use-package hydra
  :ensure t
  :init
  ;; bind keys separately to avoid showing hydra body on `SPC w'
  (johnny/set-leader-keys
    "wu"  #'hydra-window/winner-undo
    "wU"  #'hydra-window/winner-redo)
  :config
  (defhydra hydra-window
    ()
    "winner"
    ("u" winner-undo "Undo")
    ("U" winner-redo "Redo")))

(provide 'init-hydra)
;;; init-hydra.el ends here
