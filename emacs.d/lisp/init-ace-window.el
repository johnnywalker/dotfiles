;;; init-ace-window.el --- ace-window -*- lexical-binding: t -*-

;;; Commentary:

;; ace-window - jump to windows quickly

;;; Code:

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
    aw-scope 'frame)
  (johnny/set-leader-keys
    "W"  #'ace-window
    "wW" #'ace-window))

(provide 'init-ace-window)
;;; init-ace-window.el ends here
