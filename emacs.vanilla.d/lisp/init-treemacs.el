;;; init-treemacs.el --- treemacs -*- lexical-binding: t -*-

;;; Commentary:

;; treemacs - automatically resize windows to the golden ratio.

;;; Code:

(use-package treemacs
  :ensure t
  :init
  (johnny/set-leader-keys
    "0"  #'treemacs-select-window
    "ft" #'treemacs
    "fT" #'treemacs-find-file)
  :config
  (which-key-add-major-mode-key-based-replacements 'treemacs-mode
    "c"         "treemacs-create"
    "o"         "treemacs-visit-node"
    "oa"        "treemacs-visit-node-ace"
    "t"         "treemacs-toggles"
    "y"         "treemacs-copy"
    "C-c C-p"   "treemacs-projects"
    "C-c C-p c" "treemacs-projects-collapse")
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-git-mode -1))

(use-package treemacs-evil
  :ensure t
  :after (treemacs evil))

(use-package treemacs-icons-dired
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile)
  :init
  (defun johnny/treemacs-project-toggle ()
    "Toggle and add the current project to treemacs if not already added."
    (interactive)
    (if (eq (treemacs-current-visibility) 'visible)
      (delete-window (treemacs-get-local-window))
      (let ((path (projectile-ensure-project (projectile-project-root)))
             (name (projectile-project-name)))
        (unless (treemacs-current-workspace)
          (treemacs--find-workspace))
        (treemacs-do-add-project-to-workspace path name)
        (treemacs-select-window))))
  (johnny/set-leader-keys
    "pt" #'johnny/treemacs-project-toggle)
  (johnny/declare-prefix
    "pt" "treemacs-project-toggle"))

(use-package treemacs-persp
  :ensure t
  :after (treemacs persp-mode))

(provide 'init-treemacs)
;;; init-treemacs.el ends here
