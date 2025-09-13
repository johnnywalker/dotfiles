;;; init-magit.el ---  -*- lexical-binding: t -*-

;;; Code:

(use-package transient
  :ensure t)

(use-package git-link
  :ensure t
  :init
  (require 'git-link-transient)
  (johnny/declare-prefix "gl" "links")
  (johnny/set-leader-keys
    "glc" 'git-link-commit
    "gll" 'git-link
    "glm" 'git-link-dispatch
    )

  ;; default is to open the generated link
  (setq git-link-open-in-browser t))

(use-package git-timemachine
  :ensure t
  :init
  (johnny/set-leader-keys
    "gt" 'git-timemachine-toggle))

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  ;; :bind (("C-x g" . magit-status)
  ;;         ("C-c g" . magit-dispatch)
  ;;         ("C-c f" . magit-file-dispatch))
  :init
  (johnny/declare-prefix "gf" "file")
  (johnny/set-leader-keys
    "gb"  'magit-blame
    "gc"  'magit-clone
    "gfF" 'magit-find-file
    "gfl" 'magit-log-buffer-file
    "gfd" 'magit-diff
    "gfm" 'magit-file-dispatch
    "gi"  'magit-init
    "gL"  'magit-list-repositories
    "gm"  'magit-dispatch
    "gs"  'magit-status
    "gS"  'magit-stage-files
    "gU"  'magit-unstage-files)
  (setq
    ;; avoid auto-save - becomes very disruptive with magit-wip-mode
    magit-save-repository-buffers nil
    ;; full screen magit status
    magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :custom
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-diff-refine-hunk t)
  :config
  (magit-wip-mode 1)
  (add-hook 'git-commit-mode-hook 'display-fill-column-indicator-mode)
  ;; disable evil-surround-mode in magit status (conflicts with `s' binding)
  (add-hook 'magit-status-mode-hook (lambda () (evil-surround-mode -1)))
  (with-eval-after-load 'diminish
    (diminish 'magit-wip-mode))) ; hide in modeline

(use-package forge
  :ensure t
  :after magit
  :init
  (johnny/set-leader-keys-for-major-mode 'forge-topic-mode
    "a" 'forge-topic-set-assignees
    "c" 'forge-create-post
    "C" 'forge-checkout-pullreq
    "b" 'forge-browse-topic
    "D" 'forge-delete-comment
    "d" 'forge-post-toggle-draft
    "e" 'forge-edit-post
    "m" 'forge-topic-set-marks
    "M" 'forge-create-mark
    "n" 'forge-edit-topic-note
    "r" 'forge-topic-set-review-requests
    "s" 'forge-topic-state-menu
    "t" 'forge-topic-set-title
    "u" 'forge-copy-url-at-point-as-kill)
  (dolist (mode '(forge-issue-mode forge-pullreq-mode))
    (johnny/inherit-leader-keys-from-parent-mode mode 'forge-topic-mode))
  (johnny/set-leader-keys-for-major-mode 'forge-post-mode
    johnny-major-mode-leader-key 'forge-post-submit
    "c" 'forge-post-submit
    "k" 'forge-post-cancel
    "a" 'forge-post-cancel))

;; TODO try forge-llm

;; NOTE: too slow, especially with large diffs
;; (use-package magit-delta
;;   :ensure t
;;   :after magit
;;   :hook (magit-mode . magit-delta-mode))

;; TODO try this out after configuring
;; (use-package magit-gptcommit
;;   :ensure t
;;   :after magit
;;   :bind (:map git-commit-mode-map
;;           ("C-c C-g" . magit-gptcommit-commit-accept))
;;   :custom
;;   (magit-gptcommit-llm-provider (make-llm-openai :key "OPENAI-KEY"))

;;   :config
;;   ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
;;   ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
;;   ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
;;   ;; (magit-gptcommit-mode 1)

;;   ;; Add gptcommit transient commands to `magit-commit'
;;   ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
;;   (magit-gptcommit-status-buffer-setup))

(use-package magit-todos
  :ensure t
  :after magit
  :init
  ;; disable branch scanner - causes performance issues in large repos
  (setq magit-todos-branch-list 'nil)
  :config (magit-todos-mode 1))

(provide 'init-magit)
;;; init-magit.el ends here
