;;; init-persp.el --- Perspectives and window configs -*- lexical-binding: t -*-

;;; Commentary:

;; Perspectives and window configs

;;; Code:

(use-package eyebrowse
  :ensure t
  :init
  (setq eyebrowse-wrap-around t)

  (defvar johnny--workspaces-hydra-full-hint-toggle nil
    "Toggle display of workspaces hydra documentation.")

  (load "./eyebrowse/funcs.el")

  (defhydra hydra-workspaces
    (nil nil :hint nil)
    "Workspaces"
    ("?" johnny//workspaces-hydra-toggle-hint)
    ("0" johnny/workspace-switch-to-0 :exit t)
    ("1" johnny/workspace-switch-to-1 :exit t)
    ("2" johnny/workspace-switch-to-2 :exit t)
    ("3" johnny/workspace-switch-to-3 :exit t)
    ("4" johnny/workspace-switch-to-4 :exit t)
    ("5" johnny/workspace-switch-to-5 :exit t)
    ("6" johnny/workspace-switch-to-6 :exit t)
    ("7" johnny/workspace-switch-to-7 :exit t)
    ("8" johnny/workspace-switch-to-8 :exit t)
    ("9" johnny/workspace-switch-to-9 :exit t)
    ("C-0" johnny/workspace-switch-to-0)
    ("C-1" johnny/workspace-switch-to-1)
    ("C-2" johnny/workspace-switch-to-2)
    ("C-3" johnny/workspace-switch-to-3)
    ("C-4" johnny/workspace-switch-to-4)
    ("C-5" johnny/workspace-switch-to-5)
    ("C-6" johnny/workspace-switch-to-6)
    ("C-7" johnny/workspace-switch-to-7)
    ("C-8" johnny/workspace-switch-to-8)
    ("C-9" johnny/workspace-switch-to-9)
    ("<tab>" eyebrowse-last-window-config)
    ("<return>" nil :exit t)
    ("TAB" eyebrowse-last-window-config)
    ("RET" nil :exit t)
    ("c" johnny/clone-workspace :exit t)
    ("C" johnny/clone-workspace)
    ("d" johnny/workspace-close)
    ("j" eyebrowse-next-window-config)
    ("k" eyebrowse-prev-window-config)
    ("l" hydra-persp/body :exit t)
    ("n" eyebrowse-next-window-config)
    ("N" eyebrowse-prev-window-config)
    ("R" johnny/workspaces-rename :exit t)
    ("s" johnny/single-win-workspace :exit t)
    ("S" johnny/single-win-workspace)
    ("w" eyebrowse-switch-to-window-config :exit t))

  ;; override default dynamic hint with custom dynamic hint
  (setq hydra-workspaces/hint (list 'concat "Workspaces: " '(johnny//workspaces-hydra-hint)))

  ;; note: we don't need to declare the `SPC l w' binding, it is
  ;; declared in the perspectives hydra
  (johnny/set-leader-keys "bW" 'johnny/goto-buffer-workspace)

  ;; hooks
  (add-hook 'persp-before-switch-functions
    #'johnny/update-eyebrowse-for-perspective)
  (add-hook 'eyebrowse-post-window-switch-hook
    #'johnny/save-eyebrowse-for-perspective)
  (add-hook 'persp-activated-functions
    #'johnny/load-eyebrowse-for-perspective)
  (add-hook 'persp-before-save-state-to-file-functions
    #'johnny/update-eyebrowse-for-perspective)
  (add-hook 'persp-after-load-state-functions
    #'johnny/load-eyebrowse-after-loading-persp)

  ;; vim-style tab switching
  (define-key evil-motion-state-map "gt" 'eyebrowse-next-window-config)
  (define-key evil-motion-state-map "gT" 'eyebrowse-prev-window-config)

  ;; ensure scratch buffer is live, otherwise loading workspaces can fail silently
  (define-advice eyebrowse--fixup-window-config
    (:after (window-config) johnny//maybe-create-scratch-buffer)
    (unless (get-buffer "*scratch*")
      (catch 'found
        (eyebrowse--walk-window-config
          window-config
          (lambda (item)
            (when (and (eq (car item) 'buffer)
                    (equal (cadr item) "*scratch*"))
              (get-scratch-buffer-create)
              (throw 'found t)))))))

  :config
  (setq johnny--workspaces-hydra-full-hint
    ;; use internal function to format docstring
    (eval
      (hydra--format nil
        '(nil nil :hint nil :columns nil :foreign-keys nil)
        "\n
 Go to^^^^^^                          Actions^^^^
 ─────^^^^^^────────────────────────  ───────^^^^───────────────────────
 [_0_.._9_]^^     nth/new workspace   [_c_/_C_] clone workspace
 [_C-0_.._C-9_]^^ nth/new workspace   [_s_/_S_] single window workspace
 [_<tab>_]^^^^    last workspace      [_d_]^^   close current workspace
 [_j_/_n_]^^      next workspace      [_R_]^^   rename current workspace
 [_k_/_N_]^^      prev workspace      [_?_]^^   toggle help
 [_w_]^^^^        another workspace
 [_l_]^^^^        perspectives hydra"
        hydra-workspaces/heads)))

  (eyebrowse-mode))

(use-package persp-mode
  :ensure t
  :init
  (setq persp-add-buffer-on-after-change-major-mode 'free
    persp-auto-resume-time 1                        ; restore saved perspectives after 1 sec
    persp-auto-save-num-of-backups 10
    persp-autokill-buffer-on-remove 'kill-weak
    persp-nil-name "Default"
    ;; not sure if I should keep this from spacemacs
    ;; persp-reset-windows-on-nil-window-conf nil
    persp-set-last-persp-for-new-frames nil         ; use default/nil perspective for new frames
    persp-set-ido-hooks t)

  (add-hook 'window-setup-hook #'(lambda () (persp-mode 1)))

  (defvar johnny/persp-autosave-delay 900
    "Delay in seconds between each perspectives auto-save.")

  (defvar johnny/persp-enable-autosave t
    "If true, saves perspectives to file per `johnny/persp-autosave-delay'")

  (defvar johnny--persp-autosave-timer nil
    "Timer for perspectives auto-save.")

  (defvar johnny--persp-hydra-full-hint-toggle nil
    "Toggle display of perspectives hydra documentation.")

  (load "./persp-mode/funcs.el")

  ;; perspectives hydra
  (defhydra hydra-persp
    (nil nil :hint nil)
    "Perspectives"
    ("?" johnny//persp-hydra-toggle-hint)
    ("1" johnny/persp-switch-to-1 :exit t)
    ("2" johnny/persp-switch-to-2 :exit t)
    ("3" johnny/persp-switch-to-3 :exit t)
    ("4" johnny/persp-switch-to-4 :exit t)
    ("5" johnny/persp-switch-to-5 :exit t)
    ("6" johnny/persp-switch-to-6 :exit t)
    ("7" johnny/persp-switch-to-7 :exit t)
    ("8" johnny/persp-switch-to-8 :exit t)
    ("9" johnny/persp-switch-to-9 :exit t)
    ("0" johnny/persp-switch-to-0 :exit t)
    ("e" johnny/persp-switch-to :exit t)
    ("C-1" johnny/persp-switch-to-1)
    ("C-2" johnny/persp-switch-to-2)
    ("C-3" johnny/persp-switch-to-3)
    ("C-4" johnny/persp-switch-to-4)
    ("C-5" johnny/persp-switch-to-5)
    ("C-6" johnny/persp-switch-to-6)
    ("C-7" johnny/persp-switch-to-7)
    ("C-8" johnny/persp-switch-to-8)
    ("C-9" johnny/persp-switch-to-9)
    ("C-0" johnny/persp-switch-to-0)
    ("<tab>" johnny/jump-to-last-persp :exit t)
    ("<return>" nil :exit t)
    ("TAB" johnny/jump-to-last-persp)
    ("RET" nil :exit t)
    ("<" johnny/move-current-persp-left)
    (">" johnny/move-current-persp-right)
    ("a" persp-add-buffer :exit t)
    ("A" persp-import-buffers :exit t)
    ("b" johnny/consult-buffer-persp :exit t)
    ("d" johnny/persp-hydra-close)
    ("D" johnny/persp-hydra-close-other :exit t)
    ("h" johnny/persp-goto-default :exit t)
    ("j" persp-next)
    ("k" persp-prev)
    ("L" persp-load-state-from-file :exit t)
    ("l" persp-frame-switch :exit t)
    ("n" persp-next)
    ("N" persp-prev)
    ;; NOTE: add hydra if needed to support other special perspectives
    ;; - currently `@Org' is the only special one
    ("o" johnny/persp-switch-to-org :exit t)
    ("r" persp-remove-buffer :exit t)
    ("R" persp-rename :exit t)
    ("s" persp-save-state-to-file :exit t)
    ("S" persp-save-to-file-by-names :exit t)
    ("t" persp-temporarily-display-buffer :exit t)
    ("w" hydra-workspaces/body :exit t)
    ("x" johnny/persp-hydra-kill)
    ("X" johnny/persp-hydra-kill-other :exit t))

  ;; override default dynamic hint with custom dynamic hint
  (setq hydra-persp/hint (list 'concat "Perspectives: " '(johnny//persp-hydra-hint)))

  (johnny/set-leader-keys
    "l" #'hydra-persp/body
    "pl" #'johnny/persp-switch-project)
  (johnny/declare-prefix
    "pl" "persp-switch-project")

  :config
  (which-key-add-keymap-based-replacements eyebrowse-mode-map
    "C-c C-w" "eyebrowse")
  (setq johnny--persp-hydra-full-hint
    ;; use internal function to format docstring
    (eval
      (hydra--format nil
        '(nil nil :hint nil :columns nil :foreign-keys nil)
        "\n
 Go to^^^^^^                             Actions^^^^
 ─────^^^^^^───────────────────────────  ───────^^^^───────────────────────────────
 [_0_.._9_]^^     nth/new perspective    [_a_]^^   add buffer
 [_C-0_.._C-9_]^^ nth/new perspective    [_A_]^^   add all buffers from perspective
 [_<tab>_]^^^^    last perspective       [_d_]^^   close current perspective
 [_j_/_n_]^^      next perspective       [_D_]^^   close other perspective
 [_k_/_N_]^^      prev perspective       [_L_]^^   load perspectives from file
 [_b_]^^^^        buffer in perspective  [_r_]^^   remove current buffer
 [_h_]^^^^        default perspective    [_R_]^^   rename current perspective
 [_l_]^^^^        another perspective    [_s_/_S_] save all perspectives/save by names
 [_o_]^^^^        @Org perspective       [_t_]^^   show buffer w/o adding to perspective
 [_w_]^^^^        workspaces hydra       [_x_]^^   kill current w/buffers
 [_e_]^^^^        select perspective     [_X_]^^   kill other w/buffers
 ^^^^^^                                  [_<_/_>_] move perspective left/right
 ^^^^^^                                  [_?_]^^   toggle help"
        hydra-persp/heads)))
  (diminish 'projectile-mode) ; hide in modeline
  (add-hook 'persp-mode-hook 'johnny//persp-autosave)
  (advice-add 'persp-switch :before #'johnny//load-persp-local-vars)

  ;; eyebrowse's advice for rename-buffer only updates workspace window
  ;; configurations that are stored in frame properties, but this
  ;; persp-mode integration saves workspace window configurations in
  ;; perspective parameters.  We need to replace eyebrowse's advice with
  ;; perspective-aware advice in order to ensure that window
  ;; configurations for inactive perspectives get updated.
  (when (ad-find-advice 'rename-buffer 'around 'eyebrowse-fixup-window-configs)
    (ad-disable-advice 'rename-buffer 'around 'eyebrowse-fixup-window-configs)
    (ad-activate 'rename-buffer))
  (advice-add 'rename-buffer :around #'johnny//fixup-window-configs))

  (johnny/declare-prefix
    "b" "persp-buffers")
  (johnny/set-leader-keys
    "ba"   'persp-add-buffer
    "bb"   #'johnny/consult-buffer-persp
    "br"   'persp-remove-buffer)
  (johnny/declare-prefix
    "bb" "consult-buffer-persp")

(use-package winner
  :after persp-mode
  :config
  (johnny/make-variable-persp-local
    'winner-ring-alist nil
    'winner-point-alist nil
    'winner-undo-counter nil
    'winner-undone-data nil
    'winner-currents nil
    'winner-pending-undo-ring nil))

(provide 'init-persp)
;;; init-persp.el ends here
