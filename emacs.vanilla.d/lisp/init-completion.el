;;; init-completion.el --- Completion/minibuffer config. -*- lexical-binding: t -*-

;;; Commentary:

;; Completion and minibuffer settings, including Consult, Embark, Vertico, Marginalia,
;; Corfu, and Orderless.

;;; Code:

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager
;(setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(setopt text-mode-ispell-word-completion nil)          ; Disable Ispell completion function. Use cape-dict instead
(setopt
  read-extended-command-predicate
  #'command-completion-default-include-p)              ; Hide commands in M-x which do not apply to the current mode

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; For a fancier built-in completion option, try ido-mode,
;; icomplete-vertical, or fido-mode. See also the file extras/base.el

;(icomplete-vertical-mode)
;(fido-vertical-mode)
;(setopt icomplete-delay-completions-threshold 4000)

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  :demand t
  :bind (;; C-c bindings (mode-specific-map)
          ("C-c h" . consult-history)
          ("C-c m" . consult-mode-command)
          ("C-c b" . consult-bookmark)
          ("C-c k" . consult-kmacro)
          ;; C-x bindings (ctl-x-map)
          ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
          ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
          ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
          ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
          ;; Custom M-# bindings for fast register access
          ("M-#" . consult-register-load)
          ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
          ("C-M-#" . consult-register)
          ;; Other custom bindings
          ("M-y" . consult-yank-pop)                ;; orig. yank-pop
          ;; M-g bindings (goto-map)
          ("M-g e" . consult-compile-error)
          ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
          ("M-g g" . consult-goto-line)             ;; orig. goto-line
          ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
          ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
          ("M-g m" . consult-mark)
          ("M-g k" . consult-global-mark)
          ("M-g i" . consult-imenu)
          ("M-g I" . consult-imenu-multi)
          ;; M-s bindings (search-map)
          ("M-s f" . consult-find)
          ("M-s L" . consult-locate)
          ("M-s g" . consult-grep)
          ("M-s G" . consult-git-grep)
          ("M-s r" . consult-ripgrep)
          ("M-s l" . consult-line)
          ("M-s m" . consult-line-multi)
          ("M-s k" . consult-keep-lines)
          ("M-s u" . consult-focus-lines)
          ;; Isearch integration
          ("M-s e" . consult-isearch-history)
          :map isearch-mode-map
          ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
          ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
          ("M-s l" . consult-line))                 ;; needed by consult-line to detect isearch
  :init
  (defun johnny/initial-search-input (&optional force-input)
    "Get initial input from region for consult search functions. If region is not
active and `force-input' is not nil, `thing-at-point' will be returned."
    (if (region-active-p)
      (buffer-substring-no-properties
        (region-beginning) (region-end))
      (if force-input (thing-at-point 'symbol t) ""))
    )

  (defun johnny/search (force-initial-input initial-directory)
    (let* ((initial-input (regexp-quote
                            (johnny/initial-search-input force-initial-input)))
            (default-directory
              (or initial-directory (read-directory-name "Start from directory: "))))
      (consult-ripgrep default-directory initial-input)))

  (defun johnny/search-auto ()
    "Choose folder to search."
    (interactive)
    (johnny/search nil nil))

  (defun johnny/search-auto-symbol ()
    "Choose folder to search."
    (interactive)
    (johnny/search t nil))

  (defun johnny/search-dir ()
    "Search current folder."
    (interactive)
    (johnny/search nil default-directory))

  (defun johnny/search-dir-symbol ()
    "Search current folder."
    (interactive)
    (johnny/search t default-directory))

  (defun johnny/search-projectile ()
    "Search in current project."
    (interactive)
    (johnny/search nil (projectile-project-root)))

  (defun johnny/search-projectile-symbol ()
    "Search in current project."
    (interactive)
    (johnny/search t (projectile-project-root)))

  (defun johnny/search-default ()
    "Search."
    (interactive)
    (johnny/search-projectile-symbol))

  (johnny/set-leader-keys
    "SPC" #'execute-extended-command
    "#"   #'consult-register
    "*"   #'johnny/search-default
    "/"   #'johnny/search-projectile
    "bB"  #'consult-buffer
    "fb"  #'consult-bookmark
    "fL"  #'consult-locate
    "fr"  #'consult-recent-file
    "hm"  #'consult-man
    "jm"  #'consult-mark
    "jM"  #'consult-global-mark
    "sb"  #'consult-line-multi
    "ss"  #'consult-line
    "sk"  #'consult-keep-lines
    "rc"  #'consult-complex-command
    "su"  #'consult-focus-lines
    "sf"  #'johnny/search-auto
    "sF"  #'johnny/search-auto-symbol
    "sd"  #'johnny/search-dir
    "sD"  #'johnny/search-dir-symbol
    "sp"  #'johnny/search-projectile
    "sP"  #'johnny/search-projectile-symbol
    "ry"  #'consult-yank-from-kill-ring
    "Ts"  #'consult-theme)

  (johnny/declare-prefix
    "*" "search project w/ input"
    "/" "search project")

  (setq
    ;; Narrowing lets you restrict results to certain groups of candidates
    consult-narrow-key "<"
    ;; disable automatic previews
    consult-preview-key '("M-." "C-SPC" "C-M-j" "C-M-k")))

(use-package embark-consult
  :ensure t
  :demand t
  :after (embark consult)
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Embark: supercharged context-dependent menu; kinda like a
;; super-charged right-click.
(use-package embark
  :ensure t
  :demand t
  :after (avy embark-consult)
  :bind
  (("M-o" . embark-act)         ;; pick some comfortable bindings
    ("C-;" . embark-dwim)        ;; good alternative: M-.
    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (johnny/set-leader-keys "?" #'embark-bindings)
  ;; Add the option to run embark when using avy
  (defun bedrock/avy-action-embark (pt)
    (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
      (select-window
        (cdr (ring-ref avy-ring 0))))
    t)

  (defun johnny/embark-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command"
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim)))))

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :ensure t
  :demand t
  :init
  (setq vertico-count 20
    vertico-cycle nil
    ;; ignore case for all basic completions
    completion-ignore-case t
    read-buffer-completion-ignore-case t
    read-file-name-completion-ignore-case t)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
    '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (johnny/set-leader-keys
    "sl" #'vertico-repeat-previous
    "sL" #'vertico-repeat-select)
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode)
  :config
  (define-key vertico-map (kbd "C-.") #'embark-select)
  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous)
  (define-key vertico-map (kbd "C-l") #'vertico-insert)
  (define-key vertico-map (kbd "C-S-j") #'vertico-next-group)
  (define-key vertico-map (kbd "C-S-k") #'vertico-previous-group)
  ;; (define-key vertico-map (kbd "C-M-j") #'spacemacs/next-candidate-preview)
  ;; (define-key vertico-map (kbd "C-M-k") #'spacemacs/previous-candidate-preview)
  (define-key vertico-map (kbd "M-RET") #'vertico-exit-input)
  (define-key vertico-map (kbd "C-SPC") #'johnny/embark-preview)
  (define-key vertico-map (kbd "C-r") #'consult-history)
  ;; (define-key vertico-map (kbd "M-P") #'spacemacs/consult-toggle-preview)
  )

(use-package vertico-directory
  :ensure nil
  :demand t
  :after vertico
  :bind (:map vertico-map
          ("C-w" . vertico-directory-delete-word)
          ("C-h" . vertico-directory-up))
  ;; tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :demand t
  ;; TODO add if needed (doubt it)
  ;; :init
  ;; (dolist (it
  ;;           '((spacemacs/compleseus-pers-switch-project . project-file)
  ;;              ;; https://github.com/bbatsov/projectile/issues/1664
  ;;              ;; https://github.com/minad/marginalia/issues/110
  ;;              (persp-switch-to-buffer . buffer)
  ;;              (projectile-find-file . project-file)
  ;;              (projectile-find-dir . project-file)
  ;;              (projectile-recentf . project-file)
  ;;              (projectile-switch-to-buffer . buffer)
  ;;              (projectile-switch-project . project-file)))
  ;;   (push it marginalia-command-categories))
  :config
  (marginalia-mode))

;; Corfu: Popup completion-at-point
(use-package corfu
  :ensure t
  :demand t
  :init
  (global-corfu-mode)
  ;; Disable minibuffer completion which messes up a lot
  (setq global-corfu-minibuffer nil)
  (corfu-echo-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)

  ;; Transfer to minibuffer
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
        (let ((completion-extra-properties extras)
               completion-cycle-threshold completion-cycling)
          (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "C-/" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)

  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Fancy completion-at-point functions; there's too much in the cape package to
;; configure here; dive in when you're comfortable!
(use-package cape
  :ensure t
  :custom
  (cape-dict-file (concat user-emacs-directory "misc/english-words.txt"))
  :init
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-tex))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(provide 'init-completion)
;;; init-completion.el ends here
