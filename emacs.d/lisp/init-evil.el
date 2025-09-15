;;; init-evil.el --- Vim emulation -*- lexical-binding: t -*-

;;; Commentary:

;; Vim emulation

;;; Code:

;; Evil: vi emulation
(use-package evil
  :ensure t
  :init
  (setq
    evil-respect-visual-line-mode t
    evil-undo-system 'undo-fu
    ;; evil-collection wants this value
    evil-want-keybinding nil
    ;; set to nil for now until we distinguish C-i from TAB
    ;; https://github.com/doomemacs/doomemacs/issues/3090
    evil-want-C-i-jump nil
    ;; make C-u scroll up (like in Vim)
    evil-want-C-u-scroll t)

  (evil-mode 1)

  ;; TODO figure out how to use `Z' in magit for worktrees
  ;; ;; unset `Z Q', `Z Z' since I use `Z' in magit for worktrees
  ;; (define-key evil-normal-state-map "ZQ" nil)
  ;; (define-key evil-normal-state-map "ZZ" nil)
  ;; (keymap-unset evil-normal-state-map "Z Q")
  ;; (keymap-unset evil-normal-state-map "Z Z")

  ;; TODO: enable once we have a way to distinguish C-i from TAB
  ;; bind evil-jump-forward for GUI only.
  ;; (define-key evil-motion-state-map [C-i] 'evil-jump-forward)

  :config
  ;; If you use Magit, start editing in insert state
  (add-hook 'git-commit-setup-hook 'evil-insert-state)

  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'eat-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs))

;; Evil Collection: evil bindings for parts not covered already
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  (with-eval-after-load 'diminish
    (diminish 'evil-collection-unimpaired-mode))) ; hide in modeline

;; bind-map: generalized replacement for evil-leader
(use-package bind-map
  :ensure t
  :config
  (bind-map johnny-default-map
    :keys (johnny-emacs-leader-key)
    ;; Use SPC as leader key
    :evil-keys (johnny-leader-key)
    :evil-states (normal motion visual)
    :override-minor-modes t)
  (bind-map johnny-elisp-map
    :keys ((concat johnny-emacs-leader-key " m") johnny-major-mode-emacs-leader-key)
    :evil-keys ((concat johnny-leader-key " m") johnny-major-mode-leader-key)
    :major-modes (emacs-lisp-mode
                   lisp-interaction-mode)))

;; EVIL ARGS
;; The `evil-args' package provides text object support for
;; arguments in programming languages. It allows you to
;; easily select, change, and manipulate arguments in function
;; calls and other similar constructs.
(use-package evil-args
  :ensure t
  :init
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

;; EVIL EXCHANGE
;; The `evil-exchange' package provides a way to exchange text
;; objects in `evil-mode'. It allows you to swap the positions
;; of two text objects, making it easy to rearrange code or
;; text without having to cut and paste.
(use-package evil-exchange
  :ensure t
  :init
  (let ((evil-exchange-key (kbd "gx"))
         (evil-exchange-cancel-key (kbd "gX")))
    (define-key evil-normal-state-map evil-exchange-key 'evil-exchange)
    (define-key evil-visual-state-map evil-exchange-key 'evil-exchange)
    (define-key evil-normal-state-map evil-exchange-cancel-key
      'evil-exchange-cancel)
    (define-key evil-visual-state-map evil-exchange-cancel-key
      'evil-exchange-cancel)))

;; EVIL SURROUND
;; The `evil-surround' package provides text object surround
;; functionality for `evil-mode'. This allows for easily adding,
;; changing, or deleting surrounding characters such as parentheses,
;; quotes, and more.
;;
;; With this you can change 'hello there' with ci'" to have
;; "hello there" and cs"<p> to get <p>hello there</p>.
;; More examples here:
;; - https://github.com/emacs-evil/evil-surround?tab=readme-ov-file#examples
(use-package evil-surround
  :ensure t
  :after evil-collection
  :init
  ;; `s' for surround instead of `substitute'
  ;; see motivation here:
  ;; https://github.com/syl20bnr/spacemacs/blob/develop/doc/DOCUMENTATION.org#the-vim-surround-case
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)
  :config
  (global-evil-surround-mode 1))

;; EVIL NERD COMMENTER
;; The `evil-nerd-commenter' package provides easy commenting
;; and uncommenting of code blocks in `evil-mode'. It offers
;; various commands to toggle comments, comment lines, and
;; comment regions, making it convenient to manage code comments.
(use-package evil-nerd-commenter
  :ensure t
  :commands evilnc-comment-operator
  :init
  (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
  (define-key evil-normal-state-map "gy" 'spacemacs/copy-and-comment-lines)

  (johnny/set-leader-keys
    ";"  'evilnc-comment-operator
    "cl" 'evilnc-comment-or-uncomment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "ct" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cy" 'evilnc-copy-and-comment-lines))

;; EVIL MATCHIT
;; The `evil-matchit' package extends `evil-mode' by enabling
;; text object matching for structures such as parentheses, HTML
;; tags, and other paired delimiters. This makes it easier to
;; navigate and manipulate code blocks.
;; Just use % for jumping between matching structures to check it out.
(use-package evil-matchit
  :ensure t
  :after evil-collection
  :config
  (global-evil-matchit-mode 1))

(provide 'init-evil)
;;; init-evil.el ends here
