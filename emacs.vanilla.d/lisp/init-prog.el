;;; init-prog.el --- Programming modes and config. -*- lexical-binding: t -*-

;;; Built-in config

(use-package emacs
  :config
  ;; Treesitter config

  ;; Tell Emacs to prefer the treesitter mode
  (setq major-mode-remap-alist
    '((bash-mode . bash-ts-mode)
       (css-mode . css-ts-mode)
       (dockerfile-mode . dockerfile-ts-mode)
       (js2-mode . js-ts-mode)
       (json-mode . json-ts-mode)
       (js-json-mode . json-ts-mode)
       (kotlin-mode . kotlin-ts-mode)
       (python-mode . python-ts-mode)
       (rjsx-mode . js-ts-mode)
       (rust-mode . rust-ts-mode)
       (toml-mode . toml-ts-mode)
       (tsx-mode . tsx-ts-mode)
       (typescript-mode . typescript-ts-mode)
       (yaml-mode . yaml-ts-mode)))

  ;;; emacs-lisp
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (johnny/declare-prefix-for-mode mode "mc" "compile")
    (johnny/declare-prefix-for-mode mode "me" "eval")
    (johnny/declare-prefix-for-mode mode "mt" "tests")
    (johnny/declare-prefix-for-mode mode "m=" "format")
    (johnny/declare-prefix-for-mode mode "mg" "find-symbol")
    (johnny/set-leader-keys-for-major-mode mode
      "cc" 'emacs-lisp-byte-compile
      "e$" 'lisp-state-eval-sexp-end-of-line
      "eb" 'eval-buffer
      "ee" 'eval-last-sexp
      "er" 'eval-region
      "ef" 'eval-defun
      "el" 'lisp-state-eval-sexp-end-of-line
      "gb" 'xref-go-back
      ","  'lisp-state-toggle-lisp-state
      "==" 'johnny/indent-region-or-buffer
      "tq" 'ert))

  ;;; javascript
  (johnny/declare-prefix-for-mode 'js-ts-mode "m=" "format")
  (johnny/set-leader-keys-for-major-mode 'js-ts-mode
    "==" #'format-all-buffer)

  ;;; typescript
  (johnny/declare-prefix-for-mode 'typescript-ts-mode "m=" "format")
  (johnny/set-leader-keys-for-major-mode 'typescript-ts-mode
    "==" #'format-all-buffer)
  (johnny/declare-prefix-for-mode 'tsx-ts-mode "m=" "format")
  (johnny/set-leader-keys-for-major-mode 'tsx-ts-mode
    "==" #'format-all-buffer)

  :hook
  ;; Auto parenthesis matching
  (prog-mode . electric-pair-mode)
  ;; for some reason, this is not on by default in emacs-lisp-mode
  (emacs-lisp-mode . electric-indent-mode))

;;; Common file types

(use-package markdown-mode
  :ensure t
  :mode
  ("\\.mdx\\'" . markdown-mode)
  :init
  ;; Make markdown-mode behave a bit more like org w.r.t. code blocks i.e.
  ;; use proper syntax highlighting
  (setq markdown-fontify-code-blocks-natively t)

  ;; Declare prefixes and bind keys
  (dolist (prefix '(("m=" . "format")
                     ("mc" . "markdown/command")
                     ("mh" . "markdown/header")
                     ("mi" . "markdown/insert")
                     ("ml" . "markdown/lists")
                     ("mt" . "markdown/table")
                     ("mT" . "markdown/toggle")
                     ("mx" . "markdown/text")))
    (dolist (mode '(markdown-mode gfm-mode))
      (johnny/declare-prefix-for-mode
        mode (car prefix) (cdr prefix))))
  (dolist (mode '(markdown-mode gfm-mode))
    (johnny/set-leader-keys-for-major-mode mode
      "==" #'format-all-buffer
      ;; rebind this so terminal users can use it
      "'" 'markdown-edit-code-block
      "M-RET" 'markdown-insert-list-item
      ;; Movement
      "{"   'markdown-backward-paragraph
      "}"   'markdown-forward-paragraph
      ;; Completion, and Cycling
      "]"   'markdown-complete
      ;; Indentation
      ">"   'markdown-indent-region
      "<"   'markdown-outdent-region
      ;; Buffer-wide commands
      "c]"  'markdown-complete-buffer
      "cc"  'markdown-check-refs
      "ce"  'markdown-export
      "cm"  'markdown-other-window
      "cn"  'markdown-cleanup-list-numbers
      "co"  'markdown-open
      "cp"  'markdown-preview
      "cP"  'markdown-live-preview-mode
      "cv"  'markdown-export-and-preview
      "cw"  'markdown-kill-ring-save
      ;; headings
      "hi"  'markdown-insert-header-dwim
      "hI"  'markdown-insert-header-setext-dwim
      "h1"  'markdown-insert-header-atx-1
      "h2"  'markdown-insert-header-atx-2
      "h3"  'markdown-insert-header-atx-3
      "h4"  'markdown-insert-header-atx-4
      "h5"  'markdown-insert-header-atx-5
      "h6"  'markdown-insert-header-atx-6
      "h!"  'markdown-insert-header-setext-1
      "h@"  'markdown-insert-header-setext-2
      ;; Insertion of common elements
      "-"   'markdown-insert-hr
      "if"  'markdown-insert-footnote
      "ii"  'markdown-insert-image
      "ik"  'spacemacs/insert-keybinding-markdown
      "il"  'markdown-insert-link
      "iw"  'markdown-insert-wiki-link
      "iu"  'markdown-insert-uri
      "iT"  'markdown-insert-table
      ;; Element removal
      "k"   'markdown-kill-thing-at-point
      ;; List editing
      "li"  'markdown-insert-list-item
      ;; Toggles
      "Ti"  'markdown-toggle-inline-images
      "Tl"  'markdown-toggle-url-hiding
      "Tm"  'markdown-toggle-markup-hiding
      "Tt"  'markdown-toggle-gfm-checkbox
      "Tw"  'markdown-toggle-wiki-links
      ;; Table
      "ta"  'markdown-table-align
      "tp"  'markdown-table-move-row-up
      "tn"  'markdown-table-move-row-down
      "tf"  'markdown-table-move-column-right
      "tb"  'markdown-table-move-column-left
      "tr"  'markdown-table-insert-row
      "tR"  'markdown-table-delete-row
      "tc"  'markdown-table-insert-column
      "tC"  'markdown-table-delete-column
      "ts"  'markdown-table-sort-lines
      "td"  'markdown-table-convert-region
      "tt"  'markdown-table-transpose
      ;; region manipulation
      "xb"  'markdown-insert-bold
      "xB"  'markdown-insert-gfm-checkbox
      "xc"  'markdown-insert-code
      "xC"  'markdown-insert-gfm-code-block
      "xi"  'markdown-insert-italic
      "xk"  'markdown-insert-kbd
      "xp"  'markdown-insert-pre
      "xq"  'markdown-insert-blockquote
      "xs"  'markdown-insert-strike-through
      "xQ"  'markdown-blockquote-region
      "xP"  'markdown-pre-region
      ;; Following and Jumping
      "N"   'markdown-next-link
      "f"   'markdown-follow-thing-at-point
      "P"   'markdown-previous-link
      "<RET>" 'markdown-do))
  ;; Header navigation in normal state movements
  (evil-define-key 'normal markdown-mode-map
    "gj" 'outline-forward-same-level
    "gk" 'outline-backward-same-level
    "gh" 'outline-up-heading
    ;; next visible heading is not exactly what we want but close enough
    "gl" 'outline-next-visible-heading)

  :hook ((markdown-mode . visual-line-mode)
          (markdown-mode . flyspell-mode)))

;;; GitHub-flavored markdown preview
(use-package gh-md
  :ensure t
  :init
  (dolist (mode '(markdown-mode gfm-mode))
    (johnny/set-leader-keys-for-major-mode mode
      "cr" 'gh-md-render-buffer)))

(use-package json-mode
  :ensure t
  :init
  (johnny/declare-prefix-for-mode 'json-ts-mode "m=" "format")
  (johnny/set-leader-keys-for-major-mode 'json-ts-mode
    "==" #'format-all-buffer))

(use-package toml-mode
  :mode "/\\(\\.toml\\|Cargo.lock\\|\\.cargo/config\\)\\'"
  :ensure t)

(use-package yaml-mode
  :ensure t
  :init
  (johnny/declare-prefix-for-mode 'yaml-ts-mode "m=" "format")
  (johnny/set-leader-keys-for-major-mode 'yaml-ts-mode
    "==" #'format-all-buffer))

;;; nix
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :hook
  (nix-mode . format-all-mode) ; format on save
  (nix-mode . eglot-ensure)
  :init
  (johnny/declare-prefix-for-mode 'nix-mode "m=" "format")
  (johnny/set-leader-keys-for-major-mode 'nix-mode
    "==" #'format-all-buffer
    "f"  'nix-flake)
  :config
  (electric-indent-mode -1))

;;; Formatting (useful for languages without LSP support)
(use-package format-all
  :ensure t
  :commands (format-all-buffer format-all-region format-all-mode)
  :config
  (setq-default format-all-formatters
    '(("Nix" alejandra)
       ("Shell" (shfmt "-i" "4" "-ci"))))
  (define-format-all-formatter treefmt
    (:executable "treefmt")
    (:install)
    (:languages
      "CSS" "GraphQL" "HTML" "JavaScript" "JSON" "JSON5" "JSX" "Less" "Lua"
      "Markdown" "PHP" "Rust" "SCSS" "Solidity" "Svelte" "TOML" "TSX"
      "TypeScript" "Vue" "YAML")
    (:features)
    (:format (format-all--buffer-easy executable "--stdin" (buffer-file-name)))))

;;; Dhall
(use-package dhall-mode
  :ensure t
  :hook
  (dhall-mode . eglot-ensure))

;;; GraphQL
(use-package graphql-mode
  :ensure t
  :init
  (johnny/declare-prefix-for-mode 'graphql-mode "m=" "format")
  (johnny/declare-prefix-for-mode 'graphql-mode "mg" "goto")
  (johnny/set-leader-keys-for-major-mode 'graphql-mode
    "s" 'graphql-send-query
    "e" 'graphql-select-endpoint
    "h" 'graphql-edit-headers)
  )

;;; Rust

(use-package rust-mode
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t))

;; load after configuring rust-mode (enable treesitter)
(use-package rustic
  :ensure t
  :after rust-mode
  :init
  (setq rustic-lsp-client 'eglot)
  (johnny/declare-prefix-for-mode 'rustic-mode "mc" "cargo")
  (johnny/declare-prefix-for-mode 'rustic-mode "mt" "tests")
  (johnny/declare-prefix-for-mode 'rustic-mode "mg" "goto")
  (johnny/declare-prefix-for-mode 'rustic-mode "mh" "help")
  (johnny/declare-prefix-for-mode 'rustic-mode "m=" "format")
  (johnny/set-leader-keys-for-major-mode 'rustic-mode
    "c." 'rustic-cargo-run-rerun
    "c=" 'rustic-cargo-fmt
    "ca" 'rustic-cargo-add
    "cc" 'rustic-cargo-build
    "cC" 'rustic-cargo-clean
    "cd" 'rustic-cargo-doc
    "cs" 'rustic-doc-search
    "ce" 'rustic-cargo-bench
    "ci" 'rustic-cargo-init
    "cl" 'rustic-cargo-clippy
    "cf" 'rustic-cargo-clippy-fix
    "cn" 'rustic-cargo-new
    "co" 'rustic-cargo-outdated
    "cr" 'rustic-cargo-rm
    "cu" 'rustic-cargo-update
    "cU" 'rustic-cargo-upgrade
    "cv" 'rustic-cargo-check
    "cx" 'rustic-cargo-run
    "ta" 'rustic-cargo-test-run
    "tt" 'rustic-cargo-current-test))

(use-package terraform-ts-mode
  :ensure (:host github :repo "kgrotel/terraform-ts-mode" :files ("*.el")))

(use-package web-mode
  :ensure t
  :init
  (johnny/declare-prefix-for-mode 'web-mode "m=" "format")
  (johnny/declare-prefix-for-mode 'web-mode "mE" "errors")
  (johnny/declare-prefix-for-mode 'web-mode "mg" "goto")
  (johnny/declare-prefix-for-mode 'web-mode "mh" "dom")
  (johnny/declare-prefix-for-mode 'web-mode "mr" "refactor")
  (johnny/set-leader-keys-for-major-mode 'web-mode
    "El" 'web-mode-dom-errors-show
    "gb" 'web-mode-element-beginning
    "gc" 'web-mode-element-child
    "gp" 'web-mode-element-parent
    "gs" 'web-mode-element-sibling-next
    "hp" 'web-mode-dom-xpath
    "rc" 'web-mode-element-clone
    "rd" 'web-mode-element-vanish
    "rk" 'web-mode-element-kill
    "rr" 'web-mode-element-rename
    "rw" 'web-mode-element-wrap
    "z" 'web-mode-fold-or-unfold)
  :mode
  (("\\.phtml\\'"      . web-mode)
    ("\\.tpl\\'"        . web-mode)
    ("\\.twig\\'"       . web-mode)
    ("\\.html\\'"       . web-mode)
    ("\\.htm\\'"        . web-mode)
    ("\\.[gj]sp\\'"     . web-mode)
    ("\\.as[cp]x?\\'"   . web-mode)
    ("\\.eex\\'"        . web-mode)
    ("\\.erb\\'"        . web-mode)
    ("\\.mustache\\'"   . web-mode)
    ("\\.handlebars\\'" . web-mode)
    ("\\.hbs\\'"        . web-mode)
    ("\\.eco\\'"        . web-mode)
    ("\\.ejs\\'"        . web-mode)
    ("\\.ctp\\'"        . web-mode)
    ("\\.djhtml\\'"     . web-mode))
  :hook
  (web-mode . eglot-ensure))

;;; Eglot, the built-in LSP client for Emacs

;; Helpful resources:
;;
;;  - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
                                        ; :hook
                                        ; (((python-mode ruby-mode elixir-mode) . eglot-ensure))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :init
  (johnny/set-leader-keys-for-minor-mode 'eglot--managed-mode
    ;; format
    "=" "format"
    ;; allow more specific modes to define `==' binding
    ;; "==" #'eglot-format-buffer
    "=b" #'eglot-format-buffer
    "=o" #'eglot-code-action-organize-imports
    ;; code actions
    "a" "code actions"
    "aa" #'eglot-code-actions
    ;; "af" #'spacemacs//lsp-action-placeholder
    ;; "ar" #'spacemacs//lsp-action-placeholder
    ;; "as" #'spacemacs//lsp-action-placeholder
    ;; goto
    ;; N.B. implementation and references covered by xref bindings / lsp provider...
    "g" "goto"
    "gb" #'xref-go-back
    "gd" #'xref-find-definitions
    "gD" #'xref-find-definitions-other-window
    "gg" #'eglot-find-declaration
    "gi" #'eglot-find-implementation
    "gr" #'xref-find-references
    "gt" #'eglot-find-typeDefinition
    "gM" #'lsp-ui-imenu
    ;; help
    "h" "help"
    ;; jump
    ;; backend
    "b" "backend"
    "bd" #'eglot-list-connections
    "br" #'eglot-reconnect
    "bs" #'eglot-shutdown
    ;; refactor
    "r" "refactor"
    "rr" #'eglot-rename
    ;; toggles
    "T" "toggle"
    "Ti" #'eglot-inlay-hints-mode)

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
                                        ; (add-to-list 'eglot-server-programs
                                        ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  :hook
  ;; Enable Eglot in programming modes
  (prog-mode . (lambda ()
                 (unless (derived-mode-p
                           'emacs-lisp-mode 'lisp-mode
                           'makefile-mode)
                   (eglot-ensure))))
  ;; (eglot-managed-mode . eglot-eldoc-setup)) ; setup eldoc for Eglot
  )

(use-package eldoc-box
  :ensure t
  :init
  (johnny/set-leader-keys-for-minor-mode 'eglot--managed-mode
    "hh" #'eldoc-box-help-at-point))

(use-package consult-eglot
  :ensure t
  :init
  (johnny/set-leader-keys-for-minor-mode 'eglot--managed-mode
    "gs" #'consult-eglot-symbols))

(use-package consult-eglot-embark
  :ensure t
  :config
  (consult-eglot-embark-mode))

;;; Eglot booster
(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster" :files ("*.el"))
  :after eglot
  :init
  (setq eglot-booster-io-only t)
  :config
  (eglot-booster-mode))

;;; Templating

(use-package tempel
  :ensure t
  ;; By default, tempel looks at the file "templates" in
  ;; user-emacs-directory, but you can customize that with the
  ;; tempel-path variable:
  ;; :custom
  ;; (tempel-path (concat user-emacs-directory "custom_template_file"))
  :bind (("M-*" . tempel-insert)
          ("M-+" . tempel-complete)
          :map tempel-map
          ("C-c RET" . tempel-done)
          ("C-j" . tempel-next)
          ("C-n" . tempel-next)
          ("C-k" . tempel-previous)
          ("C-p" . tempel-previous))
  :init
  ;; Make a function that adds the tempel expansion function to the
  ;; list of completion-at-point-functions (capf).
  (defun tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-expand -1 'local))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package tempel-collection
  :ensure t)

(provide 'init-prog)
;;; init-prog.el ends here
