;;; init.el -*- lexical-binding: t -*-

;;; Guardrail

(when (< emacs-major-version 30)
  (error "Works with Emacs 30 and newer; you have version %s" emacs-major-version))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (concat (getenv "XDG_CONFIG_HOME") "/emacs/lisp"))
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you want to turn off the welcome screen, uncomment this
;(setopt inhibit-splash-screen t)

(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save history of minibuffer
(savehist-mode)

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control) ; You can use other modifiers here

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

;; Keep focus while navigating help buffers
(setq help-window-select t)

;; Use system trash for file deletion.
(setq delete-by-moving-to-trash t)

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)

;; The above creates nested directories in the backup folder. If
;; instead you would like all backup files in a flat structure, albeit
;; with their full paths concatenated into a filename, then you can
;; use the following configuration:
;; (Run `'M-x describe-variable RET backup-directory-alist RET' for more help)
;;
;; (let ((backup-dir (expand-file-name "emacs-backup/" user-emacs-directory)))
;;   (setopt backup-directory-alist `(("." . ,backup-dir))))

(require 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  (load-theme 'modus-vivendi))          ; for light theme, use modus-operandi

(require 'init-keybindings)
(require 'init-hydra)

(require 'init-completion)
(require 'init-dired)
(require 'init-ediff)
(require 'init-editing)
(require 'init-evil)
;; NOTE: load after evil
(require 'init-which-key)
(require 'init-ace-window)
(require 'init-persp)
(require 'init-projectile)
(require 'init-savehist)
(require 'init-shell)
(require 'init-ui-toggles)
(require 'init-wgrep)

(require 'init-magit)
(require 'init-treemacs)
(recentf-mode 1)

;; (require 'init-copilot)
(require 'init-direnv)
(require 'init-flycheck)
(require 'init-flyspell)
(require 'init-golden-ratio)
(require 'init-minuet)
(require 'init-prog)
(require 'init-org)

;; per-host config
(defvar johnny-enable-server nil
  "If non-nil, start an Emacs server if one is not already running.")
(require 'init-host)

;; TODO disable `Z' binding in magit status
;; TODO key bindings for magit commit mode
;; TODO flycheck (eslint_d)
;; TODO haskell, java, kotlin, nickel, scala
;; TODO outline mode for elisp
;; TODO try corfu-auto
;; TODO try lsp-proxy with tabby-agent
;; TODO org-roam
;; TODO distinguish C-i from TAB
;; TODO persistent server https://github.com/syl20bnr/spacemacs/blob/9aad8a0c6585cf8d500a905938ac96608c2a48ad/layers/%2Bspacemacs/spacemacs-defaults/funcs.el#L1232
;; TODO try using systemctl to start emacs server

(require 'init-async)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(when johnny-enable-server
  (require 'server)
  (unless (or (daemonp) (server-running-p))
    (message "Starting a server...")
    (server-start)))

(setq gc-cons-threshold (or bedrock--initial-gc-threshold 800000))

(provide 'init)
;;; init.el ends here
