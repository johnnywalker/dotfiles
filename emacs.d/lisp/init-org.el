;;; init-org.el --- Org-mode -*- lexical-binding: t -*-

;;; Commentary:

;; Org-mode - a powerful system for organizing notes, tasks, and projects.

;;; Code:

(use-package org
  :ensure t
  :init
  (setq org-directory "~/org"
	  org-default-notes-file (concat org-directory "/inbox.org")
	  org-agenda-files '("inbox.org" "notes.org")
	  org-log-done 'time                   ; log time when tasks are marked as done
	  org-startup-with-inline-images t     ; display inline images in Org buffers
	  org-latex-prefer-user-labels t       ; use user-defined labels in LaTeX exports
	  org-hide-emphasis-markers t          ; hide emphasis markers like *bold*
	  ;; org-ellipsis " ⤵"                   ; use a custom ellipsis for collapsed sections
	  org-src-fontify-natively t           ; syntax highlight code blocks
	  org-src-tab-acts-natively t)         ; allow tab to indent in code blocks

  (setq org-todo-keywords
	  '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|" "DONE(d!)" "OBSOLETE(o@)")))

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)

  (setq org-capture-templates
	  '(("c" "Default Capture" entry (file "inbox.org")
        "* TODO %?\n%U\n%i")
	     ;; Capture and keep an org-link to the thing we're currently working with
	     ("r" "Capture with Reference" entry (file "inbox.org")
         "* TODO %?\n%U\n%i\n%a")))

  (dolist (prefix `(
                     ("mb" . "babel")
                     ("mC" . "clocks")
                     ("md" . "dates")
                     ("me" . "export")
                     ("mf" . "feeds")
                     ("mi" . "insert")
                     ("miD" . "download")
                     ("mm" . "more")
                     ("ms" . "trees/subtrees")
                     ("mT" . "toggles")
                     ("mt" . "tables")
                     ("mtd" . "delete")
                     ("mti" . "insert")
                     ("mtt" . "toggle")
                     ("mx" . "text")))
    (johnny/declare-prefix-for-mode 'org-mode (car prefix) (cdr prefix)))

  (johnny/set-leader-keys-for-major-mode 'org-mode
		"'" 'org-edit-special
		"c" 'org-capture

		;; Clock
		;; These keybindings should match those under the "aoC" prefix (below)
		"Cc" 'org-clock-cancel
		"Cd" 'org-clock-display
		"Ce" 'org-evaluate-time-range
		"Cg" 'org-clock-goto
		"Ci" 'org-clock-in
		"CI" 'org-clock-in-last
		"Co" 'org-clock-out
		"CR" 'org-clock-report
		"Cr" 'org-resolve-clocks

		"dd" 'org-deadline
		"ds" 'org-schedule
		"dt" 'org-time-stamp
		"dT" 'org-time-stamp-inactive
		"ee" 'org-export-dispatch
		"fi" 'org-feed-goto-inbox
		"fu" 'org-feed-update-all

		"a" 'org-agenda
		"[" 'org-agenda-file-to-front
		"]" 'org-remove-file

		"p" 'org-priority

		"Tc" 'org-toggle-checkbox
		"Te" 'org-toggle-pretty-entities
		"Ti" 'org-toggle-inline-images
		"Tn" 'org-num-mode
		"Tl" 'org-toggle-link-display
		"Tt" 'org-show-todo-tree
		"TT" 'org-todo
		"TV" 'space-doc-mode
		"Tx" 'org-latex-preview

		;; More cycling options (timestamps, headlines, items, properties)
		"L" 'org-shiftright
		"H" 'org-shiftleft
		"J" 'org-shiftdown
		"K" 'org-shiftup

		;; Change between TODO sets
		"C-S-l" 'org-shiftcontrolright
		"C-S-h" 'org-shiftcontrolleft
		"C-S-j" 'org-shiftcontroldown
		"C-S-k" 'org-shiftcontrolup

		;; Subtree editing
		"sa" 'org-toggle-archive-tag
		"sA" 'org-archive-subtree-default
		"sb" 'org-tree-to-indirect-buffer
		"sd" 'org-cut-subtree
		"sy" 'org-copy-subtree
		"sp" 'org-paste-subtree
		"sh" 'org-promote-subtree
		"sj" 'org-move-subtree-down
		"sk" 'org-move-subtree-up
		"sl" 'org-demote-subtree
		"sn" 'org-narrow-to-subtree
		"sw" 'widen
		"sr" 'org-refile
		"ss" 'org-sparse-tree
		"sS" 'org-sort

		;; tables
		"ta" 'org-table-align
		"tb" 'org-table-blank-field
		"tc" 'org-table-convert
		"tdc" 'org-table-delete-column
		"tdr" 'org-table-kill-row
		"te" 'org-table-eval-formula
		"tE" 'org-table-export
		"tf" 'org-table-field-info
		"th" 'org-table-previous-field
		"tH" 'org-table-move-column-left
		"tic" 'org-table-insert-column
		"tih" 'org-table-insert-hline
		"tiH" 'org-table-hline-and-move
		"tir" 'org-table-insert-row
		"tI" 'org-table-import
		"tj" 'org-table-next-row
		"tJ" 'org-table-move-row-down
		"tK" 'org-table-move-row-up
		"tl" 'org-table-next-field
		"tL" 'org-table-move-column-right
		"tn" 'org-table-create
		"tN" 'org-table-create-with-table.el
		"tr" 'org-table-recalculate
		"tR" 'org-table-recalculate-buffer-tables
		"ts" 'org-table-sort-lines
		"ttf" 'org-table-toggle-formula-debugger
		"tto" 'org-table-toggle-coordinate-overlays
		"tw" 'org-table-wrap-region

		;; Source blocks / org-babel
		"bp"     'org-babel-previous-src-block
		"bn"     'org-babel-next-src-block
		"be"     'org-babel-execute-maybe
		"bo"     'org-babel-open-src-block-result
		"bv"     'org-babel-expand-src-block
		"bu"     'org-babel-goto-src-block-head
		"bg"     'org-babel-goto-named-src-block
		"br"     'org-babel-goto-named-result
		"bb"     'org-babel-execute-buffer
		"bs"     'org-babel-execute-subtree
		"bd"     'org-babel-demarcate-block
		"bt"     'org-babel-tangle
		"bf"     'org-babel-tangle-file
		"bc"     'org-babel-check-src-block
		"bj"     'org-babel-insert-header-arg
		"bl"     'org-babel-load-in-session
		"bi"     'org-babel-lob-ingest
		"bI"     'org-babel-view-src-block-info
		"bz"     'org-babel-switch-to-session
		"bZ"     'org-babel-switch-to-session-with-code
		"ba"     'org-babel-sha1-hash
		"bx"     'org-babel-do-key-sequence-in-edit-buffer
		;; "b."     'spacemacs/org-babel-transient-state/body
		;; Multi-purpose keys
		(or johnny-major-mode-leader-key ",") 'org-ctrl-c-ctrl-c
		"*" 'org-ctrl-c-star
		"-" 'org-ctrl-c-minus
		"#" 'org-update-statistics-cookies
		"RET"   'org-ctrl-c-ret
		"M-RET" 'org-meta-return
		;; attachments
		"A" 'org-attach
		;; insertion
		"ib" 'org-insert-structure-template
		"id" 'org-insert-drawer
		"ie" 'org-set-effort
		"if" 'org-footnote-new
		"ih" 'org-insert-heading
		"iH" 'org-insert-heading-after-current
		"ii" 'org-insert-item
		"il" 'org-insert-link
		"in" 'org-add-note
		"ip" 'org-set-property
		"is" 'org-insert-subheading
		"it" 'org-set-tags-command)

  ;; Add global evil-leader mappings. Used to access org-agenda
  ;; functionalities – and a few others commands – from any other mode.
  (johnny/declare-prefix
    "ao"  "org"
    "aof" "feeds"
    "aoC" "clocks")
  (johnny/set-leader-keys
    "ao#" 'org-agenda-list-stuck-projects
    "ao/" 'org-occur-in-agenda-files
    "aoa" 'org-agenda-list
    "aoo" 'org-agenda
    "aoc" 'org-capture
    "aoe" 'org-store-agenda-views
    "aofi" 'org-feed-goto-inbox
    "aofu" 'org-feed-update-all

    ;; Clock
    ;; These keybindings should match those under the "mC" prefix (above)
    "aoCc" 'org-clock-cancel
    "aoCg" 'org-clock-goto
    "aoCi" 'org-clock-in
    "aoCI" 'org-clock-in-last
    "aoCo" 'org-clock-out
    "aoCr" 'org-resolve-clocks

    "aol" 'org-store-link
    "aom" 'org-tags-view
    "aos" 'org-search-view
    "aot" 'org-todo-list
    "Cc" 'org-capture)

  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)

  :hook ((org-mode . visual-line-mode)   ; wrap lines at word breaks
          (org-mode . flyspell-mode))    ; spell checking!
  :config
  ;; Open links and files with RET in normal state
  (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :custom
  (evil-org-use-additional-insert t) ; make additional keybindings available in insert mode
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-agenda
  :ensure nil ; built-in
  :after org
  :init
  (setq org-agenda-restore-windows-after-quit t)
  (dolist (prefix `(("mC" . "clocks")
                     ("md" . "dates")
                     ("mi" . "insert")
                     ("ms" . "trees/subtrees")))
    (johnny/declare-prefix-for-mode 'org-agenda-mode
			(car prefix) (cdr prefix)))
  (johnny/set-leader-keys-for-major-mode 'org-agenda-mode
		(or johnny-major-mode-leader-key ",") 'org-agenda-ctrl-c-ctrl-c
		"a" 'org-agenda
		"c" 'org-agenda-capture
		"Cc" 'org-agenda-clock-cancel
		"Ci" 'org-agenda-clock-in
		"Co" 'org-agenda-clock-out
		"Cj" 'org-agenda-clock-goto
		"dd" 'org-agenda-deadline
		"ds" 'org-agenda-schedule
		"ie" 'org-agenda-set-effort
		"ip" 'org-agenda-set-property
		"iP" 'org-agenda-priority
		"it" 'org-agenda-set-tags
		"sr" 'org-agenda-refile
		"TT" 'org-agenda-todo))

(use-package org-project-capture
  :ensure t
  :init
  (defun johnny/org-project-capture-capture (&optional arg)
    (interactive "P")
    (if arg
      (org-project-capture-project-todo-completing-read :empty-lines 1)
      (org-project-capture-capture-for-current-project :empty-lines 1)))

  (defun johnny/org-project-capture-goto-todos ()
    (interactive)
    (org-project-capture-goto-location-for-project (projectile-project-name)))

  (johnny/set-leader-keys
    "aop" 'johnny/org-project-capture-capture
    "po"  'johnny/org-project-capture-goto-todos)

  (johnny/declare-prefix
    "aop" "org-project-capture-capture"
    "po"  "org-project-capture-goto-todos")

  :config
  (setq org-project-capture-per-project-filepath "TODOs.org")
  (org-project-capture-per-project))

(use-package org-projectile
  :ensure t
  :after org-project-capture ; backend for projectile after org-project-capture
  :config
  (setq org-project-capture-default-backend
    (make-instance 'org-project-capture-projectile-backend)))

(use-package org-modern
  :ensure t
  :init
  (johnny/set-leader-keys-for-major-mode 'org-mode
    "Tm" 'org-modern-mode)
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

;; use org-modern instead
;; (use-package org-superstar
;;   :ensure t
;;   :hook (org-mode . org-superstar-mode))

(use-package verb
  :ensure t
  :init
  (johnny/declare-prefix-for-mode 'org-mode "r" "verb")
  (johnny/set-leader-keys-for-major-mode
    'org-mode
    "rf" #'verb-send-request-on-point
    "rs" #'verb-send-request-on-point-other-window
    "rr" #'verb-send-request-on-point-other-window-stay
    "rm" #'verb-send-request-on-point-no-window
    "rk" #'verb-kill-all-response-buffers
    "re" #'verb-export-request-on-point
    "ru" #'verb-export-request-on-point-curl
    "rb" #'verb-export-request-on-point-verb
    "rv" #'verb-set-var)
  :config
  (johnny/set-leader-keys-for-minor-mode
    'verb-response-body-mode
    "rr" #'verb-toggle-show-headers
    "rk" #'verb-kill-response-buffer-and-window
    "rf" #'verb-re-send-request)
  (johnny/set-leader-keys-for-major-mode
    'verb-response-headers-mode
    "rq" #'verb-kill-buffer-and-window))

;;; toc-org: Generate a table of contents in Org-mode buffers.
(use-package toc-org
  :ensure t
  :after org
  :hook (org-mode . toc-org-mode))

(provide 'init-org)
;;; init-org.el ends here
