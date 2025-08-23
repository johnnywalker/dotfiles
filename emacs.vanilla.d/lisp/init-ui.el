;;; init-ui.el --- User interface config. -*- lexical-binding: t -*-

;;; Commentary:

;; User interface settings.

;;; Code:

;; Set the default font
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 105)
(set-face-attribute 'fixed-pitch nil :family 'unspecified) ; Use the default font for fixed-pitch text

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Hide the menu bar by default (if GUI is available)
(when (display-graphic-p)
  (menu-bar-mode -1))

;; Show the tab-bar as soon as tab-bar functions are invoked
(setopt tab-bar-show 1)


;; set window title with "project | filename | Emacs"
(setq frame-title-format
  (setq icon-title-format
    '((:eval
         (format "%s | %s | Emacs"
           (projectile-project-name)
           (abbreviate-file-name (or (buffer-file-name) (buffer-name))))))))

;; ;; Show the time in the modeline
;; (setopt display-time-format "%a %F %T")
;; (setopt display-time-interval 1)
;; (display-time-mode)

;; ;; Add the time to the tab-bar, if visible
;; (add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
;; (add-to-list 'tab-bar-format 'tab-bar-format-global 'append)

(setopt display-time-default-load-average nil)     ; this information is useless for most

;; Mode line information
(setopt line-number-mode t)                        ; Show current line in modeline
;; Leave column number off for now
;; (setopt column-number-mode t)                      ; Show column as well

(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Enable winner mode to restore window configurations
(winner-mode 1)

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;; We won't set these, but they're good to know about
;;
;; (setopt indent-tabs-mode nil)
;; (setopt tab-width 4)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling

;; For terminal users, make the mouse more useful

(xterm-mouse-mode 1)

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)           ; Set a minimum width

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Highlight the current line
(global-hl-line-mode t)
;; ;; Modes to highlight the current line with
;; (let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
;;   (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; Support hiding some modes from the mode line
(use-package diminish
  :ensure t)

(provide 'init-ui)
;;; init-ui.el ends here
