;;; funcs.el --- Functions for the `persp-mode' package -*- lexical-binding: t; -*-

(defun johnny//current-persp-name ()
  "Get name of the current perspective."
  (safe-persp-name (get-frame-persp)))

(defun johnny//persp-autosave ()
  "Perspectives mode autosave.
Autosaves perspectives every `persp-autosave-interal' seconds.
Cancels autosave on exiting perspectives mode."
  (if (and persp-mode johnny/persp-enable-autosave)
    (progn
      (message "Perspectives mode autosaving enabled.")
      (setq johnny--persp-autosave-timer
        (run-with-timer
          johnny/persp-autosave-delay
          johnny/persp-autosave-delay
          (lambda ()
            (message "Saving perspectives to file.")
            (persp-save-state-to-file)))))
    (when johnny--persp-autosave-timer
      (cancel-timer johnny--persp-autosave-timer)
      (setq johnny--persp-autosave-timer nil))))

(defun johnny/jump-to-last-persp ()
  "Open the previously selected perspective, if it exists."
  (interactive)
  (unless (eq 'non-existent
            (gethash persp-last-persp-name
              *persp-hash* 'non-existent))
    (persp-switch persp-last-persp-name)))

(defun johnny//persp-format-name (name pos)
  "Format the perspective name given by NAME for display in mode-line."
  (let* ((persp-name (if (file-directory-p name)
                       (file-name-nondirectory (directory-file-name name))
                       name))
          (string-name (format "%s" persp-name))
          (current (equal name (johnny//current-persp-name)))
          (caption (concat (number-to-string (if (eq 9 pos) 0 (1+ pos)))
                     ":" string-name)))
    (if current
      (propertize (concat "[" caption "]") 'face 'warning)
      caption)))

(defun johnny//persp-hydra-toggle-hint ()
  "Toggle the full hint docstring for the perspectives hydra."
  (interactive)
  (setq johnny--persp-hydra-full-hint-toggle
    (not johnny--persp-hydra-full-hint-toggle)))

(defun johnny//persp-hydra-hint ()
  "Return a one liner string containing all the perspective names."
  (let* ((persp-list (or (persp-names-current-frame-fast-ordered)
                       (list persp-nil-name)))
          (formatted-persp-list
            (concat " "
              (mapconcat (lambda (persp)
                           (johnny//persp-format-name
                             persp (cl-position persp persp-list)))
                persp-list " | "))))
    (concat
      formatted-persp-list
      (if johnny--persp-hydra-full-hint-toggle
        johnny--persp-hydra-full-hint
        (concat "  (["
          (propertize "?" 'face 'hydra-face-red)
          "] help)")))))

(defmacro johnny||switch-persp (name &rest props)
  "Switch to the perspective called NAME.

Available PROPS:

`:init EXPRESSIONS'
    One or more forms, which will be evaluated after switching to perspective
    NAME if the perspective did not already exist."
  (declare (indent 1))
  (let ((init (plist-get props :init)))
    `(let ((persp-reset-windows-on-nil-window-conf t)
            (persp-already-exists (persp-with-name-exists-p ,name)))
       (persp-switch ,name)
       (unless persp-already-exists
         ,@init))))

(defun johnny/persp-switch-by-pos (pos)
  "Switch to perspective of position POS.
If POS has no layout, ask the user if a new layout should be created."
  (let ((persp-to-switch
          (nth pos (persp-names-current-frame-fast-ordered))))
    (if persp-to-switch
      (persp-switch persp-to-switch)
      (let ((persp-reset-windows-on-nil-window-conf t)))
      (persp-switch nil))))              ; create a new layout

;; Define all `johnny/persp-switch-to-X' functions
(dolist (i (number-sequence 9 0 -1))
  (eval `(defun ,(intern (format "johnny/persp-switch-to-%s" i)) nil
           ,(format "Switch to perspective %s.\n%s"
              i "See `johnny/persp-switch-by-pos' for details.")
           (interactive)
           (johnny/persp-switch-by-pos ,(if (eq 0 i) 9 (1- i))))))

(defun johnny/persp-switch-to (pos)
  "Switch to perspective but ask for POS.
If POS has no perspective, ask the user if a new perspective should be created."
  (interactive "NPerspective to switch to/create: ")
  (johnny/persp-switch-by-pos (1- pos)))

(defvar johnny/org-persp-startup-file "~/org/notes.org"
  "Open file by default in @Org perspective")

;; original without johnny||switch-persp
;; (defun johnny/persp-switch-to-org ()
;;   "Switch to @Org perspective"
;;   (interactive)
;;   (let ((initialize (not (persp-with-name-exists-p "@Org"))))
;;     (persp-switch "@Org")
;;     (when initialize
;;       (delete-other-windows)
;;       (find-file johnny/org-persp-startup-file))))

(defun johnny/persp-switch-to-org ()
  "Switch to @Org perspective"
  (interactive)
  (johnny||switch-persp "@Org"
    :init
    (delete-other-windows)
    (find-file johnny/org-persp-startup-file)))

(defun johnny/persp-goto-default ()
  "Go to default perspective"
  (interactive)
  (persp-switch pers-nil-name))

(defun johnny/persp-hydra-rename ()
  "Rename a persp and get back to the perspectives hydra."
  (interactive)
  (call-interactively 'persp-rename)
  (hydra-persp/body))

(defun johnny/persp-hydra-close ()
  "Kill current perspective"
  (interactive)
  (persp-kill-without-buffers (johnny//current-persp-name)))

(defun johnny/persp-hydra-close-other ()
  "Kill all perspectives except the current one."
  (interactive)
  ;; TODO implement using consult
  (message "Not implemented"))

(defun johnny/persp-hydra-kill ()
  "Kill current perspective"
  (interactive)
  (persp-kill (johnny//current-persp-name)))

(defun johnny/persp-hydra-kill-other ()
  "Kill all perspectives except the current one."
  (interactive)
  ;; TODO implement using consult
  (message "Not implemented"))

(defun johnny/move-element-left (element list)
  "Move ELEMENT one step to the left in LIST."
  (let (value)
    (dolist (name list value)
      (if (and (equal name element) (car value))
        (setq value (cons (car value) (cons name (cdr value))))
        (setq value (cons name value))))
    (nreverse value)))

(defun johnny/move-element-right (element list)
  "Move ELEMENT one step to the right in LIST."
  (nreverse (johnny/move-element-left element (reverse list))))

(defun johnny/move-current-persp-right ()
  "Moves the current perspective one step to the right"
  (interactive)
  (setq persp-names-cache (johnny/move-element-right
                            (johnny//current-persp-name)
                            persp-names-cache)))

(defun johnny/move-current-persp-left ()
  "Moves the current perspective one step to the left"
  (interactive)
  (setq persp-names-cache (johnny/move-element-left
                            (johnny//current-persp-name)
                            persp-names-cache)))

(defun johnny/consult-buffer-persp ()
  "`consult-buffer' with buffers provided by persp."
  (interactive)
  (consult-buffer johnny-consult-buffer-sources))

(defcustom johnny-consult-buffer-sources
  '(consult--source-hidden-buffer
     johnny--source-buffers-hidden
     johnny--source-persp-buffers
     johnny--source-persp-modified-buffers
     johnny--source-other-persp-modified-buffers
     consult--source-recent-file
     consult--source-bookmark
     johnny--source-persp-project-buffers
     johnny--source-other-persp-project-buffers
     consult--source-project-recent-file-hidden
     johnny--source-window-buffers
     johnny--source-workspace-buffers)
  "Sources used by `johnny/consult-buffer-persp'
when persp-mode is used.
See also `consult-buffer-sources'.
See `consult--multi' for a description
of the source data structure."
  :type '(repeat symbol))

  ;;; consult sources for `johnny/consult-buffer-persp'

(defvar johnny--source-buffers-hidden nil
  "Like `consult--source-buffer' but hidden by default
and with narrowing key \"B\".")
(with-eval-after-load 'consult
  (setq johnny--source-buffers-hidden
    `(:name "Buffers (all perspectives)"
       :hidden t
       :narrow (?B . "Buffers")
       ,@consult--source-buffer)))

(defvar johnny--source-persp-modified-buffers
  `(:name "Modified Buffer (current perspective)"
     :narrow   (?* . "Modified Buffer")
     :hidden   t
     :category buffer
     :face     consult-buffer
     :history  buffer-name-history
     :state    ,#'consult--buffer-state
     :items
     ,(lambda ()
        (consult--buffer-query ;; :sort 'visibility
          :predicate (lambda (buff)
                       (and (persp-contain-buffer-p buff)
                         (buffer-file-name buff)
                         (buffer-modified-p buff)))
          ;; :directory 'project
          :as #'consult--buffer-pair)))
  "Modified buffer (current perspective) candidate source for `consult-buffer'.")

(defvar johnny--source-other-persp-modified-buffers
  `(:name "Modified Buffer (other perspectives)"
     :narrow   (?* . "Modified Buffer")
     :hidden   t
     :category buffer
     :face     consult-buffer
     :history  buffer-name-history
     :state    ,#'consult--buffer-state
     :items
     ,(lambda ()
        (consult--buffer-query
          :predicate (lambda (buff)
                       (and (not (persp-contain-buffer-p buff))
                         (buffer-file-name buff)
                         (buffer-modified-p buff)))
          :as #'consult--buffer-pair)))
  "Modified buffer (other perspectives) candidate source for `consult-buffer'.")

(defvar johnny--source-persp-buffers
  `(:name     "Perspective Buffer"
     :narrow   ?b
     :category buffer
     :face     consult-buffer
     :history  buffer-name-history
     :state    ,#'consult--buffer-state
     :default  t
     :items
     ,(lambda ()
        (consult--buffer-query
          :sort 'visibility
          :predicate #'persp-contain-buffer-p
          :as #'consult--buffer-pair)))
  "Perspective buffer candidate source for `consult-buffer'.")

(defvar johnny--source-persp-project-buffers
  `(:name     "Project Buffer (current perspective)"
     :hidden t
     :narrow   (?p . "Project")
     :category buffer
     :face     consult-buffer
     :history  buffer-name-history
     :state    ,#'consult--buffer-state
     :enabled  ,(lambda () consult-project-function)
     :items
     ,(lambda ()
        (when-let* ((root (consult--project-root)))
          (consult--buffer-query
            :sort 'visibility
            :directory root
            :predicate #'persp-contain-buffer-p
            :as #'consult--buffer-pair))))
  "Project buffer (current perspective) candidate source for `consult-buffer'.")

(defvar johnny--source-other-persp-project-buffers
  `(:name     "Project Buffer (other perspectives)"
     :hidden t
     :narrow   (?p . "Project")
     :category buffer
     :face     consult-buffer
     :history  buffer-name-history
     :state    ,#'consult--buffer-state
     :enabled  ,(lambda () consult-project-function)
     :items
     ,(lambda ()
        (when-let* ((root (consult--project-root)))
          (consult--buffer-query
            :sort 'visibility
            :directory root
            :predicate (lambda (buf) (not (persp-contain-buffer-p buf)))
            :as #'consult--buffer-pair))))
  "Project buffer (other perspectives) candidate source for `consult-buffer'.")

(defvar johnny--source-window-buffers
  `(:name     "Window Buffer"
     :hidden   t
     :narrow   ?w
     :category buffer
     :face     consult-buffer
     :history  buffer-name-history
     :state    ,#'consult--buffer-state
     :default  t
     :items
     ,(lambda ()
        (let* ((prev-buffers (delq (window-buffer) (mapcar #'car (window-prev-buffers))))
                (next-buffers (window-next-buffers))
                (buffers
                  (cl-remove-if-not
                    #'buffer-live-p
                    (if vertico-cycle
                      ;; If cycling is enabled, this order makes sense:
                      ;; One can move down to previous buffers,
                      ;; and move up to next buffers.
                      (append (list (window-buffer))
                        (seq-difference prev-buffers next-buffers)
                        (nreverse next-buffers))
                      ;; Note that next-buffers is a subset of prev-buffers.
                      (cons (window-buffer) prev-buffers)))))
          (consult--buffer-query
            :sort nil
            :filter nil
            :as #'consult--buffer-pair
            :buffer-list buffers))))
  "Window buffer candidate source for `consult-buffer'.
It contains all buffers previously displayed in the selected
window, including buffers from different perspectives and hidden
buffers.")

(defvar johnny--source-workspace-buffers
  `(:name     "Workspace Buffer"
     :hidden   t
     :narrow   ?k
     :category buffer
     :face     consult-buffer
     :history  buffer-name-history
     :state    ,#'consult--buffer-state
     :default  t
     :items
     ,(lambda ()
        (let (prev-buffers)
          (walk-windows
            (lambda (win)
              (setq prev-buffers
                (append (list (window-buffer win))
                  (mapcar #'car (window-prev-buffers win))
                  prev-buffers)))
            'no-minibuffer)
          (consult--buffer-query
            :sort 'visibility
            :filter nil
            :as #'consult--buffer-pair
            :predicate (lambda (buf)
                         (member buf prev-buffers))))))
  "Workspace buffer candidate source for `consult-buffer'.
It contains all buffers previously displayed in a live window of
the current window configuration, including buffers from
different perspectives and hidden buffers.")

;;; projectile support

(defmacro johnny||switch-project-persp (name &rest body)
  "Switch to persp and execute BODY with hook to add project buffers.

Switch to perspective NAME, and then evaluate the forms in BODY.
If the perspective did not already exist, then BODY will be
evaluated with `projectile-after-switch-project-hook' bound to
add a hook that adds the current project's buffers to the
perspective.  If the user quits during the evaluation of BODY,
the new perspective will be killed."
  (declare (indent 1))
  `(let ((projectile-after-switch-project-hook
          projectile-after-switch-project-hook))
     (johnny||switch-persp ,name
       :init
       ((add-hook 'projectile-after-switch-project-hook
           (lambda ()
             (let ((persp (persp-get-by-name ,name)))
               (when (persp-p persp)
                 (persp-add-buffer (projectile-project-buffers
                                     (expand-file-name ,name))
                   persp nil nil)))))
         (condition-case nil
           (progn
             ,@body)
           (quit (persp-kill-without-buffers ,name)))))))

(defun johnny/persp-switch-project (arg)
  "Select a project prospective using consult."
  (interactive "P")
  (let* ((current-project-maybe (if (projectile-project-p)
                                    (abbreviate-file-name (projectile-project-root))
                                  nil))
         (project (completing-read
                   "Switch to Project Perspective: "
                   (if current-project-maybe
                       (cons current-project-maybe projectile-known-projects)
                     projectile-known-projects)
                   nil
                   nil
                   nil
                   nil
                   current-project-maybe)))
    (johnny||switch-project-persp project
      (let ((projectile-switch-project-action (if (string= project current-project-maybe)
                                                  (lambda () nil)
                                                projectile-switch-project-action)))
        (projectile-switch-project-by-name project arg)))))

;;; perspective local variables

(defvar johnny--persp-local-variables nil
  "List of variables that will be local to the current perspective.")

(defvar johnny--persp-local-map (make-hash-table :test 'equal)
  "Map of perspective to their local variable values.")

(defun johnny/make-variable-persp-local (&rest vars)
  "Make variables become perspective-local whenever they are set.
Accepts a list of VARIABLE, DEFAULT-VALUE pairs.

(johnny/make-variable-persp-local 'foo 1 'bar 2)"
  (cl-loop for (symbol default-value) on vars by 'cddr
    do (add-to-list 'johnny--persp-local-variables (cons symbol default-value))))

(defun johnny//load-persp-local-vars (persp-name &rest _)
  "Load the perspective-local values of variables for PERSP-NAME."
  (let ((persp-local-vars (-filter 'boundp
                            (-map 'car
                              johnny--persp-local-variables))))
    ;; save the current perspective
    (puthash (johnny//current-persp-name)
      (--map (cons it (symbol-value it))
        persp-local-vars)
      johnny--persp-local-map)
    ;; load the default values into the new perspective
    (--each persp-local-vars
      (set it (alist-get it johnny--persp-local-variables)))
    ;; override with the previously bound values for the new perspective
    (--when-let (gethash persp-name johnny--persp-local-map)
      (-each it
        (-lambda ((var . val)) (set var val))))))
