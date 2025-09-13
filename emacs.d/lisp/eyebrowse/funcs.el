;;; funcs.el --- Functions for the `eyebrowse' package -*- lexical-binding: t -*-

(defun johnny//workspace-get-used-slots ()
  (mapcar 'car (eyebrowse--get 'window-configs)))

(defun johnny//workspace-next-free-slot ()
  "Get the next free workspace slot."
  (eyebrowse-free-slot (johnny//workspace-get-used-slots)))

(defun johnny/clone-workspace ()
  "Clone the current workspace."
  (interactive)
  (let ((eyebrowse-new-workspace nil) ; nil = clone current workspace
         (current-slot (eyebrowse--get 'current-slot))
         (next-free-slot (johnny//workspace-next-free-slot)))
    (eyebrowse-switch-to-window-config next-free-slot)
    (message "Workspace %s cloned to %s" current-slot next-free-slot)))

(defun johnny/new-workspace (&optional slot)
  "Create a new workspace, showing the home buffer.
If a optional SLOT (number) was provided,
then create the new workspace at that slot.
Otherwise create it at the next free slot."
  (let ((eyebrowse-new-workspace 'johnny/home)
         (slot (or slot (johnny//workspace-next-free-slot))))
    (eyebrowse-switch-to-window-config slot)
    (message "Workspace %s created" slot)))

(defun johnny/single-win-workspace ()
  "Create a new single window workspace,
showing the home buffer."
  (interactive)
  (johnny/new-workspace))

;; Define all `johnny/workspace-switch-to-X' functions
(dolist (i (number-sequence 9 0 -1))
  (eval `(defun ,(intern (format "johnny/workspace-switch-to-%s" i)) nil
           ,(format "Switch to workspace %s.\n%s"
              i "See `johnny/workspace-switch-by-pos' for details.")
           (interactive)
           (johnny/workspace-switch-by-pos ,(if (eq 0 i) 9 (1- i))))))

(defun johnny/workspace-switch-or-create (slot)
  "Given a workspace SLOT number.
If SLOT is current, show a message.
If SLOT exists, switch to it.
Otherwise create a new workspace at the next free slot."
  (let* ((slot-current-p (= slot (eyebrowse--get 'current-slot)))
          (slot-exists-p (and (not slot-current-p)
                           (memq slot (johnny//workspace-get-used-slots)))))
    (cond (slot-current-p (message "Already on Workspace: %s" slot))
      (slot-exists-p (eyebrowse-switch-to-window-config slot)
        (message "Workspace switched to: %s" slot))
      (t (johnny/new-workspace slot)))))

(defun johnny/workspace-close ()
  (interactive)
  (let ((current-workspace (eyebrowse--get 'current-slot))
         (last-workspace-p (= (length (eyebrowse--get 'window-configs)) 1)))
    (if last-workspace-p
      (message "The last workspace can not be closed")
      (eyebrowse-close-window-config)
      (message "Workspace %s closed" current-workspace))))

(defun johnny/workspace-rename ()
  "Rename a workspace and get back to hydra."
  (interactive)
  (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) nil)
  (hydra-workspaces/body))

(defun johnny//workspace-format-name (workspace)
  "Return a propertized string given a WORKSPACE name."
  (let* ((current (eq (eyebrowse--get 'current-slot) (car workspace)))
          (name (nth 2 workspace))
          (number (car workspace))
          (caption (if (< 0 (length name))
                     (concat (int-to-string number) ":" name)
                     (int-to-string number))))
    (if current
      (propertize (concat "[" caption "]") 'face 'warning)
      caption)))

(defun johnny//workspaces-hydra-hint ()
  "Return a one liner string containing all the workspace names."
  (concat
    " "
    (mapconcat 'johnny//workspace-format-name
      (eyebrowse--get 'window-configs) " | ")
    (if johnny--workspaces-hydra-full-hint-toggle
      johnny--workspaces-hydra-full-hint
      (concat "  (["
        (propertize "?" 'face 'hydra-face-red)
        "] help)"))))

(defun johnny//workspaces-hydra-toggle-hint ()
  "Toggle the full hint docstring for the workspaces hydra."
  (interactive)
  (setq johnny--workspaces-hydra-full-hint-toggle
    (not johnny--workspaces-hydra-full-hint-toggle)))

;;; Buffer utilities

;; Eyebrowse uses window-state objects (as returned by `window-state-get') to
;; store window configurations, so here are some utility functions to help us
;; analyse window-states.
;; it might make more sense to move these functions to a more general place

(defun johnny/window-state-window-p (object)
  "Return t if OBJECT is a window, as represented in window-state objects.
Note: this function doesn't test for real window objects, but for
representations of a window in a window-state object as returned by
`window-state-get'."
  (and (listp object)
    (memq (car object) '(leaf vc hc))))

(defun johnny/window-state-get-buffer (window)
  "Get WINDOW's buffer.
WINDOW is the representation of a window in a window-state object.
The returned value is the representation of a buffer in a window-state
object."
  (cdr (assq 'buffer window)))

(defun johnny/window-state-get-buffer-name (window)
  "Get WINDOW's buffer's name.
WINDOW is the representation of a window in a window-state object."
  (car (johnny/window-state-get-buffer window)))

(defun johnny/window-state-walk-windows-1 (window fn)
  "Helper function for `johnny/window-state-walk-windows'."
  ;; WINDOW is a misleading name. WINDOW is a list that can represent a window,
  ;; or a concatenation of several windows. window-state objects are weird.
  (let ((child-windows
          (-filter #'johnny/window-state-window-p window))
         (bare-window
           ;; if WINDOW contains more than one window, take only the first window
           (--take-while (not (johnny/window-state-window-p it))
             window)))
    (--each child-windows
      (johnny/window-state-walk-windows-1 it fn))
    (push (funcall fn bare-window) result)))

(defun johnny/window-state-walk-windows (state fn)
  "Execute FN once for each window in STATE and make a list of the results.
FN is a function to execute.
STATE is a window-state object."
  (defvar result) ;; use dynamic binding
  (let (result)
    (johnny/window-state-walk-windows-1 (cdr state) fn)
    result))

(defun johnny/window-state-all-windows (state)
  "Get all windows contained in STATE.
STATE is a window-state object.
The returned windows are not actual window objects. They are windows as
represented in window-state objects."
  (johnny/window-state-walk-windows state #'identity))

(defun johnny/window-state-get-buffer-names (state)
  "Get names of all buffers saved in STATE.
STATE is a window-state object as returned by `window-state-get'."
  (delq nil (johnny/window-state-walk-windows state #'johnny/window-state-get-buffer-name)))

(defun johnny/window-state-get-buffers (state)
  "Get all buffers saved in STATE.
STATE is a window-state object as returned by `window-state-get'."
  ;; delq nil - removes buffers stored in STATE that don't exist anymore
  (delq nil (mapcar #'get-buffer (johnny/window-state-get-buffer-names state))))

(defun johnny/find-workspace (buffer)
  "Find Eyebrowse workspace containing BUFFER.
 If several workspaces contain BUFFER, return the first one. Workspaces are
 ordered by slot number.
 If no workspace contains
 BUFFER, return nil."
  ;; the second element of a workspace is its window-state object
  (--find (memq buffer (johnny/window-state-get-buffers (cadr it)))
    (eyebrowse--get 'window-configs)))

(defun johnny/display-in-workspace (buffer alist)
  "Display BUFFER's workspace.
 Return BUFFER's window, if exists, otherwise nil.
 If BUFFER is already visible in current workspace, just return its window
 without switching workspaces."
  (or (get-buffer-window buffer)
    (-when-let (workspace (johnny/find-workspace buffer))
      (eyebrowse-switch-to-window-config (car workspace))
      (get-buffer-window buffer))))

(defun johnny/goto-buffer-workspace (buffer)
  "Switch to BUFFER's window in BUFFER's workspace.
 If BUFFER isn't displayed in any workspace, display it in the current
 workspace, preferably in the current window."
  (interactive "B")
  (pop-to-buffer buffer '((;; reuse buffer window from some workspace
                            johnny/display-in-workspace
                            ;; fallback to display in current window
                            display-buffer-same-window)
                           (inhibit-same-window . nil))))

;; Eyebrowse and Persp integration

(defun johnny//get-persp-workspace (&optional persp frame)
  "Get the correct workspace parameters for perspective.
PERSP is the perspective, and defaults to the default perspective.
FRAME is the frame where the parameters are expected to be used, and
defaults to the current frame."
  (let ((param-names (if (display-graphic-p frame)
                         '(gui-eyebrowse-window-configs
                           gui-eyebrowse-current-slot
                           gui-eyebrowse-last-slot)
                       '(term-eyebrowse-window-configs
                         term-eyebrowse-current-slot
                         term-eyebrowse-last-slot))))
    (--map (persp-parameter it persp) param-names)))

(defun johnny//set-persp-workspace (workspace-params &optional persp frame)
  "Set workspace parameters for perspective.
WORKSPACE-PARAMS should be a list containing 3 elements in this order:
- window-configs, as returned by (eyebrowse--get 'window-configs)
- current-slot, as returned by (eyebrowse--get 'current-slot)
- last-slot, as returned by (eyebrowse--get 'last-slot)
PERSP is the perspective, and defaults to the current perspective.
FRAME is the frame where the parameters came from, and defaults to the
current frame.

Each perspective has two sets of workspace parameters: one set for
graphical frames, and one set for terminal frames."
  (let ((param-names (if (display-graphic-p frame)
                         '(gui-eyebrowse-window-configs
                           gui-eyebrowse-current-slot
                           gui-eyebrowse-last-slot)
                       '(term-eyebrowse-window-configs
                         term-eyebrowse-current-slot
                         term-eyebrowse-last-slot))))
    (--zip-with (set-persp-parameter it other persp)
                param-names workspace-params)))

(defun johnny/load-eyebrowse-for-perspective (type &optional frame persp)
  "Load an eyebrowse workspace according to a perspective's parameters.
If the perspective doesn't have a workspace, create one.

See the hook `persp-activated-functions'."
  (when (eq type 'frame)
    (let* ((workspace-params (johnny//get-persp-workspace
                              (or persp (get-frame-persp frame)) frame))
           (window-configs (nth 0 workspace-params))
           (current-slot (nth 1 workspace-params))
           (last-slot (nth 2 workspace-params)))
      (if window-configs
          (progn
            (eyebrowse--set 'window-configs window-configs frame)
            (eyebrowse--set 'current-slot current-slot frame)
            (eyebrowse--set 'last-slot last-slot frame)
            (eyebrowse--load-window-config current-slot))
        (eyebrowse--set 'window-configs nil frame)
        (eyebrowse-init frame)
        (johnny/save-eyebrowse-for-perspective frame)))))

(defun johnny/load-eyebrowse-after-loading-persp (_state-file _phash persp-names)
  "Bridge between `persp-after-load-state-functions' and
`johnny/load-eyebrowse-for-perspective'.

_PHASH is the hash were the loaded perspectives were placed, and
PERSP-NAMES are the names of these perspectives."
  (let ((cur-persp (get-current-persp)))
    ;; load eyebrowse for current perspective only if it was one of the loaded
    ;; perspectives
    (when (member (or (and cur-persp (persp-name cur-persp))
                      persp-nil-name)
                  persp-names)
      (johnny/load-eyebrowse-for-perspective 'frame))))

(defun johnny/update-eyebrowse-for-perspective (&rest _args)
  "Update and save current frame's eyebrowse workspace to its perspective."
  (let* ((current-slot (eyebrowse--get 'current-slot))
         (current-tag (nth 2 (assoc current-slot (eyebrowse--get 'window-configs)))))
    (eyebrowse--update-window-config-element
     (eyebrowse--current-window-config current-slot current-tag)))
  (johnny/save-eyebrowse-for-perspective))

(defun johnny/save-eyebrowse-for-perspective (&optional frame)
  "Save FRAME's eyebrowse workspace to FRAME's perspective.
FRAME defaults to the current frame."
  (johnny//set-persp-workspace (list (eyebrowse--get 'window-configs frame)
                                        (eyebrowse--get 'current-slot frame)
                                        (eyebrowse--get 'last-slot frame))
                                  (get-frame-persp frame)
                                  frame))

(defun johnny//fixup-window-configs (orig-fn newname &optional unique)
  "Update the buffer's name in the eyebrowse window-configs of all perspectives."
  (let* ((old (buffer-name))
         (new (funcall orig-fn newname unique)))
    (dolist (persp (persp-persps))
      (dolist (window-config
               (append (persp-parameter 'gui-eyebrowse-window-configs persp)
                       (persp-parameter 'term-eyebrowse-window-configs persp)))
        (eyebrowse--rename-window-config-buffers window-config old new)))
    new))
