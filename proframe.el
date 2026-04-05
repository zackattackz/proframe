;;; proframe.el --- One frame per project -*- lexical-binding: t; -*-

;; Author: Zachary Hanham
;; URL: https://github.com/zackattackz/proframe
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (beframe "1.5.0"))

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; An opinionated beframe powered workflow where each project gets its own
;; dedicated frame.

;;; Code:

(require 'beframe)
(require 'project)
(require 'cl-lib)

(defgroup proframe nil
  "One frame per project, powered by beframe."
  :group 'frames
  :prefix "proframe-")

(defcustom proframe-tidy-exclude-buffers '("acp-client-stderr")
  "Regexps for buffer names excluded from tidying."
  :type '(repeat regexp)
  :group 'proframe)

(defconst proframe-root-parameter 'proframe-root
  "Frame parameter name used to store the root.")

(defvar-local proframe--app-root nil
  "App root this buffer belongs to, set by `proframe-display-buffer-in-app-frame'.")

(defvar proframe-after-identify-functions nil
  "Hook run after a frame's identity changes.
Each function receives (FRAME ROOT) where ROOT is the new root
string, or nil if the frame was unassigned.")

(defun proframe-gui-frames ()
  "Return all top-level graphical frames, excluding child/popup frames."
  (cl-remove-if (lambda (f)
                  (or (not (display-graphic-p f))
                      (frame-parameter f 'parent-frame)))
                (frame-list)))

(defun proframe-root (&optional frame)
  "Return the root stored on FRAME, or nil."
  (frame-parameter frame proframe-root-parameter))

(defun proframe-find-frame (root)
  "Find a GUI frame dedicated to ROOT."
  (let ((root (if (file-directory-p root) (expand-file-name root) root)))
    (cl-find-if (lambda (f) (equal root (proframe-root f)))
                (proframe-gui-frames))))

(defun proframe-find-scratch-frame ()
  "Return the first GUI frame with no root assigned, or nil."
  (cl-find-if-not #'proframe-root (proframe-gui-frames)))

(defun proframe-fresh-scratch-frame-p (frame)
  "Return non-nil if FRAME is a scratch frame containing only global buffers."
  (and (null (proframe-root frame))
       (cl-every (lambda (buf)
                   (cl-some (lambda (pattern)
                              (string-match-p pattern (buffer-name buf)))
                            beframe-global-buffers))
                 (beframe-buffer-list frame))))

(defun proframe-focus-frame (frame)
  "Focus FRAME and raise it."
  (select-frame-set-input-focus frame))

(defun proframe-recompute-names ()
  "Rename frames, adding parent dirs to disambiguate duplicate project roots.
App frames (non-directory roots) use their root string directly."
  (let ((project-entries nil)
        (app-entries nil))
    (dolist (f (proframe-gui-frames))
      (when-let* ((r (proframe-root f)))
        (if (file-directory-p r)
            (push (cons f (file-name-split (directory-file-name r)))
                  project-entries)
          (push (cons f r) app-entries))))
    (dolist (entry project-entries)
      (let* ((frame (car entry))
             (parts (cdr entry))
             (n 1))
        (while (cl-some (lambda (other)
                          (and (not (eq frame (car other)))
                               (equal (last parts n) (last (cdr other) n))))
                        project-entries)
          (cl-incf n))
        (let ((name (string-join (last parts n) "/")))
          (unless (equal name (frame-parameter frame 'name))
            (beframe-rename-frame frame name)))))
    (dolist (entry app-entries)
      (let ((frame (car entry))
            (name (cdr entry)))
        (unless (equal name (frame-parameter frame 'name))
          (beframe-rename-frame frame name))))))

(defun proframe-update-identity (frame &optional root name)
  "Update FRAME's identity to ROOT (nil clears it).
NAME overrides the frame name when ROOT is nil (scratch frames).
Recomputes all frame names and runs `proframe-after-identify-functions'."
  (set-frame-parameter frame proframe-root-parameter root)
  (when (and (null root) name)
    (beframe-rename-frame frame name))
  (proframe-recompute-names)
  (run-hook-with-args 'proframe-after-identify-functions frame root))

(defun proframe-ensure-frame (root)
  "Focus or create a frame for ROOT.
ROOT may be a project directory or an app name.
Returns (FRAME . NEW-FRAME-P) where NEW-FRAME-P is non-nil if a
brand new frame was created (vs recycling a scratch frame)."
  (let* ((root (if (file-directory-p root) (expand-file-name root) root))
         (existing (proframe-find-frame root))
         (scratch (and (not existing)
                       (cl-find-if #'proframe-fresh-scratch-frame-p
                                   (proframe-gui-frames))))
         (new-p (and (not existing) (not scratch)))
         (target (or existing scratch (make-frame))))
    (proframe-focus-frame target)
    (unless existing
      (set-window-buffer (frame-selected-window target)
                         (get-buffer-create "*scratch*"))
      (proframe-update-identity target root)
      (beframe-unassume-all-buffers-no-prompts))
    (cons target new-p)))

(defun proframe-ensure-scratch-frame ()
  "Return an existing scratch frame, or create a new one."
  (or (proframe-find-scratch-frame)
      (let* ((display (or (getenv "DISPLAY") (getenv "WAYLAND_DISPLAY")))
             (frame (make-frame (when display `((display . ,display))))))
        (proframe-update-identity frame nil "*scratch*")
        (proframe-focus-frame frame)
        (set-window-buffer (frame-selected-window frame)
                           (get-buffer-create "*scratch*"))
        (beframe-unassume-all-buffers-no-prompts)
        frame)))

(defun proframe-open-scratch-frame ()
  "Focus the existing scratch frame, or create one.
Intended as the emacsclient -e target for the WM keybinding."
  (interactive)
  (proframe-focus-frame (proframe-ensure-scratch-frame)))

(defun proframe-buffer-root (buf)
  "Return the root for BUF - app name or project path, or nil."
  (with-current-buffer buf
    (or proframe--app-root
        (when-let* ((proj (project-current nil)))
          (expand-file-name (project-root proj))))))

(defun proframe--tidy-candidate-p (buf)
  "Return non-nil if BUF should be considered for frame tidying."
  (and (not (cl-some (lambda (pat) (string-match-p pat (buffer-name buf)))
                     (append beframe-global-buffers
                             proframe-tidy-exclude-buffers)))
       (or (buffer-file-name buf)
           (proframe-buffer-root buf))))

(defun proframe-buffer-home-frame (buf &optional create-p)
  "Return the frame where BUF should live.
Project buffers belong to their project frame; app buffers to their
app frame; other buffers belong to the scratch frame.  With
CREATE-P, create the frame if it doesn't exist.  Returns nil if no
suitable frame is found."
  (let ((root (proframe-buffer-root buf)))
    (or (if root
            (proframe-find-frame root)
          (proframe-find-scratch-frame))
        (when create-p
          (if root
              (car (proframe-ensure-frame root))
            (proframe-ensure-scratch-frame))))))

(defun proframe--tidy-buffer (buf source-frame &optional create-p display-p)
  "Move BUF to its home frame.
When SOURCE-FRAME is non-nil, unassume BUF from it and replace any
windows showing it.  With CREATE-P, create the home frame if needed.
With DISPLAY-P, show BUF in a newly created frame's window.
Returns non-nil if the buffer was moved."
  (let ((frame-root (proframe-root source-frame))
        (root (proframe-buffer-root buf)))
    (when (or (null source-frame)
              (and root (not (equal root frame-root)))
              (and (null root) frame-root))
      (let ((home (proframe-buffer-home-frame buf create-p)))
        (when (or source-frame home)
          (when source-frame
            (dolist (win (get-buffer-window-list buf nil source-frame))
              (set-window-buffer win (get-buffer-create "*scratch*")))
            (with-selected-frame source-frame
              (beframe--modify-buffer-list :unassume (list buf) :no-message)))
          (when home
            (with-selected-frame home
              (beframe--modify-buffer-list :assume (list buf) :no-message)
              (when (and display-p (not (eq home source-frame)))
                (set-window-buffer (frame-selected-window home) buf))))
          t)))))

(defun proframe-tidy-frame-buffers (&optional unassume-only)
  "Tidy stray buffers in the current frame.
Without prefix: move buffers to their frames, creating new frames as
needed.  With prefix UNASSUME-ONLY: just unassume buffers that don't
belong to this frame.
Also re-homes buffers that belong to no frame."
  (interactive "P")
  (let* ((source (selected-frame))
         (create-p (not unassume-only))
         (moved 0)
         (visible-bufs (mapcar #'window-buffer
                               (window-list source 'nomini))))
    (dolist (buf (beframe-buffer-list))
      (when (and (proframe--tidy-candidate-p buf)
                 (proframe--tidy-buffer buf source create-p
                                        (memq buf visible-bufs)))
        (cl-incf moved)))
    (when create-p
      (dolist (buf (cl-set-difference
                    (buffer-list)
                    (cl-mapcan #'beframe-buffer-list (proframe-gui-frames))))
        (when (and (proframe--tidy-candidate-p buf)
                   (proframe--tidy-buffer buf nil create-p nil))
          (cl-incf moved))))
    (message "Tidied %d buffer(s)" moved)))

(defun proframe-focus-frame-for-file (file)
  "Select the best frame for FILE.
Priority: project frame > scratch frame > new frame."
  (let ((proj (let ((default-directory
                     (if (file-directory-p file) file
                       (file-name-directory file))))
                (project-current nil))))
    (if proj
        (car (proframe-ensure-frame (project-root proj)))
      (proframe-ensure-scratch-frame))))

(defun proframe-display-buffer-in-app-frame (buffer alist)
  "Display BUFFER in a dedicated app frame.
ALIST key `app-name' specifies the frame identity (e.g. \"Gnus\").

Suitable for use in `display-buffer-alist':

  (add-to-list \\='display-buffer-alist
    \\='(\"\\\\*Group\\\\*\" (proframe-display-buffer-in-app-frame)
      (app-name . \"Gnus\")))"
  (let* ((name (alist-get 'app-name alist))
         (frame (or (proframe-find-frame name)
                    (car (proframe-ensure-frame name)))))
    (with-current-buffer buffer
      (setq proframe--app-root name))
    (let ((window (frame-selected-window frame)))
      (proframe-focus-frame frame)
      (window--display-buffer buffer window 'reuse alist))))

(defun proframe-switch-project (dir)
  "Switch the current frame to project DIR.
If a frame already exists for DIR, focus it instead.
Then run `project-switch-project' for command dispatch."
  (interactive (list (project-prompt-project-dir)))
  (let* ((root (expand-file-name dir))
         (existing (proframe-find-frame root)))
    (if existing
        (progn
          (proframe-focus-frame existing)
          (project-switch-project dir))
      (project-switch-project dir)
      (proframe-update-identity (selected-frame) root))))

(defun proframe-switch-project-other-frame (dir)
  "Open project DIR in a dedicated frame and run a project command.
Reuses an existing frame for DIR if one exists, otherwise creates one.
Cleans up a newly created frame if the user quits."
  (interactive (list (project-prompt-project-dir)))
  (pcase-let* ((root (expand-file-name dir))
               (`(,frame . ,new-p) (proframe-ensure-frame root)))
    (condition-case _
        (project-switch-project dir)
      (quit
       (when new-p (delete-frame frame))
       (signal 'quit nil)))))

(defun proframe--on-delete-frame (frame)
  "Clear root from FRAME before it is deleted."
  (set-frame-parameter frame proframe-root-parameter nil))

(defun proframe--after-delete-frame (_)
  "Recompute frame names after a frame is deleted."
  (proframe-recompute-names))

(defvar proframe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x p p") #'proframe-switch-project)
    (define-key map (kbd "C-x 5 p") #'proframe-switch-project-other-frame)
    map)
  "Keymap for `proframe-mode'.")

;;;###autoload
(define-minor-mode proframe-mode
  "One frame per project, powered by beframe."
  :global t
  :group 'proframe
  :keymap proframe-mode-map
  (if proframe-mode
      (progn
        (beframe-mode 1)
        (add-hook 'delete-frame-functions #'proframe--on-delete-frame)
        (add-hook 'after-delete-frame-functions
                  #'proframe--after-delete-frame))
    (remove-hook 'delete-frame-functions #'proframe--on-delete-frame)
    (remove-hook 'after-delete-frame-functions
                 #'proframe--after-delete-frame)))

(provide 'proframe)
;;; proframe.el ends here
