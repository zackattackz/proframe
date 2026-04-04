;;; proframe-consult.el --- Consult integration for proframe -*- lexical-binding: t; -*-

;; Author: Zachary Hanham
;; URL: https://github.com/zackattackz/proframe
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (beframe "1.5.0") (consult "1.0"))

;;; Commentary:

;; Optional consult buffer source scoped to the current frame.

;;; Code:

(require 'beframe)
(require 'consult)

(defface proframe-consult-buffer
  '((t :inherit font-lock-string-face))
  "Face for frame-scoped buffers in `consult'.")

(defvar proframe-consult-source
  `( :name     "Frame-specific buffers (current frame)"
     :narrow   ?F
     :category buffer
     :face     proframe-consult-buffer
     :history  beframe-history
     :items    ,(lambda ()
                  (beframe-buffer-names nil :sort #'beframe-buffer-sort-visibility))
     :action   ,#'switch-to-buffer
     :state    ,#'consult--buffer-state)
  "Consult buffer source for frame-scoped buffers.")

;;;###autoload
(defun proframe-consult-setup ()
  "Add `proframe-consult-source' to `consult-buffer-sources'."
  (add-to-list 'consult-buffer-sources 'proframe-consult-source))

(provide 'proframe-consult)
;;; proframe-consult.el ends here
