;;; embark-consult.el --- Consult integration for Embark -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience
;; Package-Version: 20210111.1502
;; Package-Commit: 296ad8077092ff839029c0bdb53c279e921e5354
;; Version: 0.1
;; Homepage: https://github.com/oantolin/embark
;; Package-Requires: ((emacs "25.1") (embark "0.9") (consult "0.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides integration between Embark and Consult.

;; For now, it only provides access to Consult preview from
;; auto-updating Embark Collect buffer that is associated to an active
;; minibuffer for a Consult command:

;; - `embark-consult-preview-at-point', a command to trigger Consult's
;; preview for the entry at point.

;; - `embark-consult-preview-minor-mode', a minor mode for Embark
;; Collect buffers that automatically previous the entry at point as
;; you move around.

;; Eventually all Consult-related functionality in Embark will be
;; moved to this package.

;;; Code:

(require 'embark)
(require 'consult)

;;; Consult preview

(defvar-local embark-consult-preview--last-entry nil
  "Stores last entry previewed.")

(defun embark-consult-preview--preconditions ()
  "Check if Consult preview for Embark can be used in current buffer.
Signal an error unless current buffer is an auto-updating Embark
Collect buffer that is associated to an active minibuffer for a
Consult command."
  (unless (derived-mode-p 'embark-collect-mode)
    (user-error "Not in an Embark Collect buffer"))
  (unless (and (active-minibuffer-window)
               (eq (window-buffer (active-minibuffer-window))
                   embark-collect-from))
    (user-error
     "This Embark Collect buffer is not associated to an active minibuffer"))
  (unless (buffer-local-value 'consult--preview-function embark-collect-from)
    (user-error "No Consult preview function found")))

(defun embark-consult-preview--trigger ()
  "Trigger Consult preview for entry at point if different from previous."
  (let ((entry (ignore-errors (button-label (point))))) ; error at eob
    (unless (equal entry embark-consult-preview--last-entry)
      (setq embark-consult-preview--last-entry entry)
      (with-selected-window (active-minibuffer-window)
        (funcall consult--preview-function (minibuffer-contents) entry)))))

;;;###autoload
(defun embark-consult-preview-at-point ()
  "Trigger Consult preview for Embark Collect entry at point.
Must be run from an auto-updating Embark Collect buffer that is
associated to an active minibuffer for a Consult command."
  (interactive)
  (condition-case err
      (progn
        (embark-consult-preview--preconditions)
        (embark-consult-preview--trigger))
    (user-error
     (embark-consult-preview-minor-mode -1)
     (message "Turning off preview: %s" (cadr err)))))

;;;###autoload
(define-minor-mode embark-consult-preview-minor-mode
  "Minor mode to use Consult preview as you move around.
Must be used in an auto-updating Embark Collect buffer that is
associated to an active minibuffer for a Consult command."
  :init-value nil
  :lighter " Preview"
  (remove-hook 'post-command-hook #'embark-consult-preview-at-point t)
  (condition-case nil
      (progn
        (embark-consult-preview--preconditions)
        (when embark-consult-preview-minor-mode
          (add-hook 'post-command-hook
                    #'embark-consult-preview-at-point nil t)))
    (user-error (setq embark-consult-preview-minor-mode nil))))

(provide 'embark-consult)
;;; embark-consult.el ends here
