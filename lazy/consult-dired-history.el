;;; ivy-dired-history.el --- use ivy to open recent directories

;; Author: 纪秀峰 <jixiuf@gmail.com>
;; Copyright (C) 2017 纪秀峰, all rights reserved.
;; Created:  2017-06-14
;; Version: 1.0
;; X-URL:https://github.com/jixiuf/ivy-dired-history
;; Package-Requires: ((ivy "0.9.0")(counsel "0.9.0")(cl-lib "0.5"))
;;
;; Features that might be required by this library:
;;
;; `ivy' `counsel'
;;
;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; use `ivy' to open recent directories.

;; it is integrated with `dired-do-copy' and `dired-do-rename'.
;; when you press C (copy) or R (rename) , it is excellent to
;; allow users to select a directory from the recent dired history .



;;; Installation:

;; (require 'savehist)
;; (add-to-list 'savehist-additional-variables 'consult-dired-history)
;; (savehist-mode 1)

;; or if you use desktop-save-mode
;; (add-to-list 'desktop-globals-to-save 'consult-dired-history)


;; (with-eval-after-load 'dired
;;   (require 'consult-dired-history)
;; ;; if you are using ido,you'd better disable ido for dired
;; ;; (define-key (cdr ido-minor-mode-map-entry) [remap dired] nil) ;in ido-setup-hook
;;   (define-key dired-mode-map "," 'dired))


;;; Code:

(require 'dired)
(require 'dired-aux)

(defgroup consult-dired-history nil
  "Consult dired history"
  :group 'ivy)


(defcustom consult-dired-history-max 200
  "Length of history for ivy-dired-history."
  :type 'number
  :group 'consult-dired-history)

(defcustom consult-dired-history-ignore-directory '("/")
  "Length of history for ivy-dired-history."
  :type '(repeat string)
  :group 'consult-dired-history)

(defvar consult-dired-history nil)

(defvar consult-dired-history--cleanup-p nil)

(defun consult-dired-history--update(dir)
  "Update variable `consult-dired-history'.
Argument DIR directory."
       (setq dir (abbreviate-file-name (expand-file-name dir)))
       (unless (member dir consult-dired-history-ignore-directory)
         (unless consult-dired-history--cleanup-p
           (setq consult-dired-history--cleanup-p t)
           (let ((tmp-history ))
             (dolist (d consult-dired-history)
               (when (or (file-remote-p d) (file-directory-p d))
                 (add-to-list 'tmp-history d t)))
             (setq consult-dired-history tmp-history)))
         (setq consult-dired-history
               (delete-dups (delete dir consult-dired-history)))
         (setq consult-dired-history
               (append (list dir) consult-dired-history))
         (consult-dired-history)))

(defun consult-dired-history-update()
  "Update variable `consult-dired-history'."
  (consult-dired-history--update (dired-current-directory)))

;;when you open dired buffer ,update `consult-dired-history'.
(add-hook 'dired-after-readin-hook 'consult-dired-history-update)

(defun consult-dired-history()
  "Retain only the first `consult-dired-history-max' items in VALUE."
  (if (> (length consult-dired-history) consult-dired-history-max)
      (setcdr (nthcdr (1- consult-dired-history-max) consult-dired-history) nil))
  consult-dired-history)

(defvar consult-dir--source-dired
  `(:name "Recentf dired"
    :narrow ?d
    :category file
    :face consult-file
    :history file-name-history
    :items ,#'consult-dired-history)
  "Recentf directory source for `consult-dir--pick'.")

(provide 'consult-dired-history)

;; Local Variables:
;; coding: utf-8
;; End:

;;; consult-dired-history.el ends here.
