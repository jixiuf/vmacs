;;; ivy-dired-history.el --- quickly visit dired directory you have visted

;; Author: 纪秀峰 <jixiuf@gmail.com>
;; Copyright (C) 2017 纪秀峰, all rights reserved.
;; Created:  2017-06-14
;; Version: 1.0
;; Package-Version: 20170615.218
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
;; remember dired directory you have visited and list them
;; using `ivy.el'.

;; integrating dired history feature into commands like
;; dired-do-copy and dired-do-rename. What I think of is that when
;; user press C (copy) or R (rename) mode, it is excellent to have
;; an option allowing users to select a directory from the history
;; list.



;;; Installation:

;; (require 'savehist)
;; (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable)
;; (savehist-mode 1)

;; (with-eval-after-load 'dired
;;   (require 'ivy-dired-history)
;; ;; if you are using ido,you'd better disable ido for dired
;; ;; (define-key (cdr ido-minor-mode-map-entry) [remap dired] nil) ;in ido-setup-hook
;;   (define-key dired-mode-map "," 'dired))


;;; Code:

(require 'dired)
(require 'dired-aux)
(require 'ivy)
(require 'counsel)
(require 'cl-lib)

(defgroup ivy-dired-history nil
  "dired history using Ivy"
  :group 'ivy)


(defcustom ivy-dired-history-max 200
  "Length of history for ivy-dired-history."
  :type 'number
  :group 'ivy-dired-history)

(defcustom ivy-dired-ignore-directory '("/")
  "Length of history for ivy-dired-history."
  :type '(repeat string)
  :group 'ivy-dired-history)

(defvar ivy-dired-history-variable nil)

(defvar ivy-dired-history-cleanup-p nil)

(defvar ivy-dired-history-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") 'ivy-done)
    (define-key map (kbd "<RET>")    'ivy-done)
    map))

(set-keymap-parent ivy-dired-history-map counsel-find-file-map)

(defun ivy-dired-history--update(dir)
  "Update variable `ivy-dired-history-variable'.
Argument DIR directory."
       (setq dir (abbreviate-file-name (expand-file-name dir)))
       (unless (member dir ivy-dired-ignore-directory)
         (unless ivy-dired-history-cleanup-p
           (setq ivy-dired-history-cleanup-p t)
           (let ((tmp-history ))
             (dolist (d ivy-dired-history-variable)
               (when (or (file-remote-p d) (file-directory-p d))
                 (add-to-list 'tmp-history d t)))
             (setq ivy-dired-history-variable tmp-history)))
         (setq ivy-dired-history-variable
               (delete-dups (delete dir ivy-dired-history-variable)))
         (setq ivy-dired-history-variable
               (append (list dir) ivy-dired-history-variable))
         (ivy-dired-history-trim)))

(defun ivy-dired-history-update()
  "Update variable `ivy-dired-history-variable'."
  (ivy-dired-history--update (dired-current-directory)))

;;when you open dired buffer ,update `ivy-dired-history-variable'.
(add-hook 'dired-after-readin-hook 'ivy-dired-history-update)

(defun ivy-dired-history-trim()
  "Retain only the first `ivy-dired-history-max' items in VALUE."
  (if (> (length ivy-dired-history-variable) ivy-dired-history-max)
      (setcdr (nthcdr (1- ivy-dired-history-max) ivy-dired-history-variable) nil)))


;; integrating dired history feature into commands like
;; dired-do-copy and dired-do-rename.
;;see https://github.com/jixiuf/ivy-dired-history/issues/6
(defadvice dired-mark-read-file-name(around ivy-dired-history activate)
  "Wrapper ‘read-file-name’ with idv-dired-history-read-file-name."
  (cl-letf (((symbol-function 'read-file-name)
             #'ivy-dired-history-read-file-name))
    ad-do-it))

(defadvice dired-read-dir-and-switches(around ivy-dired-history activate)
  "Wrapper ‘read-file-name’ with idv-dired-history-read-file-name."
  (ivy-dired-history--update (expand-file-name default-directory))
  (let ((default-directory default-directory))
    ;; (unless (next-read-file-uses-dialog-p) (setq default-directory "/"))
    (cl-letf (((symbol-function 'read-file-name)
               #'ivy-dired-history-read-file-name))
      ad-do-it)))

(defadvice dired-do-compress-to(around ivy-dired-history activate)
  "Wrapper ‘read-file-name’ with idv-dired-history-read-file-name."
  (cl-letf (((symbol-function 'read-file-name)
             #'ivy-dired-history-read-file-name))
    ad-do-it))


(defun ivy-dired-history-read-file-name
    (prompt &optional dir default-filename mustmatch initial predicate)
  "Read file name with hisotry as collection.
Argument PROMPT prompt.
Optional argument DIR directory.
Optional argument DEFAULT-FILENAME default.
Optional argument MUSTMATCH mustmatch.
Optional argument INITIAL init value.
Optional argument PREDICATE predicate."
    (cl-letf (((symbol-function 'read-file-name-internal)
               #'ivy-read-file-name-internal))
      (let ((ivy-sort-functions-alist nil)
            (default-directory default-directory)
            (ivy-extra-directories nil))
        (when dir (setq default-directory dir))
        (ivy-read prompt
                  'read-file-name-internal
                  :initial-input initial
                  ;; :sort t
                  ;; :matcher #'counsel--find-file-matcher
                  :keymap ivy-dired-history-map
                  :caller 'read-file-name-internal)
        )))

(defalias 'ivy-dired-history--read-file-name-internal
  (completion-table-in-turn #'completion--embedded-envvar-table
                            #'completion--file-name-table)
  "same as read-file-name-internal")

(defun ivy-read-file-name-internal (string pred action)
  "Merge ivy-directory-history-variables with files in current directory.
Argument STRING string.
Argument PRED pred.
Argument ACTION action."
       (append ivy-dired-history-variable
               (ivy-dired-history--read-file-name-internal string pred action)))


(provide 'ivy-dired-history)

;;; ivy-dired-history.el ends here
