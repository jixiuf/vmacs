;;; vmacs-dired-history.el --- use vmacs to open recent directories -*- lexical-binding: t; -*-


;; Author: 纪秀峰 <jixiuf@gmail.com>
;; Copyright (C) 2017 纪秀峰, all rights reserved.
;; Created:  2017-06-14
;; Version: 1.0
;; X-URL:https://github.com/jixiuf/vmacs-dired-history
;; Package-Requires: ((vmacs "0.9.0")(counsel "0.9.0")(cl-lib "0.5"))
;;
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
;; use `vmacs' to open recent directories.

;; it is integrated with `dired-do-copy' and `dired-do-rename'.
;; when you press C (copy) or R (rename) , it is excellent to
;; allow users to select a directory from the recent dired history .



;;; Installation:

;; (require 'savehist)
;; (add-to-list 'savehist-additional-variables 'vmacs-dired-history)
;; (savehist-mode 1)

;; or if you use desktop-save-mode
;; (add-to-list 'desktop-globals-to-save 'vmacs-dired-history)


;; (with-eval-after-load 'dired
;;   (require 'vmacs-dired-history)
;; ;; if you are using ido,you'd better disable ido for dired
;; ;; (define-key (cdr ido-minor-mode-map-entry) [remap dired] nil) ;in ido-setup-hook
;;   (define-key dired-mode-map "," 'dired))


;;; Code:

(require 'dired)
(require 'dired-aux)
(require 'cl-lib)

(defgroup vmacs-dired-history nil
  "dired history using Ivy"
  :group 'vmacs)


(defcustom vmacs-dired-history-max 200
  "Length of history for vmacs-dired-history."
  :type 'number
  :group 'vmacs-dired-history)

(defcustom vmacs-dired-history-ignore-directory '("/")
  "Length of history for vmacs-dired-history."
  :type '(repeat string)
  :group 'vmacs-dired-history)

(defvar vmacs-dired-history nil)

(defvar vmacs-dired-history--cleanup-p nil)
(defvar vmacs-dired-history--default-directory nil)

(defun vmacs-dired-history--update(dir)
  "Update variable `vmacs-dired-history'.
Argument DIR directory."
       (setq dir (abbreviate-file-name (expand-file-name dir)))
       (unless (member dir vmacs-dired-history-ignore-directory)
         (unless vmacs-dired-history--cleanup-p
           (setq vmacs-dired-history--cleanup-p t)
           (let ((tmp-history ))
             (dolist (d vmacs-dired-history)
               (when (or (file-remote-p d) (file-directory-p d))
                 (push d tmp-history)))
             (setq vmacs-dired-history tmp-history)))
         (setq vmacs-dired-history
               (delete-dups (delete dir vmacs-dired-history)))
         (setq vmacs-dired-history
               (append (list dir) vmacs-dired-history))
         (vmacs-dired-history--trim)))

(defun vmacs-dired-history-update()
  "Update variable `vmacs-dired-history'."
  (vmacs-dired-history--update (dired-current-directory)))

;;when you open dired buffer ,update `vmacs-dired-history'.
(add-hook 'dired-after-readin-hook 'vmacs-dired-history-update)

(defun vmacs-dired-history--trim()
  "Retain only the first `vmacs-dired-history-max' items in VALUE."
  (if (> (length vmacs-dired-history) vmacs-dired-history-max)
      (setcdr (nthcdr (1- vmacs-dired-history-max) vmacs-dired-history) nil)))


;; integrating dired history feature into commands like
;; dired-do-copy and dired-do-rename.
;;see https://github.com/jixiuf/vmacs-dired-history/issues/6
(defadvice dired-mark-read-file-name(around vmacs-dired-history activate)
  "Wrapper ‘read-file-name’ with idv-dired-history-read-file-name."
  (cl-letf (((symbol-function 'read-file-name)
             #'vmacs-dired-history--read-file-name))
    ad-do-it))

(defadvice dired-read-dir-and-switches(around vmacs-dired-history activate)
  "Wrapper ‘read-file-name’ with idv-dired-history-read-file-name."
  (vmacs-dired-history--update (expand-file-name default-directory))
  (let ((default-directory default-directory))
    ;; (unless (next-read-file-uses-dialog-p) (setq default-directory "/"))
    (cl-letf (((symbol-function 'read-file-name)
               #'vmacs-dired-history--read-file-name))
      ad-do-it)))

(defadvice dired-do-compress-to(around vmacs-dired-history activate)
  "Wrapper ‘read-file-name’ with idv-dired-history-read-file-name."
  (cl-letf (((symbol-function 'read-file-name)
             #'vmacs-dired-history--read-file-name))
    ad-do-it))

(defun vmacs-dired-history--read-file-name
    (prompt &optional dir default-filename mustmatch initial predicate)
  ""
  (let ((default-directory default-directory))
    (when dir (setq default-directory dir))
    (setq vmacs-dired-history--default-directory default-directory)
    (completing-read prompt 'vmacs-dired-history---read-file-name
                     predicate mustmatch (abbreviate-file-name
                                          (concat (or dir "")
                                                  (or initial ""))))))


(defalias 'vmacs-dired-history--old-read-file-name-internal
  (completion-table-in-turn #'completion--embedded-envvar-table
                            #'completion--file-name-table)
  "same as read-file-name-internal")
(defvar vmacs-dired-history---last-token "")
(defun vmacs-dired-history---read-file-name (string pred action)
  "Merge vmacs-directory-history-variables with files in current directory.
Argument STRING string.
Argument PRED pred.
Argument ACTION action."
  (let* ((cands vmacs-dired-history)
         (origin-result (vmacs-dired-history--old-read-file-name-internal
                         string pred action))
         (tokens (split-string string "/"))
         (last-token (car (last tokens)))
         (last-token-2 (car (last tokens 2)))
         regexp)
    (when (string-empty-p last-token) (setq last-token last-token-2))
    (cond
     ((eq action 'metadata)
      (setq vmacs-dired-history---last-token last-token)
      '(metadata (display-sort-function . identity) ;disable sort
                 (cycle-sort-function . identity)))
     ((eq action 'lambda) origin-result)
     ((eq (car-safe action) 'boundaries)
      '(boundaries 0 . 0))
     ((not action) origin-result)
     (t
      (setq last-token vmacs-dired-history---last-token)
      (setq regexp (mapconcat #'(lambda(e) (if (char-equal ?\  e) ".*" (char-to-string e))) last-token ""))
      (setq cands (seq-filter (lambda (e) (string-match-p  regexp e)) cands))

      ;; (setq cands  (prescient-filter last-token cands))
      (vmacs-dired-history--sort
       last-token
       (append
        cands
        (mapcar (lambda(e)
                  (if (stringp e)
                      (abbreviate-file-name (expand-file-name e))
                    e))
                origin-result)
        ))))))


(defun vmacs-dired-history--sort (name candidates)
  "Re-sort candidates by NAME.
CANDIDATES is a list of directories(with path) each match NAME.
equal>prefix>substring>other."
  (if (or (string-match "^\\^" name) (string= name ""))
      candidates
    (let* ((re-prefix (concat "^\\*" name))
           (name-tokens (split-string name))
           res-prefix
           res-equal
           res-substring
           res-dirname-match-all-tokens
           res-fullpath-match-all-tokens
           res-fullpath-substring
           res-noprefix
           dirname)
      (dolist (s candidates)
        (setq dirname (file-name-nondirectory
                       (directory-file-name
                        (file-name-directory (expand-file-name s)))))
        (cond
         ((string= name dirname)
          (push s res-equal))
         ((string-match-p re-prefix dirname)
          (push s res-prefix))
         ((string-match-p name dirname)
          (push s res-substring))
         ((cl-every  (lambda(e) (string-match-p e dirname)) name-tokens)
          (push s res-dirname-match-all-tokens))
         ((cl-some  (lambda(dir) (cl-every  (lambda(e) (string-match-p e dir)) name-tokens))
                    (split-string s "/" t ))
          (push s res-fullpath-match-all-tokens))
         ((string-match-p name (expand-file-name s))
          (push s res-fullpath-substring))
         (t
          (push s res-noprefix))))
      (nconc
       (nreverse res-equal)
       (nreverse res-prefix)
       (nreverse res-substring)
       (nreverse res-dirname-match-all-tokens)
       (nreverse res-fullpath-match-all-tokens)
       (nreverse res-fullpath-substring)
       (nreverse res-noprefix)))))

(provide 'vmacs-dired-history)

;; Local Variables:
;; coding: utf-8
;; End:

;;; vmacs-dired-history.el ends here.
