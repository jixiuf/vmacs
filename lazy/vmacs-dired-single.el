;;; vmacs-single-dired.el --- Only Open One Dired Buffer.

;; Filename: vmacs-single-dired.el
;; Description: only open a single dired buffer.
;; Author: Joseph <jixiuf@gmail.com>
;; Maintainer: Joseph <jixiuf@gmail.com>
;; Copyright (C) 2011~, Joseph, all rights reserved.
;; Created: 2011-04-03
;; URL: https://github.com/jixiuf/vmacs/tree/master/vmacs-single-dired.el
;; Keywords: dired single buffer
;;
;; Features that might be required by this library:
;;
;;  `dired'
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
;;  `vmacs-single-dired.el' can make you just open a single dired buffer.
;;  I bind `dired-mouse-find-alternate-file' on [mouse-2]
;;  so even you click on a directory in dired buffer ,
;;  it will reuse current dired buffer too.
;;

;;; Installation:
;;
;;  (with-eval-after-load 'dired (require 'vmacs-single-dired))
;;


(require 'dired)

(defun vmacs-dired-single-kill-other-dired ( &optional current-buf)
  "kill all dired-buffers and diredp-w32-drivers-mode(w32 use this mode )
  except current-buf ,if current-buf is nil then kill all"
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (not (eq current-buf buf))
                 (or  (eq 'dired-mode  major-mode)
                      (eq 'diredp-w32-drives-mode major-mode))
                 (or (not (featurep 'server))
                     (not server-buffer-clients)))
        (kill-buffer buf)))))

(defadvice dired-find-file (around dired-find-file-single-buffer activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let ((orig (current-buffer))
        (filename (dired-get-file-for-visit)))
    ad-do-it
    (when (and (file-directory-p filename)
               (not (eq (current-buffer) orig)))
      (vmacs-dired-single-kill-other-dired (current-buffer)))))

(defadvice dired-up-directory (around dired-up-directory-single-buffer activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let ((orig (current-buffer)))
    ad-do-it
    (vmacs-dired-single-kill-other-dired (current-buffer))))

(defadvice dired (before dired-single-buffer activate)
  "Replace current buffer if file is a directory."
  (vmacs-dired-single-kill-other-dired))

(defun dired-mouse-find-alternate-file (event)
  "In dired, visit the file or directory you click on instead of the dired buffer."
  (interactive "e")
  (let (file)
    (save-excursion
      (with-current-buffer (window-buffer (posn-window (event-end event)))
        (save-excursion
          (goto-char (posn-point (event-end event)))
          (setq file (dired-get-filename nil t)))))
    (select-window (posn-window (event-end event)))
    (find-alternate-file (file-name-sans-versions file t))))

(define-key dired-mode-map [mouse-2] 'dired-mouse-find-alternate-file)


(provide 'vmacs-dired-single)

;; Local Variables:
;; coding: utf-8
;; End:

;;; vmacs-dired-single.el ends here.
