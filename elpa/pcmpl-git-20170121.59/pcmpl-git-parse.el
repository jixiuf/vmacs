;;; pcmpl-git-parse.el --- Parsing Git Documentation for Completion  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2017  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Keywords: tools, data
;; Version: 0.5

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Parse the ASCIIDOC files for pcmpl-git.el. This file is not required
;; to run pcmpl-git.

;;; Code:

(require 'pcmpl-git)

(defvar git-documentation-dir "Documentation"
  "The directory containing asciidoc sources for git documentation.
Normally it is the 'Documentation' direcotry under top-level git source.")

(defun git-parse-config ()
  (let (variables include (config (expand-file-name "config.txt" git-documentation-dir)))
    (when (file-exists-p config)
      (with-temp-buffer
        (insert-file-contents config)
        (goto-char (point-min))
        ;; handle includes
        (while (re-search-forward "^include::\\(.*?\\)\\[.*$" nil t)
          (setq include (match-string 1))
          (setq include (expand-file-name include git-documentation-dir))
          (delete-region (line-beginning-position)
                         (line-end-position))
          (when (file-exists-p include)
            (insert-file-contents include)))
        (goto-char (point-min))
        (while (re-search-forward "^\\([^ \t\n]+?\\)::" nil t)
          (if (not (string-match-p "^advice" (match-string 1)))
              (push (match-string 1) variables)
            (while (re-search-forward "^[ \t]+\\([^ \t\n]+?\\)::" nil t)
              (push (concat "advice." (match-string 1)) variables))))))
    (nreverse variables)))

(defvar git-parse-fixes
  '(("diff" "--cached" "--staged")))

(defun git-parse ()
  (let ((tbl (make-hash-table :size 149 :test #'equal)))
    (dolist (cmd (pcmpl-git-commands 'internal))
      (let (doc beg end include short-options long-options)
        (setq doc (expand-file-name (concat "git-" cmd ".txt")
                                    git-documentation-dir))
        (when (file-exists-p doc)
          (with-temp-buffer
            (insert-file-contents doc)
            ;; git-push.txt has OPTIONS[[OPTIONS]]
            (when (re-search-forward "^OPTIONS.*\n-------" nil t)
              (setq beg (point)))
            (setq end (point-max))      ; defaults to eob
            (when (re-search-forward "^[A-Z]+?\n-+\n" nil t)
              (setq end (match-beginning 0)))
            (when beg
              (narrow-to-region beg end)
              ;; handle INCLUDEs
              (goto-char (point-min))
              (while (re-search-forward "^include::\\(.*?\\)\\[.*$" nil t)
                (setq include (match-string 1))
                (setq include (expand-file-name include git-documentation-dir))
                (delete-region (line-beginning-position)
                               (line-end-position))
                (when (file-exists-p include)
                  (insert-file-contents include)))
              ;; parse short options
              (goto-char (point-min))
              (while (re-search-forward "^-\\([^- \t]\\).*?::" nil t)
                ;; some options might look like this '-<n>' or '-[ABC]'
                (unless (member (match-string 1) '("<" "["))
                  (push (match-string 1) short-options)))
              ;; parse long options
              (goto-char (point-min))
              (while (re-search-forward "^\\(--[^[ \t\n=]+=?\\).*?::" nil t)
                (push (match-string 1) long-options))
              ;; it seems all commands support --help
              (cl-pushnew "--help" long-options :test #'equal)
              ;; add missing options from git-parse-fixes
              (setq long-options (append long-options (cdr (assoc cmd git-parse-fixes))))
              (puthash cmd (list (mapconcat 'identity (delete-dups
                                                       (nreverse short-options)) "")
                                 (delete-dups (nreverse long-options)))
                       tbl))))))
    tbl))

(defun git-parse-and-save ()
  (interactive)
  (cl-assert (featurep 'hashtable-print-readable))
  (setq-default indent-tabs-mode nil)
  (with-temp-buffer
    (insert ";;; -*- Emacs-Lisp -*-\n")
    (insert ";;; Generated on " (format-time-string "%Y-%m-%d")
            " for " (shell-command-to-string "git version") "\n")
    (pp (git-parse) (current-buffer))
    (insert "\n\n;;; 'git config' variables\n")
    (pp (git-parse-config) (current-buffer))
    (write-region (point-min) (point-max) "git-options")))

(provide 'pcmpl-git-parse)
;;; pcmpl-git-parse.el ends here
