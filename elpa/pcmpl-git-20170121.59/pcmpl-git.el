;;; pcmpl-git.el --- pcomplete for git  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2017  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Keywords: tools
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

;; Complete both git commands and their options and arguments. Type
;; '-' to complete short options and '--' to complete long options.
;; For commands that accept commit as argument, branches and tags will
;; be tried first and then the sha1's in the first few hundred
;; commits.

;;; Code:

(require 'pcomplete)

(defcustom pcmpl-git-executable (or (executable-find "git") "git")
  "The full path of the 'git' executable."
  :type 'file
  :group 'pcomplete)

(defcustom pcmpl-git-data-directory
  (file-name-directory load-file-name)
  "Default directory used for locating the `pcmpl-git-options-file'."
  :type 'directory
  :group 'pcomplete)

(defcustom pcmpl-git-options-file
  (expand-file-name "git-options" pcmpl-git-data-directory)
  "File containing a hashtable with git options."
  :type 'file
  :group 'pcomplete)

(defvar pcmpl-git-hashtable nil
  "Hashtable containing GIT options read from `pcmpl-git-options-file'.")

(defvar pcmpl-git-config-variables nil
  "A list containing git config variables.")

(when (file-exists-p pcmpl-git-options-file)
  (with-temp-buffer
    (insert-file-contents pcmpl-git-options-file)
    (setq pcmpl-git-hashtable (read (current-buffer)))
    (setq pcmpl-git-config-variables (read (current-buffer)))))

(defsubst pcmpl-git-short-options (cmd)
  (when (hash-table-p pcmpl-git-hashtable)
    (car (gethash cmd pcmpl-git-hashtable))))

(defsubst pcmpl-git-long-options (cmd)
  (when (hash-table-p pcmpl-git-hashtable)
    (cadr (gethash cmd pcmpl-git-hashtable))))

(defun pcmpl-git-parse-region (beg end regexp &optional predicate)
  "Return a list of matches in region between BEG and END.
See `pcmpl-git-parse' for the explanation of REGEXP, PREDICATE
and the return value."
  (let (collection)
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (when (or (null predicate)
                  (save-match-data (funcall predicate)))
          (push (match-string 1) collection)))
      (nreverse collection))))

(defun pcmpl-git-parse (cmd regexp &optional predicate &rest args)
  "Parse the output of 'git CMD ARGS'.
The return value is a list of match group 1 in REGEXP. PREDICATE
is called when point is at the end of REGEXP."
  (with-temp-buffer
    (apply 'call-process pcmpl-git-executable nil t nil cmd args)
    (pcmpl-git-parse-region (point-min) (point-max) regexp predicate)))

;; (pcmpl-git-string-lessp "word" "word-")  ; => nil
;; (pcmpl-git-string-lessp "words" "word-") ; => t
(defun pcmpl-git-string-lessp (s1 s2)
  "Compare strings S1 and S2 but treat '-' specially."
  (let ((res (compare-strings s1 0 nil s2 0 nil))
        c1 c2)
    (if (eq res t) (setq res 0))
    (ignore-errors
      ;; the following setq could trigger errors
      (setq c1 (aref s1 (1- (abs res)))
            c2 (aref s2 (1- (abs res))))
      (if (or (= c1 ?-) (= c2 ?-)) (setq res (- res))))
    (if (> res 0) nil t)))

;; There are far more porcelain commands than here. See `man git' for
;; a full list.
(defun pcmpl-git-porcelain-commands ()
  "Actually returns the most commonly used porcelain commands."
  (let (beg end)
    (with-temp-buffer
      (call-process pcmpl-git-executable nil t)
      (goto-char (point-min))
      (when (re-search-forward "^$" nil t)
        (forward-line 2)
        (setq beg (point)))
      (when (re-search-forward "^$" nil t)
        (setq end (point)))
      (when (and beg end)
        (pcmpl-git-parse-region beg end "^[ ]\\{3\\}\\(\\S-+?\\)\\s-")))))

(defun pcmpl-git-commands (&optional internal)
  "Return a collection of all 'git' commands.
Porcelain commands are sorted first. If INTERNAL is non-nil, also
include internal commands."
  (let (beg end commands)
    (with-temp-buffer
      (call-process pcmpl-git-executable nil t nil "help" "--all")
      (goto-char (point-min))
      (when (or (re-search-forward "-+\n" nil t) ; older git
                (search-forward "\n\n" nil t 2))
        (setq beg (point)))
      (when (search-forward "\n\n" nil t)
        (setq end (point)))
      (when (and beg end)
        (setq commands (pcmpl-git-parse-region
                        beg end
                        "\\s-\\(\\S-+?\\)\\s-"
                        (unless internal
                          (lambda () (not (string-match "--" (match-string 1)))))))
        (delete-dups
         (append (pcmpl-git-porcelain-commands)
                 (sort commands 'pcmpl-git-string-lessp)))))))

(defvar pcmpl-git-commands (pcmpl-git-commands)
  "A collection of all 'git' commands.")

(defsubst pcmpl-git-branches ()
  (pcomplete-uniqify-list
   (pcmpl-git-parse "branch" "^\\*?\\s-+\\(\\S-+\\)\\(?:$\\|\\s-+\\)" nil "-a")))

(defsubst pcmpl-git-tags ()
  (pcomplete-uniqify-list
   (pcmpl-git-parse "tag" "^\\(\\S-+\\)$" nil "-l")))

(defsubst pcmpl-git-aliases ()
  (pcomplete-uniqify-list
   (pcmpl-git-parse "config" "^alias\\.\\(\\S-+?\\) " nil "--get-regexp" "alias.*")))

(defsubst pcmpl-git-rev-list ()
  (pcmpl-git-parse "rev-list" "^\\(\\S-+\\)$" nil "--all" "--abbrev-commit" "--max-count=300"))

(defsubst pcmpl-git-branches-or-tags ()
  (append (pcmpl-git-branches) (pcmpl-git-tags)))

;;; From 'git --help' of version 1.7.12.2
(defvar pcmpl-git-toplevel-options
  '("--version"
    "--exec-path"
    "--html-path"
    "--man-path"
    "--info-path"
    "-p"
    "--paginate"
    "--no-pager"
    "--no-replace-objects"
    "--bare"
    "--git-dir="
    "--work-tree="
    "--namespace="
    "-c"
    "--help"))

(defsubst pcmpl-git-complete-commit ()
  (if (try-completion (pcomplete-arg) (pcmpl-git-branches-or-tags))
      (pcomplete-here (pcmpl-git-branches-or-tags))
    (pcomplete-here (pcmpl-git-rev-list))))

;;;###autoload
(defun pcomplete/git ()
  "Completion rules for the `git' command."
  (let (cmd soptions loptions)
    (while (pcomplete-match "^-" 0)
      (pcomplete-here* pcmpl-git-toplevel-options)
      (when (string= "-c"
                     (nth (1- pcomplete-index) pcomplete-args))
        (pcomplete-here* (mapcar (lambda (s) (concat s "=")) pcmpl-git-config-variables))))
    (pcomplete-here* (append (pcmpl-git-aliases) pcmpl-git-commands))
    ;; `pcomplete-arg' seems broken; see bug #6027
    (setq cmd (nth (1- pcomplete-index) pcomplete-args))
    (unless (member cmd pcmpl-git-commands)
      (setq cmd (car (split-string (shell-command-to-string
                                    (concat "git config --get alias." cmd))
                                   nil t))))
    (setq soptions (pcmpl-git-short-options cmd)
          loptions (pcmpl-git-long-options cmd))
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^-$" 0)
          (pcomplete-opt soptions)
        (pcomplete-here* loptions)))
    (cond
     ((string= cmd "branch")
      (pcomplete-here (pcmpl-git-branches)))
     ((string= cmd "cat-file")
      ;; To complete all objects (not useful)
      ;; 'git ls-tree -r --abbrev HEAD' can show all blobs
      ;; 'git ls-tree -d -r --abbrev HEAD' can show only trees
      (pcmpl-git-complete-commit))
     ((string= cmd "checkout")
      (pcmpl-git-complete-commit)
      (pcomplete-here (pcomplete-entries)))
     ((string= cmd "cherry")
      (pcomplete-here (pcmpl-git-branches)))
     ((string= cmd "cherry-pick")
      (while (pcmpl-git-complete-commit)))
     ((string= cmd "config")
      (pcomplete-here pcmpl-git-config-variables))
     ((string= cmd "format-patch")
      (while (pcmpl-git-complete-commit)))
     ((string= cmd "help")
      (pcomplete-here pcmpl-git-commands))
     ((string= cmd "init")
      (pcomplete-here (pcomplete-dirs)))
     ((string= cmd "log")
      (while (pcmpl-git-complete-commit)))
     ((string= cmd "name-rev")
      (pcmpl-git-complete-commit))
     ((string= cmd "rev-list")
      (while (pcomplete-here (pcmpl-git-branches-or-tags))))
     ((member cmd '("diff" "difftool" "merge" "merge-base" "rebase" "show"))
      (while (pcmpl-git-complete-commit)))
     ((string= cmd "tag")
      (pcmpl-git-complete-commit))
     ;; rm, mv, add...
     (t (while (pcomplete-here (pcomplete-entries)))))))

(provide 'pcmpl-git)
;;; pcmpl-git.el ends here
