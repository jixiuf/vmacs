;;; magit-svn-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "magit-svn" "magit-svn.el" (0 0 0 0))
;;; Generated autoloads from magit-svn.el

(autoload 'magit-svn-show-commit "magit-svn" "\
Show the Git commit for a Svn revision read from the user.
With a prefix argument also read a branch to search in.

\(fn REV &optional BRANCH)" t nil)

(autoload 'magit-svn-create-branch "magit-svn" "\
Create svn branch NAME.

\(git svn branch [--dry-run] NAME)

\(fn NAME &optional ARGS)" t nil)

(autoload 'magit-svn-create-tag "magit-svn" "\
Create svn tag NAME.

\(git svn tag [--dry-run] NAME)

\(fn NAME &optional ARGS)" t nil)

(autoload 'magit-svn-rebase "magit-svn" "\
Fetch revisions from Svn and rebase the current Git commits.

\(git svn rebase [--dry-run])

\(fn &optional ARGS)" t nil)

(autoload 'magit-svn-dcommit "magit-svn" "\
Run git-svn dcommit.

\(git svn dcommit [--dry-run])

\(fn &optional ARGS)" t nil)

(autoload 'magit-svn-fetch "magit-svn" "\
Fetch revisions from Svn updating the tracking branches.

\(git svn fetch)

\(fn)" t nil)

(autoload 'magit-svn-fetch-externals "magit-svn" "\
Fetch and rebase all external repositories.
Loops through all external repositories found
in `magit-svn-external-directories' and runs
`git svn rebase' on each of them.

\(fn)" t nil)

(autoload 'magit-svn-mode "magit-svn" "\
Git-Svn support for Magit.

\(fn &optional ARG)" t nil)

(custom-add-option 'magit-mode-hook #'magit-svn-mode)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-svn" '("magit-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; magit-svn-autoloads.el ends here
