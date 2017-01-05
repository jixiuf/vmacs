;;; etags-table-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "etags-table" "etags-table.el" (0 0 0 0))
;;; Generated autoloads from etags-table.el

(let ((loads (get 'etags-table 'custom-loads))) (if (member '"etags-table" loads) nil (put 'etags-table 'custom-loads (cons '"etags-table" loads))))

(defvar etags-table-alist nil "\
*Map filename to tag file(s)

Example:

\(setq etags-table-alist
      (list
       '(\"/home/me/Projects/foo/.*\\\\.[ch]$\" \"/home/me/Projects/lib1/TAGS\" \"/home/me/Projects/lib2/TAGS\")
       '(\"/home/me/Projects/bar/.*\\\\.py$\" \"/home/me/Projects/python/common/TAGS\")
       '(\".*\\\\.[ch]$\" \"/usr/local/include/TAGS\")
       ))

A file named, for example, \"/home/me/Projects/foo/main.c\" would set the
`tags-table-list' to a list of:

\"/home/me/Projects/lib1/TAGS\"
\"/home/me/Projects/lib2/TAGS\"
\"/usr/local/include/TAGS\"

and possibly a local tags file at the head of the list if `etags-table-search-up-depth'
is non-nil.  You can use \\&, \\1, etc. in the tag file names to substitute pieces
captured with \\(\\) in the key.
")

(custom-autoload 'etags-table-alist "etags-table" t)

(defvar etags-table-search-up-depth nil "\
*Max depth to search up for a tags file.  nil means don't search.")

(custom-autoload 'etags-table-search-up-depth "etags-table" t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "etags-table" '("etags-table-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; etags-table-autoloads.el ends here
