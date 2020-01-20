;;; org-re-reveal-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-re-reveal" "org-re-reveal.el" (0 0 0 0))
;;; Generated autoloads from org-re-reveal.el

(autoload 'org-re-reveal-publish-to-reveal "org-re-reveal" "\
Publish an Org file to HTML.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.  Optional BACKEND may specify a derived export
backend.
Return output file name.

\(fn PLIST FILENAME PUB-DIR &optional BACKEND)" nil nil)

(autoload 'org-re-reveal-publish-to-reveal-client "org-re-reveal" "\
Publish an Org file to HTML as multiplex client.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.  Optional BACKEND may specify a derived export
backend.
If `org-re-reveal-client-multiplex-filter' is non-nil, use it as regular
expression to only publish FILENAME if it matches this regular expression.
Return output file name.

\(fn PLIST FILENAME PUB-DIR &optional BACKEND)" nil nil)

(autoload 'org-re-reveal-version "org-re-reveal" "\
Display version string for org-re-reveal from Lisp file." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-re-reveal" '("org-re-reveal-")))

;;;***

;;;### (autoloads nil nil ("org-re-reveal-pkg.el" "ox-re-reveal.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-re-reveal-autoloads.el ends here
