;;; clang-format-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "clang-format" "clang-format.el" (0 0 0 0))
;;; Generated autoloads from clang-format.el

(autoload 'clang-format-region "clang-format" "\
Use clang-format to format the code between START and END according to STYLE.
If called interactively uses the region or the current statement if there is no
no active region. If no STYLE is given uses `clang-format-style'. Use
ASSUME-FILE-NAME to locate a style config file, if no ASSUME-FILE-NAME is given
uses the function `buffer-file-name'.

\(fn START END &optional STYLE ASSUME-FILE-NAME)" t nil)

(autoload 'clang-format-buffer "clang-format" "\
Use clang-format to format the current buffer according to STYLE.
If no STYLE is given uses `clang-format-style'. Use ASSUME-FILE-NAME
to locate a style config file. If no ASSUME-FILE-NAME is given uses
the function `buffer-file-name'.

\(fn &optional STYLE ASSUME-FILE-NAME)" t nil)

(defalias 'clang-format 'clang-format-region)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "clang-format" '("clang-format-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; clang-format-autoloads.el ends here
