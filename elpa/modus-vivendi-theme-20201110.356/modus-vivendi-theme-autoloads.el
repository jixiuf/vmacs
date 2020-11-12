;;; modus-vivendi-theme-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "modus-vivendi-theme" "modus-vivendi-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from modus-vivendi-theme.el

(defvar modus-vivendi-theme-override-colors-alist 'nil "\
Association list of palette color overrides.
Values can be mapped to variables, using the same syntax as the
one present in `modus-vivendi-theme-default-colors-alist'.

This is only meant for do-it-yourself usage, with the
understanding that the user is responsible for the resulting
contrast ratio between new and existing colors.")

(custom-autoload 'modus-vivendi-theme-override-colors-alist "modus-vivendi-theme" t)

(when load-file-name (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

(register-definition-prefixes "modus-vivendi-theme" '("modus-vivendi"))

;;;***

;;;### (autoloads nil nil ("modus-vivendi-theme-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; modus-vivendi-theme-autoloads.el ends here
