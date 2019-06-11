;;; bm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bm" "bm.el" (0 0 0 0))
;;; Generated autoloads from bm.el

(autoload 'bm-toggle "bm" "\
Toggle bookmark at point." t nil)

(autoload 'bm-toggle-mouse "bm" "\
Toggle a bookmark with a mouse click.
EV is the mouse event.

\(fn EV)" t nil)

(autoload 'bm-lifo-previous "bm" "\
Goto previous bookmark in LIFO order . (that is, most
recently set ones come first, oldest ones come last)" t nil)

(autoload 'bm-lifo-next "bm" "\
Goto next bookmark in LIFO order .(that is, most
recently set ones come first, oldest ones come last)" t nil)

(autoload 'bm-next "bm" nil t nil)

(autoload 'bm-common-next "bm" "\
Goto next bookmark." t nil)

(autoload 'bm-next-mouse "bm" "\
Go to the next bookmark with the scroll wheel.
EV is the mouse event.

\(fn EV)" t nil)

(autoload 'bm-previous "bm" nil t nil)

(autoload 'bm-common-previous "bm" "\
Goto previous bookmark." t nil)

(autoload 'bm-previous-mouse "bm" "\
Go to the previous bookmark with the scroll wheel.
EV is the mouse event.

\(fn EV)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bm" '("bm-" "temporary-bookmark-p")))

;;;***

;;;### (autoloads nil "bm-sync" "bm-sync.el" (0 0 0 0))
;;; Generated autoloads from bm-sync.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bm-sync" '("bm-bookmark-")))

;;;***

;;;### (autoloads nil nil ("bm-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bm-autoloads.el ends here
