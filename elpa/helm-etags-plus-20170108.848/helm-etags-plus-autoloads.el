;;; helm-etags-plus-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "helm-etags-plus" "helm-etags-plus.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from helm-etags-plus.el

(autoload 'helm-etags-plus-select "helm-etags-plus" "\
Find Tag using `etags' and `helm'

\(fn &optional ARG)" t nil)

(autoload 'helm-etags-plus-history-go-back "helm-etags-plus" "\
Go Back.

\(fn)" t nil)

(autoload 'helm-etags-plus-history-go-forward "helm-etags-plus" "\
Go Forward.

\(fn)" t nil)

(autoload 'helm-etags-plus-history "helm-etags-plus" "\
show all tag historys using `helm'

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-etags-plus" '("helm-etags-plus-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; helm-etags-plus-autoloads.el ends here
