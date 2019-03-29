;;; lsp-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-clients" "lsp-clients.el" (0 0 0 0))
;;; Generated autoloads from lsp-clients.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-clients" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-mode" "lsp-mode.el" (0 0 0 0))
;;; Generated autoloads from lsp-mode.el

(autoload 'lsp "lsp-mode" "\
Entry point for the server startup.
When IGNORE-MULTI-FOLDER is t the lsp mode will start new
language server even if there is language server which can handle
current language. When IGNORE-MULTI-FOLDER is nil current file
will be openned in multi folder language server if there is
such.

\(fn &optional IGNORE-MULTI-FOLDER)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-mode" '("log--notification-performance" "lsp-" "make-lsp-client" "seq-" "when-lsp-workspace" "with-lsp-workspace")))

;;;***

;;;### (autoloads nil nil ("lsp-mode-pkg.el" "lsp.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-mode-autoloads.el ends here
