;;; conf-dap-mode.el --- Description -*- lexical-binding: t; -*-
;;; Code:
;; https://emacs-lsp.github.io/dap-mode/page/configuration/
;; https://medium.com/@apmattil/debug-go-golang-with-emacs-fbf840c0aa56
;; https://github.com/go-delve/delve/blob/master/Documentation/installation/osx/install.md
;; (setq dap-auto-configure-features '(sessions locals controls tooltip))
;; go get github.com/go-delve/delve/cmd/dlv
;; brew install nodejs
;; sudo /usr/sbin/DevToolsSecurity -enable
(require 'dap-hydra)
(require 'dap-ui)
(require 'dap-mouse)
(dap-auto-configure-mode)
;; (dap-mode 1)
;; ;; ;; The modes above are optional
;; (dap-ui-mode 1)
;; ;; ;; enables mouse hover support
;; (dap-tooltip-mode 1)
;; ;; ;; use tooltips for mouse hover
;; ;; ;; if it is not enabled `dap-mode' will use the minibuffer.
;; (tooltip-mode 1)
;; ;; ;; displays floating panel with debug buttons
;; ;; ;; requies emacs 26+
;; (dap-ui-controls-mode 1)

(add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra)))


(provide 'conf-dap-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-dap-mode.el ends here.
