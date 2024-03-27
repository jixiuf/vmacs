;;; conf-dap-mode.el --- Description -*- lexical-binding: t; -*-
;;; Code:
;; https://medium.com/@apmattil/debug-go-golang-with-emacs-fbf840c0aa56
;; https://github.com/go-delve/delve/blob/master/Documentation/installation/osx/install.md
;; go get github.com/go-delve/delve/cmd/dlv
(setq dape-info-variable-table-row-config
 `((name . 20)
   (value . 120)
   (type . 20)))
(with-eval-after-load 'dape
  ;; (setq dape-key-prefix  "\C-c\C-c")
  (setq dape-buffer-window-arrangment 'gud)
  (setq dape-on-start-hooks '(dape-info))
  ;; inside your dape-config
  (setq dape-configs (assq-delete-all 'dlv dape-configs))
  (add-to-list 'dape-configs
               `(delve
                 modes (go-mode go-ts-mode)
                 ensure dape-ensure-command
                 fn (dape-config-autoport dape-config-tramp)
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1::autoport")
                 command-cwd dape-command-cwd
                 port :autoport
                 :type "debug"
                 :request "launch"
                 :mode (lambda() (if (string-suffix-p "_test.go"   (buffer-name)) "test" "debug"))
                 :cwd dape-cwd
                 :program (lambda()(if (string-suffix-p "_test.go"   (buffer-name))
                                       (concat "./" (file-relative-name default-directory (funcall dape-cwd-fn)))
                                     (funcall dape-cwd-fn)))
                 :args (lambda()
                         (require 'which-func)
                         (if (string-suffix-p "_test.go"   (buffer-name))
                             (when-let* ((test-name (which-function))
                                         (test-regexp (concat "^" test-name "$")))
                               (if test-name `["-test.run" ,test-regexp]
                                 (error "No test selected")))
                           []))))
  )

(provide 'conf-dap-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-dap-mode.el ends here.
