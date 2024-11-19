;;; conf-dap-mode.el --- Description -*- lexical-binding: t; -*-
;;; Code:
;; https://github.com/go-delve/delve/blob/master/Documentation/installation/osx/install.md
;; go get github.com/go-delve/delve/cmd/dlv
(setq dape-key-prefix  "\C-cd")
(setq dape-request-timeout 30)
(setq dape-info-variable-table-row-config
      `((name . 20)
        (value . 120)
        (type . 20)))
(setq dape-buffer-window-arrangement 'right)
(setq dape-start-hook '())
(setq dape-info-buffer-window-groups
      '((dape-info-scope-mode dape-info-watch-mode
                              dape-info-stack-mode dape-info-modules-mode
                              dape-info-sources-mode dape-info-breakpoints-mode
                              dape-info-threads-mode)))
(defvar-keymap dape-active-mode-map
  :doc "Keymap used when `dape-active-mode' is active."
  "M-n" 'dape-next
  "M-c" 'dape-continue
  "M-i" 'dape-step-in
  "M-o" 'dape-step-out
  "M-h" 'dape-info
  ;; "C-c C-c" 'dape-breakpoint-toggle
  )



(require 'dape)
(define-minor-mode dape-active-mode
  "On when dape debugging session is active.
Non interactive global minor mode."
  :global t
  :keymap dape-active-mode-map
  :interactive nil)

(define-key dape-global-map "t" #'dape-repl-dwim)
(define-key dape-global-map "T" #'dape-select-thread)
(define-key dape-global-map "k" #'dape-breakpoint-remove-all)
(define-key dape-global-map "b" #'dape-breakpoint-toggle)
(define-key dape-global-map "d" #'dape-breakpoint-toggle)
(define-key dape-global-map "v" #'dape-eval)

(add-hook 'dape-repl-mode-hook #'(lambda()(tab-line-mode -1)))
(dolist (h '(dape-info-scope-mode-hook dape-info-watch-mode-hook
                                       dape-info-stack-mode-hook dape-info-modules-mode-hook
                                       dape-info-sources-mode-hook dape-info-breakpoints-mode-hook
                                       dape-info-threads-mode-hook))
  (add-hook h #'(lambda()(tab-line-mode -1))))

(global-set-key dape-key-prefix dape-global-map)
;; inside your dape-config
(setq dape-configs (assq-delete-all 'dlv dape-configs))
(add-to-list 'dape-configs
             `(delve
               modes (go-mode go-ts-mode)
               ensure dape-ensure-command
               fn (dape-config-autoport dape-config-tramp)
               command "dlv"
               command-args ("dap" "--listen" "127.0.0.1::autoport")
               command-insert-stderr t
               command-cwd (lambda()(if (string-suffix-p "_test.go" (buffer-name))
                                     default-directory (dape-cwd)))
               port :autoport
               :type "debug"
               :request "launch"
               :mode (lambda() (if (string-suffix-p "_test.go" (buffer-name)) "test" "debug"))
               :program "."
               :cwd "."
               :args (lambda()
                       (require 'which-func)
                       (if (string-suffix-p "_test.go" (buffer-name))
                           (when-let* ((test-name (which-function))
                                       (test-regexp (concat "^" test-name "$")))
                             (if test-name `["-test.run" ,test-regexp]
                               (error "No test selected")))
                         []))))


(provide 'conf-dap-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-dap-mode.el ends here.
