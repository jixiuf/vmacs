;; https://github.com/saibing/tools
;; go get  golang.org/x/tools/cmd/gopls

;; go install golang.org/x/tools/cmd/goimports
;; (let ((gopath (getenv "GOPATH")))
;;   (when
;;       (dolist (path  (parse-colon-path gopath))
;;         (setq exec-path (delete-dups  (cons (concat path  "/bin") exec-path))))))

;; (when (executable-find "gofmt") (setq-default gofmt-command (executable-find "gofmt")))
;; (when (executable-find "goimports") (setq-default gofmt-command (executable-find "goimports")))
(add-hook 'go-ts-mode-hook 'vmacs-go-mode-hook)
;; 采用 after-save-hook 触发，此时文件已经实质落盘,异步执行，不卡 UI
;; (defun vmacs-auto-gofmt()
;;   (when (and buffer-file-name
;;          (eq major-mode 'go-mode))
;;     (set-process-query-on-exit-flag
;;      (start-process-shell-command
;;       gofmt-command nil
;;       (format "%s -w %s" gofmt-command buffer-file-name))
;;      nil)))

(with-eval-after-load 'dape
  ;; (setq dape-key-prefix  "\C-c\C-c")
(setq dape-on-start-hooks '(dape-info))
  (setq dape-info-display-buffer-action
        '((display-buffer-in-direction)
          . ((direction . left)
             (window-width . 52)
             )))

  (setq dape-buffers-on-start  '(dape-info))

  (defun vmacs-dape--select-go-args ()
    (require 'which-func)
    (if (string-suffix-p "_test.go"   (buffer-name))
        (when-let* ((test-name (which-function))
                    (test-regexp (concat "^" test-name "$")))
          (if test-name
              `["-test.run" ,test-regexp]
            (error "No test selected")))
      (if  current-prefix-arg
          (vconcat (split-string (read-shell-command "args: " nil
                                                     (if (equal (car compile-history) "")
                                                         '(compile-history . 1)
                                                       'compile-history))))
        [])))

  ;; https://github.com/go-delve/delve/blob/master/Documentation/usage/dlv_dap.md
  (defun vmacs-dape-test-p ()
    (if (string-suffix-p "_test.go"   (buffer-name))
        "test" "debug"))

  (defun vmacs-dape-relative-dir ()
    "Return the file directory relative to dape's cwd. This is used by Delve debugger."
    (if (string-suffix-p "_test.go"   (buffer-name))
        (concat "./" (file-relative-name
                      default-directory (funcall dape-cwd-fn)))
      (funcall dape-cwd-fn)))

  ;; inside your dape-config
  (add-to-list 'dape-configs
               `(delve
                 modes (go-mode go-ts-mode)
                 command "dlv"
                 command-cwd dape-cwd-fn
                 command-args ("dap" "--listen" "127.0.0.1:55878")
                 host "127.0.0.1"
                 port 55878
                 :type "go"
                 :name "go-debug"
                 :request "launch"
                 :mode vmacs-dape-test-p
                 :cwd dape-cwd-fn
                 :program vmacs-dape-relative-dir
                 :args vmacs-dape--select-go-args))
  )

(defun vmacs-go-mode-hook()
  (setq go-ts-mode-indent-offset 4)
  ;; (add-hook 'after-save-hook 'vmacs-auto-gofmt nil t)
  (require 'dape)
  (local-set-key dape-key-prefix dape-global-map)
  (local-set-key (kbd "C-c d") 'dape)
  (local-set-key (kbd "C-c n") 'dape-next)
  (local-set-key (kbd "C-c e") 'dape-expression-breakpoint)
  (local-set-key (kbd "C-c s") 'dape-step-in)
  (local-set-key (kbd "C-c o") 'dape-step-out)
  (local-set-key (kbd "C-c c") 'dape-continue)
  (local-set-key (kbd "C-c r") 'dape-restart)
  (local-set-key (kbd "C-c R") 'dape-repl)
  (local-set-key (kbd "C-c q") 'dape-quit)
  (local-set-key (kbd "C-c p") 'dape-pause)
  (local-set-key (kbd "C-c c") 'dape-continue)
  (local-set-key (kbd "C-c w") 'dape-watch-dwim)
  (local-set-key (kbd "C-c b") 'dape-toggle-breakpoint)
  (local-set-key (kbd "C-c C-c") 'dape-toggle-breakpoint)
  (local-set-key (kbd "C-c B") 'dape-remove-all-breakpoints)


  (local-set-key (kbd "C-c g") 'golang-setter-getter)
  (local-set-key (kbd "C-c C-p") 'go-get-package-path)

  (setq eglot-workspace-configuration
        ;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
        '((:gopls .
                  ((usePlaceholders . t)
                   (completeUnimported . t) ;;
                   (staticcheck . t)
                   (allowModfileModifications . t)
                   (allowImplicitNetworkAccess . t)
                   (directoryFilters . ["-vendor"])
                   ;; (analyses . ((unusedparams . t) (unusedwrite . t)))
                   ;; (annotations . ((bounds . t) (escape . t) (inline . t) (nil . t)))
                   ;; (codelenses . ((vendor . t)))
                   ;; (buildFlags . ["-mod=readonly"])
                   ;; (experimentalWorkspaceModule  . t)
                   ))))

  ;; (setq require-final-newline nil)
  ;; (modify-syntax-entry ?_  "_" (syntax-table)) ;还是让 "_" 作为 symbol，还不是 word
  )

(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)
(defun go-get-package-path ()
  "Get the package path of the Go file currently being edited and copy it to the clipboard.
   This function needs `go` command installed and a `go.mod` file in the project root directory."
  (interactive)
  (let* ((filename (buffer-file-name))
         (root (substring filename 0 (string-match-p "\\(?:/src/\\)" filename)))
         (pkg-path-cmd (concat "go list -f '{{ .Dir }}' " (file-name-directory filename))) ; Generate command to get the package path
         (mod-path-cmd (concat "go list -m")) ; Generate command to get the mod path
         (pkg-path-output (shell-command-to-string pkg-path-cmd))
         (mod-path-output (shell-command-to-string mod-path-cmd))
         )
    (setq pkg-path (substring pkg-path-output 0 -1)) ; Remove the trailing newline from pkg-path
    (setq mod-path (substring mod-path-output 0 -1)) ; Remove the trailing newline from pkg-path
    (setq pkg-path (substring pkg-path (+ 5 (length root ))))
    (setq pkg-path (format"\"%s%s\"" (file-name-parent-directory mod-path) pkg-path) )
    (when (not (string= pkg-path ""))
      (kill-new pkg-path)
      (message pkg-path))))

(provide 'conf-program-golang)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-golang.el ends here.
