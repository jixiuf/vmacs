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

(defun vmacs-go-mode-hook()
  (setq go-ts-mode-indent-offset 4)
  ;; (add-hook 'after-save-hook 'vmacs-auto-gofmt nil t)
  (local-set-key (kbd "C-c i") 'go-goto-imports)
  (local-set-key (kbd "C-c g") 'golang-setter-getter)
  (local-set-key (kbd "C-c p") 'go-get-package-path)

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
         (pkg-path-output (shell-command-to-string pkg-path-cmd)))
    (setq pkg-path (substring pkg-path-output 0 -1)) ; Remove the trailing newline from pkg-path
    (setq pkg-path (substring pkg-path (+ 5 (length root )))) ; Remove the trailing newline from pkg-path
    (when (not (string= pkg-path ""))
      (kill-new (format"\"%s\"" pkg-path))
      (message pkg-path)
      ))) ; Copy the package path to the clipboard

(provide 'conf-program-golang)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-golang.el ends here.
