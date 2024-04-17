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
(defun vmacs-go-mode-hook()
  (setq go-ts-mode-indent-offset 4)
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
                   )))))

(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)


(provide 'conf-program-golang)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-golang.el ends here.
