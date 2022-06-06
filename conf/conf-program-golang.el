;;; -*- lexical-binding: t; -*-
;; go get  golang.org/x/tools/cmd/gopls

;; go install golang.org/x/tools/cmd/goimports
;; (let ((gopath (getenv "GOPATH")))
;;   (when
;;       (dolist (path  (parse-colon-path gopath))
;;         (setq exec-path (delete-dups  (cons (concat path  "/bin") exec-path))))))

(when (executable-find "gofmt") (setq-default gofmt-command (executable-find "gofmt")))
(when (executable-find "goimports") (setq-default gofmt-command (executable-find "goimports")))

(add-hook 'go-mode-hook 'vmacs-go-mode-hook)
;; 采用after-save-hook 触发，此时文件已经实质落盘,异步执行，不卡UI
(defun vmacs-auto-gofmt()
  (when (and buffer-file-name
             (eq major-mode 'go-mode))
    (let ((proc (start-process-shell-command
                 gofmt-command nil
                 (format "%s -w %s" gofmt-command buffer-file-name))))
      (set-process-query-on-exit-flag proc nil))))

(defun vmacs-go-mode-hook()
  (setq gofmt-show-errors 'buffer)
  (add-hook 'before-save-hook 'gofmt-before-save nil t)
  ;; (add-hook 'after-save-hook 'vmacs-auto-gofmt nil t)
  (local-set-key (kbd "C-c i") 'go-goto-imports)
  (local-set-key (kbd "C-c g") 'golang-setter-getter)
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
  ;; (modify-syntax-entry ?_  "_" (syntax-table)) ;还是让 "_" 作为symbol，还不是word
  )

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
