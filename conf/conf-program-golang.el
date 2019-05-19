;; https://github.com/saibing/bingo/wiki/Install

;; # (-u flag for "update")
;; go get -u github.com/nsf/gocode
;; gocode set
;; gocode set autobuild true
;; windows
;; go get -u -ldflags -H=windowsgui github.com/nsf/gocode
;; go get -u github.com/dougm/goflymake
;; go get -u -v github.com/9fans/go/...
;; mv $GOPATH/src/github.com/9fans $GOPATH/src/9fans.net
;; go get github.com/rogpeppe/godef

;; if [ ! -d $GOPATH/src/golang.org/x/tools ]; then
;;     cd $GOPATH/src/golang.org/x;git clone  https://github.com/golang/tools.git;cd -
;; else
;;     cd $GOPATH/src/golang.org/x/tools;git pull;cd -
;; fi
;; go install golang.org/x/tools/cmd/goimports
(let ((gopath (getenv "GOPATH")))
  (when
      (dolist (path  (parse-colon-path gopath))
        (setq exec-path (delete-dups  (cons (concat path  "/bin") exec-path))))))


(when (executable-find "gofmt")
  (setq-default gofmt-command (executable-find "gofmt")))
(when (executable-find "goimports")
  (setq-default gofmt-command (executable-find "goimports")))

(add-hook 'go-mode-hook 'vmacs-go-mode-hook)
(setq company-go-show-annotation t)

;; (defun vmacs-go-format()
;;   (when (eq major-mode 'go-mode)
;;     (gofmt-before-save)
;;     (save-mark-and-excursion
;;       (goto-char (point-min))
;;       (while  (search-forward-regexp "}\nfunc")
;;         (replace-match "}\n\nfunc")))
;;     )
;;   )

;; ;; 启用 company-lsp 补全后端
;; (push 'company-lsp company-backends)
;; ;; 各种设置, 避免 lsp-mode 花式报错.
;; (setq lsp-enable-eldoc nil) ;we will got error "Wrong type argument: sequencep" from `eldoc-message' if `lsp-enable-eldoc' is non-nil
;; (setq lsp-message-project-root-warning t) ;avoid popup warning buffer if lsp can't found root directory (such as edit simple *.py file)
;; (setq create-lockfiles nil) ;we will got error "Error from the Language Server: FileNotFoundError" if `create-lockfiles' is non-nil
;; ;; 避免 eglot 在 minibuffer 各种弹帮助文档, 弹得我眼睛疼.
;; (setq eglot-ignored-server-capabilites '(:hoverProvider)) ;disable show help document in minibuffer
;; (setf (cdr (assoc 'go-mode  eglot-server-programs)) '("bingo" "--mode"
;;                                                       "stdio" "--logfile" "/tmp/lspserver-go.log"
;;                                                       "--trace"
;;                                                       "--pprof" ":6060" ))
(require 'lsp-clients)
(setq lsp-enable-snippet t)

;; go get -u golang.org/x/tools/cmd/gopls
;; (setq lsp-clients-go-server "gopls")
;; (setq lsp-clients-go-diagnostics-enabled nil)
;; (require 'eglot)
(defun vmacs-go-mode-hook()
  ;; (require 'go-eldoc) ;; Don't need to require, if you install by package.el
  ;; (go-eldoc-setup)                    ;autoloaded
  (lsp)
  ;; github.com/syohex/emacs-go-eldoc
  ;; (eglot-ensure)
  (setq company-backends `(( company-lsp company-yasnippet company-files )
                           company-dabbrev
                           company-dabbrev-code))

  ;; git pre-commit for gofmt
  ;; http://tip.golang.org/misc/git/pre-commit
  (add-hook 'before-save-hook 'gofmt-before-save t t)
  (add-hook 'after-save-hook 'auto-go-install t t)


  (setq require-final-newline nil)
  (modify-syntax-entry ?_  "_" (syntax-table)) ;还是让 "_" 作为symbol，还不是word
  ;; (flycheck-mode 1)
  (local-set-key (kbd "C-c C-a") 'go-imports-insert-import)
  (local-set-key (kbd "C-c C-r") 'go-imports-reload-packages-list)
  (local-set-key (kbd "C-c i") 'go-goto-imports)
  ;; (local-set-key (kbd "C-c C-a") 'go-import-add)
  (local-set-key (kbd "C-c g") 'golang-setter-getter))


(defun auto-go-install()
  (when (equal major-mode 'go-mode)
    (unless (get-buffer-process  " *go-install*")
      (set-process-query-on-exit-flag
       (start-process-shell-command
        "go-install-generate-shell" " *go-install*"
        (format "go install"))nil))
    )
  )

(defun vmacs-auto-build-package()
  (interactive)
  (require 'go-imports)
  (go-imports-reload-packages-list)
  (unless (get-buffer-process  " *go-install*")
    (set-process-query-on-exit-flag
     (start-process-shell-command
      "go-install-generate-shell" " *go-install*"
      (format "perl %s ~/go |cut -d '\"' -f 4|grep -v vendor|sort|uniq|sed 's/^/go install /g'|sh"
              go-imports-find-packages-pl-path))nil)))

(run-with-idle-timer (* 60 5) t 'vmacs-auto-build-package)

(provide 'conf-program-golang)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-golang.el ends here.
