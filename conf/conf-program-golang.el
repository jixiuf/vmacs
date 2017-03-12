;; # (-u flag for "update")
;; go get -u github.com/nsf/gocode
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

(defun vmacs-go-mode-hook()
  ;; (require 'go-eldoc) ;; Don't need to require, if you install by package.el
  ;; github.com/syohex/emacs-go-eldoc
  (go-eldoc-setup)                    ;autoloaded
  (setq company-backends `((company-go company-yasnippet )
                           (company-files company-yasnippet)
                           company-capf
                           company-dabbrev
                           company-dabbrev-code
                           ))

  ;; git pre-commit for gofmt
  ;; http://tip.golang.org/misc/git/pre-commit
  (add-hook 'before-save-hook 'gofmt-before-save)

  (setq require-final-newline nil)
  (modify-syntax-entry ?_  "_" (syntax-table)) ;还是让 "_" 作为symbol，还不是word
  (flycheck-mode 1)
  (local-set-key (kbd "C-c i") 'go-goto-imports)
  (local-set-key (kbd "C-c g") 'golang-setter-getter)
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports))

(provide 'conf-program-golang)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-golang.el ends here.
