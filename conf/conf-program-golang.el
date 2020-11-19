;; https://github.com/saibing/tools
;; go get  golang.org/x/tools/cmd/gopls

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


(defun vmacs-go-mode-hook()
  (setq eglot-workspace-configuration
        '((:gopls . (:usePlaceholders t :completeUnimported  t ;; :staticcheck t
                                      ))))
  (add-hook 'before-save-hook #'gofmt 20 t)
  ;; (add-hook 'before-save-hook #'lsp-organize-imports 20 t)
  (setq require-final-newline nil)
  (modify-syntax-entry ?_  "_" (syntax-table)) ;还是让 "_" 作为symbol，还不是word
  (local-set-key (kbd "C-c i") 'go-goto-imports)
  (local-set-key (kbd "C-c g") 'golang-setter-getter))


(provide 'conf-program-golang)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-golang.el ends here.
