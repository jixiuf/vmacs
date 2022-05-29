;;; -*- lexical-binding: t; -*-
;; https://github.com/saibing/tools
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
    (set-process-query-on-exit-flag
     (start-process-shell-command
      gofmt-command nil
      (format "%s -w %s" gofmt-command buffer-file-name))
     nil)))
(setq gofmt-show-errors 'echo)
(defun vmacs-go-mode-hook()
  ;; (add-hook 'after-save-hook 'vmacs-auto-gofmt nil t)
 (add-hook 'before-save-hook 'gofmt-before-save nil t)
  ;; (add-hook 'before-save-hook 'gofmt-async nil t)
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
;; 尝试以异步的方式执行gofmt ,扫行时copy 至临时文件，生成patch文件
;; 生成后，检测文件有没有被再次编辑，有的话放弃此次gofmt
(defun gofmt-async ()
  "Format the current buffer according to the formatting tool.

The tool used can be set via ‘gofmt-command’ (default: gofmt) and additional
arguments can be set as a list via ‘gofmt-args’."
  (interactive)
  (let ((tmpfile (make-nearby-temp-file "gofmt" nil ""))
        (tmpfile2 (make-nearby-temp-file "gofmt" nil ""))
        (patchbuf (generate-new-buffer "*Gofmt patch*"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (buf (current-buffer))
        our-gofmt-args)
    (unwind-protect
        (save-restriction
          (widen)
          (write-region nil nil tmpfile)
          (write-region nil nil tmpfile2)

          (when (and (gofmt--is-goimports-p) buffer-file-name)
            (setq our-gofmt-args
                  (append our-gofmt-args
                          ;; srcdir, despite its name, supports
                          ;; accepting a full path, and some features
                          ;; of goimports rely on knowing the full
                          ;; name.
                          (list "-srcdir" (file-local-name
                                           (file-truename buffer-file-name))))))
          (setq our-gofmt-args
                (append our-gofmt-args gofmt-args
                        (list "-w" (file-local-name tmpfile))))
          (message "Calling gofmt: %s %s" gofmt-command our-gofmt-args)
          ;; We're using errbuf for the mixed stdout and stderr output. This
          ;; is not an issue because gofmt -w does not produce any stdout
          ;; output in case of success.
          (setq proc (apply #'start-file-process gofmt-command  nil gofmt-command  our-gofmt-args))
          (set-buffer-modified-p nil)
          (set-process-sentinel
           proc #'(lambda(p msg)
                    (when (string-prefix-p  "finished" msg)
                      (let ((local-copy (file-local-copy tmpfile))
                            diff-proc)
                        (unwind-protect
                            (let ((content (buffer-string)))
                              (setq diff-proc
                                    (start-process "gmftm-diff"
                                                   patchbuf "diff" "-n"
                                                   tmpfile2 (or local-copy tmpfile)))
                              (set-process-sentinel
                               diff-proc
                               #'(lambda(p msg)
                                   (unless (string-prefix-p  "finished" msg)
                                     (with-current-buffer buf
                                       (unless (buffer-modified-p)
                                         (go--apply-rcs-patch patchbuf)
                                         (basic-save-buffer)
                                         (message "Applied gofmt")))
                                     )
                                   (kill-buffer patchbuf)
                                   (delete-file tmpfile)
                                   (delete-file tmpfile2))))
                          (when local-copy (delete-file local-copy)))))
                    ))))))

(provide 'conf-program-golang)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-golang.el ends here.
