(eval-when-compile (require 'cl-lib))

(autoload 'find-library-name "find-func" "find libary")
;; 跳转到函数定义的地方，跳转前在原处设置一个书签,space,可回原处
;; default bind on gd
;;;###autoload
(defun goto-definition (&optional arg)
  "Make use of emacs' find-func and etags possibilities for finding definitions."
  (interactive "P")
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position)))
        ;; (marker (point-marker))
        )
    (bm-bookmark-add line nil t)        ;跳转前在原处加一个书签
    (cl-case major-mode
      (emacs-lisp-mode
       (elisp-def-mode 1)
       (call-interactively 'elisp-def))
      (lisp-interaction-mode
       (elisp-def-mode 1)
       (call-interactively 'elisp-def))
      ;; (erlang-mode (erl-find-source-under-point))
      (c++-mode
       (if (string-match "[ ]*#[ \t]*include[ \t]+[\"<]\\(.*\\)[\">]" line)
           ;; for c++-mode ,in current line contains #include ,then try to open the include file using helm-gtags
           (helm-gtags-find-files (match-string 1 line))
         (helm-gtags-find-tag-and-symbol)))
      (c-mode
       (if (string-match "[ ]*#[ \t]*include[ \t]+[\"<]\\(.*\\)[\">]" line)
           ;; for c-mode ,in current line contains #include ,then try to open the include file using helm-gtags
           (helm-gtags-find-files (match-string 1 line))
         (helm-gtags-find-tag-and-symbol)))
      (lua-mode
       (call-interactively 'helm-gtags-find-tag-and-symbol)
       )
      (csharp-mode
       (call-interactively 'helm-gtags-find-tag-and-symbol)
       )
      (js-mode
       (if (functionp 'tern-find-definition)
           (call-interactively 'tern-find-definition)
         (helm-gtags-find-tag-and-symbol)))
      (go-mode
       ;;(xref-find-definitions (xref-backend-identifier-at-point (xref-find-backend)))
       (let ((output (godef-jump (point))))
         (when (string= "godef: no identifier found" output)
           (bm-bookmark-remove)
           ;; (lsp-find-declaration)
           )))
      (python-mode
       (call-interactively 'lsp-find-definition)
       )
      (otherwise
       (helm-gtags-find-tag-and-symbol)))
    ;; (when (and (equal (point) (marker-position marker))
    ;;                (equal (current-buffer)(marker-buffer marker))
    ;;                )
    ;;   (bm-bookmark-remove))
    )
  (setq this-command 'goto-definition)
  )


(provide 'lazy-goto-definition)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-goto-def.el ends here.
