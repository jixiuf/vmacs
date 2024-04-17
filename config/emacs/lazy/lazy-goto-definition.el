;;; -*- lexical-binding: t; -*-
;;  ;; (evil-set-command-property 'goto-definition :jump t)

;; (eval-when-compile (require 'cl-lib))
;; ;; 跳转到函数定义的地方，跳转前在原处设置一个书签,space,可回原处
;; ;; default bind on gd
;; ;;;###autoload
;; (defun goto-definition (&optional _arg)
;;   "Make use of emacs' find-func and etags possibilities for finding definitions."
;;   (interactive "P")
;;   (let ((line (buffer-substring-no-properties
;;                (line-beginning-position) (line-end-position))))
;;     (bm-bookmark-add line nil t)        ;跳转前在原处加一个书签
;;     (cl-case major-mode
;;       (emacs-lisp-mode
;;        (require 'elisp-def)
;;        (elisp-def-mode 1)
;;        (condition-case nil (call-interactively 'elisp-def)
;;          (error (bm-bookmark-remove))))
;;       (lisp-interaction-mode
;;        (require 'elisp-def)
;;        (elisp-def-mode 1)
;;        (condition-case nil (call-interactively 'elisp-def)
;;          (error (bm-bookmark-remove))))
;;       (js-mode
;;        (if (functionp 'tern-find-definition)
;;            (call-interactively 'tern-find-definition)
;;          (lsp-find-definition)))
;;       (go-mode
;;        (lsp-find-definition))
;;       (otherwise
;;        (condition-case nil (lsp-find-definition)
;;          (error (bm-bookmark-remove))))))

;;   (setq this-command 'goto-definition))


(provide 'lazy-goto-definition)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-goto-def.el ends here.
