;; ;; ;; 像ido 一样 在目录上return 则进入目录而不是打开dired
;; ;;;###autoload
;; (defun vmacs-helm-ido-exit-minibuffer()
;;   (interactive)
;;   ;; 如果选中的是..或.则认为选中的是第一个非. 或..项目, 即跳过.. 与.
;;   (let ((selection (helm-get-selection)))
;;     (when (string-match-p "/\\.$" selection)
;;       (helm-move-selection-common :where 'line :direction 'next)
;;       (helm-move-selection-common :where 'line :direction 'next))
;;     (when (string-match-p "/\\.\\.$" selection)
;;       (helm-move-selection-common :where 'line :direction 'next))
;;     (when (string-match-p "\\.DS_Store" (helm-get-selection))
;;       (helm-move-selection-common :where 'line :direction 'next))

;;     (if (file-directory-p (helm-get-selection))
;;         (call-interactively 'helm-execute-persistent-action)
;;       (call-interactively 'helm-maybe-exit-minibuffer ))))

;; ;;;###autoload
;; (defun vmacs-helm-find-file-select-text()
;;   "like `C-j' in ido."
;;   (interactive)
;;   (let ((file (find-file-noselect helm-pattern)))
;;     (helm-run-after-exit 'switch-to-buffer file)
;;     (helm-exit-minibuffer)))

;; ;;;###autoload
;; (defun vmacs-helm-magic-delete-char( arg)
;;   "ido like `C-d' for helm."
;;   (interactive "P")
;;   (if (eobp)
;;       (helm-select-nth-action 0)
;;     (delete-char (min (prefix-numeric-value arg)
;;                       (- (point-max) (point))))))

;; ;;;###autoload
;; (defun vmacs-helm-magic-eol( arg)
;;   "C-e move to end of line or execute helm-execute-persistent-action"
;;   (interactive "P")
;;   (if (eobp)
;;       (call-interactively 'helm-execute-persistent-action)
;;     (call-interactively 'end-of-line)))

;; ;; ;; 像ido 一样 在目录上return 则进入目录而不是打开dired
;; ;; (defun vmacs-helm-find-files-navigate-forward (orig-fun &rest args)
;; ;;   (if (file-directory-p (helm-get-selection))
;; ;;       (helm-execute-persistent-action)
;; ;;     (apply orig-fun args)))
;; ;; (advice-add 'helm-maybe-exit-minibuffer :around #'vmacs-helm-find-files-navigate-forward)
;; ;; ;; (advice-add 'helm-confirm-and-exit-minibuffer :around #'vmacs-helm-find-files-navigate-forward)


;; ;; (defun vmacs-helm-find-files-navigate-forward (orig-fun &rest args)
;; ;;   (if (and (equal "Find Files" (assoc-default 'name (helm-get-current-source)))
;; ;;            (equal args nil)
;; ;;            (stringp (helm-get-selection))
;; ;;            (not (file-directory-p (helm-get-selection))))
;; ;;       (helm-maybe-exit-minibuffer)
;; ;;     (apply orig-fun args)))
;; ;; (advice-add 'helm-execute-persistent-action :around #'vmacs-helm-find-files-navigate-forward)

;; ;;;###autoload
;; (defun helm-search(&optional arg)
;;   (interactive "P")
;;   (if (vc-find-root default-directory ".git")
;;       (call-interactively 'helm-grep-do-git-grep)
;;     (helm-do-grep-1 (list default-directory) (not arg))))

;; ;; (defun helm-do-grep (&optional arg)
;; ;;   (interactive "P")
;; ;;   (helm-do-grep-1 (list default-directory) (not arg)))

;; ;; (defun helm-do-my-grep-ag (arg)
;; ;;   "Preconfigured helm for grepping with AG in `default-directory'.
;; ;; With prefix-arg prompt for type if available with your AG version."
;; ;;   (interactive "P")
;; ;;   (require 'helm-files)
;; ;;   (let ((helm-grep-ag-command))
;; ;;     (if  (> (prefix-numeric-value current-prefix-arg) 1)
;; ;;         (setq helm-grep-ag-command   "ag --line-numbers -S --hidden --color-match=\"10;35\" --depth 25 --nogroup %s %s %s")
;; ;;       (setq helm-grep-ag-command   "ag --line-numbers -S --hidden --color-match=\"10;35\"  --nogroup %s %s %s")))
;; ;;   (helm-grep-ag default-directory nil))


(provide 'lazy-helm)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-helm.el ends here.
