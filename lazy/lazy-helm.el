;; ;; 像ido 一样 在目录上return 则进入目录而不是打开dired
;;;###autoload
(defun vmacs-helm-ido-exit-minibuffer()
  (interactive)
  ;; 如果选中的是..或.则认为选中的是第一个非. 或..项目, 即跳过.. 与.
  (let ((selection (helm-get-selection)))
    (when (string-match-p "/\\.$" selection)
      (helm-move-selection-common :where 'line :direction 'next)
      (helm-move-selection-common :where 'line :direction 'next))
    (when (string-match-p "/\\.\\.$" selection)
      (helm-move-selection-common :where 'line :direction 'next))
    (when (string-match-p "\\.DS_Store" (helm-get-selection))
      (helm-move-selection-common :where 'line :direction 'next))

    (if (file-directory-p (helm-get-selection))
        (call-interactively 'helm-execute-persistent-action)
      (call-interactively 'helm-maybe-exit-minibuffer ))))

;;;###autoload
(defun vmacs-helm-find-file-select-text()
  "like `C-j' in ido."
  (interactive)
  (let ((file (find-file-noselect helm-pattern)))
    (helm-run-after-exit 'switch-to-buffer file)
    (helm-exit-minibuffer)))

;;;###autoload
(defun vmacs-helm-magic-delete-char( arg)
  "ido like `C-d' for helm."
  (interactive "P")
  (if (eobp)
      (helm-select-nth-action 0)
    (delete-char (min (prefix-numeric-value arg)
                      (- (point-max) (point))))))

;;;###autoload
(defun vmacs-helm-magic-eol( arg)
  "C-e move to end of line or execute helm-execute-persistent-action"
  (interactive "P")
  (if (eobp)
      (call-interactively 'helm-execute-persistent-action)
    (call-interactively 'end-of-line)))

(provide 'lazy-helm)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-helm.el ends here.
