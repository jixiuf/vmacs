
;; (defun vmacs-switch-buffer--list()
;;   (let ((list  )
;;         (default-directory default-directory))
;;     (dolist (b (internal-complete-buffer "" nil t))
;;       (put-text-property 0 (length b) 'type 'buffer b)
;;       (add-to-list 'list b))
;;     (setq list (append list recentf-list))
;;     ;; (dolist (b recentf-list)
;;     ;;   (put-text-property 0 (length b) 'type 'file b)
;;     ;;   (add-to-list 'list b))

;;     (setq counsel--git-dir (locate-dominating-file default-directory ".git"))
;;     (when counsel--git-dir
;;       (setq counsel--git-dir (expand-file-name counsel--git-dir))
;;       (setq default-directory counsel--git-dir)
;;       (setq list (append list (split-string (shell-command-to-string (format "git ls-files --full-name --|sed \"s|^|%s/|g\"" default-directory)) "\n" t)))
;;       )
;;     list))

;; (defun vmacs-ivy-switch-buffer-action (buffer)
;;   "Switch to BUFFER.
;; BUFFER may be a string or nil."
;;        (with-ivy-window
;;          (if (zerop (length buffer))
;;              (switch-to-buffer
;;               ivy-text nil 'force-same-window)
;;            (let ((virtual (not (eq 'buffer (get-text-property 0 'type buffer))))
;;                  (view (assoc buffer ivy-views)))
;;              (cond ((and virtual
;;                          (not (get-buffer buffer)))
;;                     (find-file buffer))
;;                    (view
;;                     (delete-other-windows)
;;                     (let (
;;                     ;; silence "Directory has changed on disk"
;;                     (inhibit-message t))
;;                       (ivy-set-view-recur (cadr view))))
;;                    (t
;;                     (switch-to-buffer
;;                      buffer nil 'force-same-window)))))))

;; (defun vmacs-switch-buffer()
;;   "Switch to another buffer."
;;   (interactive)
;;   (let ((this-command 'ivy-switch-buffer)
;;         (ivy-sort-matches-functions-alist '((t . ivy-dired-history--sort)))

;;         )
;;     (ivy-read "Switch to buffer: " (vmacs-switch-buffer--list)
;;               :matcher #'ivy--switch-buffer-matcher
;;               :preselect (buffer-name (other-buffer (current-buffer)))
;;               :action #'vmacs-ivy-switch-buffer-action
;;               :keymap ivy-switch-buffer-map
;;               ;; :caller 'ivy-switch-buffer
;;               )))



(provide 'vmacs-switch-buffer)

;; Local Variables:
;; coding: utf-8
;; End:

;;; vmacs-switch-buffer.el ends here.
