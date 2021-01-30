;; (add-hook 'grep-setup-hook 'grep-mode-fun)
(setq-default wgrep-auto-save-buffer nil ;真正的打开文件，会处理各种find-file save-file的hook,慢，如gofmt引入package
              wgrep-too-many-file-length 1
              ;; wgrep-enable-key "i"
              wgrep-change-readonly-file t)

(defun vmacs-wgrep-finish-edit()
  (interactive)
  (if  current-prefix-arg
      (let ((wgrep-auto-save-buffer t))
        (call-interactively #'wgrep-finish-edit)
        )
    (call-interactively #'wgrep-finish-edit)
    (let ((count 0))
      (dolist (b (buffer-list))
        (with-current-buffer b
          (when (buffer-file-name)
            (let ((ovs (wgrep-file-overlays)))
              (when (and ovs (buffer-modified-p))
                (basic-save-buffer)
                ;; (kill-this-buffer) ;for xref project-find-regexp
                (setq count (1+ count)))))))
      (cond
       ((= count 0)
        (message "No buffer has been saved."))
       ((= count 1)
        (message "Buffer has been saved."))
       (t
        (message "%d buffers have been saved." count))))))

(with-eval-after-load 'wgrep
  (define-key wgrep-mode-map (kbd "C-g") 'wgrep-abort-changes)
  (define-key wgrep-mode-map (kbd "C-c C-c") 'vmacs-wgrep-finish-edit))

(evil-collection-define-key 'normal 'grep-mode-map
   "/" #'consult-focus-lines
   "z" #'consult-hide-lines
   "r" #'consult-reset-lines
   "i" #'evil-insert-state)


(defun enable-wgrep-when-entry-insert()
  (when (derived-mode-p  'rg-mode 'grep-mode 'embark-collect-mode)
    (require 'wgrep) (wgrep-change-to-wgrep-mode)))

(add-hook 'evil-insert-state-entry-hook 'enable-wgrep-when-entry-insert)




(provide 'conf-wgrep)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-wgrep.el ends here.
