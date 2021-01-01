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

(vmacs-define-key grep-mode-map "g" nil  'grep)
(vmacs-define-key grep-mode-map "/" #'grep-hide-lines-not-matching  'grep)
(vmacs-define-key grep-mode-map "z" #'grep-hide-lines-matching  'grep)
(defun enable-wgrep-when-entry-insert()
  (when (derived-mode-p 'ivy-occur-mode 'rg-mode 'grep-mode 'embark-occur-mode
                        'ivy-occur-grep-mode 'helm-grep-mode)
    (require 'wgrep)
    (wgrep-change-to-wgrep-mode)
    ;; (when (equal last-command 'iedit-mode)
    ;;   ;; 恢复iedit bug导致rg iedit在进入wgrep 模式下 iedit 消失
    ;;   ;; 但是在 rg-group-result 为t 时似乎无效， 未明其因。
    ;;   (run-with-timer 1 nil 'iedit-mode '(4)))
    ))

(add-hook 'evil-insert-state-entry-hook 'enable-wgrep-when-entry-insert)


(defun grep-hide-lines-not-matching (search-text)
  "Hide lines that don't match the specified regexp."
  (interactive "MHide lines not matched by regexp: ")
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (let ((inhibit-read-only t)
          (start-position (point))
          (pos (re-search-forward search-text nil t)))
      (while pos
        (beginning-of-line)
        (delete-region start-position (point))
        (forward-line 1)
        (setq start-position (point))
        (if (eq (point) (point-max))
            (setq pos nil)
          (setq pos (re-search-forward search-text nil t))))
              (delete-region start-position (point-max) ))))

(defun grep-hide-lines-matching  (search-text)
  "Hide lines matching the specified regexp."
  (interactive "MHide lines matching regexp: ")
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (let ((inhibit-read-only t)
          (pos (re-search-forward search-text nil t))
          start-position)
      (while pos
        (beginning-of-line)
        (setq start-position (point))
        (end-of-line)
        (delete-region start-position (+ 1 (point)))
        (if (eq (point) (point-max))
            (setq pos nil)
          (setq pos (re-search-forward search-text nil t)))))))


(provide 'conf-wgrep)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-wgrep.el ends here.
