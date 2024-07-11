;; (add-hook 'grep-setup-hook 'grep-mode-fun)
;; Then M-g n or M-g p for the next/previous match.
;; With (repeat-mode) consequent next/previous are just n and p.
;; (repeat-mode 1)
(setq grep-command "rg -nS --no-heading "
      grep-use-null-device nil)

(setq-default wgrep-auto-save-buffer nil ;真正的打开文件，会处理各种find-file save-file的hook,慢，如gofmt引入package
              wgrep-too-many-file-length 1
              wgrep-enable-key "i"
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
(with-eval-after-load 'grep
  (set-keymap-parent grep-mode-map meow-normal-state-keymap)
  ;; (define-key grep-mode-map (kbd "C-s") #'consult-focus-lines)
  ;; (define-key grep-mode-map "z" #'consult-hide-lines)
  (require 'wgrep)
  (advice-add 'grep-exit-message :after #'wgrep-change-to-wgrep-mode)

)
(with-eval-after-load 'wgrep
  (define-key wgrep-mode-map (kbd "C-c N/") #'consult-focus-lines)
  (define-key wgrep-mode-map (kbd "C-c Nz") #'consult-hide-lines)
  (define-key wgrep-mode-map (kbd "C-g") 'wgrep-abort-changes)
  (define-key wgrep-mode-map (kbd "C-c C-c") 'vmacs-wgrep-finish-edit)
  (define-key wgrep-mode-map (kbd "C-x C-s") 'vmacs-wgrep-finish-edit)
  (define-key wgrep-mode-map (kbd "M-n") 'compilation-next-error)
  (define-key wgrep-mode-map (kbd "M-p") 'compilation-previous-error)
  (define-key wgrep-mode-map (kbd "M-s-n") 'compilation-next-file)
  (define-key wgrep-mode-map (kbd "M-s-p") 'compilation-previous-file)
  )

(defvar my/re-builder-positions nil  "Store point and region bounds before calling re-builder")
;; https://karthinks.com/software/bridging-islands-in-emacs-1/
(with-eval-after-load 're-builder
  (setq reb-re-syntax 'string)          ; read/string/rx 写正则的时候  group写成\(\) 而非默认的 \\(\\)
  (define-advice re-builder (:around (orig-fun &rest args) query-replace)
    (setq my/re-builder-positions
          (cons (point)
                (when (region-active-p)
                  (list (region-beginning)
                        (region-end)))))
    (apply orig-fun args)
    (message "Return:Query Replace,C-cC-k:quit"))

  (define-key reb-mode-map (kbd "M-n") #'reb-next-match)
  (define-key reb-mode-map (kbd "M-p") #'reb-prev-match)
  ;; 可以使用 \(.*\)  后使用\1 来指定原始group值
  (define-key reb-mode-map (kbd "RET") #'reb-replace-regexp)
  ;; (define-key reb-lisp-mode-map (kbd "RET") #'reb-replace-regexp) # rx 模式
  (define-key reb-mode-map (kbd "C-c C-k") #'reb-quit))

(provide 'conf-wgrep)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-wgrep.el ends here.
