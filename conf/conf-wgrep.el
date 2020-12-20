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

(defun enable-wgrep-when-entry-insert()
  (when (derived-mode-p 'ivy-occur-mode 'rg-mode 'grep-mode 'embark-occur-mode
                        'ivy-occur-grep-mode 'helm-grep-mode)
    (wgrep-change-to-wgrep-mode)
    ;; (when (equal last-command 'iedit-mode)
    ;;   ;; 恢复iedit bug导致rg iedit在进入wgrep 模式下 iedit 消失
    ;;   ;; 但是在 rg-group-result 为t 时似乎无效， 未明其因。
    ;;   (run-with-timer 1 nil 'iedit-mode '(4)))
    ))

(add-hook 'evil-insert-state-entry-hook 'enable-wgrep-when-entry-insert)


(require 'wgrep)

;;;###autoload
(defun embark-occur-wgrep-setup ()
  (set (make-local-variable 'wgrep-header/footer-parser)
       'embark-occur-wgrep-prepare-header/footer)
  (set (make-local-variable 'wgrep-results-parser)
       'embark-occur-wgrep-parse-command-results)
  (wgrep-setup-internal))

(defun embark-occur-wgrep-prepare-header/footer ()
  (let ((beg (point-min))
        (end (point-min))
        (overlays (overlays-at (point-min))))
    ;; Set read-only grep result header
    (dolist (o overlays)
      (when (eq (overlay-get o 'face) 'tabulated-list-fake-header)
        (setq end (overlay-end o))))
    (put-text-property beg end 'read-only t)
    (put-text-property beg end 'wgrep-header t)
    ;; embark-occur-mode have NO footer.
    (put-text-property (1- (point-max)) (point-max) 'read-only t)
    (put-text-property (1- (point-max)) (point-max) 'wgrep-footer t)))


(defun embark-occur-wgrep-parse-command-results ()
  (while (not (eobp))
    (when (looking-at wgrep-line-file-regexp)
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (line (string-to-number (match-string 3)))
             (fn (match-string 1))
             (fnamelen (length fn))
             (dir (locate-dominating-file default-directory fn)))
        (unless (file-name-absolute-p fn)
          (setq fn (expand-file-name fn dir)))
        (let* ((fprop (wgrep-construct-filename-property fn)))
          (put-text-property start end 'wgrep-line-filename fn)
          (put-text-property start end 'wgrep-line-number line)
          (put-text-property start (+ start fnamelen) fprop fn))))
    (forward-line 1)))


;;;###autoload
(add-hook 'embark-occur-mode-hook 'embark-occur-wgrep-setup)

;; For `unload-feature'
(defun embark-wgrep-unload-function ()
  (remove-hook 'embark-occur-mode-hook 'embark-occur-wgrep-setup))

(provide 'conf-wgrep)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-wgrep.el ends here.
