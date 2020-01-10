;; (eval-when-compile
;;   (require 'term)
;;   (require 'ange-ftp)
;;   (require 'em-term)
;;   (require 'em-hist)
;;   (require 'eshell))


;; ;; nil表示默认打开eshell ,t表示默认打开term
;; (defcustom vmacs-eshell-default-term "vterm"
;;   ""
;;   :type 'string
;;   :group 'vmacs-eshell)
;; (defvar vmacs-window-configration nil)

;; ;;;###autoload
;; (defun vmacs-eshell-term-new (&optional args)
;;   (interactive "P")
;;   (cond
;;    ((string-equal vmacs-eshell-default-term "vterm")
;;     (vterm))
;;    ((string-equal vmacs-eshell-default-term "eshell")
;;     (vmacs-eshell-new))
;;    ((string-equal vmacs-eshell-default-term "term")
;;     (vmacs-term-new))))

;; ;;;###autoload
;; (defun vmacs-term-mode-p(&optional ignore-scratch)
;;   (or (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode 'tsmterm-mode)
;;       (if ignore-scratch
;;           nil
;;         (string-match-p "\\*scratch-.*" (buffer-name)))))


;; ;;;###autoload
;; (defun vmacs-eshell-new()
;;   (interactive )
;;   (let* ((shell-buffer-name
;;           (generate-new-buffer-name
;;            (vmacs-eshell--generate-buffer-name "*esh* " "" default-directory))))
;;     (unless (vmacs-term-mode-p)
;;       (setq vmacs-window-configration (current-window-configuration)))
;;     (setq eshell-buffer-name shell-buffer-name)
;;     (eshell)
;;     (goto-char (point-max))
;;     ;; (insert (concat "cd " (concat "\""default-directory "\""))) ;;make sure current directory is default-directory
;;     ;; (eshell-send-input)
;;     (delete-other-windows)
;;     (pop-to-buffer shell-buffer-name))
;;   (evil-insert-state))

;; ;;;###autoload
;; (defun vmacs-term-new()
;;   (interactive )
;;   (term  (or explicit-shell-file-name
;; 			 shell-file-name) )
;;   (evil-insert-state))


;; ;;;###autoload
;; (defun vmacs-eshell-term-hide()
;;   (interactive)
;;   (dolist (buf (buffer-list))
;;     (with-current-buffer buf
;;       (when (vmacs-term-mode-p)
;;         (evil-normal-state)
;;         (bury-buffer))))
;;   (switch-to-buffer (vmacs-eshell--recent-other-buffer))
;;   (when vmacs-window-configration
;;     (set-window-configuration vmacs-window-configration)))

;; ;;;###autoload
;; (defun vmacs-eshell-term-show(&optional  ignore-scratch make-cd)
;;   (interactive)
;;   (let* ((shell-buffer (vmacs-eshell--recent-buffer ignore-scratch make-cd))
;;          (dir (and make-cd
;;                    (or list-buffers-directory default-directory)))
;;          cd-cmd cur-host term-dir)
;;     (when make-cd
;;       (if (ignore-errors (file-remote-p default-directory))
;;           (with-parsed-tramp-file-name (or list-buffers-directory default-directory) nil
;;             (setq cur-host host)
;;             (setq dir localname))
;;         (setq cur-host (system-name)))
;;       (setq cd-cmd (concat " cd " (shell-quote-argument dir)))
;;       )



;;     (if shell-buffer                 ;存在eshell，直接切到这个 eshell buffer
;;         (progn
;;           (unless (vmacs-term-mode-p ignore-scratch)
;;             (setq vmacs-window-configration (current-window-configuration)))
;;           (pop-to-buffer shell-buffer)
;;           (with-current-buffer shell-buffer
;;             (when (equal major-mode 'vterm-mode)
;;               (if (ignore-errors (file-remote-p default-directory))
;;                   (with-parsed-tramp-file-name default-directory nil
;;                     (setq vterm-dir localname))
;;                 (setq vterm-dir default-directory))

;;               (when (and (not (equal vterm-dir dir))
;;                          (equal vterm-host cur-host))
;;                 (vterm-send-key "u" nil nil t)
;;                 (vterm-send-string cd-cmd t)
;;                 (vterm-send-return))))

;;           (delete-other-windows))
;;       ;; 不存在已打开的eshell buffer
;;       (vmacs-eshell-term-new )))
;;   (evil-insert-state))

;; ;;;###autoload
;; (defun vmacs-eshell-term-toggle(&optional ignore-scratch)
;;   (interactive "P")
;;   (cond
;;    ((vmacs-term-mode-p)
;;     (vmacs-eshell-term-hide))
;;    (t                                   ; ;当前不在eshell中
;;     (vmacs-eshell-term-show ignore-scratch))))

;; ;;;###autoload
;; (defun vmacs-eshell-term-toggle-cd(&optional ignore-scratch)
;;   (interactive "P")
;;   (cond
;;    ((vmacs-term-mode-p)
;;     (vmacs-eshell-term-hide))
;;    (t                                   ; ;当前不在eshell中
;;     (vmacs-eshell-term-show ignore-scratch t))))

;; ;; 返回最近打开过的eshell term mode的buffer
;; (defun vmacs-eshell--recent-buffer(&optional ignore-scratch check-vterm-accept-cmd-p)
;;   (let ((shell-buffer ))
;;     (dolist (buf (buffer-list))
;;       (with-current-buffer buf
;;         (when (vmacs-term-mode-p ignore-scratch)
;;           (cond
;;            ((and (equal major-mode 'vterm-mode)
;;                  check-vterm-accept-cmd-p)
;;             (goto-char (point-at-bol))
;;             (when (vterm-skip-prompt)
;;               (unless shell-buffer
;;                 (setq shell-buffer buf))))
;;            (t
;;             (unless shell-buffer
;;               (setq shell-buffer buf)))))))
;;     shell-buffer))

;; ;; 返回最近打开过的buffer,即，切换到eshell buffer之前的那个buffer
;; (defun vmacs-eshell--recent-other-buffer()
;;   (let ((list (buffer-list))
;;         (index 0)
;;         shell-buffer buf )
;;     (cl-loop until shell-buffer do
;;              (setq buf (nth index list))
;;              (with-current-buffer buf
;;                (unless (vmacs-term-mode-p)
;;                  (setq shell-buffer buf)))
;;              (setq index (1+ index)))
;;     shell-buffer))

;; ;;;###autoload
;; (defun vmacs-eshell--generate-buffer-name(prefix cmd default-directory)
;;   (let* ((cmd (car (split-string cmd "[ |\t]" t " ")))
;;          (pwd (abbreviate-file-name default-directory))
;;          (dir-tokens (split-string pwd "[/|\\]" t " ")))
;;     (when (> (length dir-tokens) 2)
;;       (setq pwd (mapconcat  'identity (last dir-tokens 2)  "/")))
;;     (format "%s%s%s"  prefix (or cmd "") pwd)))

;; (defun vmacs-kill-buffer-hook()
;;   (when (vmacs-term-mode-p)
;;     (let ((proc (get-buffer-process (current-buffer))))
;;       (when (process-live-p proc)
;;         (when (derived-mode-p 'term-mode)
;;           (term-send-raw-string "\^C")
;;           (term-send-raw-string "\^D")
;;           (term-send-raw-string "\^\\"))
;;         (when (derived-mode-p 'vterm-mode)
;;           (vterm-send-C-c))
;;         (kill-process proc)))))

;; (add-hook 'kill-buffer-hook 'vmacs-kill-buffer-hook)


(provide 'lazy-toggle-eshell)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-toggle-eshell.el ends here.
