(global-tab-line-mode t)
(global-set-key  (kbd "s-C-j") 'vmacs-prev-tab) ;H-k default C-x left
(global-set-key  (kbd "s-C-k") 'vmacs-next-tab)     ;H-j default C-x right
(global-set-key  (kbd "s-j") 'vmacs-prev-tab) ;H-k default C-x left
(global-set-key  (kbd "s-k") 'vmacs-next-tab)     ;H-j default C-x right
(setq tab-line-new-button-show nil)  ;; do not show add-new button
(setq tab-line-close-button-show nil)  ;; do not show close button
(setq tab-line-separator (propertize "▶" 'face  `(:inherit tab-line-tab-inactive :foreground "SeaGreen3")))

(defun vmacs-prev-tab()
  (interactive)
  (let ((buffers (tab-line-tabs-window-buffers)))
    (if (eq (car (last buffers)) (current-buffer))
        (tab-line-select-tab-buffer (nth 0 buffers)(selected-window))
      (call-interactively 'previous-buffer))))

(defun vmacs-next-tab()
  (interactive)
  (let ((buffers (tab-line-tabs-window-buffers)))
    (if (eq (nth 0 buffers) (current-buffer))
        (tab-line-select-tab-buffer (car (last buffers)) (selected-window))
      (call-interactively 'next-buffer))))

(setq switch-to-prev-buffer-skip #'vmacs-switch-to-prev-buffer-skip)
;; switch-to-prev-buffer 与 switch-to-next-buffer 时 skip 特定的 buffers
;;而 tab-line-switch-to-prev/next-tab 恰好使用了上面两个函数
(defun vmacs-switch-to-prev-buffer-skip(win buf bury-or-kill)
  (when (member this-command '(next-buffer previous-buffer
                                           vmacs-prev-tab
                                           vmacs-next-tab
                                           tab-line-switch-to-prev-tab
                                           tab-line-switch-to-next-tab))
    (cond
     ((vmacs-vterm-p)                ;当前 buffer 是 vterm
      (not (vmacs-vterm-p buf)))     ;若 buf 不是 vterm,则 skip
     ((vmacs-boring-buffer-p (current-buffer))
      (not (vmacs-boring-buffer-p buf)))
     (t                                 ;当前 buffer 是正常 buffer
      (or (vmacs-boring-buffer-p buf)   ;若 buf 是 boring buf 或 vterm，则跳过
          (vmacs-vterm-p buf))))))
(defadvice tab-line-tabs-window-buffers (around skip-buffer activate)
  "Return a list of tabs that should be displayed in the tab line
but skip uninterested buffers."
  (let ((buffers (reverse ad-do-it)))
    (cond
     ((vmacs-vterm-p)               ;当前 buffer 是 vterm
      ;; 只返回 vterm buffer 作为当前 tab group 的 tab
      (setq ad-return-value (seq-filter #'vmacs-vterm-p buffers)))
     ((vmacs-boring-buffer-p (current-buffer))
      (setq ad-return-value (seq-filter #'vmacs-boring-buffer-p buffers)))
     (t
      ;; skip boring buffer 及 vterm
      (setq buffers (seq-remove #'vmacs-boring-buffer-p buffers))
      (setq ad-return-value  (seq-remove #'vmacs-vterm-p buffers))))))

(defun vmacs-vterm-p(&optional buf)
  (eq (buffer-local-value 'major-mode (or buf (current-buffer))) 'vterm-mode))

(defun vmacs-boring-buffer-p(&optional buf)
  (string-match-p (rx (or
                       "\*Async-native-compile-log\*"
                       "magit"
                       "\*company-documentation\*"
                       "\*eaf" "\*eldoc" "\*Launch " "*dap-"
                       "*EGLOT " "\*Flymake log\*"
                       "\*gopls::stderr\*" "\*gopls\*"
                       "\*Compile-Log\*" "*Backtrace*"
                       "*Package-Lint*" "\*sdcv\*" "\*tramp"
                       "\*lsp-log\*" "\*tramp" "\*Ibuffer\*"
                       "\*Help\*" "\*ccls" "\*vc"
                       "\*xref" "\*Warnings*" "\*Http"
                       "\*Async Shell Command\*"
                       "\*Shell Command Output\*"
                       "\*Calculator\*" "\*Calc "
                       "\*Flycheck error messages\*"
                       "\*Gofmt Errors\*"
                       "\*Ediff" "\*sdcv\*"
                       "\*Org PDF LaTex Output\*"
                       "\*Org Export"
                       "\*osx-dictionary\*" "\*Messages\*"
                       ))
                  (buffer-name buf)))



;; 最多打开 10 个文件
(defun vmacs-prevent-open-too-much-files()
  (let* ((buffers (reverse (tab-line-tabs-window-buffers)))
         (buffer-save-without-query t)
         (len (length buffers))
         (max 9) (i 0))
    (dolist (buffer buffers)
      (when (and (< i (- len max)) (>= len max))
        (when (and (buffer-live-p buffer)
                   (not (member (buffer-name buffer) '("*scratch*"))))
          (with-current-buffer buffer
            (when (buffer-file-name buffer)
              (basic-save-buffer))
            (message "kill %s" (buffer-name buffer))
            (kill-buffer buffer))))
      (setq i (1+ i)))))

(add-hook 'find-file-hook #'vmacs-prevent-open-too-much-files)

;; (setq tab-line-tabs-buffer-group-function #'vmacs-tab-line-buffer-group)
;; (setq tab-line-tabs-function #'tab-line-tabs-buffer-groups)
;; (setq tab-line-tabs-buffer-list-function #'vmacs-tab-line-tabs-buffer-list)
;; (defun vmacs-tab-line-tabs-buffer-list ()
;;   (seq-remove #'vmacs-boring-buffer-p (tab-line-tabs-buffer-list)))



;; (setq-default centaur-tabs-hide-tabs-hooks   nil)
;; (setq-default centaur-tabs-cycle-scope 'tabs)
;; (setq-default centaur-tabs-display-sticky-function-name nil)
;; (setq-default centaur-tabs-style "zigzag")

;; (require 'centaur-tabs)
;; (vmacs-leader (kbd "e") 'centaur-tabs-forward-group)

;; ;; 只为 eshell-mode term-mode 启用 centaur-tabs

;; (setq centaur-tabs-buffer-groups-function 'vmacs-centaur-tabs-buffer-groups)

;; (defun vmacs-centaur-tabs-buffer-groups ()
;;   "`centaur-tabs-buffer-groups' control buffers' group rules.
;;     Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
;;     All buffer name start with * will group to \"Emacs\".
;;     Other buffer group by `projectile-project-p' with project name."
;;   (list
;;    (cond
;;     ((derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)
;;      "Term")
;;     ((string-match-p (rx (or
;;                           "\*Launch "
;;                           "*dap-"
;;                           ))
;;                      (buffer-name))
;;      "Debug")
;;     ((and (string-match-p (rx (or
;;                           "\*Async-native-compile-log\*"
;;                           "\*Helm"
;;                           "magit-process"
;;                           "\*company-documentation\*"
;;                           "\*helm"
;;                           "\*eaf"
;;                           "\*eldoc"
;;                           "\*Launch "
;;                           "*dap-"
;;                           "*EGLOT "
;;                           "\*Flymake log\*"
;;                           "\*gopls::stderr\*"
;;                           "\*gopls\*"
;;                           "\*Compile-Log\*"
;;                           "*Backtrace*"
;;                           "*Package-Lint*"
;;                           "\*sdcv\*"
;;                           "\*tramp"
;;                           "\*lsp-log\*"
;;                           "\*tramp"
;;                           "\*ccls"
;;                           "\*vc"
;;                           "\*xref"
;;                           "\*Warnings*"
;;                           "\*Http"
;;                           "\*Async Shell Command\*"
;;                           "\*Shell Command Output\*"
;;                           "\*Calculator\*"
;;                           "\*Calc "
;;                           "\*Flycheck error messages\*"
;;                           "\*Gofmt Errors\*"
;;                           "\*Ediff"
;;                           "\*sdcv\*"
;;                           "\*Org PDF LaTex Output\*"
;;                           "\*Org Export"
;;                           "\*osx-dictionary\*"
;;                           "\*Messages\*"
;;                           ))
;;                      (buffer-name))
;;           (not (string-match-p "\.el$" (buffer-name)) )
;;           )
;;      "Emacs")
;;     ;; ((not (vmacs-show-tabbar-p)) nil)
;;     (t "Common"))))
;; (setq centaur-tabs-adjust-buffer-order 'left)
;; (centaur-tabs-enable-buffer-reordering)
;; ;; (setq centaur-tabs-label-fixed-length 30)
;; (centaur-tabs-mode t)

;; (add-hook 'after-init-hook (lambda() (remove-hook 'kill-buffer-hook 'centaur-tabs-buffer-track-killed)))

(provide 'conf-tabs)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-centaur-tabs.el ends here.
