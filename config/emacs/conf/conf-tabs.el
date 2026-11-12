;;; -*- lexical-binding: t; -*-
(global-tab-line-mode t)
(global-set-key  (kbd "C-,") 'vmacs-tab-line-switch-to-prev-tab)
(global-set-key  (kbd "C-.") 'vmacs-tab-line-switch-to-next-tab)
(setq tab-line-new-button-show nil)  ;; do no
(setq tab-line-close-button-show nil)  ;; do not show close button
(setq tab-line-separator " ")

(setq tab-line-tabs-window-buffers-filter-function #'tab-line-tabs-non-excluded)
(setq tab-line-exclude-buffers
      '(or "\*eshell\*"
           (derived-mode completion-list-mode
                         eshell-mode
                         term-mode)
           ))

(setq tab-line-tabs-function #'tab-line-tabs-window-buffers)

;;; ghostel 单独分组 {{{
;; ghostel-mode 的 buffer 自成一组：tab line 只显示当前 buffer 所在组的 tab，
;; 组内仍按窗口的访问顺序排列（`tab-line-tabs-window-buffers' 的天然顺序），
;; C-, / C-. 也只在当前组内前后切换。

(defun vmacs-ghostel-buffer-p (&optional buffer)
  "Return non-nil if BUFFER (default: current buffer) is a ghostel terminal."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (derived-mode-p 'ghostel-mode)))))

(defun vmacs-tab-line-ghostel-group-p (&optional window)
  "Return non-nil if WINDOW's buffer belongs to the ghostel tab group."
  (vmacs-ghostel-buffer-p (window-buffer (or window (selected-window)))))

(define-advice tab-line-tabs-window-buffers (:around (orig &rest args) vmacs-ghostel-group)
  "Only show window buffers belonging to the same group as the current one."
  (let ((group (vmacs-tab-line-ghostel-group-p)))
    (seq-filter (lambda (buf)
                  (eq group (vmacs-ghostel-buffer-p buf)))
                (apply orig args))))

(defvar vmacs-tab-line-switch-group-scope nil
  "Non-nil while switching tabs, restricting the switch to the current group.")

(defun vmacs-switch-to-prev-buffer-skip (window buffer &optional _bury-or-kill)
  "Skip BUFFER when it is in another tab group than WINDOW's buffer.
Only applies while cycling tabs, see `vmacs-tab-line-switch-group-scope'."
  (and vmacs-tab-line-switch-group-scope
       (not (eq (vmacs-tab-line-ghostel-group-p window)
                (vmacs-ghostel-buffer-p buffer)))))

(setq switch-to-prev-buffer-skip #'vmacs-switch-to-prev-buffer-skip)

(defun vmacs-tab-line-switch-to-prev-tab (&optional event arg)
  "Like `tab-line-switch-to-prev-tab' but stay inside the current tab group."
  (interactive (list last-nonmenu-event
                     (prefix-numeric-value current-prefix-arg)))
  (let ((vmacs-tab-line-switch-group-scope t))
    (condition-case nil
        (tab-line-switch-to-prev-tab event arg)
      (user-error nil))))

(defun vmacs-tab-line-switch-to-next-tab (&optional event arg)
  "Like `tab-line-switch-to-next-tab' but stay inside the current tab group."
  (interactive (list last-nonmenu-event
                     (prefix-numeric-value current-prefix-arg)))
  (let ((vmacs-tab-line-switch-group-scope t))
    (condition-case nil
        (tab-line-switch-to-next-tab event arg)
      (user-error nil))))

;; }}}

;; (define-advice tab-line-tabs-window-buffers (:around (orig-fun &rest args) reverse-list)
;;   "Return a list of tabs that should be displayed in the tab line
;; but skip uninterested buffers."
;;   (let ((buffers (reverse (apply orig-fun args))))
;;     buffers))

;; 最多打开 10 个文件
(defun vmacs-prevent-open-too-much-files()
  (when (string-match-p "/tmp/pager/" (buffer-file-name))
    (setq truncate-lines nil)
    )
  (let* ((buffers (tab-line-tabs-window-buffers))
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

;; (defun vmacs-prev-tab()
;;   (interactive)
;;   (let ((buffers (tab-line-tabs-window-buffers)))
;;     (if (eq (car (last buffers)) (current-buffer))
;;         (tab-line-select-tab-buffer (nth 0 buffers)(selected-window))
;;       (call-interactively 'previous-buffer))))

;; (defun vmacs-next-tab()
;;   (interactive)
;;   (let ((buffers (tab-line-tabs-window-buffers)))
;;     (if (eq (nth 0 buffers) (current-buffer))
;;         (tab-line-select-tab-buffer (car (last buffers)) (selected-window))
;;       (call-interactively 'next-buffer))))

;; (setq switch-to-prev-buffer-skip #'vmacs-switch-to-prev-buffer-skip)
;; switch-to-prev-buffer 与 switch-to-next-buffer 时 skip 特定的 buffers
;;而 tab-line-switch-to-prev/next-tab 恰好使用了上面两个函数
;; (defun vmacs-switch-to-prev-buffer-skip(win buf bury-or-kill)
;;   (when (member this-command '(next-buffer previous-buffer
;;                                            vmacs-prev-tab
;;                                            vmacs-next-tab
;;                                            tab-line-switch-to-prev-tab
;;                                            tab-line-switch-to-next-tab))
;;     (cond
;;      ((vmacs-vterm-p)                ;当前 buffer 是 vterm
;;       (not (vmacs-vterm-p buf)))     ;若 buf 不是 vterm,则 skip
;;      ((vmacs-boring-buffer-p (current-buffer))
;;       (not (vmacs-boring-buffer-p buf)))
;;      (t                                 ;当前 buffer 是正常 buffer
;;       (or (vmacs-boring-buffer-p buf)   ;若 buf 是 boring buf 或 vterm，则跳过
;;           (vmacs-vterm-p buf))))))
;; (define-advice tab-line-tabs-window-buffers (:around (orig-fun &rest args) skip-buffer)
;;   "Return a list of tabs that should be displayed in the tab line
;; but skip uninterested buffers."
;;   (let ((buffers (reverse (apply orig-fun args))))
;;     (cond
;;      ((vmacs-vterm-p)               ;当前 buffer 是 vterm
;;       ;; 只返回 vterm buffer 作为当前 tab group 的 tab
;;       (seq-filter #'vmacs-vterm-p buffers))
;;      ((vmacs-boring-buffer-p (current-buffer))
;;       (seq-filter #'vmacs-boring-buffer-p buffers))
;;      (t
;;       ;; skip boring buffer 及 vterm
;;       (setq buffers (seq-remove #'vmacs-boring-buffer-p buffers))
;;       (seq-remove #'vmacs-vterm-p buffers)))))

;; (defun vmacs-vterm-p(&optional buf)
;;   (eq (buffer-local-value 'major-mode (or buf (current-buffer))) 'vterm-mode))

;; (defun vmacs-boring-buffer-p(&optional buf)
;;   (string-match-p (rx (or
;;                        "\*Async-native-compile-log\*"
;;                        "magit"
;;                        "^lua_*"
;;                        "\*company-documentation\*"
;;                        "\*eaf" "\*eldoc" "\*Launch " "*dap-"
;;                        "*EGLOT " "\*Flymake log\*"
;;                        "\*gopls::stderr\*" "\*gopls\*"
;;                        "\*Compile-Log\*" "*Backtrace*"
;;                        "*Package-Lint*" "\*sdcv\*" "\*tramp"
;;                        "\*lsp-log\*" "\*tramp" "\*Ibuffer\*"
;;                        "\*Help\*" "\*ccls" "\*vc"
;;                        "\*xref" "\*Warnings*" "\*Http"
;;                        "\*Async Shell Command\*"
;;                        "\*Shell Command Output\*"
;;                        "\*Calculator\*" "\*Calc "
;;                        "\*Flycheck error messages\*"
;;                        "\*Gofmt Errors\*"
;;                        "\*Ediff" "\*sdcv\*"
;;                        "\*Org PDF LaTex Output\*"
;;                        "\*Org Export"
;;                        "\*osx-dictionary\*" "\*Messages\*"
;;                        ))
;;                   (buffer-name buf)))




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
