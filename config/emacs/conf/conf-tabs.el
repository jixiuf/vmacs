;;; -*- lexical-binding: t; -*-
(global-tab-line-mode t)
(global-set-key  (kbd "C-,") 'vmacs-tab-line-switch-to-prev-tab)
(global-set-key  (kbd "C-.") 'vmacs-tab-line-switch-to-next-tab)
;; xremap sends C-, / C-. as C-x <left>/<right> when a client surface
;; (firefox, tmux, feishu) has focus; rebind them to group-scoped cycling
;; so every path lands in the same logic.
(global-set-key  (kbd "C-x <left>") 'vmacs-tab-line-switch-to-prev-tab)
(global-set-key  (kbd "C-x <right>") 'vmacs-tab-line-switch-to-next-tab)
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

;; Per-group tab order: MRU on deliberate switch (recency adjacency),
;; frozen while cycling with C-, / C-. so they move along the tab line.
(setq tab-line-tabs-function #'vmacs-tab-line-tabs-stable-group)

;;; ghostel / firefox 单独分组 {{{
;; ghostel-mode、ewm firefox surface 的 buffer 各自成一组：tab line 只显示当前
;; buffer 所在组的 tab，组内按最近访问就近排列（刚离开的 buffer 在当前 buffer
;; 左邻），C-, / C-. 只在当前组内按显示顺序左右切换（见
;; `vmacs-tab-line-cycle-tabs'）。

(defun vmacs-ghostel-buffer-p (&optional buffer)
  "Return non-nil if BUFFER (default: current buffer) is a ghostel terminal."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (derived-mode-p 'ghostel-mode)))))

(defun vmacs-ewm-app-buffer-p (app-regexp &optional buffer)
  "Return non-nil if BUFFER is an ewm surface whose app matches APP-REGEXP."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (buffer-local-value 'ewm-surface-app buffer)
         (string-match-p app-regexp (buffer-local-value 'ewm-surface-app buffer)))))

(defun vmacs-firefox-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is a firefox/chrome ewm surface."
  (vmacs-ewm-app-buffer-p (rx (or "firefox" "firefox-bin" "firefox-esr" "google-chrome")) buffer))

(defun vmacs-tab-line-buffer-group (&optional buffer)
  "Return the tab group symbol BUFFER (default: current buffer) belongs to."
  (cond
   ((vmacs-ghostel-buffer-p buffer) 'ghostel)
   ((vmacs-firefox-buffer-p buffer) 'firefox)
   (t 'main)))

(define-advice tab-line-tabs-window-buffers (:around (orig &rest args) vmacs-ghostel-group)
  "Only show window buffers belonging to the same group as the current one."
  (let ((group (vmacs-tab-line-buffer-group)))
    (seq-filter (lambda (buf)
                  (eq group (vmacs-tab-line-buffer-group buf)))
                (apply orig args))))

(defun vmacs-tab-line-tabs-stable-group ()
  "Tabs for the current group: MRU on re-seed, stable while cycling.
Any command other than consecutive C-, / C-. presses re-sorts the
tabs by recency (MRU), so the buffer you just visited sits right
next to the current one (recency adjacency).  Consecutive C-, / C-.
presses freeze the order, visually moving along the tab line and
walking the whole group without reshuffling.
Written as a standalone `tab-line-tabs-function' because
`tab-line-tabs-fixed-window-buffers' snapshots its sort-key table
before calling `tab-line-tabs-window-buffers', which makes in-call
re-seeding invisible to its own sort."
  (let* ((group (vmacs-tab-line-buffer-group))
         (cycling (memq last-command
                        '(vmacs-tab-line-switch-to-prev-tab
                          vmacs-tab-line-switch-to-next-tab)))
         (fresh (seq-filter (lambda (buf)
                              (eq group (vmacs-tab-line-buffer-group buf)))
                            (tab-line-tabs-window-buffers)))
         (old (seq-filter #'buffer-live-p
                          (window-parameter nil 'tab-line-buffers)))
         ;; Freeze only while cycling within the same group; any other
         ;; command (switch-to-buffer, consult-buffer, focus change, ...)
         ;; re-sorts by recency.
         (reseed (or (not cycling)
                     (not (eq group (window-parameter nil 'vmacs-tab-line--last-group))))))
    (set-window-parameter nil 'vmacs-tab-line--last-group group)
    (if (and reseed (not (equal fresh old)))
        (progn
          ;; TEMP DEBUG
          (vmacs-tab-line--log
           "re-seed last-cmd=%S group=%S cur=%s fresh=%S"
           last-command group (buffer-name) (mapcar #'buffer-name fresh))
          (set-window-parameter nil 'tab-line-buffers fresh)
          fresh)
      ;; Stable: keep stored order for buffers still present; append newcomers.
      (let* ((kept (seq-filter (lambda (b) (memq b fresh)) old))
             (new (seq-remove (lambda (b) (memq b kept)) fresh))
             (result (if reseed fresh (append kept new))))
        (set-window-parameter nil 'tab-line-buffers result)
        result))))

(defvar vmacs-tab-line-switch-group-scope nil
  "Non-nil while switching tabs, restricting the switch to the current group.")

(defun vmacs-switch-to-prev-buffer-skip (window buffer &optional _bury-or-kill)
  "Skip BUFFER when it is in another tab group than WINDOW's buffer.
Only applies while cycling tabs, see `vmacs-tab-line-switch-group-scope'."
  (and vmacs-tab-line-switch-group-scope
       (not (eq (vmacs-tab-line-buffer-group (window-buffer window))
                (vmacs-tab-line-buffer-group buffer)))))

(setq switch-to-prev-buffer-skip #'vmacs-switch-to-prev-buffer-skip)

(defvar tab-line-tabs-function)  ; dynamic binding, defined in tab-line.el
(defvar tab-line-switch-cycling) ; dynamic binding, defined in tab-line.el

(defun vmacs-bury-tab-group (group)
  "Bury every live buffer in tab GROUP, then switch out of it.
Switches to the most recent buffer outside GROUP, so after hiding a
whole group (ghostel terminals, firefox surfaces, ...) the tab line
lands on the main group."
  (let ((matched (seq-filter (lambda (buf)
                               (and (buffer-live-p buf)
                                    (eq (vmacs-tab-line-buffer-group buf) group)))
                             (buffer-list))))
    (dolist (buf matched)
      (bury-buffer buf))
    (let ((target (seq-find
                   (lambda (buf)
                     (and (not (memq buf matched))
                          (not (string-prefix-p " " (buffer-name buf)))))
                   (buffer-list))))
      (when target
        (switch-to-buffer target)))))

;; TEMP DEBUG: remove after investigation
(defun vmacs-tab-line--log (format-string &rest args)
  "Append a debug line to /tmp/vmacs-tabs.log."
  (ignore-errors
    (let ((line (concat (format-time-string "[%H:%M:%S] ")
                        (apply #'format format-string args) "\n"))
          (old (if (file-exists-p "/tmp/vmacs-tabs.log")
                   (with-temp-buffer
                     (insert-file-contents "/tmp/vmacs-tabs.log")
                     (buffer-string))
                 "")))
      (with-temp-file "/tmp/vmacs-tabs.log"
        (insert old line)))))

(defun vmacs-tab-line-cycle-tabs (step)
  "Cycle STEP tabs within the current tab group.
With the stable per-group tab order, this simply moves along the
displayed tab line, so the selected tab moves left/right visually."
  (let* ((tabs (funcall tab-line-tabs-function))
         (buffers (seq-keep
                   (lambda (tab)
                     (or (and (bufferp tab) tab)
                         (cdr (assq 'buffer tab))))
                   tabs))
         (old-pos (seq-position buffers (current-buffer)))
         (len (length buffers)))
    (vmacs-tab-line--log
     "cycle step=%s cur=%s(%S) pos=%s len=%s tabs=%s"
     step (buffer-name) (vmacs-tab-line-buffer-group) old-pos len
     (mapcar #'buffer-name buffers))
    (when (and old-pos (> len 1))
      (let ((new-pos (mod (+ old-pos step) len)))
        (unless (eq (nth new-pos buffers) (current-buffer))
          (switch-to-buffer (nth new-pos buffers)))))))

(defun vmacs-tab-line-switch-to-prev-tab (&optional _event arg)
  "Switch to the previous tab within the current tab group."
  (interactive (list last-nonmenu-event
                     (prefix-numeric-value current-prefix-arg)))
  (vmacs-tab-line-cycle-tabs (- (or arg 1))))

(defun vmacs-tab-line-switch-to-next-tab (&optional _event arg)
  "Switch to the next tab within the current tab group."
  (interactive (list last-nonmenu-event
                     (prefix-numeric-value current-prefix-arg)))
  (vmacs-tab-line-cycle-tabs (or arg 1)))

;; (原实现已移除：旧版通过绑定 vmacs-tab-line-switch-group-scope 靠
;; `switch-to-prev-buffer-skip' 过滤 `previous-buffer'，窗口历史耗尽时会
;; 跳到全局 buffer-list，可能跨组且顺序不随 tab line 显示顺序。)

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
