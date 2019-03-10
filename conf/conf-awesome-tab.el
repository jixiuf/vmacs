(setq-default awesome-tab-cycle-scope 'tabs)
(setq-default awesometab-hide-tabs-hooks   nil)
(setq-default awesome-tab-style "zigzag")
(global-set-key  (kbd "s-n") 'awesome-tab-forward)
(global-set-key  (kbd "s-C-M-n") 'awesome-tab-forward)
(global-set-key  (kbd "s-p") 'awesome-tab-backward)
(global-set-key  (kbd "s-C-M-p") 'awesome-tab-backward)
(define-key evil-normal-state-map (kbd "gh") 'awesome-tab-move-current-tab-to-left)
(define-key evil-normal-state-map (kbd "gl") 'awesome-tab-move-current-tab-to-right)
;; (vmacs-leader "e" 'awesome-tab-build-ivy-source)
(vmacs-leader "e" 'awesome-tab-forward-group)

;; 只为eshell-mode term-mode 启用awesome-tab

(setq awesome-tab-buffer-groups-function 'vmacs-awesome-tab-buffer-groups)

(defun vmacs-awesome-tab-buffer-groups ()
  "`awesome-tab-buffer-groups' control buffers' group rules.
    Group awesome-tab with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `projectile-project-p' with project name."
  (list
   (cond
    ((or (string-match-p "\\*scratch-.*" (buffer-name))
         (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode))
     "Term")
    ((string-match-p (rx (or
                          "\*Helm"
                          "\*helm"
                          "\*tramp"
                          "\*Completions\*"
                          "\*sdcv\*"
                          "\*Messages\*"
                          "\*Ido Completions\*"
                          ))
                     (buffer-name))
     "Emacs")
    ((not (vmacs-show-tabbar-p)) nil)
    (t "Common"))))

;; (defun vmacs-awesome-tab-buffer-list ()
;;   "Return the list of buffers to show in tabs.
;; only show eshell-mode term-mode and shell-mode."
;;   (awesome-tab-filter
;;    #'vmacs-show-tabbar-p
;;    (buffer-list)))
;; (setq awesome-tab-buffer-list-function 'vmacs-awesome-tab-buffer-list)

(defun vmacs-show-tabbar-p(&optional buf redisplay)
  (let ((show t))
    (with-current-buffer (or buf (current-buffer))
      (cond
       ((char-equal ?\  (aref (buffer-name) 0))
        (setq show nil))
       ((member (buffer-name) '("*Ediff Control Panel*"
                                "\*Flycheck error messages\*"
                                "\*Gofmt Errors\*"))
        (setq show nil))
       (t t))
      (unless show
        ;; (kill-local-variable 'header-line-format)
        (setq header-line-format nil)
        (when redisplay (redisplay t))
        )
      show)))

(defun vmacs-hide-tab-p(buf)
  (not (vmacs-show-tabbar-p buf t)))

(setq awesome-tab-hide-tab-function #'vmacs-hide-tab-p)

(defun vmacs-awesome-buffer-order ()
  "Put the two buffers switched to the adjacent position after current buffer changed."
  ;; Don't trigger by awesome-tab command, it's annoying.
  ;; This feature should trigger by search plugins, such as ibuffer, helm or ivy.
  (unless (or (string-prefix-p "awesome-tab" (format "%s" this-command))
              (string-equal "ignore" (format "%s" this-command))) ;鼠标点击的时候有时坐产生一个ignore事件，
    ;; Just continue when buffer changed.
    (unless (buffer-live-p awesome-tab-last-focus-buffer)
      (setq awesome-tab-last-focus-buffer (current-buffer)))
    (when (and (not (eq (current-buffer) awesome-tab-last-focus-buffer))
               (buffer-live-p awesome-tab-last-focus-buffer)
               (not (minibufferp)))
      (let* ((current (current-buffer))
             (previous awesome-tab-last-focus-buffer)
             (current-group (first (funcall awesome-tab-buffer-groups-function))))
        ;; Record last focus buffer.
        (setq awesome-tab-last-focus-buffer current)

        ;; Just continue if two buffers are in same group.
        (when (eq current-group awesome-tab-last-focus-buffer-group)
          (let* ((bufset (awesome-tab-get-tabset current-group))
                 (current-group-tabs (awesome-tab-tabs bufset))
                 (current-group-buffers (mapcar 'car current-group-tabs))
                 (current-buffer-index (cl-position current current-group-buffers))
                 (previous-buffer-index (cl-position previous current-group-buffers)))

            ;; If the two tabs are not adjacent, swap the positions of the two tabs.
            (when (and current-buffer-index
                       previous-buffer-index
                       (> (abs (- current-buffer-index previous-buffer-index)) 1))
              (let* ((copy-group-tabs (copy-list current-group-tabs))
                     (previous-tab (nth previous-buffer-index copy-group-tabs))
                     (current-tab (nth current-buffer-index copy-group-tabs))
                     (base-group-tabs (awesome-tab-remove-nth-element current-buffer-index copy-group-tabs))
                     (new-group-tabs (awesome-tab-insert-after base-group-tabs previous-tab current-tab)))
                (set bufset new-group-tabs)
                (awesome-tab-set-template bufset nil)
                (awesome-tab-display-update)
                ))))

        ;; Update the group name of the last access tab.
        (setq awesome-tab-last-focus-buffer-group current-group)
        ))))

(setq awesome-tab-adjust-buffer-order-function #'vmacs-awesome-buffer-order)



;; term 分组下 默认选中前一个tab
(defun vmacs-awesome-tab-buffer-track-killed ()
  "Hook run just before actually killing a buffer.
In Awesome-Tab mode, try to switch to a buffer in the current tab bar,
after the current buffer has been killed.  Try first the buffer in tab
after the current one, then the buffer in tab before.  On success, put
the sibling buffer in front of the buffer list, so it will be selected
first."
  (when (or (string-match-p "\\*scratch-.*" (buffer-name))
            (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode))
    (and (eq header-line-format awesome-tab-header-line-format)
         (eq awesome-tab-current-tabset-function 'awesome-tab-buffer-tabs)
         (eq (current-buffer) (window-buffer (selected-window)))
         (let ((bl (awesome-tab-tab-values (awesome-tab-current-tabset)))
               (b  (current-buffer))
               found sibling)
           (while (and bl (not found))
             (if (eq b (car bl))
                 (setq found t)
               (setq sibling (car bl)))
             (setq bl (cdr bl)))
           (when (and (setq sibling (or sibling (car bl) ))
                      (buffer-live-p sibling))
             ;; Move sibling buffer in front of the buffer list.
             (save-current-buffer
               (switch-to-buffer sibling)))))))


(defun vmacs-awesometab-hook()
  ;; 直接去除自动选下一个tab的hook,让它默认
  (remove-hook 'kill-buffer-hook 'awesome-tab-buffer-track-killed)
  (add-hook 'kill-buffer-hook 'vmacs-awesome-tab-buffer-track-killed)
  )

(add-hook 'awesome-tab-mode-hook #'vmacs-awesometab-hook)


(setq awesome-tab-label-fixed-length 20)
;; Copied from s.el
(defadvice awesome-tab-truncate-string (around vmacs-tab activate)
  "If S is longer than LEN, cut it down and add ELLIPSIS to the end.

The resulting string, including ellipsis, will be LEN characters
long.

When not specified, ELLIPSIS defaults to ‘...’."
  (declare (pure t) (side-effect-free t))
  (unless ellipsis (setq ellipsis ""))
  (setq ad-return-value
        (if (> (length s) len)
            (format "%s%s" (substring s 0 (- len (length ellipsis))) ellipsis)
          s)))

(awesome-tab-mode t)


(provide 'conf-awesome-tab)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-awesome-tab.el ends here.
