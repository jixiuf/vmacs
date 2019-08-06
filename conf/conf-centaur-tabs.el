(setq-default centaur-tabs-hide-tabs-hooks   nil)
(setq-default centaur-tabs-cycle-scope 'tabs)
(setq-default centaur-tabs-display-sticky-function-name nil)
(setq-default centaur-tabs-style "zigzag")

(require 'centaur-tabs)
(global-set-key  (kbd "s-n") 'centaur-tabs-forward)
(global-set-key  (kbd "s-C-M-n") 'centaur-tabs-forward)
(global-set-key  (kbd "s-p") 'centaur-tabs-backward)
(global-set-key  (kbd "s-C-M-p") 'centaur-tabs-backward)
(define-key evil-normal-state-map (kbd "gh") 'centaur-tabs-move-current-tab-to-left)
(define-key evil-normal-state-map (kbd "gl") 'centaur-tabs-move-current-tab-to-right)
;; (vmacs-leader "e" 'centaur-tabs-build-ivy-source)
(vmacs-leader "e" 'centaur-tabs-forward-group)

;; 只为eshell-mode term-mode 启用centaur-tabs

(setq centaur-tabs-buffer-groups-function 'vmacs-centaur-tabs-buffer-groups)

(defun vmacs-centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules.
    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
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
                          "\*Flymake log\*"
                          "\*Help\*"
                          "\*Ibuffer\*"
                          "\*gopls::stderr\*"
                          "\*gopls\*"
                          "\*sdcv\*"
                          "\*tramp"
                          "\*lsp-log\*"
                          "\*tramp"
                          "\*ccls"
                          "\*vc"
                          "\*xref"
                          "\*Completions\*"
                          "\*Warnings*"
                          "magit-"
                          "\*Async Shell Command\*"
                          "\*Shell Command Output\*"
                          "\*Calculator\*"
                          "\*Calc "
                          "\*Flycheck error messages\*"
                          "\*Gofmt Errors\*"
                          "\*sdcv\*"
                          "\*Messages\*"
                          "\*Ido Completions\*"
                          ))
                     (buffer-name))
     "Emacs")
    ((not (vmacs-show-tabbar-p)) nil)
    (t "Common"))))

;; (defun vmacs-centaur-tabs-buffer-list ()
;;   "Return the list of buffers to show in tabs.
;; only show eshell-mode term-mode and shell-mode."
;;   (centaur-tabs-filter
;;    #'vmacs-show-tabbar-p
;;    (buffer-list)))
;; (setq centaur-tabs-buffer-list-function 'vmacs-centaur-tabs-buffer-list)

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

(setq centaur-tabs-hide-tab-function #'vmacs-hide-tab-p)

(setq centaur-tabs-adjust-buffer-order 'left)
(centaur-tabs-enable-buffer-reordering)



;; term 分组下 默认选中前一个tab
(defun vmacs-centaur-tabs-buffer-track-killed ()
  "Hook run just before actually killing a buffer.
In Awesome-Tab mode, try to switch to a buffer in the current tab bar,
after the current buffer has been killed.  Try first the buffer in tab
after the current one, then the buffer in tab before.  On success, put
the sibling buffer in front of the buffer list, so it will be selected
first."
  (when (or (string-match-p "\\*scratch-.*" (buffer-name))
            (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode))
    (and (eq header-line-format centaur-tabs-header-line-format)
         (eq centaur-tabs-current-tabset-function 'centaur-tabs-buffer-tabs)
         (eq (current-buffer) (window-buffer (selected-window)))
         (let ((bl (centaur-tabs-tab-values (centaur-tabs-current-tabset)))
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
  (remove-hook 'kill-buffer-hook 'centaur-tabs-buffer-track-killed)
  (add-hook 'kill-buffer-hook 'vmacs-centaur-tabs-buffer-track-killed)
  )

(add-hook 'centaur-tabs-mode-hook #'vmacs-awesometab-hook)


(setq centaur-tabs-label-fixed-length 30)
;; Copied from s.el
(defadvice centaur-tabs-truncate-string (around vmacs-tab activate)
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

(centaur-tabs-mode t)


(provide 'conf-centaur-tabs)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-centaur-tabs.el ends here.
