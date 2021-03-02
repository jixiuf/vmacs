(setq-default centaur-tabs-hide-tabs-hooks   nil)
(setq-default centaur-tabs-cycle-scope 'tabs)
(setq-default centaur-tabs-display-sticky-function-name nil)
(setq-default centaur-tabs-style "zigzag")

(require 'centaur-tabs)
(global-set-key  (kbd "s-C-M-k") 'centaur-tabs-backward)
(global-set-key  (kbd "s-C-M-j") 'centaur-tabs-forward)
;; (define-key evil-normal-state-map (kbd "gh") 'centaur-tabs-move-current-tab-to-left)
;; (define-key evil-normal-state-map (kbd "gl") 'centaur-tabs-move-current-tab-to-right)
;; (vmacs-leader (kbd "e") 'centaur-tabs-build-ivy-source)
(vmacs-leader (kbd "e") 'centaur-tabs-forward-group)

;; 只为eshell-mode term-mode 启用centaur-tabs

(setq centaur-tabs-buffer-groups-function 'vmacs-centaur-tabs-buffer-groups)

(defun vmacs-centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules.
    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `projectile-project-p' with project name."
  (list
   (cond
    ((derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)
     "Term")
    ((string-match-p (rx (or
                          "\*Launch "
                          "*dap-"
                          ))
                     (buffer-name))
     "Debug")
    ((string-match-p (rx (or
                          "\*Async-native-compile-log\*"
                          "\*Helm"
                          "\*company-documentation\*"
                          "\*helm"
                          "\*eldoc"
                          "\*Launch "
                          "*dap-"
                          "*EGLOT "
                          "\*Flymake log\*"
                          "\*Help\*"
                          "\*Ibuffer\*"
                          "\*gopls::stderr\*"
                          "\*gopls\*"
                          "\*Compile-Log\*"
                          "*Backtrace*"
                          "*Package-Lint*"
                          "\*sdcv\*"
                          "\*tramp"
                          "\*lsp-log\*"
                          "\*tramp"
                          "\*ccls"
                          "\*vc"
                          "\*xref"
                          "\*Warnings*"
                          "magit-"
                          "\*Http"
                          "\*Verb"
                          "\*Org Agenda\*"
                          "\*Async Shell Command\*"
                          "\*Shell Command Output\*"
                          "\*Calculator\*"
                          "\*Calc "
                          "\*Flycheck error messages\*"
                          "\*Gofmt Errors\*"
                          "\*Ediff"
                          "\*sdcv\*"
                          "\*osx-dictionary\*"
                          "\*Messages\*"
                          ))
                     (buffer-name))
     "Emacs")
    ;; ((not (vmacs-show-tabbar-p)) nil)
    (t "Common"))))
(setq centaur-tabs-adjust-buffer-order 'left)
(centaur-tabs-enable-buffer-reordering)
(setq centaur-tabs-label-fixed-length 30)
(centaur-tabs-mode t)


;; (defun vmacs-centaur-tabs-buffer-list ()
;;   "Return the list of buffers to show in tabs.
;; only show eshell-mode term-mode and shell-mode."
;;   (centaur-tabs-filter
;;    #'vmacs-show-tabbar-p
;;    (buffer-list)))
;; (setq centaur-tabs-buffer-list-function 'vmacs-centaur-tabs-buffer-list)

;; (defun vmacs-show-tabbar-p(&optional buf redisplay)
;;   (let ((show t))
;;     (with-current-buffer (or buf (current-buffer))
;;       (cond
;;        ((char-equal ?\  (aref (buffer-name) 0))
;;         (setq show nil))
;;        ((member (buffer-name) '("*Ediff Control Panel*"
;;                                 "*Completions*"
;;                                 "*Ido Completions*"
;;                                 "\*Flycheck error messages\*"
;;                                 "\*Gofmt Errors\*"))
;;         (setq show nil))
;;        (t t))
;;       (unless show
;;         ;; (kill-local-variable 'header-line-format)
;;         (setq header-line-format nil)
;;         (when redisplay (redisplay t))
;;         )
;;       show)))

;; (defun vmacs-hide-tab-p(buf)
;;   (not (vmacs-show-tabbar-p buf t)))

;; (setq centaur-tabs-hide-tab-function #'vmacs-hide-tab-p)


(defun vmacs-awesometab-hook()
  ;; 直接去除自动选下一个tab的hook,让它默认
  ;; (add-hook 'kill-buffer-hook 'vmacs-centaur-tabs-buffer-track-killed)
  (remove-hook 'kill-buffer-hook 'centaur-tabs-buffer-track-killed))
(add-hook 'centaur-tabs-mode-hook #'vmacs-awesometab-hook)


;; ;; Copied from s.el
;; (defadvice centaur-tabs-truncate-string (around vmacs-tab activate)
;;   "If S is longer than LEN, cut it down and add ELLIPSIS to the end.

;; The resulting string, including ellipsis, will be LEN characters
;; long.

;; When not specified, ELLIPSIS defaults to ‘...’."
;;   (declare (pure t) (side-effect-free t))
;;   (unless ellipsis (setq ellipsis ""))
;;   (setq ad-return-value
;;         (if (> (length s) len)
;;             (format "%s%s" (substring s 0 (- len (length ellipsis))) ellipsis)
;;           s)))



(provide 'conf-centaur-tabs)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-centaur-tabs.el ends here.
