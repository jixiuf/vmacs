(global-tab-line-mode t)
(global-set-key  (kbd "s-C-M-k") 'tab-line-switch-to-prev-tab)
(global-set-key  (kbd "s-C-M-j") 'tab-line-switch-to-next-tab)
(setq tab-line-new-button-show nil)  ;; do not show add-new button
(setq tab-line-close-button-show nil)  ;; do not show close button
(setq tab-line-separator (propertize " ▶" 'face  '(foreground-color . "cyan")))
(defun vmacs-tabline-same-mode-buffer() (setq tab-line-tabs-function #'tab-line-tabs-mode-buffers) (force-mode-line-update))
(defun vmacs-tabline-window-buffer() (setq tab-line-tabs-function #'tab-line-tabs-window-buffers)(force-mode-line-update))
;; only enable group by same mode buffers for vterm
(add-hook 'vterm-toggle-show-hook #'vmacs-tabline-same-mode-buffer)
(add-hook 'vterm-toggle-hide-hook #'vmacs-tabline-window-buffer)


(defadvice tab-line-tabs-window-buffers (around skip-buffer activate)
  "Return a list of tabs that should be displayed in the tab line
but skip uninterested buffers."
  (let ((buffers ad-do-it))
    (if (vmacs-tab-filter (current-buffer))
        (setq ad-return-value (seq-filter #'vmacs-tab-filter buffers))
      (setq ad-return-value (seq-remove #'vmacs-tab-filter buffers)))))

(defun vmacs-tab-filter(&optional buf)
  (string-match-p (rx (or
                       "\*Async-native-compile-log\*"
                       "magit-process"
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

;; switch-to-prev-buffer 与 switch-to-next-buffer 时 skip 特定的buffers
;;而 tab-line-switch-to-prev/next-tab 恰好使用了上面两个函数
(defun vmacs-switch-to-prev-buffer-skip(win buf bury-or-kill)
  (when (member this-command '(tab-line-switch-to-prev-tab tab-line-switch-to-next-tab))
    (if (vmacs-tab-filter (current-buffer))
        (not (vmacs-tab-filter buf))
      (vmacs-tab-filter buf))))
(setq switch-to-prev-buffer-skip #'vmacs-switch-to-prev-buffer-skip)

;; (setq tab-line-tabs-buffer-group-function #'vmacs-tab-line-buffer-group)
;; (setq tab-line-tabs-function #'tab-line-tabs-buffer-groups)
;; (setq tab-line-tabs-buffer-list-function #'vmacs-tab-line-tabs-buffer-list)
;; (defun vmacs-tab-line-tabs-buffer-list ()
;;   (seq-remove #'vmacs-tab-filter (tab-line-tabs-buffer-list)))



;; (setq-default centaur-tabs-hide-tabs-hooks   nil)
;; (setq-default centaur-tabs-cycle-scope 'tabs)
;; (setq-default centaur-tabs-display-sticky-function-name nil)
;; (setq-default centaur-tabs-style "zigzag")

;; (require 'centaur-tabs)
;; (vmacs-leader (kbd "e") 'centaur-tabs-forward-group)

;; ;; 只为eshell-mode term-mode 启用centaur-tabs

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
