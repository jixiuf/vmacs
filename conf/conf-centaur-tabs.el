(setq-default centaur-tabs-hide-tabs-hooks   nil)
(setq-default centaur-tabs-cycle-scope 'tabs)
(setq-default centaur-tabs-display-sticky-function-name nil)
(setq-default centaur-tabs-style "zigzag")

(require 'centaur-tabs)
(global-set-key  (kbd "s-C-M-k") 'centaur-tabs-backward)
(global-set-key  (kbd "s-C-M-j") 'centaur-tabs-forward)
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
    ((and (string-match-p (rx (or
                          "\*Async-native-compile-log\*"
                          "\*Helm"
                          "magit-process"
                          "\*company-documentation\*"
                          "\*helm"
                          "\*eaf"
                          "\*eldoc"
                          "\*Launch "
                          "*dap-"
                          "*EGLOT "
                          "\*Flymake log\*"
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
                          "\*Http"
                          "\*Async Shell Command\*"
                          "\*Shell Command Output\*"
                          "\*Calculator\*"
                          "\*Calc "
                          "\*Flycheck error messages\*"
                          "\*Gofmt Errors\*"
                          "\*Ediff"
                          "\*sdcv\*"
                          "\*Org PDF LaTex Output\*"
                          "\*Org Export"
                          "\*osx-dictionary\*"
                          "\*Messages\*"
                          ))
                     (buffer-name))
          (not (string-match-p "\.el$" (buffer-name)) )
          )
     "Emacs")
    ;; ((not (vmacs-show-tabbar-p)) nil)
    (t "Common"))))
(setq centaur-tabs-adjust-buffer-order 'left)
(centaur-tabs-enable-buffer-reordering)
;; (setq centaur-tabs-label-fixed-length 30)
(centaur-tabs-mode t)

(add-hook 'after-init-hook (lambda() (remove-hook 'kill-buffer-hook 'centaur-tabs-buffer-track-killed)))

(provide 'conf-centaur-tabs)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-centaur-tabs.el ends here.
