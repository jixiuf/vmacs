;;; conf-dap-mode.el --- Description -*- lexical-binding: t; -*-
;;; Code:
;; https://emacs-lsp.github.io/dap-mode/page/configuration/
;; https://medium.com/@apmattil/debug-go-golang-with-emacs-fbf840c0aa56
;; https://github.com/go-delve/delve/blob/master/Documentation/installation/osx/install.md
;; (setq dap-auto-configure-features '(sessions locals controls tooltip))
;; go get github.com/go-delve/delve/cmd/dlv
;; brew install nodejs
;; sudo /usr/sbin/DevToolsSecurity -enable
(require 'dap-hydra)
(require 'dap-ui)
(require 'dap-mouse)
(setq dap-output-buffer-filter  '("stdout" ))
(defadvice dap-ui--show-buffer (around display (buff) activate)
  "Show BUF according to defined rules."
  (let ((win (display-buffer-in-side-window
              buff
              `((side . bottom) (slot . 1) (window-width . 0.20)))))
    ;; (set-window-dedicated-p win t)
    (select-window win)
    (fit-window-to-buffer nil dap-output-window-max-height dap-output-window-min-height)))

(defadvice dap-go-to-output-buffer (around display () activate)
  "Go to output buffer."
  (interactive)
  (let ((win (display-buffer-in-side-window
              (dap--debug-session-output-buffer (dap--cur-session-or-die))
              `((side . bottom) (slot . 1) (window-width . 0.20)))))
    ;; (set-window-dedicated-p win t)
    (select-window win)
    (fit-window-to-buffer nil dap-output-window-max-height dap-output-window-min-height)))

(dap-auto-configure-mode 1)
;; (dap-mode 1)
;; ;; ;; ;; The modes above are optional
;; (dap-ui-mode 1)
;; ;; ;; ;; enables mouse hover support
;; (dap-tooltip-mode 1)
;; ;; ;; use tooltips for mouse hover
;; ;; ;; if it is not enabled `dap-mode' will use the minibuffer.
;; (tooltip-mode 1)
;; ;; ;; displays floating panel with debug buttons
;; ;; ;; requies emacs 26+
;; (dap-ui-controls-mode 1)
(setq dap-output-window-max-height 10)
(add-hook 'dap-session-created-hook  #'vmacs-dap-session-created-hook)
(add-hook 'dap-terminated-hook  #'vmacs-dap-terminated-hook)

(defun vmacs-dap-quit()
  (interactive)
  (if (= 0 (hydra-get-property 'dap-hydra :verbosity))
      (call-interactively 'vmacs-dap-hydra)
    (lv-delete-window)))

;; append f8
(defhydra+ dap-hydra (:color pink :hint nil :foreign-keys run)
  ("<f8>" vmacs-dap-quit "hide" :color blue))

(global-set-key (kbd "<f8>") 'vmacs-dap-hydra)
(defun vmacs-dap-hydra(&optional _arg)
  (interactive)
  (message "<f8> show dap-hydra help window")
  (if (called-interactively-p 'any)
      (hydra-set-property 'dap-hydra :verbosity 1) ;显示提示窗口
    (hydra-set-property 'dap-hydra :verbosity 0) ;隐藏提示窗口
    )
  (call-interactively 'dap-hydra))

(add-hook 'dap-stopped-hook #'vmacs-dap-hydra)
(defvar dap-window-config nil)
(defun vmacs-dap-session-created-hook(&optional sess)
  (setq dap-window-config (current-window-configuration))
  (golden-ratio-mode -1)
  (horizontal-scroll-bar-mode 1))

(defun vmacs-dap-terminated-hook(&optional sess)
  (golden-ratio-mode 1)
  (horizontal-scroll-bar-mode -1)
  (when dap-window-config
    (set-window-configuration dap-window-config)
    (setq dap-window-config nil))
  (kill-buffer (dap--debug-session-output-buffer (dap--cur-session-or-die))))

(provide 'conf-dap-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-dap-mode.el ends here.
