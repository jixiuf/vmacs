;; 窗口相关操作，
;; 当分屏时，默认的emacs是C-x 2 ,C-x 3 两个窗口中显示的内容都是同一个buffer
;; 此处则在新开的窗口中显示不同的buffer
(vmacs-leader (kbd "2") 'vmacs-split-window-vertically) ;横着分屏
(vmacs-leader (kbd "3") 'vmacs-split-window-horizontally) ;竖着分屏
(global-set-key (kbd "C-x 2")  'vmacs-split-window-vertically)
(global-set-key (kbd "C-x 3")  'vmacs-split-window-horizontally)
(vmacs-leader (kbd "4") 'toggle-split-window)
(global-set-key  (kbd "C-M-s-\\") #'toggle-split-window)
(vmacs-leader (kbd "4") 'toggle-split-window)
(vmacs-leader (kbd "1") 'delete-other-windows) ;只保留当前窗口
(vmacs-leader (kbd "0") 'delete-window)        ;删除当前窗口

;; 尽量保证光标所在的窗口最大
;; 黄金分隔 多窗口操作时
(golden-ratio-mode 1)
;; Work with ediff and helm
(setq golden-ratio-adjust-factor 0.91)
(setq golden-ratio-extra-commands
      (append golden-ratio-extra-commands
              '(
                dap-hydra/dap-ui-locals
                dap-ui-locals
                evil-window-left
                evil-window-right
                evil-window-up
                evil-window-down
                avy-goto-char-timer
                avy-goto-word-1
                avy-goto-char-timer
                evil-window-rotate-downwards
                vmacs-split-window-or-other-window
                vmacs-split-window-or-prev-window
                magit-show-commit
                magit-stash-show
                avy-goto-word-1
                ace-jump-mode-pop-mark)))

(defun vmacs-evil-window-cmd-p ()
  (when (symbolp last-command)
    (string-prefix-p "evil-window-" (symbol-name last-command))))

(add-to-list 'golden-ratio-inhibit-functions 'vmacs-evil-window-cmd-p)

;; (setq pop-up-windows t)                    ; display-buffer: avoid splitting
;; (setq even-window-sizes nil)               ; display-buffer: avoid resizing

;; ;; fix window splitting behavior when possible
;; https://emacs-china.org/t/display-buffer-alist/8162/4
(setq display-buffer-alist
      '(
        ( "^*v?term*.*"
         ;; (display-buffer-reuse-window )
         ;; (inhibit-same-window . t)
         (display-buffer-reuse-window display-buffer-at-bottom) ;display-buffer-in-direction
         ;; (dedicated . t) ;dedicated is supported in emacs27
         ;;display-buffer-in-direction/direction/dedicated is added in emacs27
         (direction . bottom)
         (side . bottom)
         (window-height . 0.3)
         ;; (display-buffer-reuse-window display-buffer-at-bottom)
         ;; (inhibit-same-window . t)
         ;; (reusable-frames . nil)
         ;; (side . bottom)
         ;; (window-height . 1)
         )
        ((lambda (bufname _)
           (memq this-command '( next-error previous-error compile-goto-error)))
         (display-buffer-same-window )
         (inhibit-same-window . nil))
        ("\\*rg\\*"
         (display-buffer-same-window ))
        ("\\*Embark Export Grep\\*"
         (display-buffer-same-window ))
        ("\\*xref\\*"
         (display-buffer-reuse-window display-buffer-at-bottom) ;display-buffer-in-direction
         )
        ("\\*Annotate .*"
         (display-buffer-same-window ))
        ;; default
        ;; (".*" (display-buffer-pop-up-window))
        )
      )

;; 左右分屏
(defun vmacs-display-buffer-pop-up-horizontally (buffer alist)
  "A `display-buffer' ACTION forcing a vertical window split.
    See `split-window-sensibly' and `display-buffer-pop-up-window'."
  (let ((split-width-threshold 0)
        (split-height-threshold nil))
    (display-buffer-pop-up-window buffer alist)))

;; 上下分屏
(defun vmacs-display-buffer-pop-up-vertically (buffer alist)
  "A `display-buffer' ACTION forcing a vertical window split.
    See `split-window-sensibly' and `display-buffer-pop-up-window'."
  (let ((split-width-threshold 0)
        (split-height-threshold nil))
    (display-buffer-pop-up-window buffer alist)))

;; (defun vmacs-helm-alive-p ()
;;   (if (boundp 'helm-alive-p)
;;       (symbol-value 'helm-alive-p)))

;; (add-to-list 'golden-ratio-inhibit-functions 'vmacs-helm-alive-p)

;; q kill buffer,C-uq bury

(defun vmacs-quit-and-kill-window ()
  "Kill buffer and its window on quitting"
  (local-set-key (kbd "q") 'vmacs-kill-buffer-dwim))
(add-hook 'special-mode-hook 'vmacs-quit-and-kill-window)

(provide 'conf-evil-window)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-evil-window.el ends here.
