;; 窗口相关操作，
;; 当分屏时，默认的emacs是C-x 2 ,C-x 3 两个窗口中显示的内容都是同一个buffer
;; 此处则在新开的窗口中显示不同的buffer
(vmacs-leader "2" 'vmacs-split-window-vertically) ;横着分屏
(vmacs-leader "3" 'vmacs-split-window-horizontally) ;竖着分屏
(global-set-key (kbd "C-x 2")  'vmacs-split-window-vertically)
(global-set-key (kbd "C-x 3")  'vmacs-split-window-horizontally)
(vmacs-leader "4" 'toggle-split-window)
(vmacs-leader "1" 'delete-other-windows) ;只保留当前窗口
(vmacs-leader "0" 'delete-window)        ;删除当前窗口

;; 尽量保证光标所在的窗口最大
;; 黄金分隔 多窗口操作时
(golden-ratio-mode 1)
;; Work with ediff and helm
(add-to-list 'golden-ratio-exclude-modes "ediff-mode")
;; (add-to-list 'golden-ratio-exclude-modes "magit-mode")
(add-to-list 'golden-ratio-exclude-modes "magit-key-mode")
(setq golden-ratio-adjust-factor 0.91)
(setq golden-ratio-extra-commands
      (append golden-ratio-extra-commands
              '(
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

(add-to-list 'golden-ratio-inhibit-functions 'golden-ratio-ediff-comparison-buffer-p)

(defun golden-ratio-ediff-comparison-buffer-p ()
  "用于判断当前buffer是不是ediff session中， 以便决定用不用golden-ratio"
  (when (boundp 'ediff-this-buffer-ediff-sessions)
    ediff-this-buffer-ediff-sessions))


(defun vmacs-evil-window-cmd-p ()
  (string-prefix-p "evil-window-" (symbol-name last-command)))

(add-to-list 'golden-ratio-inhibit-functions 'vmacs-evil-window-cmd-p)


;; (defun vmacs-helm-alive-p ()
;;   (if (boundp 'helm-alive-p)
;;       (symbol-value 'helm-alive-p)))

;; (add-to-list 'golden-ratio-inhibit-functions 'vmacs-helm-alive-p)


(provide 'conf-evil-window)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-evil-window.el ends here.
