;; 窗口相关操作，
;; 当分屏时，默认的emacs是C-x 2 ,C-x 3 两个窗口中显示的内容都是同一个buffer
;; 此处则在新开的窗口中显示不同的buffer
(evil-leader/set-key "2" 'split-window-func-with-other-buffer-vertically) ;横着分屏
(evil-leader/set-key "3" 'split-window-func-with-other-buffer-horizontally) ;竖着分屏
(global-set-key (kbd "C-x 2")  'split-window-func-with-other-buffer-vertically)
(global-set-key (kbd "C-x 3")  'split-window-func-with-other-buffer-horizontally)
(evil-leader/set-key "4" 'toggle-split-window-horizontally-vertically)
(evil-leader/set-key "1" 'delete-other-windows) ;只保留当前窗口
(evil-leader/set-key "0" 'delete-window)        ;删除当前窗口

;; 尽量保证光标所在的窗口最大
;; 黄金分隔 多窗口操作时
(golden-ratio-mode 1)
;; Work with ediff and helm
(add-to-list 'golden-ratio-exclude-modes "ediff-mode")
;; (add-to-list 'golden-ratio-exclude-modes "magit-mode")
(add-to-list 'golden-ratio-exclude-modes "magit-key-mode")
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
                vmacs-split-window-or-other-window
                vmacs-split-window-or-prev-window
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
