;;; Code:

;; 绑定之后，你访问过的dired都会被记录住，当你copy rename 及打开dired时，可以从这些
;; 已访问的目录中筛选以方便快速访问
(require 'vmacs-dired-history)

  (require 'dired-async nil t)
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map
    "r" 'revert-buffer                ; "l"
    "gr" 'revert-buffer
    "gg" 'dired-beginning-of-buffer
    "G" 'dired-end-of-buffer
    ";" nil))                             ;取消对;的绑定，；进行clipboard的操作


(provide 'conf-dired)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-dired.el ends here.
