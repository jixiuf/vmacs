;;; Code:


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
