;;ctrl-g ctrl-g 两次ctrl-关闭一些临时buffer

;; bury-boring-windows with `C-gC-g'
(defvar boring-window-modes
  '(help-mode compilation-mode log-view-mode log-edit-mode ibuffer-mode)
  )

(defvar boring-window-bof-name-regexp
  (rx (or
       "\*Helm"
       "\*vc-diff\*"
       "*Completions*"
       "\*vc-change-log\*"
       "\*VC-log\*"
       "\*Async Shell Command\*"
       "\*Shell Command Output\*"
       "\*sdcv\*"
       "\*Messages\*"
       "\*joseph_compile_current_el\*"
       "\*Ido Completions\*")))


(defun bury-boring-windows(&optional bury-cur-win-if-boring)
  "close boring *Help* windows with `C-g'"
  (let ((opened-windows (window-list))
        (cur-buf-win (get-buffer-window)))
    (dolist (win opened-windows)
      (with-current-buffer (window-buffer win)
        (when (or (memq  major-mode boring-window-modes)
                  (string-match boring-window-bof-name-regexp (buffer-name)))
          (when (and (>  (length (window-list)) 1)
                     (or bury-cur-win-if-boring
                         (not (equal cur-buf-win win)))
                     (delete-window win))))))))


(defadvice keyboard-quit (before bury-boring-windows activate)
  (when (equal last-command 'keyboard-quit)
    (bury-boring-windows ))
  ;; (when (active-minibuffer-window)
  ;;   (helm-keyboard-quit))
  )
(provide 'conf-boring-buffer)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-boring-buffer.el ends here.
