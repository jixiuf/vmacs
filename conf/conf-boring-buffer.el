;;ctrl-g ctrl-g 两次ctrl-关闭一些临时buffer
(eval-when-compile (require 'cl-macs))
;; bury-boring-windows with `C-gC-g'

(evil-leader/set-key "d" 'vmacs-prev-buffer)
(evil-leader/set-key "k" 'vmacs-next-buffer) ;
(evil-leader/set-key "q" 'kill-other-buffers) ;

(global-set-key  (kbd "s-k") 'kill-buffer-or-server-edit) ; default on mac

(define-key evil-normal-state-map "q" 'vmacs-prev-buffer)
(with-eval-after-load 'dired (define-key dired-mode-map "q" 'kill-this-buffer))
(define-key evil-normal-state-map "o" 'vmacs-next-buffer)
(define-key evil-motion-state-map "o" 'vmacs-next-buffer)
(define-key evil-normal-state-map "Q" 'kill-buffer-and-window)



(defvar boring-window-modes
  '(help-mode compilation-mode log-view-mode log-edit-mode ibuffer-mode))

(defvar boring-window-bof-name-regexp
  (rx (or
       "\*Helm"
       "\*helm"
       "\*vc-diff\*"
       "\*magit-"
       "\*vc-"
       "\*vc*"
       "*Completions*"
       "\*vc-change-log\*"
       "\*VC-log\*"
       "\*Async Shell Command\*"
       "\*Shell Command Output\*"
       "\*sdcv\*"
       "\*Messages\*"
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
    (bury-boring-windows )))

(defun vmacs-prev-buffer()
  (interactive)
  "switch to prev buffer ,but skip boring buffer."
  (let ((buf-name (buffer-name))
        (found  nil))
    (cl-loop until found do
             (previous-buffer)
             (unless (or (memq  major-mode boring-window-modes)
                         (string-match boring-window-bof-name-regexp (buffer-name)))
               (setq found t))
             (when (string= (buffer-name) buf-name)
               (previous-buffer)
               (setq found t)))))

(defun vmacs-next-buffer()
  (interactive)
  "switch to next buffer ,but skip boring buffer."
  (let ((buf-name (buffer-name))
        (found  nil))
    (cl-loop until found do
             (next-buffer)
             (unless (or (memq  major-mode boring-window-modes)
                         (string-match boring-window-bof-name-regexp (buffer-name)))
               (setq found t))
             (when (string= (buffer-name) buf-name)
               (next-buffer)
               (setq found t)))))

(provide 'conf-boring-buffer)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-boring-buffer.el ends here.
