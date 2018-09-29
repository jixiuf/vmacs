;;ctrl-g ctrl-g 两次ctrl-关闭一些临时buffer
(eval-when-compile (require 'cl-macs))
;; bury-boring-windows with `C-gC-g'

(vmacs-leader "d" 'vmacs-prev-buffer)
(vmacs-leader "q" 'kill-other-buffers) ;

(vmacs-leader "fr" 'vmacs-undo-kill-buffer)

(vmacs-leader "k" 'vmacs-kill-buffer-dwim) ;
(global-set-key  (kbd "s-C-M-k") 'vmacs-kill-buffer-dwim) ; hyper-k default on mac
(global-set-key  (kbd "s-C-M-u") 'vmacs-undo-kill-buffer)
(global-set-key  (kbd "s-C-M-h") 'vmacs-undo-kill-buffer)
(vmacs-leader (kbd "u") 'vmacs-undo-kill-buffer) ;
(vmacs-leader (kbd "h") 'vmacs-undo-kill-buffer) ;

;; (define-key evil-normal-state-map "q" 'vmacs-prev-buffer)
;; (with-eval-after-load 'dired (define-key dired-mode-map "q" 'vmacs-prev-buffer))
(define-key evil-normal-state-map "Q" 'kill-buffer-and-window)



(defvar boring-window-modes
  '(help-mode compilation-mode log-view-mode log-edit-mode
              org-agenda-mode magit-revision-mode ibuffer-mode))

(defvar boring-window-bof-name-regexp
  (rx (or
       "\*Helm"
       "\*helm"
       "\*vc-diff\*"
       "\*magit-"
       "\*vc-"
       "todo.txt"
       "\*vc*"
       "*Completions*"
       "\*vc-change-log\*"
       "\*VC-log\*"
       "\*Async Shell Command\*"
       "\*Shell Command Output\*"
       "\*sdcv\*"
       "\*Messages\*"
       "magit-process"
       "magit-diff"
       "magit-stash"
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

(provide 'conf-buffer)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-boring-buffer.el ends here.
