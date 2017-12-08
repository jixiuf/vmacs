;;ctrl-g ctrl-g 两次ctrl-关闭一些临时buffer
(eval-when-compile (require 'cl-macs))
;; bury-boring-windows with `C-gC-g'

(evil-leader/set-key "d" 'vmacs-prev-buffer)
(evil-leader/set-key "k" 'vmacs-next-buffer) ;
(evil-leader/set-key "q" 'kill-other-buffers) ;

(evil-leader/set-key "fr" 'vmacs-undo-kill-buffer)

(global-set-key  (kbd "s-k") 'kill-buffer-or-server-edit) ; default on mac
(global-set-key  (kbd "s-C-M-S-k") 'kill-buffer-or-server-edit) ; hyper-k default on mac

(define-key evil-normal-state-map "q" 'kill-buffer-or-server-edit)
(with-eval-after-load 'dired (define-key dired-mode-map "q" 'kill-this-buffer))
(define-key evil-normal-state-map "Q" 'kill-buffer-and-window)



(defvar boring-window-modes
  '(help-mode compilation-mode log-view-mode log-edit-mode org-agenda-mode ibuffer-mode))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar vmacs-killed-file-list nil
  "List of recently killed files.")

(defun vmacs-add-to-killed-file-list()
  "If buffer is associated with a file name, add that file to the
`vmacs-killed-file-list' when killing the buffer."
  (when buffer-file-name
    (unless (or (string-match-p "COMMIT_EDITMSG" buffer-file-name)
                (string-match-p "/cache/recentf" buffer-file-name))
      (push buffer-file-name vmacs-killed-file-list))))

(add-hook 'kill-buffer-hook #'vmacs-add-to-killed-file-list)

(defun vmacs-undo-kill-buffer()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (when vmacs-killed-file-list
    (message "reopen file: %s" (find-file (pop vmacs-killed-file-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'conf-buffer)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-boring-buffer.el ends here.
