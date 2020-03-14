;;ctrl-g ctrl-g 两次ctrl-关闭一些临时buffer
(eval-when-compile (require 'cl-macs) (require 'cl-seq))

;; bury-boring-windows with `C-gC-g'

(vmacs-leader "d" 'vmacs-next-buffer)
(vmacs-leader "u" 'vmacs-prev-buffer)
(global-set-key  (kbd "s-C-M-u") 'vmacs-prev-buffer)
(vmacs-leader "q" 'kill-other-buffers) ;

(vmacs-leader "fr" 'vmacs-undo-kill-buffer)

(vmacs-leader "k" 'vmacs-kill-buffer-dwim) ;
(global-set-key  (kbd "s-C-M-k") 'vmacs-kill-buffer-dwim) ; hyper-k default on mac
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "s-w") 'vmacs-kill-buffer-dwim)

;; (global-set-key  (kbd "s-C-M-u") 'vmacs-undo-kill-buffer)
(global-set-key  (kbd "s-C-M-h") 'vmacs-undo-kill-buffer)
;; (vmacs-leader (kbd "u") 'vmacs-undo-kill-buffer) ;
(vmacs-leader (kbd "h") 'vmacs-undo-kill-buffer) ;

;; (define-key evil-normal-state-map "q" 'vmacs-prev-buffer)
;; (with-eval-after-load 'dired (define-key dired-mode-map "q" 'vmacs-prev-buffer))
(define-key evil-normal-state-map "Q" 'kill-buffer-and-window)

(defadvice keyboard-quit (before bury-boring-windows activate)
  (when (equal last-command 'keyboard-quit)
    (require 'lazy-buffer)
    (bury-boring-windows))
  ;; (let ((win (active-minibuffer-window)))
  ;;   (when (windowp win)
  ;;     (switch-to-buffer (window-buffer win))))

  )


(provide 'conf-buffer)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-boring-buffer.el ends here.
