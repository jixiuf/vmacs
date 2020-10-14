;;ctrl-g ctrl-g 两次ctrl-关闭一些临时buffer

;; bury-boring-windows with `C-gC-g'

(evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>d") 'vmacs-next-buffer)
(evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>u") 'vmacs-prev-buffer)
(global-set-key  (kbd "s-C-M-u") 'vmacs-prev-buffer)
(evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>q") 'kill-other-buffers) ;

(evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>fr") 'vmacs-undo-kill-buffer)

(evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>k") 'vmacs-kill-buffer-dwim) ;
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "s-w") 'vmacs-kill-buffer-dwim)

;; (global-set-key  (kbd "s-C-M-u") 'vmacs-undo-kill-buffer)
(global-set-key  (kbd "s-C-M-h") 'vmacs-undo-kill-buffer)
(evil-define-key 'normal 'global  (kbd "<SPC>h") 'vmacs-undo-kill-buffer)

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
