;;; conf-term.el  -*- lexical-binding: t; -*-

;;; Code:

(with-eval-after-load 'ghostel
  (define-key  ghostel-mode-map (kbd "C-s-v") #'ghostel-yank)
  (helix-define-key 'insert  (kbd "C-s-v") #'ghostel-yank 'ghostel-mode)
  (helix-define-key 'normal  (kbd "C-s-v") #'ghostel-yank 'ghostel-mode)
  (helix-define-key 'normal "k" #'ghostel-copy-mode-previous-line 'ghostel-mode)
  (helix-define-key 'normal  "j" #'ghostel-copy-mode-next-line 'ghostel-mode)
  )
(setq-default term-prompt-regexp "^[^#$%>\n]*[#$%>] *") ;默认 regex 相当于没定义，term-bol 无法正常中转到开头处
(setq ghostel-enable-osc52 t)
(setq ghostel-copy-mode-auto-load-scrollback t)

(defun ghostel-latest ()
  "Switch to the next ghostel terminal buffer, or create one."
  (interactive)
  (let* ((bufs (cl-remove-if-not
                (lambda (b)
                  (with-current-buffer b
                    (derived-mode-p 'ghostel-mode)))
                (buffer-list)))
         )
    (if bufs
        (pop-to-buffer (car bufs) (append display-buffer--same-window-action
                                          '((category . comint))))
      (ghostel))))


(defun vmacs-ghostel-disable-copy()
  (when (member major-mode '(ghostel-mode))
    (ghostel-copy-mode-exit)
    (display-line-numbers-mode)
    (setq display-line-numbers 'absolute)
    (ghostel--set-cursor-style 0 t)))

(defvar-local vmacs-ghostel-starttime nil)
(defun vmacs-ghostel-hook()
  (setq vmacs-ghostel-starttime (current-time)))
(add-hook 'ghostel-mode-hook #'vmacs-ghostel-hook)

(defun vmacs-ghostel-enable-copy()
  (when (member major-mode '(ghostel-mode))
    (display-line-numbers-mode)
    (setq display-line-numbers nil)
    (when (member this-command '(keyboard-quit
                                 bray-state-stack-pop
                                 helix-insert-exit
                                 ghostel-latest
                                 ghostel
                                 ghostel-other))
      (when (> (float-time (time-since vmacs-ghostel-starttime)) 2)
        (ghostel-copy-mode))
      (ghostel--set-cursor-style 1 t))))

;; (add-hook 'meep-state-hook-insert-enter 'vmacs-ghostel-disable-copy)
;; (add-hook 'meep-state-hook-normal-enter 'vmacs-ghostel-enable-copy)

;; (defun vmacs-ghostel-self-insert()
;;   (interactive)
;;   (unless (bray-state-derived-p 'insert)
;;     (bray-state-stack-push meep-state-insert))
;;   (call-interactively 'ghostel--self-insert))

(provide 'conf-term)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-term.el ends here.
