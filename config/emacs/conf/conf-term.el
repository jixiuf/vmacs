;;; conf-term.el  -*- lexical-binding: t; -*-

;;; Code:

(define-key ghostel-mode-map (kbd "C-,") #'ghostel-other)
(setq-default term-prompt-regexp "^[^#$%>\n]*[#$%>] *") ;默认 regex 相当于没定义，term-bol 无法正常中转到开头处
(setq ghostel-enable-osc52 t)

(defun vmacs-ghostel-self-insert()
  (interactive)
  ;; (unless (evil-insert-state-p)
  ;;   (evil-insert-state))
  (unless (bray-state-derived-p 'insert)
    (bray-state-stack-push meep-state-insert))
  (call-interactively 'ghostel--self-insert))


(defun vmacs-ghostel-disable-copy()
  (when (member major-mode '(ghostel-mode))
    (ghostel-copy-mode-exit)
    (ghostel--set-cursor-style 0 t)))

(defun vmacs-ghostel-enable-copy()
  (when (member major-mode '(ghostel-mode))
    (ghostel-copy-mode)
    (ghostel--set-cursor-style 1 t)
    ))

(add-hook 'meep-state-hook-insert-enter 'vmacs-ghostel-disable-copy)
(add-hook 'meep-state-hook-normal-enter 'vmacs-ghostel-enable-copy)
(provide 'conf-term)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-term.el ends here.
