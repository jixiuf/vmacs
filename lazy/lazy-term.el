;; (require 'shell-toggle)
;; (require 'sane-term)

;; ;; 外部emacsclient 调用 显示term-buffer
;; ;;;###autoload
;; (defun vmacs-frame-pop-shell-buffer()
;;   (dolist (frame (visible-frame-list))
;;     (with-selected-frame frame
;;       (when (frame-parameter frame 'window-id)
;;         (vmacs-shell-toggle 0)
;;         ))))



;; ;;;###autoload
;; (defun vmacs-shell-toggle(&optional create-new-term)
;;   (interactive "p")
;;   (if (> create-new-term 1) (vmacs-shell-toggle-new)
;;     (shell-toggle nil)))

;; ;;;###autoload
;; (defun vmacs-shell-toggle-cd(&optional create-new-term)
;;   (interactive "p")
;;   (when (> create-new-term 1) (setq shell-toggle-shell-buffer nil))
;;   (call-interactively 'shell-toggle-cd))

;; ;;;###autoload
;; (defun vmacs-shell-toggle-new()
;;   (interactive )
;;   (shell-toggle-ansi-term)
;;   (shell-toggle-this-is-the-shell-buffer))


;; ;; sane-term.el 中当term exit 时会自动切到另一个term中，如果有的话
;; ;; 而此defadvice 则将当前term 设置为shell-toggle的对象
;; ;; 需要把此defadvice放在sane-term的 defadvice之后，所以使用last 关键字
;; (defadvice term-handle-exit(after shell-toggle-set-buffer last activate)
;;   (when (derived-mode-p 'term-mode)
;;     (shell-toggle-this-is-the-shell-buffer)))


;; (defadvice sane-term-cycle(around shell-toggle-set-buffer activate)
;;   ad-do-it
;;   (when (derived-mode-p 'term-mode)
;;     (shell-toggle-this-is-the-shell-buffer)))

(provide 'lazy-term)



;; Local Variables:
;; coding: utf-8
;; End:

;;; shell-toggle.el ends here.
