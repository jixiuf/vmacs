;;;###autoload
(defun vmacs-shell-toggle-cd(&optional create-new-term)
  (interactive "P")
  (when create-new-term (setq shell-toggle-shell-buffer nil))
  (call-interactively 'shell-toggle-cd))

(provide 'shell-toggle)



;; Local Variables:
;; coding: utf-8
;; End:

;;; shell-toggle.el ends here.
