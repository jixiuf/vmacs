
;;;###autoload
(defun vmacs-ivy-magic-eol( arg)
  "C-e move to end of line or execute `ivy-immediate-done'"
  (interactive "P")
  (if (eolp)
      (call-interactively 'ivy-immediate-done)
    (call-interactively 'end-of-line)))

;;;###autoload
(defun vmacs-ivy-swithc-buffer-open-dired(&optional buffer)
  (interactive)
  (if (zerop (length buffer))
      (dired ivy-text)
    (let ((virtual (assoc buffer ivy--virtual-buffers)))
      (if (and virtual
               (not (get-buffer buffer)))
          (dired (file-name-directory (cdr virtual)))
        (with-current-buffer buffer (dired default-directory))))))

;;;###autoload
(defun vmacs-ivy-dired(&optional buf)
  (interactive)
  (if ivy--directory
      (ivy-quit-and-run
       (dired ivy--directory))
    (user-error
     "Not completing files currently")))

(provide 'lazy-ivy)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-ivy.el ends here.
