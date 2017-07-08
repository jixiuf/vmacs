;;;###autoload
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

;;;###autoload
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


(provide 'lazy-buffer)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-buffer.el ends here.
