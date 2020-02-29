
;; ;; "回到上一层目录.同时更新*Completions*"
;; ;;;###autoload
;; (defun minibuffer-up-parent-dir()
;;   (interactive)
;;   (goto-char (point-max))
;;   (let ((directoryp  (equal ?/ (char-before)))
;;         (bob         (minibuffer-prompt-end)))
;;     (while (and (> (point) bob) (not (equal ?/ (char-before))))  (delete-char -1))
;;     (when directoryp
;;       (delete-char -1)
;;       (while (and (> (point) bob) (not (equal ?/ (char-before))))  (delete-char -1)))))

(provide 'lazy-minibuffer)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-minibuffer.el ends here.
