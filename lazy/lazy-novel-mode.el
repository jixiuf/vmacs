;; 小说 段首缩进4格
(defun novel-fill(&optional args)
  (interactive)
  (if (not mark-active)
      (novel-fill-intern)
    (save-excursion
      (let ((begin (region-beginning))
            (end (region-end)))
        (deactivate-mark)
        (goto-char begin)
        (while  (< (point) end)
          (novel-fill-intern)
          (forward-paragraph)
          (skip-chars-forward "[ |\t|\n|\r]*"))))))

(defun novel-fill-intern()
  (let* ((p (point))
         (begin (save-excursion
                  (forward-paragraph)
                  (backward-paragraph)
                  (point)))
         (is-blank-string (buffer-substring-no-properties begin p)))
    (save-excursion
      (goto-char begin)
      (skip-chars-forward "[ |\t|\n|\r]*")
      (delete-horizontal-space)
      (insert "    ")
      (fill-paragraph))
    (when (string-blank-p  is-blank-string)
      (skip-chars-forward "[ |\t|\n|\r]*"))))



(provide 'lazy-novel-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-norvel-mode.el ends here.
