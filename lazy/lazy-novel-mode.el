;; 小说 段首缩进4格
;;;###autoload
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

;; 标点使用中文标点
;;;###autoload
(defun chinese-normal()
  (interactive)
  (let ((begin)
        (end))
    (if (not mark-active)
        (progn
          (setq begin (point-min))
          (setq end (point-max)))
      (setq begin (region-beginning))
      (setq end (region-end))
      )
    (save-excursion
      (goto-char begin)
      (while (search-forward "," end t) (replace-match "，"))
      (goto-char begin)
      (while (search-forward "?" end t) (replace-match "？"))

      (goto-char begin)
      (while (search-forward "“" end t) (replace-match "「"))
      (goto-char begin)
      (while (search-forward "”" end t) (replace-match "」"))
      (goto-char begin)
      (let ((cnt 0))
        (while (search-forward "\"" end t)
          (setq cnt (1+ cnt))
          (if (equal 1 (% cnt 2))
              (replace-match "「")
            (replace-match "」")))))
    (call-interactively 'count-words)))   ;最后统计字数


(provide 'lazy-novel-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-norvel-mode.el ends here.
