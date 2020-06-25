;;; -*- lexical-binding: t; coding:utf-8 -*-
;; protobuf align 对齐与缩进
;;;###autoload
(defun protobuf-indent-align(begin end &optional _column)
  "do indent and align for protobuf.
bind`indent-region-function' to this function in protobuf-hook"
  (interactive "r")
  (save-excursion
    (let (end-marker
          (keyword-regexp (rx (or "required" "optional" "repeated"))))
      (unless mark-active
        (setq begin (point-min))
        (setq end (point-max)))
      (goto-char end)
      (setq end-marker (point-marker))
      (c-indent-region begin (marker-position end-marker))
      (while (re-search-backward "^message[ \t]+[a-zA-Z0-9_-]+[ \t]*{[ \t]*\\(//\\)?" begin t)
        (let ((max-type-column 0)
              message-end message-beginning)
          (goto-char (point-at-bol))
          (setq message-beginning (point-marker))
          (c-end-of-defun)
          (setq message-end (point-marker))
          (while (re-search-backward keyword-regexp (marker-position message-beginning) t)
            (forward-word 2)
            (when (> (current-column) max-type-column)
              (setq max-type-column (current-column)))
            (goto-char (point-at-bol)))
          (goto-char (marker-position message-end))
          (while (re-search-backward keyword-regexp (marker-position message-beginning) t)
            (forward-word )
            (just-one-space)
            (forward-word )
            (delete-horizontal-space)
            (insert (make-string (1+ (- max-type-column (current-column))) ? ))
            (goto-char (point-at-bol)))
          (align-regexp (marker-position message-beginning) (marker-position message-end) "\\(\\s-*\\)=") ;align for =
          ;; (align-regexp (marker-position message-beginning) (marker-position message-end) "\\(\\s-*\\)//") ;align for //
          (goto-char (marker-position message-beginning))))
      (goto-char (marker-position end-marker))
      (while (re-search-backward "\\(=\\)[ \t]*[0-9]+[ \t]*;" begin t)
        (goto-char (match-end 1))
        (just-one-space)                ;make sure one space after =
        (goto-char (match-end 0))
        (if (looking-at "[ \t]*//")
            (just-one-space)
          (delete-horizontal-space))
        (goto-char (point-at-bol))))))

(provide 'lazy-program-protobuf)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-protobuf.el ends here.
