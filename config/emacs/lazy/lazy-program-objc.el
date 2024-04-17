;;; -*- lexical-binding: t; coding:utf-8 -*-
;;;###autoload
(defun objc-surround()
  (interactive)
  (unless (looking-back "\]" (point-at-bol)) (forward-char 1))
  (backward-sexp)
  (insert "[")
  (forward-sexp)
  (insert "]")
  (backward-char 1))

(provide 'lazy-program-objc)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-program-objc.el ends here.
