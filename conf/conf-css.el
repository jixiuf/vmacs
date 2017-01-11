
;;; 为颜色着色
(defvar hexcolor-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background
                     (match-string-no-properties 0)))))))

(defun hexcolor-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil hexcolor-keywords))
(add-hook 'css-mode-hook 'hexcolor-add-to-font-lock)

(provide 'conf-css)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-css.el ends here.
