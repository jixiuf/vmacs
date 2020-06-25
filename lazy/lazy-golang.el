;;; -*- lexical-binding: t; coding:utf-8 -*-
;; golang 生成getter setter
;;;###autoload
(defun golang-setter-getter(beg end)
  "generate sets and gets for golang."
  (interactive "r")
  (require 'lazy-camelize)
  (let ((region-string (buffer-substring-no-properties beg end))
        (struct-name)
        (pairs)
        (buf-str))
    (with-temp-buffer
      (modify-syntax-entry ?< "w" )
      (modify-syntax-entry ?> "w" )
      (insert region-string) (insert "\n")
      (goto-char (point-min))
      (when (re-search-forward "type \\(.*\\) struct" (point-max) t)
        (setq struct-name (match-string 1))
        (message struct-name)
        (forward-line)(beginning-of-line)
        (while (re-search-forward "\\([a-zA-Z0-9_]+\\)[ \t]+\\([a-zA-z0-9_.]+\\)\\b" (point-at-eol) t)
          (setq pairs (append pairs (list (list (match-string 1)(match-string 2)))))
          (forward-line) (beginning-of-line)
          )
        (erase-buffer)
        (dolist (pair pairs)
          (insert (format "func (this *%s) Set%s(value %s) *%s{\n" struct-name (upcase-first-char(car pair)) (nth 1 pair) struct-name) )
          (insert (format "    this.%s = value\n" (car pair)) )
          (insert (format "    return this\n"))
          (insert (format "}\n") )
          (insert (format "func (this %s) Get%s() %s {\n" struct-name (upcase-first-char(car pair)) (nth 1 pair)) )
          (insert (format "    return this.%s\n" (car pair)) )
          (insert (format "}\n") )
          )
        (setq buf-str (buffer-string))))
    (goto-char end)
    (insert "\n")
    (insert buf-str)))

(provide 'lazy-golang)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-golang.el ends here.
