;; -*- lexical-binding: t; coding:utf-8 -*-
(defun camelize  (s) (mapconcat 'capitalize (split-name s) ""))
(defun underscore (s) (mapconcat 'downcase   (split-name s) "_"))
(defun dasherize  (s) (mapconcat 'downcase   (split-name s) "-"))
;; (camelize-method "hello_world_everyone") =="hello_world_everyone"
;; (camelize-method "HELLO_WORLD_EVERYONE") =="hello_world_everyone"
(defun camelize-method (s)
  "Convert under_score string S to camelCase string."
  (mapconcat 'identity (mapcar-head
                        '(lambda (word) (downcase word))
                        '(lambda (word) (capitalize (downcase word)))
                        (split-name s)) ""))

;; (upcase-first-char "hello") "Hello"
;; (upcase-first-char "HELLO") "HELLO"
;; (upcase-first-char "helloWorld") "HelloWorld"
;; (upcase-first-char "hello_world") "Hello_world"
;; (capitalize "hello_world") "Hello_World"
;; (upcase-first-char "helloWorld" "set") "setHelloWorld"
(defun upcase-first-char (s &optional prefix)
  "make the first char `upcase' and return (concat prefix upcasedstring)"
  (when  (>  (length s) 0)
    (let ( (first-char (substring s 0 1 ))
           (rest  (substring s  1 )))
      (concat (or prefix "") (upcase first-char) rest))))


(defun split-name (s)
  (split-string
   (let ((case-fold-search nil))
	 (downcase
	  (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))



(defun camelscore (s)
  (cond ((string-match-p "\\(?:[a-z]+_\\)+[a-z]+" s)	(dasherize  s))
        ((string-match-p "\\(?:[a-z]+-\\)+[a-z]+" s)	(camelize  s))
        (t						(underscore s)) ))

;;;###autoload
(defun toggle-camelize()
  (interactive)
  (let* ((case-fold-search nil)
         (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
         (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
         (txt (buffer-substring beg end))
         (cml (camelscore txt)) )
	(if cml (progn (delete-region beg end) (insert cml))) ))



(defun swagger-type(gotype)
  (cond
   ((string= gotype "string") "string")
   ((string= gotype "int") "integer")
   ((string= gotype "int64") "integer")
   ((string= gotype "uint64") "integer")
   ((string= gotype "int32") "integer")
   ((string= gotype "float64") "number")
   ((string= gotype "float32") "number")
   ((string= gotype "bool") "boolean")
   (t gotype)))


(provide 'lazy-camelize)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-camelize.el ends here
