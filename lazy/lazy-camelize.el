;; -*- lexical-binding: t; coding:utf-8 -*-
(defun mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

;; (camelize "hello_world") =="Hello_World"
;; (camelize "hello_world" "_") =="HelloWorld"
;; (camelize "HELLO_WORLD" "_") =="HelloWorld"
;; (camelize "helloworld") =="Helloworld"
(defun camelize (s &optional separator )
  "Convert under_score string S to CamelCase string."
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (capitalize (downcase word)))
                        (if separator (split-string s "_") (list s))
                        ) ""))

;; (camelize-method "hello_world_everyone") =="hello_world_everyone"
;; (camelize-method "HELLO_WORLD_EVERYONE") =="hello_world_everyone"
;; (camelize-method "hello_world_everyone" "_") =="helloWorldEveryone"
;; (camelize-method "HELLO_WORLD_EVERYONE" "_")== "helloWorldEveryone"
(defun camelize-method (s &optional separator)
  "Convert under_score string S to camelCase string."
  (mapconcat 'identity (mapcar-head
                        '(lambda (word) (downcase word))
                        '(lambda (word) (capitalize (downcase word)))
                        (if separator (split-string s "_") (list s))) ""))
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

;; (un-camelcase-string "helloWorld") == "hello_world"
;; (un-camelcase-string "helloWorld" "") == "helloworld"
(defun un-camelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
    Default for SEP is a hyphen \"-\".

    If third argument START is non-nil, convert words after that
    index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "_")
                                     (downcase (match-string 0 s)))
                             t nil s)))
    (downcase s)))

;;;###autoload
(defun toggle-camelize()
  "hello_word <->HelloWorld"
  (interactive)
  (save-excursion
    (let ((symbol-name (thing-at-point 'symbol))
          (bounds (bounds-of-thing-at-point 'symbol))
          changed-symbol
          )

      (if (or (string-match "_" symbol-name) (s-lowercase symbol-name))
          (setq changed-symbol (camelize symbol-name "_"))
        (setq changed-symbol  (un-camelcase-string symbol-name)))
      (delete-region (car bounds) (cdr bounds))
      (insert changed-symbol)
      )
    )
  )
(defun s-lowercase (s)
  (let ((case-fold-search nil))
    (not (string-match-p "[A-Z]+" s))))

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
