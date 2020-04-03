
(defun completion-prescient-completion (string table predicate point
                                               &optional all-p)
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (boundaries (completion-boundaries beforepoint table predicate afterpoint))
         (prefix (substring beforepoint 0 (car boundaries)))
         (infix (concat
                 (substring beforepoint (car boundaries))
                 (substring afterpoint 0 (cdr boundaries))))
         (suffix (substring afterpoint (cdr boundaries)))
         ;; |-              string                  -|
         ;;              point^
         ;;            |-  boundaries -|
         ;; |- prefix -|-    infix    -|-  suffix   -|
         ;;
         ;; Infix is the part supposed to be completed by table, AFAIKT.
         (candidates (prescient-filter infix (all-completions prefix table predicate))))
    (if all-p
        ;; Implement completion-all-completions interface
        (when candidates
          ;; Not doing this may result in an error.
          (setcdr (last candidates) (length prefix))
          candidates)
      ;; Implement completion-try-completions interface
      (cond
       ((and (= (length candidates) 1)
             (equal infix (car candidates)))
        t)
       ((= (length candidates) 1)
        (cons string point))
       ;; Do nothing, i.e leave string as it is.
       (t (cons string point))))))

(defun completion-prescient-try-completion (string table predicate point)
  (completion-prescient-completion string table predicate point))
(defun completion-prescient-all-completions (string table predicate point)
  (completion-prescient-completion string table predicate point 'all))

(add-to-list 'completion-styles-alist
             '(prescient
               completion-prescient-try-completion
               completion-prescient-all-completions
               "prescient completion"))

(put 'prescient 'completion--adjust-metadata 'completion--prescient-adjust-metadata)

(defun completion--prescient-adjust-metadata(metadata))


(provide 'prescient-complete)

;; Local Variables:
;; coding: utf-8
;; End:

;;; prescient.el ends here.
