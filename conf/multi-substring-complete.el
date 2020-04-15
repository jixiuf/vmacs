;; https://github.com/oantolin/orderless
(require 'cl-lib)
;;
(defun orderless-highlight-match (regexp string)
  (when (string-match regexp string)
    (font-lock-prepend-text-property
     (match-beginning 0)
     (match-end 0)
     'face 'completions-common-part
     string)
    t))

(defun orderless-all-completions (string table pred _point)
  (save-match-data
    (let* ((limit (car (completion-boundaries string table pred "")))
           (prefix (substring string 0 limit))
           (all (all-completions prefix table pred))
           (regexps (split-string (substring string limit))))
      (when minibuffer-completing-file-name
        (setq all (completion-pcm--filename-try-filter all)))
      (condition-case nil
          (progn
            (setq all
                  (cl-loop for original in all
                           for candidate = (copy-sequence original)
                           when (cl-loop for regexp in regexps
                                         always (orderless-highlight-match
                                                 regexp candidate))
                           collect candidate))
            (when all (nconc all (length prefix))))
        (invalid-regexp nil)))))

(defun orderless-try-completion (string table pred point &optional _metadata)
  (let* ((limit (car (completion-boundaries string table pred "")))
         (prefix (substring string 0 limit))
         (all (orderless-all-completions string table pred point)))
    (cl-flet ((measured (string) (cons string (length string))))
      (cond
       ((null all) nil)
       ((atom (cdr all)) (measured (concat prefix (car all))))
       (t (measured string))))))

(cl-pushnew '(orderless
              orderless-try-completion orderless-all-completions
              "Completion of multiple regexps, in any order.")
            completion-styles-alist
            :test #'equal)


(provide 'multi-substring-complete)

;; Local Variables:
;; coding: utf-8
;; End:

;;; multisubstring-complete.el ends here.
