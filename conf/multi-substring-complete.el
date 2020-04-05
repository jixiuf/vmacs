;; modified based on flex completion-styles on emacs27
;;
(defun completion-multisubstring--make-pattern (pattern)
  "Convert PCM-style PATTERN into PCM-style multisubstring pattern.

This turns
    (prefix \"foo bar\" point)
into
    (prefix \"f\"  \"o\"  \"o\" any \"b\" \"a\" \"r\" point)
which is at the core of multisubstring logic.  The extra
'any' is optimized away later on."
  (mapcan (lambda (elem)
            (if (stringp elem)
                (mapcan (lambda (char)
                          (if (char-equal ?\  char)
                              (list 'any)
                            (list (string char))))
                        elem)
              (list elem)))
          pattern))

(defun completion-multisubstring-try-completion (string table pred point)
  "Try to multisubstring-complete STRING in TABLE given PRED and POINT."
  (pcase-let ((`(,all ,pattern ,prefix ,suffix ,_carbounds)
               (completion-substring--all-completions
                string table pred point
                #'completion-multisubstring--make-pattern)))
    (if minibuffer-completing-file-name
        (setq all (completion-pcm--filename-try-filter all)))
    (completion-pcm--merge-try pattern all prefix suffix)))

(defun completion-multisubstring-all-completions (string table pred point)
  "Get multisubstring-completions of STRING in TABLE, given PRED and POINT."
  (pcase-let ((`(,all ,pattern ,prefix ,_suffix ,_carbounds)
               (completion-substring--all-completions
                string table pred point
                #'completion-multisubstring--make-pattern)))
    (when all
      (nconc (completion-pcm--hilit-commonality pattern all)
             (length prefix)))))

(add-to-list 'completion-styles-alist
             '(multisubstring
               completion-multisubstring-try-completion
               completion-multisubstring-all-completions
               "multi substring completion,split by space"))

;; (put 'multisubstring 'completion--adjust-metadata 'completion--flex-adjust-metadata)



(provide 'multi-substring-complete)

;; Local Variables:
;; coding: utf-8
;; End:

;;; multisubstring-complete.el ends here.
