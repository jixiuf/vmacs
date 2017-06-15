;; -*- lexical-binding: t -*-
;;;###autoload
(defun vmacs-ivy-reloading (cmd)
    (lambda (x)
      (funcall cmd x)
      (ivy--reset-state ivy-last)))
;;;###autoload
(defun vmacs-ivy-given-file (cmd prompt) ; needs lexical-binding
  (lambda (source)
    (let ((target
           (let ((enable-recursive-minibuffers t))
             (read-file-name
              (format "%s %s to:" prompt source)))))
      (funcall cmd source target 1))))

(defun vmacs-confirm-delete-file (x)
  (dired-delete-file x 'confirm-each-subdirectory))

;;;###autoload
(defun vmacs-ivy-magic-eol( arg)
  "C-e move to end of line or execute `ivy-immediate-done'"
  (interactive "P")
  (if (eolp)
      (call-interactively 'ivy-immediate-done)
    (call-interactively 'end-of-line)))


(provide 'lazy-ivy)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-ivy.el ends here.
