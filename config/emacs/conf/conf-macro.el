(global-set-key (kbd "C-c h") help-map)
(define-key help-map (kbd "C-f") nil)
(define-key help-map (kbd "C-m") nil)

(defmacro vmacs-leader (key cmd)
  `(global-set-key
    (if (stringp ,key)
        (kbd (concat "C-c " ,key))
      (vconcat [3] ,key))               ;[3] =C-c ?
    ,cmd))

(defmacro vmacs-defun (fun-name &rest body)
  (declare (indent defun)
           (doc-string 3))
  (let ((fun (intern (format "%s" fun-name))))
    `(defun ,fun()
       (interactive)
       ,@body)))


;; ;; (print (macroexpand-1 '(with-mode-off icomplete-vertical-mode  (find-file-at-point))))
;; (defmacro with-mode-off (mode &rest body)
;;   (declare (indent defun)
;;            (doc-string 3))
;;   (macroexp-let2 nil mode-p `(bound-and-true-p ,mode)
;;     `(progn
;;        (when ,mode-p (,mode -1))
;;        (unwind-protect
;;            (progn ,@body)
;;          (when ,mode-p (,mode 1))))))

;; ;; (print (macroexpand-1 '(with-mode-off icomplete-vertical-mode  (find-file-at-point))))
;; ;; (print (macroexpand-1 '(with-mode-on icomplete-vertical-mode  (find-file-at-point))))
;; (defmacro with-mode-on (mode &rest body)
;;   (declare (indent defun)
;;            (doc-string 3))
;;   (macroexp-let2 nil mode-p `(bound-and-true-p ,mode)
;;     `(progn
;;        (unless ,mode-p (,mode 1))
;;        (unwind-protect
;;            (progn ,@body)
;;          (unless ,mode-p (,mode -1))))))

;; (print (macroexpand-1 '(icomplete-horizontal find-file  (find-file-at-point))))
;; (icomplete-horizontal find-file  (find-file-at-point))



(provide 'conf-macro)

;; Local Variables:
;; coding: utf-8
;; End:

;;; init-macro.el ends here.
