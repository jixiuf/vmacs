;;; -*- lexical-binding: t; coding:utf-8 -*-
(eval-when-compile (require 'dired))

;;;###autoload
(defun dired-add-to-load-path-or-load-it()
  "on `dired-mode',if thing under point is directory add it to `load-path'
if it is a el-file ,then `load' it"
  (interactive)
  (let ((dir-or-file  (dired-get-filename)))
    (if (file-directory-p dir-or-file)
        (progn
          (add-to-list 'load-path dir-or-file)
          (message (concat dir-or-file " added to load-path" ))
          )
      (if (string-equal  (file-name-extension  dir-or-file ) "el")
          (load  dir-or-file)
        (message (concat dir-or-file "is loaded"))))))


(provide 'lazy-dired)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-dired.el ends here.
