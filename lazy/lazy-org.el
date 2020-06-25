;;; -*- lexical-binding: t; coding:utf-8 -*-
;; 插动图片到org 文件时， 自动将文件放到org下的static/下，并插入[[file:...static/image.jpg]]
;;;###autoload
(defun vmacs-org-insert-image (event)
  (interactive "e")
  (x-focus-frame nil)
  (let* ((payload (car (last event)))
         (type (car payload))
         (fromname (nth 2 payload))
         (img-regexp "\\(gif\\|png\\|jp[e]?g\\)\\>")
         (destname fromname)
         img-dir
         )
    (when (file-exists-p "../img/")
      (setq img-dir "../img/"))
    (when (file-exists-p "./img/")
      (setq img-dir "./img/"))
    (when (and  (eq 'drag-n-drop (car event))
                (eq 'file type)
                (string-match img-regexp fromname)
                img-dir)
      (let ((filebasename (file-name-base (buffer-file-name)) ))
        (setq destname (concat img-dir filebasename "-" (format-time-string "%Y-%m-%d-%H-%M-%S") "." (file-name-extension fromname)))
        (rename-file fromname destname t))
      (goto-char (nth 1 (event-start event)))
      (insert (format "[[file:%s]]" (file-relative-name destname (file-name-directory (buffer-file-name))))))))


;;;###autoload
(defun show-todo-list-after-init(&optional _frame)
  (require 'org-agenda)
  (dolist (f org-agenda-files)
    (unless (file-exists-p f)
      (setq org-agenda-files (delete f org-agenda-files))))
  (when org-agenda-files
    (call-interactively 'org-todo-list)
    (switch-to-buffer "*Org Agenda*")))


(provide 'lazy-org)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-org.el ends here.
