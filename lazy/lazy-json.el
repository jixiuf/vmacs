;;; -*- lexical-binding: t; coding:utf-8 -*-
;; man json_pp
;;;###autoload
(defun vmacs-json-pretty()
  (interactive)
  (let ((start (point-min))
        (end (point-max)))
    (when (region-active-p)
      (setq start (region-beginning))
      (setq end (region-end)))
    (save-excursion
      (shell-command-on-region start end "json_pp --json_opt=canonical,pretty" (current-buffer) t) "*Message*")))

(provide 'lazy-json)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-json.el ends here.
