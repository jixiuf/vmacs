;; some code for windows system
(setq exec-path (delete-dups  (cons (expand-file-name "~/.emacs.d/w32bin/") exec-path)))
(setenv "PATH" (concat (convert-standard-filename (expand-file-name  "~/.emacs.d/w32bin/")) ";" (getenv "PATH") ))


(provide 'conf-w32)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-w32.el ends here.
