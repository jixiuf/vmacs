;; pip install basedpyright
;; pip install debugpy
(setq exec-path (delete-dups  (cons (concat (expand-file-name "~") "/venv/bin") exec-path)))
(setenv "PATH" (concat  (concat (expand-file-name "~") "/venv/bin:") (getenv "PATH") ))
;; (defun vmacs-python-mode-hook ()
;;   ;; (unless (executable-find "pyls")
;;   ;;   (find-file "~/.emacs.d/conf/conf-program-python.el")
;;   ;;   (message "pyls not found,try setup python now"))
;;   (require 'lsp-python-ms)
;;   (lsp-deferred))
;; (add-hook 'python-mode-hook 'vmacs-python-mode-hook)


;; ; Set PYTHONPATH, because we don't load .bashrc
;; (defun set-python-path-from-shell-PYTHONPATH ()
;; (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PYTHONPATH'")))
;; (setenv "PYTHONPATH" path-from-shell)))





(provide 'conf-program-python)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-program-python.el ends here.
