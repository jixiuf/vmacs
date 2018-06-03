(defvar vmacs-dump-process nil)
(defvar vmacs-dump-buffer-name " *vmacs-dump*")
(defvar vmacs-dump-directory "~/.emacs.d/cache/dump/")
(defvar vmacs-pdumper-executable-file "emacs")
(defvar vmacs-dump-file "emacs.pdump")

(defun vmacs-dumping-p()
  (eq vmacs-dumping-state 'dumping))

(defun vmacs-not-dumping-p()
  (not (eq vmacs-dumping-state 'dumping)))

(defun vmacs-dump-emacs ()
  "Dump emacs in a subprocess."
  (interactive)
  (when vmacs-dump-process
    (message "Cancel running dumping process to start a new one.")
    (delete-process vmacs-dump-process)
    (when (buffer-live-p vmacs-dump-buffer-name)
      (with-current-buffer vmacs-dump-buffer-name
        (erase-buffer))))
  (make-directory vmacs-dump-directory t)
  (setq vmacs-dump-process
        (make-process
         :name "vmacs-dumper"
         :buffer vmacs-dump-buffer-name
         :command
         (list vmacs-pdumper-executable-file
               "--batch"
               "-l" "~/.emacs.d/dump-init.el"
               "-eval" (concat "(dump-emacs-portable \""
                               (concat vmacs-dump-directory
                                       vmacs-dump-file)
                               "\")")))))



(provide 'conf-dump)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-dump.el ends here.
