(setq awesome-tab-cycle-scope 'tabs)
(global-set-key  (kbd "s-n") 'awesome-tab-forward)
(global-set-key  (kbd "s-C-M-n") 'awesome-tab-forward)
(global-set-key  (kbd "s-p") 'awesome-tab-backward)
(global-set-key  (kbd "s-C-M-p") 'awesome-tab-backward)

(vmacs-leader "e" 'awesome-tab-build-ivy-source)
;; 只为eshell-mode term-mode 启用awesome-tab

(setq awesome-tab-buffer-groups-function 'vmacs-awesome-tab-buffer-groups)

(defun vmacs-awesome-tab-buffer-groups ()
  "`awesome-tab-buffer-groups' control buffers' group rules.
    Group awesome-tab with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `projectile-project-p' with project name."
  (list
   (cond
    ((or (member (buffer-name) '("*scratch*") )
         (string-match-p "\\*scratch.*" (buffer-name))
         (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)
         )
     "Term")
    ;; ((memq major-mode '(org-mode  diary-mode novel-mode))
    ;;  "OrgMode")
    ((or (and (string-equal "*" (substring (buffer-name) 0 1))
              (vmacs-show-tabbar-p))
         (memq major-mode '(magit-process-mode
                            magit-status-mode
                            magit-diff-mode
                            magit-log-mode
                            magit-file-mode
                            magit-blob-mode
                            magit-blame-mode
                            )))
     "Emacs")
    ((not (vmacs-show-tabbar-p)) nil)
    (t "Common")
    ))
  )

(defun vmacs-awesome-tab-buffer-list ()
  "Return the list of buffers to show in tabs.
only show eshell-mode term-mode and shell-mode."
  (awesome-tab-filter
   #'vmacs-show-tabbar-p
   (buffer-list)))

(setq awesome-tab-buffer-list-function 'vmacs-awesome-tab-buffer-list)

(defun vmacs-show-tabbar-p(&optional buf redisplay)
  (let ((show t))
    (with-current-buffer (or buf (current-buffer))
      (cond
       ((char-equal ?\  (aref (buffer-name) 0))
        (setq show nil))
       ((member (buffer-name) '("*Messages*"  "*Org Agenda*" "*Compile-Log*"
                                "*Ediff Control Panel*"))
        (setq show nil))
       ((member major-mode '(compilation-mode))
        (setq show nil))
       ((string-match boring-window-bof-name-regexp (buffer-name))
        (setq show nil))
       (t t))
      (unless show
        ;; (kill-local-variable 'header-line-format)
        (setq header-line-format nil)
        (when redisplay (redisplay t)))
      show)))

(defun vmacs-awesome-tab-inhibit-function()
  (not (vmacs-show-tabbar-p (current-buffer) t)))

(setq awesome-tab-inhibit-functions '(vmacs-awesome-tab-inhibit-function))


(awesome-tab-mode t)
(provide 'conf-awesome-tab)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-awesome-tab.el ends here.
