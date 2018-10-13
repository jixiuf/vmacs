(awesome-tab-mode t)
(setq awesome-tab-cycle-scope 'tabs)
(global-set-key  (kbd "s-n") 'awesome-tab-forward)
(global-set-key  (kbd "s-C-M-n") 'awesome-tab-forward)
(global-set-key  (kbd "s-p") 'awesome-tab-backward)
(global-set-key  (kbd "s-C-M-p") 'awesome-tab-backward)

;; 只为eshell-mode term-mode 启用awesome-tab

(setq awesome-tab-buffer-groups-function 'vmacs-awesome-tab-buffer-groups)
(defun vmacs-awesome-tab-buffer-groups ()
  "`awesome-tab-buffer-groups' control buffers' group rules.

Group awesome-tab with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `projectile-project-p' with project name."
  (list
   (cond
    ((derived-mode-p 'eshell-mode 'term-mode 'shell-mode)
     "Shell")
    ((or (string-equal "*" (substring (buffer-name) 0 1))
         (memq major-mode '(magit-process-mode
                            magit-status-mode
                            magit-diff-mode
                            magit-log-mode
                            magit-file-mode
                            magit-blob-mode
                            magit-blame-mode
                            )))
     "Emacs")
    ;; ((derived-mode-p 'emacs-lisp-mode)
    ;;  "Elisp")
    ;; ((derived-mode-p 'dired-mode)
    ;;  "Dired")
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
     "OrgMode")
    (t
     (if (projectile-project-p)
         (awesome-tab-get-group-name (current-buffer))
       "Common"))
    ))
  )

(defun vmacs-awesome-tab-buffer-list ()
  "Return the list of buffers to show in tabs.
only show eshell-mode term-mode and shell-mode."
  (awesome-tab-filter
   (lambda (x)
     (let ((name (format "%s" x)))
       (and
        (not (string-prefix-p "*epc" name))
        (not (string-prefix-p "*helm" name))
        (not (string-prefix-p "*Compile-Log*" name))
        (not (string-prefix-p "*lsp" name))
        (not (and (string-prefix-p "magit" name)
                  (not (file-name-extension name))))
        )))
   (delq nil
         (mapcar #'(lambda (b)
                     (cond
                      ;; Always include the current buffer.
                      ((eq (current-buffer) b) b)
                      ((with-current-buffer b (derived-mode-p 'eshell-mode 'term-mode 'shell-mode)) b)
                      ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                      (t b)
                      ))
                 (buffer-list)))))
(setq awesome-tab-buffer-list-function 'vmacs-awesome-tab-buffer-list)

(provide 'conf-awesome-tab)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-awesome-tab.el ends here.
