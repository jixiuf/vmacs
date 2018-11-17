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
    ((derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)
     "Shell")
    ;; ((memq major-mode '(org-mode  diary-mode novel-mode))
    ;;  "OrgMode")
    ( (or (member (buffer-name) '("*scratch*") )
          (string-match-p "\\*scratch.*" (buffer-name)))
      "Scratch")
    ((or (and (string-equal "*" (substring (buffer-name) 0 1))
              (not (vmacs-hide-tabbar-p)))
         (memq major-mode '(magit-process-mode
                            magit-status-mode
                            magit-diff-mode
                            magit-log-mode
                            magit-file-mode
                            magit-blob-mode
                            magit-blame-mode
                            )))
     "Emacs")
    ((vmacs-hide-tabbar-p) "Hidden")
    (t "Common")
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
                      ;; ((eq (current-buffer) b) b)
                      ((with-current-buffer b (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)) b)
                      ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                      (t b)
                      ))
                 (buffer-list)))))

(setq awesome-tab-buffer-list-function 'vmacs-awesome-tab-buffer-list)



(defun vmacs-hide-tabbar-p()
  (cond
   ((member (buffer-name) '("*scratch*" "*Messages*" "*Org Agenda*" "*Compile-Log*") ) t)
   ((member major-mode '(compilation-mode))  t)
   ((string-match boring-window-bof-name-regexp (buffer-name)) t)
   (t
    nil)))

(defun vmacs-hide-tabbar()
  (when  (vmacs-hide-tabbar-p)
    (setq-local header-line-format nil)))

(dolist (hook '(emacs-lisp-mode-hook
                org-agenda-mode-hook
                ;; compilation-mode-hook
                ;; messages-buffer-mode-hook
                ))
  (add-hook hook 'vmacs-hide-tabbar))




(awesome-tab-mode t)
(provide 'conf-awesome-tab)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-awesome-tab.el ends here.
