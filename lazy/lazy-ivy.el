;; (require 'thingatpt)
;; (require 'cl-macs)
;; (eval-when-compile (require 'counsel))

;; ;;;###autoload
;; (defun vmacs-ivy-magic-eol( arg)
;;   "C-e move to end of line or execute `ivy-immediate-done'"
;;   (interactive "P")
;;   (if (eolp)
;;       (call-interactively 'ivy-immediate-done)
;;     (call-interactively 'end-of-line)))

;; ;;;###autoload
;; (defun vmacs-ivy-magic-kill( arg)
;;   "C-e move to end of line or execute `ivy-immediate-done'"
;;   (interactive "P")
;;   (if (eolp)
;;       (call-interactively 'ivy-switch-buffer-kill)
;;     (call-interactively 'ivy-kill-line)))

;; ;;;###autoload
;; (defun vmacs-ivy-magic-call( arg)
;;   "C-f move forward or execute `ivy-call'"
;;   (interactive "P")
;;   (if (eolp)
;;       (call-interactively 'ivy-call)
;;     (call-interactively 'forward-char)))

;; ;; ;;;###autoload
;; ;; (defun vmacs-ivy-swithc-buffer-open-dired(&optional buffer)
;; ;;   (interactive)
;; ;;   (if (zerop (length buffer))
;; ;;       (dired ivy-text)
;; ;;     (let ((virtual (assoc buffer ivy--virtual-buffers)))
;; ;;       (if (and virtual
;; ;;                (not (get-buffer buffer)))
;; ;;           (dired (file-name-directory (cdr virtual)))
;; ;;         (with-current-buffer buffer (dired default-directory))))))

;; ;; ;;;###autoload
;; ;; (defun vmacs-ivy-dired(&optional buf)
;; ;;   (interactive)
;; ;;   (if ivy--directory
;; ;;       (ivy-quit-and-run
;; ;;        (dired ivy--directory))
;; ;;     (user-error
;; ;;      "Not completing files currently")))

;; (require 'counsel)



;; ;;;###autoload
;; (defun vmacs-ivy-dropto-counsel-git(&optional init)
;;   (interactive)
;;   (ivy-quit-and-run
;;    (counsel-git ivy-text)))

;; (unless (executable-find "rg")
;;   (message "you need install rg on mac(brew install rg)")
;;   (when (eq system-type 'darwin) (shell-command "brew install rg")))

;; ;; ;;;###autoload
;; ;; (defun vmacs-ivy-search(&optional arg)
;; ;;   (interactive "P")
;; ;;   (if current-prefix-arg
;; ;;       (vmacs-counsel-git-grep-region-or-symbol)
;; ;;     (call-interactively 'vmacs-counsel-rg)))

;;   ;; (if (vc-find-root default-directory ".git")
;;   ;;     (call-interactively 'counsel-git-grep)
;;   ;;   (call-interactively 'counsel-rg)))

;; ;;;###autoload
;; (defun vmacs-counsel-git-grep-region-or-symbol ()
;;   "Use `counsel-git-grep' to search for the selected region or
;;  the symbol around point in the current project with git grep."
;;        (interactive)
;;        (let ((input (if (region-active-p)
;;                         (buffer-substring-no-properties
;;                          (region-beginning) (region-end))
;;                       (thing-at-point 'symbol t))))
;;          (counsel-git-grep nil nil)))

;; ;;;###autoload
;; (defun vmacs-counsel-rg (&optional arg)
;;   "Use `counsel-rg' to search for the selected region or
;;  the symbol around point in the current project with riggrep"
;;   (interactive "P")
;;   (let ((input (if (region-active-p)
;;                    (buffer-substring-no-properties
;;                     (region-beginning) (region-end))
;;                  ""))
;;         (default-directory default-directory)
;;         (extra-rg-args ""))
;;     (when current-prefix-arg
;;       (setq extra-rg-args
;;             (read-from-minibuffer (format
;;                                    "%s args: "
;;                                    (car (split-string counsel-ag-command))))))
;;     (counsel-rg  input default-directory extra-rg-args
;;                  (concat "rg in " (abbreviate-file-name default-directory)))))

;; ;;;###autoload
;; (defun vmacs-counsel-toggle-case-senstive (&optional arg)
;;   (interactive)
;;   (cond
;;    ((string-match " -i " counsel-ag-command )
;;     (setq counsel-ag-command (replace-match " -s " t t counsel-ag-command 0)))
;;    ((string-match " -s " counsel-ag-command )
;;     (setq counsel-ag-command (replace-match " -i " t t counsel-ag-command 0))))
;;   (counsel-ag-function ivy-text))

;; ;;;###autoload
;; (defun vmacs-counsel-ag-up-directory ()
;;   "Go to the parent directory."
;;   (interactive)
;;   (let* ((cur-dir (directory-file-name (abbreviate-file-name default-directory)))
;;          (up-dir (abbreviate-file-name (file-name-directory (expand-file-name cur-dir)))))
;;     (setf (ivy-state-directory ivy-last) up-dir)
;;     (when (string-suffix-p cur-dir  (directory-file-name(ivy-state-prompt ivy-last)))
;;       ;; (setf (ivy-state-prompt ivy-last) (concat "rg in" up-dir))
;;       (setq ivy--prompt (concat "rg in " up-dir)))
;;     (setq default-directory up-dir))
;;   (counsel-ag-function ivy-text))


;; (defvar vmacs-last-ag-directory default-directory)
;; ;;;###autoload
;; (defun vmacs-counsel-ag-toggle-git-root ()
;;   "Toggle go to the git root directory."
;;   (interactive)
;;   (if current-prefix-arg
;;       (vmacs-counsel-rg-select-directory)
;;     (let (dir (vc-root (vc-find-root default-directory ".git")))
;;       (if (string= (expand-file-name default-directory)
;;                    (expand-file-name vc-root))
;;           (setq dir (abbreviate-file-name (or vmacs-last-ag-directory default-directory)))
;;         (setq dir (abbreviate-file-name vc-root)))
;;       (setf (ivy-state-directory ivy-last) dir)
;;       ;; (setf (ivy-state-prompt ivy-last) (concat "rg in " dir))
;;       (setq ivy--prompt (concat "rg in " dir))
;;       (setq vmacs-last-ag-directory default-directory)
;;       (setq default-directory dir))
;;     (counsel-ag-function ivy-text)))


;; (defvar vmacs-last-ivy-text )
;; ;; (defvar vmacs-last-state )
;; ;;;###autoload
;; (defun vmacs-counsel-rg-select-directory()
;;   " dynamicly select directory in counsel-ag session."
;;   (interactive)
;;   (setq vmacs-last-ivy-text (or ivy-text ""))
;;   ;; (setq vmacs-last-state ivy-last)
;;   (ivy-quit-and-run
;;     (let ((extra-rg-args "")
;;           (default-directory default-directory))
;;       (setq default-directory (read-directory-name "rg in directory:"))
;;       (setq ivy-last vmacs-last-ivy-text)
;;       (counsel-rg  vmacs-last-ivy-text default-directory extra-rg-args
;;                    (concat "rg in " (abbreviate-file-name default-directory)))


;;       )
;;     ))

(provide 'lazy-ivy)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-ivy.el ends here.
