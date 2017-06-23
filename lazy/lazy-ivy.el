(require 'thingatpt)
(eval-when-compile (require 'counsel))

;;;###autoload
(defun vmacs-ivy-magic-eol( arg)
  "C-e move to end of line or execute `ivy-immediate-done'"
  (interactive "P")
  (if (eolp)
      (call-interactively 'ivy-immediate-done)
    (call-interactively 'end-of-line)))

;;;###autoload
(defun vmacs-ivy-swithc-buffer-open-dired(&optional buffer)
  (interactive)
  (if (zerop (length buffer))
      (dired ivy-text)
    (let ((virtual (assoc buffer ivy--virtual-buffers)))
      (if (and virtual
               (not (get-buffer buffer)))
          (dired (file-name-directory (cdr virtual)))
        (with-current-buffer buffer (dired default-directory))))))

;;;###autoload
(defun vmacs-ivy-dired(&optional buf)
  (interactive)
  (if ivy--directory
      (ivy-quit-and-run
       (dired ivy--directory))
    (user-error
     "Not completing files currently")))

(require 'counsel)



;;;###autoload
(defun vmacs-ivy-dropto-counsel-git(&optional init)
  (interactive)
  (ivy-quit-and-run
   (counsel-git ivy-text)))

(unless (executable-find "rg")
  (message "you need install rg on mac(brew install rg)")
  (when (eq system-type 'darwin) (shell-command "brew install rg")))

;; ;;;###autoload
;; (defun vmacs-ivy-search(&optional arg)
;;   (interactive "P")
;;   (if current-prefix-arg
;;       (vmacs-counsel-git-grep-region-or-symbol)
;;     (call-interactively 'vmacs-counsel-rg-region-or-symbol)))

  ;; (if (vc-find-root default-directory ".git")
  ;;     (call-interactively 'counsel-git-grep)
  ;;   (call-interactively 'counsel-rg)))

;;;###autoload
(defun vmacs-counsel-git-grep-region-or-symbol ()
  "Use `counsel-git-grep' to search for the selected region or
 the symbol around point in the current project with git grep."
       (interactive)
       (let ((input (if (region-active-p)
                        (buffer-substring-no-properties
                         (region-beginning) (region-end))
                      (thing-at-point 'symbol t))))
         (counsel-git-grep nil input)))

;;;###autoload
(defun vmacs-counsel-rg-region-or-symbol (&optional arg)
  "Use `counsel-rg' to search for the selected region or
 the symbol around point in the current project with riggrep"
       (interactive "P")
       (let ((input (if (region-active-p)
                        (buffer-substring-no-properties
                         (region-beginning) (region-end))
                      (thing-at-point 'symbol t)))
             (default-directory default-directory)
             vc-root)
         (when (= (prefix-numeric-value current-prefix-arg) 16)
           (setq default-directory (read-directory-name "rg in directory: ")) )
         (when (= (prefix-numeric-value current-prefix-arg) 4)
           (if (setq vc-root (vc-find-root default-directory ".git"))
               (setq default-directory vc-root)
             (setq default-directory (read-directory-name "rg in directory: "))))
         (counsel-rg  input default-directory nil
                      (concat "rg in " (abbreviate-file-name default-directory)))))

(provide 'lazy-ivy)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-ivy.el ends here.
