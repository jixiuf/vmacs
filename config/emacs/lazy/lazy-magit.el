;;; -*- lexical-binding: t; -*-
;; (require 'magit)



;; (setq-default magit-log-format-graph-function 'magit-log-format-unicode-graph)

;; ;;;###autoload
;; (defun vmacs-update-repo-revision()
;;   (when  (or (string-prefix-p "magit-commit" (symbol-name last-command))
;;              (string-prefix-p "magit-commit" (symbol-name this-command))
;;              (string-prefix-p "with-editor-finish" (symbol-name this-command))
;;              (string-prefix-p "with-editor-finish" (symbol-name last-command))
;;              )
;;     (let* (( repos (magit-toplevel))
;;            (config-file (expand-file-name "config.online.toml" repos)))
;;       (when (and repos (file-exists-p config-file)
;;                  (equal "master" (magit-get-current-branch)))
;;         (with-current-buffer (find-file-noselect config-file)
;;           (goto-char (point-min))
;;           (when (search-forward-regexp "git=\"\\(.*\\)\"" nil t)
;;             (replace-match (magit-rev-parse "--short" "HEAD") t t nil 1)
;;             ))))))

;; 浏览magit某个文件的历史版本时，调用此方法，等于恢复此文件的版本
;; C-cC-c vmacs-smart-double-ctrl-c会调用vmacs-magit-blob-save
;;;###autoload
(defun vmacs-magit-blob-save()
  (interactive)
  (let ((file magit-buffer-file-name)
        (blob-buf (current-buffer)))
    (when file
      (with-current-buffer (find-file file)
        (widen)
        (replace-buffer-contents  blob-buf))
      (message "save blob to file %s" file))
    (dolist (buf (buffer-list))         ;关闭此文件所有版本的blob buffer
      (with-current-buffer buf
        (when (equal magit-buffer-file-name file)
          (kill-this-buffer))))))

;;;###autoload
(defun vmacs-magit-blob-quit()
  (interactive)
  (let ((file magit-buffer-file-name))
    (unless file
      (setq file (buffer-file-name)))
    (when file
      (dolist (buf (buffer-list))         ;关闭此文件所有版本的blob buffer
        (with-current-buffer buf
          (when (equal magit-buffer-file-name file)
            (kill-this-buffer)))))))

;;;###autoload
(defun vmacs-magit-blob-toggle()
  (interactive)
  (if magit-buffer-file-name
      (vmacs-magit-blob-quit)
    (magit-blob-previous)
    (message "magit timemachine ...")))

;;在magit-log-buffer-file 产生的log buffer中
;; return : 查看当前commit 的diff
;; 有选择区域 则查看区域内的diff
;; C-u 则打开当前对应的版本的文件
;;;###autoload
(defun vmacs-magit-diff-range (rev-or-range &optional args files range-p)
  "Show differences between two commits for current file."
  (interactive
   (pcase-let* ((mcommit (magit-section-value-if 'module-commit))
                (atpoint (or mcommit
                             (thing-at-point 'git-revision t)
                             (magit-branch-or-commit-at-point)))
                (range-p (region-active-p))
                (`(,args ,files) (magit-diff-arguments)))
     (list (or (and (not range-p) atpoint)
               (magit-diff-read-range-or-commit "Diff for range"))
           args files range-p)))
  (if range-p
      (magit-diff-setup-buffer rev-or-range nil args (or (list (magit-current-file) files)))
    (let ((file (magit-current-file)))
      (if (and current-prefix-arg file)
          (magit-find-file rev-or-range file)
        (call-interactively #'magit-show-commit)))
    ))
;; ;;;###autoload
;; (defun vmacs-magit-find-file-at-point ()
;;   "View FILE from REV at point."
;;   (interactive)
;;   (let ((file (magit-current-file))
;;         (rev (or (magit-branch-or-commit-at-point)
;;                  (magit-get-current-branch))))
;;     (if (and file rev )
;;         (magit-find-file rev file)
;;       (call-interactively #'magit-find-file))))


(defun git-email--extract-headers (patch-file)
  "Extract all the headers from a PATCH-FILE."
  (with-temp-buffer
    (insert-file-contents patch-file)
    ;; Find the first headers, and jump to the start of it's line
    (re-search-forward "\\([A-Za-z0-9-]+\\): \\(.*\\)")
    (beginning-of-line)
    (let ((headers (mail-header-extract-no-properties)))
      ;; Headers are returned as downcased symbols, but all
      ;; compose-mail functions expect headers to be capitialized
      ;; strings.
      (dolist (h headers headers)
        (when (symbolp (car h))
          (setcar h (capitalize (symbol-name (car h)))))))))

(defun git-email--send-files (files &optional subject)
  "Send email for each file in FILES."
  (dolist (file files)
    (git-email--compose-email file subject)
    ))

(defun git-email--compose-email (patch-file &optional subject)
  "Given a PATCH-FILE, compose an email.
Extracts the relevant headers and the diff from the PATCH-FILE and inserts
them into the message buffer."
  (let* ((headers (git-email--extract-headers patch-file))
         ;; Remove empty headers.
         (used-headers (seq-filter
                        (lambda (header)
                          (not (string-equal (cdr header) "")))
                        headers))
         (to-address (cdr (assoc "To" used-headers 'string-equal)))
         (diff (git-email--extract-diff patch-file))
         (dir default-directory)
         (mu4e-compose-context-policy 'ask-if-none))
    (require 'mu4e)
    (require 'conf-mail)
    (mu4e t)
    (sit-for 0.3)
    (mu4e-context-switch t "qq" )
    (mu4e-compose-new
     to-address
     (or (cdr (assoc "Subject" used-headers 'string-equal)) subject)
     ;; Remove "from" header, as it interferes with mu4e's
     ;; built in context feature.
     ;; (seq-filter #'git-email--remove-subject used-headers)
     (seq-filter (lambda (header)
                   (not (eq (car header) 'from)))
                 (seq-filter #'git-email--remove-subject used-headers))
     )
    (setq default-directory dir)
    ;; Insert diff at the beginning of the body
    (goto-char (point-min))
    (let ((_ (or (re-search-forward
                  "^<#part \\(encrypt\\|sign\\)=.*mime>$"
                  nil t)
                 (re-search-forward (regexp-quote mail-header-separator) nil t))))
      (save-excursion
        (insert (git-email--fontify-using-faces
                 (git-email--fontify-diff diff)))
        (set-buffer-modified-p nil)))
    ;; Jump to subject or 'to' address if they are emtpy
    (goto-char (point-min))
    (re-search-forward "To: " nil t)))

(defun git-email--extract-diff (patch-file)
  "Extract the diff from PATCH-FILE."
  (with-temp-buffer
    (insert-file-contents patch-file)
    (goto-char (point-min))
    (buffer-substring
     (- (re-search-forward "\n\n") 1)
     (point-max))))

(defun git-email--remove-subject (header)
  "Remove HEADER if it is the subject."
  (not (string-equal (car header) "Subject")))

;; See https://emacs.stackexchange.com/a/5408
(defun git-email--fontify-diff (text)
  "Fontify TEXT as diffs in `message-mode'."
  (with-temp-buffer
    (erase-buffer)
    (insert text)
    (delay-mode-hooks (diff-mode))
    (font-lock-default-function #'diff-mode)
    (font-lock-default-fontify-region (point-min)
                                      (point-max)
                                      nil)
    (buffer-string)))

;;;###autoload (autoload 'git-email-magit-patch-send "lazy-magit" nil t)
(defun git-email-magit-patch-send (range args files )
  "Send a set of patches via email."
  ;; This is largely copied from magit-patch's `magit-patch-create'
  ;; function.
  (interactive
   (if (not (eq transient-current-command 'magit-patch-create))
       (list nil nil nil nil)
     (cons (if-let ((revs (magit-region-values 'commit)))
               (if (length= revs 1)
                   (list "-1" (car revs))
                 (concat (car (last revs)) "^.." (car revs)))
             (let ((range (magit-read-range-or-commit
                           "Format range or commit")))
               (if (string-search ".." range)
                   range
                 (format "%s~..%s" range range))))
           (let ((args (transient-args 'magit-patch-create)))
             (list (-filter #'stringp args)
                   (cdr (assoc "--" args)))))))
  (let ((files (nreverse
                (split-string
                 (magit--with-temp-process-buffer
                   (let* ((status (magit-process-git t "format-patch" range args "--" files))
                          (output (buffer-string)))
                     output))))))
    (git-email--send-files files)
    (mapc #'delete-file files)))


(defun git-email--fontify-using-faces (text)
  "Fontify TEXT using faces."
  (let ((pos 0)
        next)
    (while (setq next (next-single-property-change pos 'face text))
      (put-text-property pos next 'font-lock-face
                         (get-text-property pos 'face text) text)
      (setq pos next))
    (add-text-properties 0  (length text) '(fontified t) text)
    text))
(provide 'lazy-magit)
