;;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require  'ediff)
  (require 'magit))
;;;###autoload
(defun toggle-diff-whitespace()
  (interactive)
  (cond
   ((equal vc-git-diff-switches '("--ignore-space-at-eol" "--ignore-blank-lines" "--ignore-space-change"))
      (setq vc-git-diff-switches '("--ignore-space-at-eol" "--ignore-blank-lines" "--ignore-space-change" "--ignore-all-space")))
   ((equal vc-git-diff-switches '("--ignore-space-at-eol" "--ignore-blank-lines" "--ignore-space-change" "--ignore-all-space"))
    (setq vc-git-diff-switches t))
   (t
    (setq vc-git-diff-switches '("--ignore-space-at-eol" "--ignore-blank-lines" "--ignore-space-change"))))

  (cond
   ((equal magit-buffer-diff-args '("--ignore-space-at-eol" "--ignore-blank-lines" "--ignore-space-change" "--no-ext-diff"))
    (setq magit-buffer-diff-args '("--ignore-space-at-eol" "--ignore-blank-lines" "--ignore-space-change" "--ignore-all-space" "--no-ext-diff")))
   ((equal magit-buffer-diff-args '("--ignore-space-at-eol" "--ignore-blank-lines" "--ignore-space-change" "--ignore-all-space" "--no-ext-diff"))
    (setq magit-buffer-diff-args '("--no-ext-diff")))
   (t
    (setq magit-buffer-diff-args '("--ignore-space-at-eol" "--ignore-blank-lines" "--ignore-space-change" "--no-ext-diff"))))


  (if  (and (boundp 'vc-svn-diff-switches)(equal vc-svn-diff-switches t))
      (setq-default vc-svn-diff-switches '("-x --ignore-eol-style"  ))
    (setq-default vc-svn-diff-switches t))
  (cond
   ((equal major-mode 'diff-mode)
    (revert-buffer))
   ((equal major-mode 'magit-status-mode)
    (magit-refresh)))
  (message "toggle diff show whitespace "))
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



(provide 'lazy-magit)
