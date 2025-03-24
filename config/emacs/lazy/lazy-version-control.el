;;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require  'vc)
  ;; (require  'ediff)
  (require 'dash)
  (require 'ediff-vers)
  (require  'vc-dir))

;; (declare-function ediff-vc-internal "ediff-vers")


;; c-xvl列出当前文件的历史版本
;; 此函数可以对各个历史版本进行比较
;; 使用方法在你在比较的两个版本中分别用m标记一下
;; 然后调用此函数即可
;;;###autoload
;; (defun log-view-ediff (beg end)
;;   "the ediff version of `log-view-diff'"
;;   (interactive
;;    (list (if mark-active (region-beginning) (point))
;;          (if mark-active (region-end) (point))))
;;   (let ((marked-entities (log-view-get-marked)) pos1 pos2)
;;     (when (= (length marked-entities) 2)
;;       (setq pos1 (progn (log-view-goto-rev (car marked-entities) ) (point) ))
;;       (setq pos2 (progn (log-view-goto-rev (nth 1 marked-entities) ) (point)))
;;       (setq beg  (if (< pos1 pos2 ) pos1 pos2))
;;       (setq end  (if (> pos1 pos2 ) pos1 pos2))
;;       ))
;;   (let ((fr (log-view-current-tag beg))
;;         (to (log-view-current-tag end)))
;;     (when (string-equal fr to)
;;       (save-excursion
;;         (goto-char end)
;;         (log-view-msg-next)
;;         (setq to (log-view-current-tag))))
;;     (require 'ediff-vers)
;;     (ediff-vc-internal to fr)))

;;;###autoload
(defun vc-command (&optional cmd)
  "run vc command"
  (interactive)
  (let* ((backend vc-dir-backend)
         (files (vc-dir-marked-files))
         (backend-cmd (symbol-value  (intern (concat "vc-" (downcase (symbol-name backend)) "-program"))))
         (readed-sub-cmd  (or cmd (read-shell-command (concat "run " backend-cmd " command:")
                                                      (concat backend-cmd " "))))
         (params-list (append  (split-string-and-unquote readed-sub-cmd) files))
         (process-buf (concat "*vc-" backend-cmd "-command-out*"))
         process)
    (message "%s"  (prin1-to-string params-list))
    (when (bufferp process-buf) (kill-buffer process-buf))
    (setq process
          (apply 'start-process ;;
                 (buffer-name (current-buffer)) process-buf
                 (car params-list) (cdr params-list)))
    (set-process-sentinel
     process
     (lambda (proc change)
       (when (string-match "\\(finished\\|exited\\)" change)
         (let ((cur-buf (current-buffer) ))
           (set-buffer   (process-buffer proc))
           (setq major-mode 'vc-command-output-mode)
           (local-set-key "\C-g" 'kill-buffer-and-window)
           (local-set-key "g" 'kill-buffer-and-window)
           (local-set-key "q" 'kill-buffer-and-window)
           (set-buffer cur-buf)
           )
         (when (and (bufferp (get-buffer (process-name proc)))
                    (buffer-live-p (get-buffer (process-name proc))))
           (with-current-buffer (get-buffer (process-name proc))
             (vc-dir-unmark-all-files t)
             (revert-buffer t t t)
             ))
         (if (> (buffer-size (process-buffer proc)) 200)
             (progn
               (with-current-buffer (process-buffer proc)
                 (while (search-forward "\^M" nil t)
                   (replace-match "\n" nil t)))
               (switch-to-buffer-other-window (process-buffer proc) t))
           (message "%s " (with-current-buffer  (process-buffer proc) (buffer-string)))))))))


;; (require 'magit)
(defun git-svn-repos-p(&optional dir)
  (let ((topdir (vc-root-dir)))
    (when topdir (file-exists-p (expand-file-name ".git/refs/remotes/git-svn" topdir)))))

;;;###autoload
(defun vc-push-default(&optional args  _upstream)
  (interactive)
  (if (git-svn-repos-p)
      (vc-git--out-ok  "svn" "dcommit" args)
    (vc-git--pushpull "push" nil '("--force-with-lease"))
    ;; (vc-push args)
    ))

;;;###autoload
(defun vc-pull-default(&optional args  _upstream)
  (interactive)
  (if (git-svn-repos-p)
      (vc-git--out-ok  "svn" "rebase" args)
    (vc-git--pushpull "pull" nil '("--rebase"))
    ;; (call-interactively 'vc-pull)
    ))

;;;###autoload
(defun vmacs-vc-next-action()
  (interactive)
  (call-interactively 'vc-next-action)
  (let* ((vc-fileset (vc-deduce-fileset nil t 'state-model-only-files))
         (state (nth 3 vc-fileset)))
    (when (and (eq state 'up-to-date)
               (not (zerop (vc-get-unpushed-count))))
      (call-interactively 'vc-push-default))))

(defun vc-get-unpushed-count()
  "从git status OUTPUT中提取领先的提交数"
  (with-temp-buffer
    (insert (vc-git--run-command-string "status"))
    (goto-char (point-min))
    (if (or (save-excursion (re-search-forward "领先.*共[ \t]*\\([0-9]+\\)[ \t]*个提交" nil t))
            (re-search-forward "您的分支和 '.*' 出现了偏离，[ \t\n]*并且分别有 \\([0-9]+\\) 和 \\([0-9]+\\) 处不同的提交。" nil t)            )
        (string-to-number (match-string 1))
      0)))

;; (defun vc-git-current-branch()
;;   (string-trim
;;    (vc-git--run-command-string
;;     nil "symbolic-ref" "--short" "HEAD")))

;; In vc-git and vc-dir for git buffers, make (C-x v) a run git add, u run git
;; reset, and r run git reset and checkout from head.
(defun vmacs-vc-git-command (verb fn vc-fileset)
  (let* ((fileset-arg (or vc-fileset (vc-deduce-fileset nil t)))
         (backend (car fileset-arg))
         (files (nth 1 fileset-arg)))
    (if (eq backend 'Git)
        (progn (funcall fn files)
               (message (concat verb " " (number-to-string (length files))
                                " file(s).")))
      (message "Not in a vc git buffer."))))

;;;###autoload
(defun vc-git-stage (&optional revision vc-fileset comment)
  (interactive "P")
  (vmacs-vc-git-command "Staged" 'vc-git-register vc-fileset))

;;;###autoload
(defun vc-git-unstage (&optional revision vc-fileset comment)
  (interactive "P")
  (vmacs-vc-git-command "Unstaged"
                        (lambda (files) (vc-git-command nil 0 files "reset" "-q" "--"))
                        vc-fileset))

;;;###autoload
(defun vc-git-reset (&optional args)
  (interactive "P")
  (let ((commit (log-view-current-tag (point))))
    (if args
        (vc-git--out-str  "reset"  commit "--hard" )
      (vc-git--out-str  "reset"  commit)))
  (revert-buffer))

;;;###autoload
(defun vc-print-branch (branch &optional end)
  "Show the change log for BRANCH in another window.
The command prompts for the branch whose change log to show.
C-u prompts for the end of log to show"
  (interactive
   (let* ((backend (vc-responsible-backend default-directory))
          (rootdir (vc-call-backend backend 'root default-directory)))
     (list
      (vc-read-revision "Branch to log: " (list rootdir) backend)
      (if current-prefix-arg
          (setq end (vc-read-revision "Dnd to log: " (list rootdir) backend))))))
  (when (equal branch "")
    (error "No branch specified"))
  (let* ((backend (vc-responsible-backend default-directory))
         (rootdir (vc-call-backend backend 'root default-directory)))
    (vc-print-log-internal backend
                           (list rootdir) branch t
                           (if end end
                             (when (> vc-log-show-limit 0) vc-log-show-limit))
                           )))

(provide 'lazy-version-control)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-vc.el ends here.
