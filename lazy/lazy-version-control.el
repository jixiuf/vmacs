;;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require  'vc)
  (require  'ediff)
  (require 'dash)
  (require 'ediff-vers)
  (require  'vc-dir))

(declare-function ediff-vc-internal "ediff-vers")


;; c-xvl列出当前文件的历史版本
;; 此函数可以对各个历史版本进行比较
;; 使用方法在你在比较的两个版本中分别用m标记一下
;; 然后调用此函数即可
;;;###autoload
(defun log-view-ediff (beg end)
  "the ediff version of `log-view-diff'"
  (interactive
   (list (if mark-active (region-beginning) (point))
         (if mark-active (region-end) (point))))
  (let ((marked-entities (log-view-get-marked)) pos1 pos2)
    (when (= (length marked-entities) 2)
      (setq pos1 (progn (log-view-goto-rev (car marked-entities) ) (point) ))
      (setq pos2 (progn (log-view-goto-rev (nth 1 marked-entities) ) (point)))
      (setq beg  (if (< pos1 pos2 ) pos1 pos2))
      (setq end  (if (> pos1 pos2 ) pos1 pos2))
      ))
  (let ((fr (log-view-current-tag beg))
        (to (log-view-current-tag end)))
    (when (string-equal fr to)
      (save-excursion
        (goto-char end)
        (log-view-msg-next)
        (setq to (log-view-current-tag))))
    (require 'ediff-vers)
    (ediff-vc-internal to fr)))

;;;###autoload
(defun vc-command ()
  "run vc command"
  (interactive)
  (let* ((backend vc-dir-backend)
         (files (vc-dir-marked-files))
         (backend-cmd (symbol-value  (intern (concat "vc-" (downcase (symbol-name backend)) "-program"))))
         (readed-sub-cmd  (concat  (read-shell-command (concat "run " backend-cmd " command:") (concat backend-cmd " ")) ))
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


(require 'magit)
(defun magit-svn-repos-p(&optional dir)
  (let ((topdir (magit-toplevel (or dir default-directory))))
    (when topdir (file-exists-p (expand-file-name ".git/refs/remotes/git-svn" topdir)))))

;;;###autoload
(defun vmacs-magit-push-default(&optional args  _upstream)
  (interactive)
  (if (magit-svn-repos-p)
      (magit-run-git-async "svn" "dcommit" args)
      (call-interactively 'magit-push-current-to-pushremote)))
;;;###autoload
(defun vmacs-magit-pull-default(&optional args  _upstream)
  (interactive)
  (if (magit-svn-repos-p)
      (magit-run-git-async "svn" "rebase" args)
    (call-interactively 'magit-pull-from-pushremote)))

;;;###autoload
(defun vmacs-vc-next-action()
  (interactive)
  (call-interactively 'vc-next-action)
  (let* ((vc-fileset (vc-deduce-fileset nil t 'state-model-only-files))
         (state (nth 3 vc-fileset)))
    (when (and (eq state 'up-to-date)
               (not (zerop (vmacs-magit-get-unpushed-count))))
      (call-interactively 'vmacs-magit-push-default))))

(defun vmacs-magit-get-unpushed-count()
  (--when-let (magit-get-push-branch)
    (car (magit-rev-diff-count "HEAD" it))))

(provide 'lazy-version-control)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-vc.el ends here.
