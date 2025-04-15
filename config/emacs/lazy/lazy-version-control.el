;;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require  'vc)
  (require 'vcgit)
  (require 'project)
  (require  'vc-git)
  (require  'vc-dir))

;;;###autoload
(defun vcgit-push-other()
  (interactive)
  (let* ((br (vc-git--current-branch))
         (branch (vc-read-revision
                  (if current-prefix-arg "Push(--force) : "
                    "Push : ")
                  nil nil br))
         (remote-br (vc-read-revision "To: " nil nil
                                      (vcgit--tracking-branch br)
                                      (concat (vcgit--branch-remote branch) "/")))
         remote)
    (when (string-match "^\\(.+?\\)/\\(.+\\)$" remote-br)
      (setq remote (match-string 1 remote-br))
      (setq remote-br (match-string 2 remote-br))
      (if current-prefix-arg
          (vc-git--pushpull "push" nil
                            `("--force"
                              ,remote ,(format "%s:%s" branch remote-br)))
        (vc-git--pushpull "push" nil
                          `("--force-with-lease"
                            ,remote ,(format "%s:%s" branch remote-br)))))))
(defun vcgit-push-tags ()
  (interactive)
  (vc-git--pushpull "push" nil '("--tags")))


;;;###autoload
(defun vcgit-git-delete ()
  "Delete tag branch and remote branch."
  (interactive)
  (let* ((refs (split-string (vc-git--out-str
                              "for-each-ref"
                              "--format=%(refname)")
                             "\n" t))
         (filtered-refs (seq-filter
                         (lambda (ref)
                           (string-match-p
                            "^refs/\\(heads\\|remotes\\|tags\\)/"
                            ref))
                         refs))
         (choices (mapcar (lambda (ref)
                            (replace-regexp-in-string
                             "^refs/\\(heads/\\|remotes/\\)?"
                             "" ref))
                          filtered-refs))
         (selected (completing-read "Delete(branch or tag): " choices)))
    (when (and selected (not (string-empty-p selected)))
      (let* ((full-ref (car (seq-filter (lambda (ref)
                                          (string-suffix-p selected ref))
                                        filtered-refs)))
             (command
              (cond
               ((string-prefix-p "refs/heads/" full-ref)
                (format "git branch -D %s" selected))
               ((string-prefix-p "refs/remotes/" full-ref)
                (let* ((remote-branch (replace-regexp-in-string "^refs/remotes/" "" full-ref))
                       (remote (car (split-string remote-branch "/")))
                       (branch (replace-regexp-in-string "^[^/]+/" "" remote-branch)))
                  (format "git push %s :%s" remote branch)))
               ((string-prefix-p "refs/tags/" full-ref)
                (let* ((remote-refs (seq-filter
                                     (lambda (ref)
                                       (string-prefix-p "refs/remotes/" ref))
                                     filtered-refs))
                       (remotes (delete-dups
                                 (mapcar (lambda (ref)
                                           (car (split-string
                                                 (replace-regexp-in-string
                                                  "^refs/remotes/" "" ref)
                                                 "/")))
                                         remote-refs)))
                       (tag (replace-regexp-in-string "^tags/" "" selected))
                       (push-commands (mapcar (lambda (remote)
                                                (format "git push %s :refs/%s" remote selected))
                                              remotes)))
                  (format "git tag -d %s && %s" tag (string-join push-commands " && ")))))))
        (let ((display-buffer-alist
               '((t (display-buffer-no-window )))))
          (async-shell-command command (messages-buffer) (messages-buffer)))))))

;;;###autoload
(defun vcgit-pull-default(&optional args  _upstream)
  (interactive)
  (if (vcgit-svn-repos-p)
      (vc-git--out-ok  "svn" "rebase" args)
    (if current-prefix-arg
        (vc-git--pushpull "fetch" nil '("--all" "--tags"))
      (vc-git--pushpull "pull" nil '("--rebase" "--stat")))))
(defun vcgit-svn-repos-p()
  (let ((topdir (vc-root-dir)))
    (when topdir (file-exists-p (expand-file-name ".git/refs/remotes/git-svn" topdir)))))


;;;###autoload
(defun vcgit-fetch-all()
  (interactive)
  (vc-git--pushpull "fetch" nil '("--all")))
;;;###autoload
(defun vcgit-fetch-tags()
  (interactive)
  (vc-git--pushpull "fetch" nil '("--tags")))
(defun vcgit-cmd (verb fn)
  (let* ((fileset-arg (vc-deduce-fileset nil t))
         (backend (car fileset-arg))
         (files (nth 1 fileset-arg)))
    (if (eq backend 'Git)
        (progn (funcall fn files)
               (message (concat verb " " (number-to-string (length files))
                                " file(s).")))
      (message "Not in a vc git buffer."))))

;;;###autoload
(defun vcgit-stage ()
  (interactive)
  (vcgit-cmd "Staged" 'vc-git-register))

;;;###autoload
(defun vcgit-unstage ()
  (interactive)
  (vcgit-cmd
   "Unstaged"
   (lambda (files) (vc-git-command nil 0 files "reset" "-q" "--"))))

;;;###autoload
(defun vcgit-reset (&optional args)
  (interactive "P")
  (let ((commit (log-view-current-tag (point))))
    (when (and commit
               (y-or-n-p (format
                          "Do you really want to reset commit %s"
                          commit)))
      (if args
          (vc-git-command nil 0 nil "reset" commit "--hard")
        (vc-git-command nil 0 nil "reset" commit ))
      (revert-buffer))))

;;;###autoload
(defun vcgit-revert-commit ()
  (interactive )
  (let* ((commit (log-view-current-tag (point))))
    (when (and commit
               (y-or-n-p (format
                          "Do you really want to revert commit %s"
                          commit)))
      (vc-git-command nil 0 nil "revert" commit))))

;;;###autoload
(defun vcgit-rebase-i ()
  (interactive)
  (let* ((root (vc-git-root default-directory))
         (commit (log-view-current-tag (point)))
	     (buffer (format "*vc-git : %s*" (expand-file-name root))))
    (when commit
      (vc-git-command buffer 'async nil
                      "rebase" "-i"  (concat commit "^1"))))
  (revert-buffer))
;;;###autoload
(defun vcgit-continue ()
  (interactive)
  (let ((gitdir (vc-git--git-path)))
    ;; See contrib/completion/git-prompt.sh in git.git.
    (when (or (file-directory-p
	           (expand-file-name "rebase-merge" gitdir))
	          (file-exists-p
	           (expand-file-name "rebase-apply/rebasing" gitdir)))
      (vc-git-command nil 'async nil "rebase" "--continue"))
    (when (file-exists-p
	       (expand-file-name "rebase-apply/applying" gitdir))
      (vc-git-command nil 'async nil "am" "--continue"))
    (when (file-exists-p (expand-file-name "MERGE_HEAD" gitdir))
      (vc-git-command nil 'async nil "merge" "--continue"))
    (when (file-exists-p (expand-file-name "REVERT_HEAD" gitdir))
      (vc-git-command nil 'async nil "revert" "--continue"))
    (when (file-exists-p (expand-file-name "CHERRY_PICK_HEAD" gitdir))
      (vc-git-command nil 'async nil "cherry-pick" "--continue"))))
;;;###autoload
(defun vcgit-skip ()
  (interactive)
  (let ((gitdir (vc-git--git-path)))
    ;; See contrib/completion/git-prompt.sh in git.git.
    (when (or (file-directory-p
	           (expand-file-name "rebase-merge" gitdir))
	          (file-exists-p
	           (expand-file-name "rebase-apply/rebasing" gitdir)))
      (vc-git-command nil 'async nil "rebase" "--skip"))
    (when (file-exists-p
	       (expand-file-name "rebase-apply/applying" gitdir))
      (vc-git-command nil 'async nil "am" "--skip"))
    (when (file-exists-p (expand-file-name "MERGE_HEAD" gitdir))
      (vc-git-command nil 'async nil "merge" "--skip"))
    (when (file-exists-p (expand-file-name "REVERT_HEAD" gitdir))
      (vc-git-command nil 'async nil "revert" "--skip"))
    (when (file-exists-p (expand-file-name "CHERRY_PICK_HEAD" gitdir))
      (vc-git-command nil 'async nil "cherry-pick" "--skip"))))

;;;###autoload
(defun vcgit-abort ()
  (interactive)
  (let ((gitdir (vc-git--git-path)))
    ;; See contrib/completion/git-prompt.sh in git.git.
    (when (or (file-directory-p
	           (expand-file-name "rebase-merge" gitdir))
	          (file-exists-p
	           (expand-file-name "rebase-apply/rebasing" gitdir)))
      (vc-git-command nil 'async nil "rebase" "--abort"))
    (when (file-exists-p
	       (expand-file-name "rebase-apply/applying" gitdir))
      (vc-git-command nil 'async nil "am" "--abort"))
    (when (file-exists-p (expand-file-name "MERGE_HEAD" gitdir))
      (vc-git-command nil 'async nil "merge" "--abort"))
    (when (file-exists-p (expand-file-name "REVERT_HEAD" gitdir))
      (vc-git-command nil 'async nil "revert" "--abort"))
    (when (file-exists-p (expand-file-name "CHERRY_PICK_HEAD" gitdir))
      (vc-git-command nil 'async nil "cherry-pick" "--abort"))))

;;;###autoload
(defun vcgit-cherry-pick-commit ()
  (interactive )
  (let* ((commit (log-view-current-tag (point))))
    (if commit
        (vc-git-command nil 0 nil "cherry-pick" commit)
      (message "should run in log-view-mode"))))

;;;###autoload
(defun vcgit-am-apply-patches (&optional files )
  "Apply the patches FILES."
  (interactive (list (expand-file-name
                      (read-file-name "Apply patch: "))))
  (vc-git-command nil 0 nil "am"  "--" files))

;;;###autoload
(defun vcgit-apply-plain-patches (&optional files )
  "Apply the patches FILES."
  (interactive (list (expand-file-name
                      (read-file-name "Apply plain patch: "))))
  (vc-git-command nil 0 nil "apply"  "--" files))


(defun vcgit-rebase ()
  "Rebase changes into the current Git branch.
This prompts for a branch to merge from."
  (interactive)
  (let* ((root (vc-git-root default-directory))
	 (buffer (format "*vc-git : %s*" (expand-file-name root)))
	 (branches (cdr (vc-git-branches)))
	 (merge-source
	  (completing-read "Rebase from branch: "
			   (if (or (member "FETCH_HEAD" branches)
				   (not (file-readable-p
                                         (vc-git--git-path "FETCH_HEAD"))))
			       branches
			     (cons "FETCH_HEAD" branches))
			   nil t)))
    (apply #'vc-do-async-command buffer root vc-git-program "rebase"
	   (list merge-source))
    (with-current-buffer buffer (vc-run-delayed (vc-compilation-mode 'git)))
    (vc-set-async-update buffer)))

;;;###autoload
(defun vc-switch-project()
  (interactive)
  (require 'project)
  (vc-dir (funcall project-prompter)))

;;;###autoload
(defun vcgit-next-revision ()
  "Visit the next blob which modified the current file."
  (interactive)
  (let* ((buffname (buffer-name))
         (prev-buffer (current-buffer))
         (parent-buffer vc-parent-buffer)
         (fileset-arg (vc-deduce-fileset nil t))
         (backend (car fileset-arg))
         (filename (car (cadr fileset-arg)))
         rev next)
    (when parent-buffer
      (if (bound-and-true-p vc-buffer-revision)
          (setq rev vc-buffer-revision)
        (when (string-match "^\\([^~]+?\\)\\(?:\\.~\\([^~]+\\)~\\)?$" buffname)
          (setq rev (match-string 2 buffname))))
      (setq next (vc-call-backend backend 'next-revision
                                  filename rev))
      (kill-buffer prev-buffer)
      (if next
          (switch-to-buffer (vc-find-revision filename next))
        (find-file filename)
        (user-error "vcgit timemachine: You have reached the end of time")))))
;;;###autoload
(defun vcgit-prev-revision ()
  "Visit the prev blob which modified the current file."
  (interactive)
  (let* ((buffname (buffer-name))
         (cur-buffer (current-buffer))
         (parent-buffer vc-parent-buffer)
         (fileset-arg (vc-deduce-fileset nil t))
         (backend (car fileset-arg))
         (filename (car (cadr fileset-arg)))
         rev prev)
    (if (bound-and-true-p vc-buffer-revision)
        (setq rev vc-buffer-revision)
      (when (string-match "^\\([^~]+?\\)\\(?:\\.~\\([^~]+\\)~\\)?$" buffname)
        (setq rev (match-string 2 buffname))))
    (if rev
        (setq prev (vc-call-backend backend 'previous-revision
                                    filename rev))
      (setq prev (vc-working-revision filename)))
    (if prev
        (progn  (switch-to-buffer (vc-find-revision filename prev))
                (when parent-buffer (kill-buffer cur-buffer)))
      (user-error "vcgit timemachine: You have reached the beginning of time"))))

;;;###autoload
(defun vc-remember-project()
  (let ((filename (vc-root-dir))
        dir)
    (when filename
      (setq dir (abbreviate-file-name
                 (file-truename filename)))
      (unless (file-remote-p dir)
        (project-remember-project (project-current))))))
(defvar log-view-per-file-logs t)

;;;###autoload
(defun vcgit-reflog ()
  "Show git reflog in a new buffer with ANSI colors and custom keybindings."
  (interactive)
  (let* ((buffer (get-buffer-create "*vcgit-reflog*")))
	(with-current-buffer buffer
	  (let ((inhibit-read-only t))
	    (erase-buffer)
	    (goto-char (point-min))
	    (ansi-color-apply-on-region (point-min) (point-max)))
	  (setq buffer-read-only t)
	  (setq mode-name "Git-Reflog")
	  (setq major-mode 'special-mode))
    (pop-to-buffer buffer)))


;;;###autoload
(defun vcgit-print-remote-branch ()
  (interactive)
  (let ((branch (vcgit--tracking-branch)))
    (vc-print-branch-log branch)))
;; got from https://www.rahuljuliato.com/posts/vc-git-functions
(defun vc-diff-on-current-hunk ()
  "Show the diff for the current file and jump to the hunk containing the current line."
  (interactive)
  (let ((current-line (line-number-at-pos)))
	(message "Current line in file: %d" current-line)
	(vc-diff) ; Generate the diff buffer
	(with-current-buffer "*vc-diff*"
	  (goto-char (point-min))
	  (let ((found-hunk nil))
		(while (and (not found-hunk)
					(re-search-forward "^@@ -\\([0-9]+\\), *[0-9]+ \\+\\([0-9]+\\), *\\([0-9]+\\) @@" nil t))
		  (let* ((start-line (string-to-number (match-string 2)))
				   (line-count (string-to-number (match-string 3)))
				   (end-line (+ start-line line-count)))
			(message "Found hunk: %d to %d" start-line end-line)
			(when (and (>= current-line start-line)
						 (<= current-line end-line))
			  (message "Current line %d is within hunk range %d to %d" current-line start-line end-line)
			  (setq found-hunk t)
			  (goto-char (match-beginning 0))
              (recenter 2))))
		(unless found-hunk
		  (message "Current line %d is not within any hunk range." current-line)
		  (goto-char (point-min)))))))
(defun log-view-kill-revision ()
  "Append to `kill-ring' log-view revision at or around point.

When the log-view is in the short format (one compact line per
revision), the revision is the one on the current line.  If the
revision is expanded with `log-view-expanded-log-entry-function'
and point is somewhere inside the expanded text, the revision is
still the same.

When the log-view is in the long format (detailed view where each
revision spans several lines), the revision is the one pertinent
to the text at point."
  (interactive)
  (when-let ((revision (cadr (log-view-current-entry (point) t))))
    (kill-new (format "%s" revision))
    (message "Copied: %s" revision)))


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

;;;;;###autoload
;; (defun vc-git-print-log-unpulled ()
;;   (interactive)
;;   (let* ((branch (vcgit-current-branch))
;;          (remote (cadr branch))
;;          cnt vc-log-show-limit)
;;     (vc-git-command nil 0 nil "fetch" (nth 2 branch))
;;     (setq cnt (cadr (vc-rev-diff-count (car branch) remote)))
;;     (setq vc-log-show-limit cnt)
;;     (message "%d commits unpulled to: %s" cnt (cadr branch))
;;     (unless (zerop cnt)
;;       (vc-print-branch-log (cadr branch)))))

;;;;;###autoload
;; (defun vc-git-print-log-unpushed ()
;;   (interactive)
;;   (let* ((branch (vcgit-current-branch))
;;          (cnt (car (vc-rev-diff-count (car branch) (cadr branch))))
;;          (vc-log-show-limit cnt))
;;     (message "%d commits unpushed to: %s" cnt (cadr branch))
;;     (unless (zerop cnt)
;;       (vc-print-branch-log (car branch) ))))


;;;;;###autoload
;; (defun vmacs-vc-next-action()
;;   (interactive)
;;   (when (and (eq major-mode 'vc-dir-mode)
;;              (not (vc-dir-marked-files)))
;;     (vc-dir-mark-all-files nil))
;;   (call-interactively 'vc-next-action)
;;   (let* ((vc-fileset (vc-deduce-fileset nil t 'state-model-only-files))
;;          (state (nth 3 vc-fileset)))
;;     (when (and (or (eq state 'up-to-date) (not state))
;;                (not (zerop (vc-get-unpushed-count))))
;;       (call-interactively 'vc-push-default))))

;; (defun vc-rev-diff-count (a b &optional)
;;   "Return the commits in A but not B and vice versa.
;; Return a list of two integers: (A>B B>A).
;; "
;;   (mapcar #'string-to-number
;;           (split-string (vc-git--out-str "rev-list"
;;                                          "--count" "--left-right"
;;                                          ;; (and first-parent "--first-parent")
;;                                          (concat a "..." b))
;;                         "\t")))

;; (defun vc-get-unpushed-count()
;;   "从git status OUTPUT中提取领先的提交数"
;;   (let* ((branch (vcgit-current-branch))
;;          (remote (cadr branch)))
;;     (if remote
;;         (car (vc-rev-diff-count (car branch) remote))
;;         -1)))

;;;###autoload
;; (defun vc-push-default(&optional args  )
;;   (interactive)
;;   (if (vcgit-svn-repos-p)
;;       (vc-git--out-ok  "svn" "dcommit" args)
;;     (if current-prefix-arg
;;         (vc-git--pushpull "push" nil '("--force"))
;;       (vc-git--pushpull "push" nil '("--force-with-lease")))))

;; (defun vc-wait-for-processes (&optional proc timeout)
;;   "Wait until PROCS have completed execution.
;; If TIMEOUT is non-nil, wait at most that many seconds.  Return non-nil
;; if process finished executing before the timeout expired."
;;   (let ((expiration (when timeout (time-add (current-time) timeout))))
;;     (catch 'timeout
;;       (while (process-live-p proc)
;;         (when (input-pending-p)
;;           (discard-input))
;;         (when (and expiration
;;                    (not (time-less-p (current-time) expiration)))
;;           (throw 'timeout nil))
;;         (sit-for 0.05))
;;       t)))

;; ;;;###autoload
;; (defun vcgit-log-outgoing (buffer &optional remote-location)
;;   (when (vcgit--tracking-branch)
;;     (vc-log-outgoing (or remote-location ""))
;;     (with-current-buffer buffer
;;       (vc-wait-for-processes (get-buffer-process buffer))
;;       (buffer-string))))
;; ;;;###autoload
;; (defun vcgit-log-header (func header limit buf)
;;   (let ((inhibit-redisplay t)
;;         (display-buffer-alist
;;          '((t (display-buffer-no-window )))))
;;     (save-window-excursion
;;       (funcall func buf))
;;     (with-current-buffer buf
;;       (if (= (point-max)(point-min))
;;           ""
;;         (concat (propertize  (format"%s(%d):\n" header
;;                                     (count-lines (point-min)
;;                                                  (point-max)))
;;                              'face 'vc-dir-header)
;;                 (propertize (buffer-substring (point-min)
;;                                               (save-excursion
;;                                                 (goto-char (point-min))
;;                                                 (if limit
;;                                                     (forward-line limit)
;;                                                   (goto-char (point-max)))
;;                                                 (point)))
;;                             'keymap log-view-mode-map)
;;                 "\n")))))


(provide 'lazy-version-control)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-vc.el ends here.
