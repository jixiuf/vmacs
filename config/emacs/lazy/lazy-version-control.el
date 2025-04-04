;;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require  'vc)
  (require 'project)
  (require  'vc-git)
  (require  'vc-dir))

(unless (fboundp 'vc-git--current-branch)
  (defun vc-git--out-match (args regexp group)
    "Run `git ARGS...' and return match for group number GROUP of REGEXP.
Return nil if the output does not match.  The exit status is ignored."
    (let ((out (apply #'vc-git--out-str args)))
      (when (string-match regexp out)
        (match-string group out))))
  (defun vc-git--current-branch ()
    (vc-git--out-match '("symbolic-ref" "HEAD")
                       "^\\(refs/heads/\\)?\\(.+\\)$" 2)))

(defun vc-git--branch-remote (&optional branch)
  "Return the remote name that the given BRANCH is tracking.
If BRANCH is not provided, use the current branch.
If the branch is tracking a local branch, return `.'."
  (let ((br (or branch (vc-git--current-branch))))
    (vc-git--out-match
     `("config" ,(concat "branch." br ".remote"))
     "\\([^\n]+\\)" 1)))

(defun vc-git--branch-merge (&optional branch)
  "Return the remote branch name that the given BRANCH is merging into.
If BRANCH is not provided, use the current branch."
  (let ((br (or branch (vc-git--current-branch))))
    (vc-git--out-match
     `("config" ,(concat "branch." br ".merge"))
     "^\\(refs/heads/\\)?\\(.+\\)$" 2)))

(defun vc-git--tracking-branch (&optional branch remote-name)
  "Return the full tracking branch name for the given BRANCH.
If BRANCH is not provided, use the current branch.
If REMOTE-NAME is provided, use it instead of fetching the remote name.
If the branch is not tracking a remote branch, return nil."
  (when-let* ((br (or branch (vc-git--current-branch)))
              (branch-merge (vc-git--branch-merge br))
              (branch-remote (vc-git--branch-remote br)))
    (unless (string= branch-remote ".")
      (concat branch-remote "/" branch-merge))))

;;;###autoload
(defun vcgit-push-other()
  (interactive)
  (let* ((br (vc-git--current-branch))
         (branch (vc-read-revision
                  (if current-prefix-arg "Push(--force) : "
                    "Push : ")
                  nil nil br))
         (remote-br (vc-read-revision "To: " nil nil
                                      (vc-git--tracking-branch br)))
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
  "删除 Git 分支、远程分支或标签。"
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
                      "rebase" "-i"  commit)))
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
      ;; FIXME: support other bankend for find rev?
      (when (string-match "^\\([^~]+?\\)\\(?:\\.~\\([^~]+\\)~\\)?$" buffname)
        (setq rev (match-string 2 buffname)))
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
    ;; FIXME: support other bankend for find rev?
    (when (string-match "^\\([^~]+?\\)\\(?:\\.~\\([^~]+\\)~\\)?$" buffname)
      (setq rev (match-string 2 buffname)))
    (if rev
        (setq prev (vc-call-backend backend 'previous-revision
                                    filename rev))
      (setq prev (vc-short-revision filename)))
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

;; copy from vc-git-log-view-mode
;;;###autoload
(define-minor-mode vcgit-log-view-minor-mode
  ""
  :keymap:nil
  (setq-local log-view-file-re regexp-unmatchable)
  (setq-local log-view-per-file-logs nil)
  (setq-local log-view-message-re
              (if (not (memq vc-log-view-type '(long log-search with-diff)))
                  (cadr vc-git-root-log-format)
                "^commit +\\([0-9a-z]+\\)"))
  ;; Allow expanding short log entries.
  (when (memq vc-log-view-type '(short log-outgoing log-incoming mergebase))
    (setq truncate-lines t)
    (setq-local log-view-expanded-log-entry-function
                'vc-git-expanded-log-entry))
  (setq-local log-view-font-lock-keywords
       (if (not (memq vc-log-view-type '(long log-search with-diff)))
	   (list (cons (nth 1 vc-git-root-log-format)
		       (nth 2 vc-git-root-log-format)))
	 (append
	  `((,log-view-message-re (1 'change-log-acknowledgment)))
	  ;; Handle the case:
	  ;; user: foo@bar
	  '(("^\\(?:Author\\|Commit\\):[ \t]+\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)"
	     (1 'change-log-email))
	    ;; Handle the case:
	    ;; user: FirstName LastName <foo@bar>
	    ("^\\(?:Author\\|Commit\\):[ \t]+\\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]"
	     (1 'change-log-name)
	     (2 'change-log-email))
	    ("^ +\\(?:\\(?:[Aa]cked\\|[Ss]igned-[Oo]ff\\)-[Bb]y:\\)[ \t]+\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)"
	     (1 'change-log-name))
	    ("^ +\\(?:\\(?:[Aa]cked\\|[Ss]igned-[Oo]ff\\)-[Bb]y:\\)[ \t]+\\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]"
	     (1 'change-log-name)
	     (2 'change-log-email))
	    ("^Merge: \\([0-9a-z]+\\) \\([0-9a-z]+\\)"
	     (1 'change-log-acknowledgment)
	     (2 'change-log-acknowledgment))
	    ("^\\(?:Date:   \\|AuthorDate: \\|CommitDate: \\)\\(.+\\)" (1 'change-log-date))
	    ("^summary:[ \t]+\\(.+\\)" (1 'log-view-message)))))))

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
  (let ((branch (vc-git--tracking-branch)))
    (vc-print-branch-log branch)))
(defun vcgit-header ( func header limit)
  (with-temp-buffer
    (funcall func (current-buffer) "")
    ;; (vcgit-log-outgoing-sync (current-buffer) "")
    (let ((msg ""))
      (unless (= (point-max)(point-min))
        (setq msg (concat msg (propertize  (format"%s(%d):\n" header
                                                  (count-lines (point-min)
                                                               (point-max)))
                                           'face 'vc-dir-header)))
        (setq msg (concat msg (propertize (buffer-substring (point-min)
                                                            (save-excursion
                                                              (goto-char (point-min))
                                                              (if limit
                                                                  (forward-line limit)
                                                                (goto-char (point-max)))
                                                              (point)))
                                          'keymap log-view-mode-map)
                          "\n")))

      msg)))
;;;###autoload
(defun vcgit-log-outgoing-sync (buffer remote-location)
  (vc-setup-buffer buffer)
  (when (vc-git--tracking-branch)
      (apply #'vc-git-command buffer 0 nil
             `("log"
               "--no-color" "--graph" "--decorate" "--date=short"
               ,(format "--pretty=tformat:%s" (car vc-git-root-log-format))
               "--abbrev-commit"
               ,@(ensure-list vc-git-shortlog-switches)
               ,(concat (if (string= remote-location "")
	                        "@{upstream}"
	                      remote-location)
	                    "..HEAD")))))

(defun vcgit-log-incoming-sync (buffer remote-location)
  (vc-setup-buffer buffer)
  (when (vc-git--tracking-branch)
    (vc-git-command nil 'async nil "fetch"
                    (unless (string= remote-location "")
                      ;; `remote-location' is in format "repository/branch",
                      ;; so remove everything except a repository name.
                      (replace-regexp-in-string
                       "/.*" "" remote-location)))
    (apply #'vc-git-command buffer 0 nil
           `("log"
             "--no-color" "--graph" "--decorate" "--date=short"
             ,(format "--pretty=tformat:%s" (car vc-git-root-log-format))
             "--abbrev-commit"
             ,@(ensure-list vc-git-shortlog-switches)
             ,(concat "HEAD.." (if (string= remote-location "")
			                       "@{upstream}"
		                         remote-location))))))

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

(provide 'lazy-version-control)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-vc.el ends here.
