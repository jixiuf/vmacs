;;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require  'vc)
  (require  'vc-dir))

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
    (if current-prefix-arg
        (vc-git--pushpull "push" nil '("--force"))
      (vc-git--pushpull "push" nil '("--force-with-lease")))))

;;;###autoload
(defun vc-push-other(&optional args)
  (interactive)
  (let* ((br (vc-git-current-branch))
         (branch (vc-read-revision
                  (if current-prefix-arg "Push(--force) : "
                    "Push : ")
                  nil nil (car br)))
         (remote-br (vc-read-revision "To: " nil nil (nth 1 br)))
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

;;;###autoload
(defun vc-git-delete ()
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
(defun vc-pull-default(&optional args  _upstream)
  (interactive)
  (if (git-svn-repos-p)
      (vc-git--out-ok  "svn" "rebase" args)
    (if current-prefix-arg
        (vc-git--pushpull "fetch" nil '("--all" "--tags"))
      (vc-git--pushpull "pull" nil '("--rebase" "--stat")))))
;;;###autoload
(defun vc-git-fetch-all()
  (interactive)
  (vc-git--pushpull "fetch" nil '("--all")))
;;;###autoload
(defun vc-git-fetch-tags()
  (interactive)
  (vc-git--pushpull "fetch" nil '("--tags")))
(defun vc-git-cmd (verb fn)
  (let* ((fileset-arg (vc-deduce-fileset nil t))
         (backend (car fileset-arg))
         (files (nth 1 fileset-arg)))
    (if (eq backend 'Git)
        (progn (funcall fn files)
               (message (concat verb " " (number-to-string (length files))
                                " file(s).")))
      (message "Not in a vc git buffer."))))

;;;###autoload
(defun vc-git-stage ()
  (interactive)
  (vc-git-cmd "Staged" 'vc-git-register))

;;;###autoload
(defun vc-git-unstage ()
  (interactive)
  (vc-git-cmd
   "Unstaged"
   (lambda (files) (vc-git-command nil 0 files "reset" "-q" "--"))))

;;;###autoload
(defun vc-git-reset (&optional args)
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
(defun vc-git-revert-commit (&optional args)
  (interactive "P")
  (let* ((commit (log-view-current-tag (point))))
    (when (and commit
               (y-or-n-p (format
                          "Do you really want to revert commit %s"
                          commit)))
      (vc-git-command nil 0 nil "revert" commit))))

;;;###autoload
(defun vc-git-rebase-i (&optional args)
  (interactive "P")
  (let* ((root (vc-git-root default-directory))
         (commit (log-view-current-tag (point)))
	     (buffer (format "*vc-git : %s*" (expand-file-name root))))
    (when commit
      (vc-git-command buffer 'async nil
                      "rebase" "-i"  commit)))
  (revert-buffer))
;;;###autoload
(defun vc-git-continue ()
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
(defun vc-git-skip ()
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
(defun vc-git-abort ()
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
(defun vc-git-cherry-pick-commit (&optional args)
  (interactive "P")
  (let* ((commit (log-view-current-tag (point))))
    (if commit
        (vc-git-command nil 0 nil "cherry-pick" commit)
      (message "should run in log-view-mode"))))

;;;###autoload
(defun vc-git-am-apply-patches (&optional files )
  "Apply the patches FILES."
  (interactive (list (expand-file-name
                      (read-file-name "Apply patch: "))))
  (vc-git-command nil 0 nil "am"  "--" files))

;;;###autoload
(defun vc-git-apply-plain-patches (&optional files )
  "Apply the patches FILES."
  (interactive (list (expand-file-name
                      (read-file-name "Apply plain patch: "))))
  (vc-git-command nil 0 nil "apply"  "--" files))


(defun vc-git-rebase ()
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

;; fork from vc-git-dir-extra-headers
(defun vc-git-current-branch ()
  "return current local branch and remote tracking-branch"
  (let ((str (vc-git--out-str "symbolic-ref" "HEAD"))
	    branch  remote  tracking-branch remote-branch)
    (when(string-match "^\\(refs/heads/\\)?\\(.+\\)$" str)
	  (setq branch (match-string 2 str))
      (setq remote (vc-git--out-str
                    "config" (concat "branch." branch ".remote")))
      (let ((merge (vc-git--out-str
                    "config" (concat "branch." branch ".merge"))))
        (when (string-match "\\([^\n]+\\)" remote)
	      (setq remote (match-string 1 remote)))
        (when (string-match "^\\(refs/heads/\\)?\\(.+\\)$" merge)
          (setq tracking-branch (match-string 2 merge)))
        (pcase remote
          ("." (setq remote ""))
          ((pred (not string-empty-p))
           (setq remote-branch (concat remote "/" tracking-branch))))))
    (list branch remote-branch remote tracking-branch)))

;;;###autoload
(defun vc-git-print-remote-branch ()
  (interactive)
  (let ((branch (vc-git-current-branch)))
    (vc-print-branch-log (cadr branch))))

;;;###autoload
(defun vc-git-log-outgoing-sync (buffer remote-location)
  (vc-setup-buffer buffer)
  (apply #'vc-git-command buffer 0 nil
         `("log"
           "--no-color" "--graph" "--decorate" "--date=short"
           ,(format "--pretty=tformat:%s" (car vc-git-root-log-format))
           "--abbrev-commit"
           ,@(ensure-list vc-git-shortlog-switches)
           ,(concat (if (string= remote-location "")
	                "@{upstream}"
	              remote-location)
	                "..HEAD"))))
(defun vc-git-log-incoming-sync (buffer remote-location)
  (vc-setup-buffer buffer)
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
		               remote-location)))))


;;;###autoload
(defun vc-switch-project()
  (interactive)
  (require 'project)
  (vc-dir (funcall project-prompter)))

(defun vc-blob-successor (rev file)
  "Find next rev of `REV' for file.

return the rev and filepath of file."
  (let ((lines (split-string
                (vc-git--out-str
                 "log" "--format=%h" "--name-only"
                 "--follow" "HEAD" "--" file)
                "\n" t )))
    (catch 'found
      (while lines
        (if (and rev (not (string-empty-p rev))
                 (string-prefix-p rev (nth 2 lines) ))
            (throw 'found (list (nth 0 lines) (nth 1 lines)))
          (setq lines (nthcdr 2 lines)))))))
(defun vc-blob-ancestor (rev file)
  (let ((lines (split-string
                (vc-git--out-str
                 "log" "-2" "--format=%h" "--name-only"
                 "--follow" (or rev "HEAD") "--" file)
                "\n" t )))
    (if rev (cddr lines) (butlast lines 2))))

;;;###autoload
(defun vc-git-next-revision ()
  "Visit the next blob which modified the current file."
  (interactive)
  (let ((prev-buffer (current-buffer))
        (filename buffer-file-name)
        (dir default-directory)
        rev next)
    (when (string-match "^\\(.+\\)\\.~\\([^~]+\\)~$" filename)
      (setq rev (match-string 2 filename))
      (setq filename (match-string 1 filename))
      (setq next (vc-blob-successor rev filename))
      (kill-buffer prev-buffer)
      (if next
          (switch-to-buffer
           (vc-find-revision
            (expand-file-name (cadr next) (vc-git-root dir))
            (car next)))
        (find-file filename)
        (user-error "magit timemachine: You have reached the end of time")))))
;;;###autoload
(defun vc-git-prev-revision ()
  "Visit the previous blob which modified the current file."
  (interactive)
  (let ((prev-buffer (current-buffer))
        (filename buffer-file-name)
        (dir default-directory)
        rev next)
    (when (string-match "^\\([^~]+?\\)\\(?:\\.~\\([^~]+\\)~\\)?$" filename)
      (setq rev (match-string 2 filename))
      (setq filename (match-string 1 filename))
      (setq next (vc-blob-ancestor rev filename))
      (if rev (kill-buffer prev-buffer)
        (save-buffer))
      (if next
          (switch-to-buffer
           (vc-find-revision
            (expand-file-name (cadr next) (vc-git-root dir))
            (car next)))
        (find-file filename)
        (user-error "magit timemachine: You have reached the beginning of time")))))

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
(define-minor-mode vc-git-log-view-minor-mode
  "Git-Log-View-Minor"
  :keymap nil
  (require 'add-log) ;; We need the faces add-log.
  ;; Don't have file markers, so use impossible regexp.
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
;;   (let* ((branch (vc-git-current-branch))
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
;;   (let* ((branch (vc-git-current-branch))
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
;;   (let* ((branch (vc-git-current-branch))
;;          (remote (cadr branch)))
;;     (if remote
;;         (car (vc-rev-diff-count (car branch) remote))
;;         -1)))


(provide 'lazy-version-control)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-vc.el ends here.
