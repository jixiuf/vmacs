(setq magit-git-executable (executable-find "git"))
(setq-default
 transient-default-level 5
 magit-commit-show-diff nil
 magit-show-long-lines-warning nil
 ;; slow ,if t
 magit-diff-refine-hunk 'all  ;'all, This is super useful when only a single identifier/word is changed all over the place
 magit-diff-highlight-hunk-body nil
 magit-section-keep-region-overlay nil
 magit-log-arguments  '("-n256" "--graph" "--decorate" "--follow") ;加了--follow ,rename 的 log 也能看到
 magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1
 magit-section-highlight-hook nil       ;不必 hightlight,光标移动的时候，默认会显示当前 section 区域
 magit-section-unhighlight-hook nil)
(define-key magit-mode-map (kbd "M-w") 'magit-copy-section-value)

(add-to-list 'magit-process-password-prompt-regexps "输入密码：")

;; recent commit always expand when i open magit status
(setf (alist-get 'unpushed magit-section-initial-visibility-alist) 'show)
(defun vmacs-magit-status-list()
  (interactive)
  (let (list)
    (dolist (ele magit-repository-directories)
      (when (file-exists-p (car ele))
        (add-to-list 'list ele)))
    (setq magit-repository-directories list))
  (magit-status (magit-read-repository)))

(define-key magit-mode-map "," #'vmacs-magit-status-list)

(remove-hook 'server-switch-hook 'magit-commit-diff)


;; (define-key magit-mode-map "q" 'magit-mode-bury-buffer)
;; 小指按tab 太疼，d 与tab 交换
(define-key magit-mode-map "d" 'magit-section-toggle)
(define-key magit-mode-map "e" 'magit-delete-thing)
;; (define-key magit-mode-map (kbd "C-c M/") nil)
;; (define-key magit-mode-map (kbd "C-c M/") nil)
(define-key magit-mode-map "`" 'magit-process-buffer)
(define-key magit-mode-map "." 'magit-log)
(transient-suffix-put 'magit-log "l" :key (kbd "."))
(define-key magit-mode-map (kbd "TAB") 'magit-diff)
(define-key magit-mode-map [(tab)]  'magit-diff)
(transient-suffix-put 'magit-diff "d" :key (kbd "<tab>"))

;; (define-key magit-mode-map "v" 'magit-push-popup)
(define-key magit-mode-map "P" 'magit-section-backward)
;; (transient-append-suffix 'magit-push ?v 'magit-push-current-to-pushremote)

;; 不想按shift ,将pull fetch 常用功能 放到f上
(setq magit-pull-or-fetch t)
(define-key magit-mode-map (kbd "f") 'magit-pull)
(define-key magit-mode-map (kbd "F") 'magit-fetch)
;; (transient-suffix-put 'magit-pull "p" :key "f")
(transient-remove-suffix 'magit-pull "f")
(transient-append-suffix 'magit-pull "u" '("f"  magit-pull-from-pushremote))

(transient-insert-suffix 'magit-pull "F" '("a"  "all remotes" magit-fetch-all))
;; (transient-insert-suffix 'magit-pull "F"
;;      '("e"  "elsewhere" magit-fetch-other))
(transient-insert-suffix 'magit-pull "F" '("p" magit-fetch-from-pushremote))
(transient-insert-suffix 'magit-pull "F" '("U" magit-fetch-from-upstream))

(with-eval-after-load 'magit-patch
  (transient-insert-suffix 'magit-patch "c" '("m" "email" vmacs-magit-send-email)))



;; 不想小指按p ,将push 常用功能 放到v上
(transient-suffix-put 'magit-push "p" :key "v")
(transient-suffix-put 'magit-rebase "p" :key "r")
(define-key transient-map        "q" 'transient-quit-one)
(define-key transient-edit-map   "q" 'transient-quit-one)
(define-key transient-sticky-map "q" 'transient-quit-seq)

(define-key magit-mode-map "v"  #'magit-push)
(define-key  magit-mode-map (kbd "C-c Gw") #'toggle-diff-whitespace) ;gw
(define-key  magit-mode-map (kbd "C-c Gr") #'magit-refresh)
(define-key  magit-mode-map (kbd "C-c Gm") #'magit-toggle-margin)
(define-key  magit-mode-map (kbd "C-c Mz") #'magit-stash)
(with-eval-after-load 'diff-mode
  (define-key  diff-mode-map (kbd "C-c Gw") #'toggle-diff-whitespace)) ;gw

(setq magit-todos-branch-list nil)
(defun vmacs-magit-mode-hook()
  ;; (require 'magit-backup)
  ;; (magit-backup-mode -1)
  ;; (magit-auto-revert-mode -1)
  ;; https://magit.vc/manual/magit/Wip-Modes.html
  (magit-wip-mode 1)                    ; magit-wip-log
  (magit-todos-mode)
  ;; brew install git-delta
  (let ((dir (abbreviate-file-name (file-truename (directory-file-name (magit-toplevel))))))
    (unless (file-remote-p dir)
      (delete (cons dir 0) magit-repository-directories)
      (add-to-list 'magit-repository-directories  (cons dir 0)))))

(add-hook 'magit-mode-hook 'vmacs-magit-mode-hook)

;; (add-hook 'magit-post-refresh-hook 'vmacs-update-repo-revision)


(define-advice magit-blob-next (:around (orig-fun &rest args) kill-all-blob-after-quit)
  "kill last viewed buffer"
  (let ((prev-buffer (current-buffer)))
    (apply orig-fun args)
    (kill-buffer prev-buffer)
    (unless magit-buffer-file-name
      (user-error "magit timemachine: You have reached the end of time"))))

(define-advice magit-blob-previous (:around (orig-fun &rest args) kill-all-blob-after-quit)
  "kill last viewed buffer"
  (let ((prev-buffer (current-buffer)))
    (apply orig-fun args)
    (unless (equal magit-buffer-file-name (buffer-file-name prev-buffer))
      (kill-buffer prev-buffer))))


;;在 magit-log-buffer-file 产生的 log buffer 中
;; return : 查看当前 commit 的 diff
;; 有选择区域 则查看区域内的 diff
;; C-u 则打开当前对应的版本的文件
(define-key magit-commit-section-map  [return] 'vmacs-magit-diff-range)
(define-key magit-commit-section-map  [C-return] 'magit-show-commit) ;old return

(vmacs-leader (kbd "vm") 'vmacs-magit-blob-toggle) ;类似于 time machine
(define-key magit-blob-mode-map (kbd "M-n") 'magit-blob-next)
(define-key magit-blob-mode-map (kbd "M-p") 'magit-blob-previous)
(define-key magit-blob-mode-map (kbd "C-c C-c") 'vmacs-magit-blob-save)
(define-key magit-blob-mode-map (kbd "s-w") 'vmacs-magit-blob-quit)
(define-key magit-blob-mode-map (kbd "s-C-w") 'vmacs-magit-blob-quit)
(global-set-key (kbd "M-p") 'magit-blob-previous)
(global-set-key (kbd "M-n") 'magit-blob-next)

(define-key magit-status-mode-map (kbd "s-w") 'vmacs-magit-kill-buffers)
(define-key magit-status-mode-map (kbd "s-C-w") 'vmacs-magit-kill-buffers)
(defun vmacs-magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (dolist (buf buffers)
      (unless  (get-buffer-process buf)
        (kill-buffer buf)))))

(magit-add-section-hook 'magit-status-sections-hook 'magit-insert-assume-unchanged-files nil t)
(remove-hook 'magit-status-sections-hook 'magit-insert-bisect-output)
(remove-hook 'magit-status-sections-hook 'magit-insert-bisect-rest)
(remove-hook 'magit-status-sections-hook 'magit-insert-bisect-log)



(with-eval-after-load 'git-link
  (defun git-link-gitlab (hostname dirname filename branch commit start end)
    (format "https://%s/%s/blob/%s/%s"
            hostname dirname
            (or branch commit)
            (concat filename
                    (when start
                      (concat "#"
                              (if end
                                  (format "L%s-%s" start end)
                                (format "L%s" start))))))))


(defun vmacs-magit-diff-visit-file (file &optional other-window)
  "From a diff visit the appropriate version of FILE.

C-u: Visit the worktree version of the appropriate file.  The location
of point inside the diff determines which file is being visited.
The visited version depends on what changes the diff is about.

Note that this command only works if point is inside a diff.
In other cases `magit-find-file' (which see) has to be used."
  (interactive (list (magit-diff--file-at-point t t) current-prefix-arg))
  (magit-diff-visit-file--internal file current-prefix-arg
                                   'pop-to-buffer-same-window))

(fset 'magit-diff-visit-file 'vmacs-magit-diff-visit-file)

(defun vmacs-magit-send-email(range)
  (interactive (list (magit-diff-read-range-or-commit "Create patches for range or commit"
                                                      nil current-prefix-arg)))
  (unless (string-search ".." range)
    (setq range    (format "%s~..%s" range range)))
  (let ((lines (magit-git-lines "format-patch" "--stdout" range)))
    (with-current-buffer
        (call-interactively #'mu4e-compose-new)
      (end-of-buffer)
      (insert "\n")
      (dolist (line lines)
        (insert line)
        (insert "\n"))
      (beginning-of-buffer)
      (end-of-line)
      )
    )
  )

(provide 'conf-magit)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-magit.el ends here
