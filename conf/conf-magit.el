(setq magit-git-executable (executable-find "git"))
(setq-default
 magit-status-margin '(t age magit-log-margin-width t 10) ;magit-status 中的Recent commits列表有没有办法增加作者列
 magit-commit-show-diff nil
 ;; slow ,if t
 magit-diff-refine-hunk nil  ;'all, This is super useful when only a single identifier/word is changed all over the place
 magit-diff-highlight-hunk-body nil
 magit-log-arguments  '("-n256" "--graph" "--decorate" "--follow") ;加了--follow ,rename的log也能看到
 magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
 magit-diff-section-arguments '("--ignore-space-at-eol" "--ignore-blank-lines" "--no-ext-diff") ;do not set this ;use  toggle-diff-whitespace to toggle
 magit-section-highlight-hook nil       ;不必hightlight,光标移动的时候，默认会显示当前section区域
 magit-section-unhighlight-hook nil)                         ;
(define-key magit-mode-map (kbd "M-w") 'magit-copy-section-value)
(defun vmacs-magit-status-list()
  (interactive)
  (let (list)
    (dolist (ele magit-repository-directories)
      (when (file-exists-p (car ele))
        (add-to-list 'list ele)))
    (setq magit-repository-directories list))
  (magit-status-internal (magit-read-repository)))

(define-key magit-mode-map "," #'vmacs-magit-status-list)

(remove-hook 'server-switch-hook 'magit-commit-diff)


;; (define-key magit-mode-map "q" 'magit-mode-bury-buffer)
(define-key magit-mode-map "d" 'magit-section-toggle)
(define-key magit-mode-map (kbd "TAB") 'magit-diff)
(define-key magit-mode-map [(tab)]  'magit-diff)
(transient-suffix-put 'magit-diff "d" :key (kbd "<tab>"))



(define-key magit-mode-map "e" nil)     ;disable ediff
;; (define-key magit-mode-map "v" 'magit-push-popup)
(define-key magit-mode-map "P" 'magit-section-backward)
;; (transient-append-suffix 'magit-push ?v 'magit-push-current-to-pushremote)

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

;; (transient-append-suffix 'magit-fetch "f"
;;      '("f" "Full from pushremote" magit-pull))

;; (require 'evil-magit)
(transient-insert-suffix 'magit-revert "o" '("_" "Revert no commit" magit-revert-no-commit))



(transient-suffix-put 'magit-push "p" :key "v")
(transient-suffix-put 'magit-rebase "p" :key "r")
;; (magit-define-popup-action 'magit-fetch-popup ?f "Pull or svn rebase" 'vmacs-magit-pull-default)
;; (magit-define-popup-action 'magit-rebase-popup ?g "Refresh" 'magit-refresh)
;; (magit-define-popup-action 'magit-rebase-popup ?r "Refresh" 'magit-refresh)
(define-key transient-map        "q" 'transient-quit-one)
(define-key transient-edit-map   "q" 'transient-quit-one)
(define-key transient-sticky-map "q" 'transient-quit-seq)
(evil-collection-define-key 'normal 'magit-mode-map
  "v"  'magit-push
       "C-w"  evil-window-map
       "gw" 'toggle-diff-whitespace
       "gm"  'magit-toggle-margin)


(defun vmacs-magit-mode-hook()
  ;; (require 'magit-backup)
  ;; (magit-backup-mode -1)
  ;; (magit-auto-revert-mode -1)
  ;; https://magit.vc/manual/magit/Wip-Modes.html
  (magit-wip-mode 1)                    ; magit-wip-log
  ;; brew install git-delta
  (let ((dir (abbreviate-file-name (file-truename (directory-file-name (magit-toplevel))))))
    (unless (file-remote-p dir)
      (delete (cons dir 0) magit-repository-directories)
      (add-to-list 'magit-repository-directories  (cons dir 0)))))

(add-hook 'magit-mode-hook 'vmacs-magit-mode-hook)

;; (add-hook 'magit-post-refresh-hook 'vmacs-update-repo-revision)


(defadvice magit-blob-next (around kill-all-blob-after-quit activate)
  "kill last viewed buffer"
  (let ((prev-buffer (current-buffer)))
    ad-do-it
    (kill-buffer prev-buffer)
    (unless magit-buffer-file-name
      (user-error "magit timemachine: You have reached the end of time"))))

(defadvice magit-blob-previous (around kill-all-blob-after-quit activate)
  "kill last viewed buffer"
  (let ((prev-buffer (current-buffer)))
    ad-do-it
    (unless (equal magit-buffer-file-name (buffer-file-name prev-buffer))
      (kill-buffer prev-buffer))))

(vmacs-leader (kbd "vm") 'vmacs-magit-blob-toggle) ;类似于time machine
(define-key magit-blob-mode-map (kbd "M-n") 'magit-blob-next)
(define-key magit-blob-mode-map (kbd "M-p") 'magit-blob-previous)
(define-key magit-blob-mode-map (kbd "C-c C-c") 'vmacs-magit-blob-save)
(define-key magit-blob-mode-map (kbd "s-w") 'vmacs-magit-blob-quit)
(global-set-key (kbd "M-p") 'magit-blob-previous)
(global-set-key (kbd "M-n") 'magit-blob-next)

(define-key magit-status-mode-map (kbd "s-w") 'vmacs-magit-kill-buffers)
(defun vmacs-magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (dolist (buf buffers)
      (unless  (get-buffer-process buf)
        (kill-buffer buf)))))

  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-assume-unchanged-files nil t)

(provide 'conf-magit)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-magit.el ends here
