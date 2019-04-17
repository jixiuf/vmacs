(eval-when-compile
  (require 'magit)
  (require 'magit-push)
  (require 'evil-magit))

(setq-default evil-magit-use-y-for-yank t)
(setq-default
 magit-status-margin '(t age magit-log-margin-width t 18) ;magit-status 中的Recent commits列表有没有办法增加作者列
 ;; slow ,if t
 magit-diff-refine-hunk nil  ;'all, This is super useful when only a single identifier/word is changed all over the place
 magit-diff-show-diffstat nil
 magit-diff-highlight-hunk-body nil
 magit-log-arguments  '("-n256" "--graph" "--decorate" "--follow") ;加了--follow ,rename的log也能看到
 magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
 magit-diff-section-arguments '("--ignore-space-at-eol" "--ignore-blank-lines" "--no-ext-diff") ;do not set this ;use  toggle-diff-whitespace-eol to toggle
 magit-section-highlight-hook nil       ;不必hightlight,光标移动的时候，默认会显示当前section区域
 magit-section-unhighlight-hook nil)                         ;

(define-key magit-mode-map (kbd "C-w") nil)
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



;; (define-key magit-mode-map "q" 'magit-mode-bury-buffer)
(define-key magit-mode-map "d" 'magit-section-toggle)
(define-key magit-mode-map (kbd "TAB") 'magit-diff)
(define-key magit-mode-map [(tab)]  'magit-diff)
(transient-suffix-put 'magit-diff "d" :key (kbd "<tab>"))



;; (define-key magit-mode-map "v" 'magit-push-popup)
(define-key magit-mode-map "P" 'magit-section-backward)
;; (transient-append-suffix 'magit-push ?v 'magit-push-current-to-pushremote)

(setq magit-pull-or-fetch t)
(define-key magit-mode-map (kbd "f") 'magit-pull)
(define-key magit-mode-map (kbd "F") 'magit-fetch)
;; (transient-suffix-put 'magit-pull "p" :key "f")
(transient-remove-suffix 'magit-pull "f")
(transient-append-suffix 'magit-pull "u"
  '("f"  magit-pull-from-pushremote))

(transient-insert-suffix 'magit-pull "F"
     '("a"  "all remotes" magit-fetch-all))
;; (transient-insert-suffix 'magit-pull "F"
;;      '("e"  "elsewhere" magit-fetch-other))
(transient-insert-suffix 'magit-pull "F"
  '("p" magit-fetch-from-pushremote))
(transient-insert-suffix 'magit-pull "F"
  '("U" magit-fetch-from-upstream))

;; (transient-append-suffix 'magit-fetch "f"
;;      '("f" "Full from pushremote" magit-pull))



(transient-suffix-put 'magit-push "p" :key "v")
(transient-suffix-put 'magit-rebase "p" :key "r")
;; (magit-define-popup-action 'magit-fetch-popup ?f "Pull or svn rebase" 'vmacs-magit-pull-default)
;; (magit-define-popup-action 'magit-rebase-popup ?g "Refresh" 'magit-refresh)
;; (magit-define-popup-action 'magit-rebase-popup ?r "Refresh" 'magit-refresh)
(define-key transient-map        "q" 'transient-quit-one)
(define-key transient-edit-map   "q" 'transient-quit-one)
(define-key transient-sticky-map "q" 'transient-quit-seq)
(require 'evil-magit)
;;(setq evil-window-map  ctl-w-map)
(evil-magit-define-key evil-magit-state 'magit-mode-map
                       "v"  'magit-push)
(evil-magit-define-key evil-magit-state 'magit-mode-map
                       "C-w"  evil-window-map)

(evil-magit-define-key evil-magit-state 'magit-mode-map
                       "gw" 'toggle-diff-whitespace-eol)


;; (define-key magit-mode-map "\s" nil) ;space
;; (define-key magit-diff-mode-map "\s" nil) ;space
;; (define-key magit-diff-mode-map "j" nil)


(defun vmacs-magit-mode-hook()
  ;; (require 'magit-backup)
  ;; (magit-backup-mode -1)
  ;; (magit-auto-revert-mode -1)
  (define-key magit-status-mode-map "q" #'ignore)
  (let ((dir (magit-toplevel)))
    (unless (file-remote-p dir)
      (add-to-list 'magit-repository-directories (list dir 0)))))

(define-key magit-status-mode-map (kbd "s-w") 'vmacs-magit-kill-buffers)
(vmacs-leader-for-major-mode  'magit-status-mode "k" 'vmacs-magit-kill-buffers)
(vmacs-leader-for-major-mode  'magit-revision-mode "k" 'magit-mode-bury-buffer)
(vmacs-leader-for-major-mode  'magit-log-mode "k" 'magit-mode-bury-buffer)

(add-hook 'magit-mode-hook 'vmacs-magit-mode-hook)

(add-hook 'magit-post-refresh-hook 'vmacs-update-repo-revision)

(defun vmacs-magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (dolist (buf buffers)
      (unless  (get-buffer-process buf)
        (kill-buffer buf)))))



(provide 'conf-magit)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-magit.el ends here
