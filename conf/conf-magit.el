(eval-when-compile
  (require 'magit)
  (require 'evil-magit))

(setq-default evil-magit-use-y-for-yank t)
(setq-default
 ;; slow ,if t
 magit-diff-refine-hunk nil  ;'all, This is super useful when only a single identifier/word is changed all over the place
 magit-diff-show-diffstat nil
 magit-diff-highlight-hunk-body nil
 magit-diff-section-arguments '("--ignore-space-at-eol" "--ignore-blank-lines" "--no-ext-diff") ;do not set this ;use  toggle-diff-whitespace-eol to toggle
 magit-section-highlight-hook nil       ;不必hightlight,光标移动的时候，默认会显示当前section区域
 magit-section-unhighlight-hook nil)                         ;

(define-key magit-mode-map (kbd "C-w") nil)
(define-key magit-mode-map (kbd "M-w") 'magit-copy-section-value)
(define-key magit-mode-map "," '(lambda() (interactive)(magit-status-internal (magit-read-repository nil))))


(define-key magit-mode-map "p" 'magit-push-popup)
(define-key magit-mode-map "P" 'magit-section-backward)
(magit-define-popup-action 'magit-push-popup ?p "Push Upstream or svn dcommit" 'vmacs-magit-push-default)
(magit-define-popup-action 'magit-fetch-popup ?f "Pull or svn rebase" 'vmacs-magit-pull-default)
(magit-define-popup-action 'magit-rebase-popup ?g "Refresh" 'magit-refresh)
(magit-define-popup-action 'magit-rebase-popup ?r "Refresh" 'magit-refresh)

(require 'evil-magit)
;;(setq evil-window-map  ctl-w-map)
(evil-magit-define-key evil-magit-state 'magit-mode-map
                       "C-w"  evil-window-map)

(evil-magit-define-key evil-magit-state 'magit-mode-map
                       "gw" 'toggle-diff-whitespace-eol)


;; (define-key magit-mode-map "\s" nil) ;space
;; (define-key magit-diff-mode-map "\s" nil) ;space
;; (define-key magit-diff-mode-map "j" nil)

;; (require 'magit-svn)

(defun magit-mode-hook-fun()
  ;; (magit-svn-mode)
  ;; (require 'magit-backup)
  ;; (magit-backup-mode -1)
  ;; (magit-auto-revert-mode -1)
  (let ((dir (magit-git-dir)))
    (when (file-remote-p dir)
      (add-to-list 'magit-repository-directories (expand-file-name ".." )))))

;; (eval-after-load 'git-commit-mode '(setq git-commit-setup-hook (delete 'git-commit-turn-on-flyspell git-commit-setup-hook)))
(add-hook 'magit-mode-hook 'magit-mode-hook-fun)


(unless magit-repository-directories
  (setq magit-repository-directories (list user-emacs-directory)))

(provide 'conf-magit)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-magit.el ends here
