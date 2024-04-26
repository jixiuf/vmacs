(provide 'custom-file)
;;下面的值是通过 Emacs 的 custom 系统关于外观的设置,如无必要不要手动修改

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-line-tab-current ((t (:inherit bold :background "chartreuse4" :box (:line-width (1 . -2) :color "#000000" :style pressed-button))))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "~/.cache/emacs/backup_files/" t)))
 '(auto-save-visited-interval 5)
 '(auto-save-visited-mode t)
 '(backup-directory-alist '((".*" . "~/.cache/emacs/backup_files/")))
 '(blink-cursor-mode t)
 '(bookmark-default-file "~/.cache/emacs/bookmarks")
 '(column-number-mode nil)
 '(custom-group-tag-faces '(default))
 '(custom-safe-themes t)
 '(display-fill-column-indicator t)
 '(electric-pair-mode t)
 '(global-auto-revert-mode t)
 '(ignored-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil (shell-command (concat markdown-command " README.md > README.html"))) nil
           'local)
     (eval add-hook 'after-save-hook (lambda nil (shell-command "pkill --signal RTMIN+13 waybar"))
           nil 'local)
     (eval add-hook 'after-save-hook (lambda nil (shell-command "pkill --signal RTMIN+11 waybar"))
           nil 'local)
     (eval add-hook 'before-save-hook 'time-stamp)
     (eval add-hook (make-local-variable 'after-save-hook) 'bh/ensure-in-vc-or-check-in t)))
 '(magit-commit-ask-to-stage t)
 '(magit-log-margin '(t "%y-%m-%d %H:%M " magit-log-margin-width t 6))
 '(magit-no-confirm
   '(reverse rename abort-merge resect-bisect kill-process stage-all-changes unstage-all-changes))
 '(magit-save-repository-buffers 'dontask)
 '(magit-status-margin '(t "%y-%m-%d %H:%M " magit-log-margin-width t 6))
 '(modus-themes-scale-1 1.2)
 '(modus-themes-scale-2 1.3)
 '(modus-themes-scale-3 1.4)
 '(modus-themes-scale-4 1.5)
 '(modus-themes-scale-headings t)
 '(modus-themes-scale-title 1.5)
 '(package-selected-packages
   '(async cape clipetty consult-dir corfu corfu-terminal dired-filetype-face eglot emamux embark
           embark-consult exec-path-from-shell fanyi git-link golden-ratio-scroll-screen goto-chg
           kind-icon lua-mode magit marginalia markdown-mode mu4e-alert orderless org-ai org-msg
           ox-gfm pinyinlib protobuf-mode verb vundo wgrep with-editor yasnippet))
 '(proced-enable-color-flag t)
 '(proced-format 'long)
 '(recentf-save-file "~/.cache/emacs/recentf")
 '(safe-local-variable-values
   '((eval add-hook (make-local-variable 'after-save-hook)
           #'(lambda nil (shell-command "systemctl --user restart waybar" nil nil)) t)
     (eval add-hook (make-local-variable 'after-save-hook)
           #'(lambda nil (shell-command "gpg -d authorized_keys.gpg>authorized_keys")) t)
     (eval add-hook (make-local-variable 'after-save-hook)
           #'(lambda nil (shell-command "gpg -d config.gpg>config")) t)
     (eval add-hook (make-local-variable 'after-save-hook)
           #'(lambda nil (shell-command "systemctl --user restart xremap")) t)
     (eval add-hook (make-local-variable 'after-save-hook) #'(lambda nil (shell-command "make")) t)
     (diff-add-log-use-relative-names . t) (vc-git-annotate-switches . "-w")
     (checkdoc-minor-mode . t) (flycheck-disabled-checkers emacs-lisp-checkdoc)
     (git-commit-major-mode . git-commit-elisp-text-mode)))
 '(save-place-file "~/.cache/emacs/place")
 '(savehist-file "~/.cache/emacs/history")
 '(scroll-bar-mode nil)
 '(tramp-persistency-file-name "~/.cache/emacs/tramp")
 '(tramp-syntax 'default nil (tramp))
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(warning-suppress-log-types '((comp) (emacs) (with-editor)))
 '(warning-suppress-types
   '((comp) (initialization) (eglot) (yasnippet backquote-change)))
 '(window-divider-default-bottom-width 1)
 '(window-divider-default-places 'bottom-only)
 '(window-divider-default-right-width 1)
 '(window-divider-mode t)
 '(xref-after-jump-hook '(recenter)))
