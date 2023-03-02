(provide 'custom-file)
;;下面的值是通过Emacs的custom 系统关于外观的设置,如无必要不要手动修改

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
 '(auth-sources
   '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc" macos-keychain-internet macos-keychain-generic))
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/cache/backup_files/" t)))
 '(auto-save-visited-interval 5)
 '(auto-save-visited-mode t)
 '(backup-directory-alist '((".*" . "~/.emacs.d/cache/backup_files/")))
 '(blink-cursor-mode t)
 '(bookmark-default-file "~/.emacs.d/cache/bookmarks")
 '(centaur-tab-background-color "#202020")
 '(column-number-mode nil)
 '(custom-group-tag-faces '(default))
 '(custom-safe-themes t)
 '(display-fill-column-indicator t)
 '(electric-pair-mode t)
 '(flycheck-check-syntax-automatically '(save mode-enabled))
 '(flycheck-idle-change-delay 1)
 '(global-auto-revert-mode t)
 '(helm-minibuffer-history-key "C-r")
 '(ivy-posframe-border-width 10)
 '(lsp-auto-guess-root t)
 '(lsp-enable-symbol-highlighting nil)
 '(lsp-restart 'auto-restart)
 '(magit-commit-ask-to-stage t)
 '(magit-no-confirm
   '(reverse rename abort-merge resect-bisect kill-process stage-all-changes unstage-all-changes))
 '(magit-save-repository-buffers 'dontask)
 '(modus-themes-scale-1 1.2)
 '(modus-themes-scale-2 1.3)
 '(modus-themes-scale-3 1.4)
 '(modus-themes-scale-4 1.5)
 '(modus-themes-scale-headings t)
 '(modus-themes-scale-title 1.5)
 '(package-selected-packages
   '(corfu cape consult-dir dired-filetype-face embark embark-consult evil-collection exec-path-from-shell git-link golden-ratio golden-ratio-scroll-screen iedit kind-icon lua-mode magit marginalia markdown-mode mu4e-alert orderless org-msg osx-dictionary ox-gfm pinyinlib protobuf-mode verb vterm vterm-toggle vundo wgrep with-editor yasnippet))
 '(proced-enable-color-flag t)
 '(proced-format 'long)
 '(recentf-save-file "~/.emacs.d/cache/recentf")
 '(safe-local-variable-values
   '((diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (checkdoc-minor-mode . t)
     (flycheck-disabled-checkers emacs-lisp-checkdoc)
     (git-commit-major-mode . git-commit-elisp-text-mode)
     (projectile-project-run-cmd . "mkdir -p build; cd build; cmake ..; make run")
     (projectile-project-compilation-cmd . "mkdir -p build; cd build; cmake ..; make")
     (projectile-project-compilation-cmd . "bear make")
     (compile-command . "make lint")
     (projectile-project-run-cmd . "make run")))
 '(save-place-file "~/.emacs.d/cache/place")
 '(savehist-file "~/.emacs.d/cache/history")
 '(scroll-bar-mode nil)
 '(tramp-persistency-file-name "~/.emacs.d/cache/tramp")
 '(tramp-syntax 'default nil (tramp))
 '(transient-history-file "~/.emacs.d/cache/transient/history.el")
 '(transient-levels-file "~/.emacs.d/cache/transient/levels.el")
 '(transient-values-file "~/.emacs.d/cache/transient/values.el")
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(warning-suppress-log-types '((with-editor)))
 '(warning-suppress-types
   '((comp)
     (initialization)
     (eglot)
     (yasnippet backquote-change)))
 '(window-divider-default-bottom-width 1)
 '(window-divider-default-places 'bottom-only)
 '(window-divider-default-right-width 1)
 '(window-divider-mode t)
 '(xref-after-jump-hook '(recenter)))
