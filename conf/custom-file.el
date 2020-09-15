(provide 'custom-file)
;;下面的值是通过Emacs的custom 系统关于外观的设置,如无必要不要手动修改

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/cache/backup_files/" t)))
 '(auto-save-visited-interval 15)
 '(auto-save-visited-mode t)
 '(backup-directory-alist '((".*" . "~/.emacs.d/cache/backup_files/")))
 '(blink-cursor-mode t)
 '(bookmark-default-file "~/.emacs.d/cache/bookmarks")
 '(centaur-tab-background-color "#202020")
 '(column-number-mode nil)
 '(custom-group-tag-faces '(default))
 '(custom-safe-themes
   '("d8e2234814aee232b5970bfab4da14da62e91876e7500c18ce875eb6cb58ea3f" "3190c0cfdbe5f356f9dca98ca9d5f97425bbf348de0442e68667b904dab990c4"))
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
 '(package-selected-packages
   '(modus-vivendi-theme projectile dap-mode posframe orderless rg magit-libgit docker-tramp ox-gfm forge clang-format centaur-tabs lsp-python-ms ccls org-re-reveal htmlize evil-magit git-link evil elisp-def dockerfile-mode json-mode iedit osx-dictionary dired-narrow magit gitconfig-mode dired-filetype-face auto-compile golden-ratio-scroll-screen company evil-textobj-anyblock exec-path-from-shell bm ethan-wspace git-commit go-mode golden-ratio goto-chg lua-mode markdown-mode protobuf-mode thrift web-mode wgrep with-editor yaml-mode yasnippet))
 '(recentf-save-file "~/.emacs.d/cache/recentf")
 '(safe-local-variable-values
   '((checkdoc-minor-mode . t)
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
 '(warning-suppress-types '((yasnippet backquote-change)))
 '(window-divider-default-bottom-width 1)
 '(window-divider-default-places 'bottom-only)
 '(window-divider-default-right-width 1)
 '(window-divider-mode t)
 '(xref-after-jump-hook '(recenter)))
