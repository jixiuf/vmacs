(provide 'custom-file)

(defface font-lock-todo-face nil
  "Font Lock mode face used to highlight TODO."
  :group 'font-lock-faces)
(defface font-lock-done-face nil
  "Font Lock mode face used to highlight DONE."

  :group 'font-lock-faces)
(dolist (mode '(c-mode c++-mode java-mode lisp-mode emacs-lisp-mode erlang-mode
                       go-mode
                       actionscript-mode lisp-interaction-mode sh-mode sgml-mode))
  (font-lock-add-keywords
   mode
   '(("\\<\\(FIXME\\|TODO\\|Todo\\|HACK\\|todo\\):" 1  'font-lock-todo-face prepend)
     ("@\\<\\(FIXME\\|TODO\\|Todo\\|HACK\\|todo\\)" 1  'font-lock-todo-face prepend)
     ("\\<\\(DONE\\|Done\\|done\\):" 1 'font-lock-done-face t)
     ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)
     )))

;; show some functions as keywords
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("\\<\\(quote\\|add-hook\\|equal\\)" .
                           font-lock-keyword-face)))
;; recognize some things as functions
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("\\<\\(autoload\\|setq-default\\|\\|setq-local\\|setq\\|add-hook\\||define-key\\|global-set-key\\)\\>" .
                           font-lock-function-name-face)))
;; recognize some things as constants
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("\\<\\(nil\\|\\t\\)\\_>" .
                           font-lock-constant-face)))

(when (and (equal system-type 'darwin) (window-system))
  (add-hook 'after-init-hook 'create-frame-font-mac))

(when (and (equal system-type 'windows-nt) (window-system))
  (add-hook 'after-init-hook 'create-frame-font-w32))

;; 禁用工具栏，滚运条 菜单栏
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(defun  emacs-daemon-after-make-frame-hook(&optional f) ;emacsclient 打开的窗口相关的设置
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (with-selected-frame f
    (when (window-system)
      (when (equal system-type 'darwin) (create-frame-font-mac))
      (when (equal system-type 'windows-nt) (create-frame-font-w32))
      (window-divider-mode t)
      (set-frame-position f 40 50)
      (set-frame-size f 138 50)
      (set-frame-parameter f 'alpha 85)
      (raise-frame))))

(add-hook 'after-make-frame-functions 'emacs-daemon-after-make-frame-hook)

(setq-default window-system-default-frame-alist ;直接emacs命令打开的窗口相关设置,不要在这里设置字体，否则daemon 启动时字体有可能没创建好，会导致字体设置失败
              '( (x ;; if frame created on x display
                  (alpha . 80)
                  (cursor-color . "green")
                  )
                 (mac ;; mac-port emacs
                  (alpha . 80)
                  (height . 50)
                  (width . 140)
                  (left . 160)
                  (top . 80)
                  (foreground-color . "#eeeeec")
                  (background-color . "#202020") ;;
                  (background-mode . dark)
                  )
                 (ns ;; if frame created on mac
                  ;; (alpha . 80)
                  ;; (height . 50)
                  ;; (width . 140)
                  ;; (left . 160)
                  ;; (top . 80)
                  (foreground-color . "#eeeeec")
                  (background-color . "#202020") ;;
                  (background-mode . dark)
                  )
                 (w32
                  (foreground-color . "#eeeeec")
                  (background-color . "#202020") ;;
                  (background-mode . dark)
                  (alpha . 95)
                  (cursor-color . "green")
                  (height . 30)
                  (width . 100)
                  (left . 200)
                  (top . 20)
                  )
                 (nil ;; if on term
                  (background-mode . dark)
                  )))

;;
;; (setq-default undo-tree-mode-lighter " Ü") ;undo
;; (setq-default helm-completion-mode-string " H")

;; (setq-default mode-line-cleaner-alist
;;               `((auto-complete-mode . " á")
;;                 (company-mode . "")
;;                 (yas-minor-mode . "")
;;                 (undo-tree-mode . "")
;;                 (golden-ratio-mode . "")
;;                 (flymake-mode . " Fly")
;;                 (ivy-mode . "")
;;                 ;; major mode
;;                 (dired-mode . " Dired")
;;                 (fundamental-mode . "Fd")
;;                 (ibuffer-mode . "iBuf")
;;                 (python-mode . "Py")
;;                 (lisp-interaction-mode . "iEL")
;;                 (emacs-lisp-mode . "EL")))

;; (defun clean-mode-line ()
;;   (interactive)
;;   (dolist (cleaner mode-line-cleaner-alist)
;;     (let* ((mode (car cleaner))
;;            (mode-str (cdr cleaner))
;;            (old-mode-str (cdr (assq mode minor-mode-alist))))
;;       (when old-mode-str
;;         (setcar old-mode-str mode-str))
;;       ;; major mode
;;       (when (eq mode major-mode)
;;         (setq mode-name mode-str)))))

;; (add-hook 'after-change-major-mode-hook 'clean-mode-line)
;; ;; 然后将mode-line 的face header调成0.1，变成一条线
(setq-default mode-line-format nil)


;;下面的值是通过Emacs的custom 系统关于外观的设置,如无必要不要手动修改

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-face ((t (:background "#272728"))))
 '(buffers-tab ((t (:background "#0C1021" :foreground "#F8F8F8"))))
 '(company-scrollbar-bg ((t (:background "RoyalBlue4"))))
 '(company-scrollbar-fg ((t (:background "green"))))
 '(company-template-field ((t (:background "green" :foreground "black"))))
 '(company-tooltip ((t (:foreground "green"))))
 '(company-tooltip-annotation ((t (:foreground "#FBDE2D"))))
 '(company-tooltip-common ((t (:foreground "#FBDE2D"))))
 '(company-tooltip-selection ((t (:background "dark slate gray"))))
 '(completions-common-part ((t (:inherit default :foreground "Cyan"))))
 '(completions-first-difference ((t (:background "black" :foreground "Gold2" :weight extra-bold :height 1.3))))
 '(cursor ((t (:background "tomato"))))
 '(custom-comment-tag ((t (:inherit default))))
 '(custom-face-tag ((t (:inherit default))))
 '(custom-group-tag ((t (:inherit variable-pitch :weight bold :height 1.2))))
 '(custom-variable-tag ((t (:inherit default :weight bold))))
 '(diff-added ((t (:foreground "OliveDrab1"))))
 '(diff-changed ((t (:foreground "yellow"))))
 '(diff-context ((t (:inherit default))))
 '(diff-file-header ((t (:foreground "tan1"))))
 '(diff-function ((t (:inherit diff-header :inverse-video t))))
 '(diff-header ((t (:foreground "light steel blue"))))
 '(diff-hunk-header ((t (:inherit diff-header :inverse-video t))))
 '(diff-index ((t (:foreground "Magenta"))))
 '(diff-nonexistent ((t (:foreground "yellow"))))
 '(diff-refine-added ((t (:background "gray26"))))
 '(diff-refine-changed ((t (:background "gray26"))))
 '(diff-refine-removed ((t (:background "gray26"))))
 '(diff-removed ((t (:inherit font-lock-comment-face :slant italic))))
 '(dired-directory ((t (:background "Blue4" :foreground "gray"))))
 '(ediff-current-diff-A ((t (:background "dark cyan"))))
 '(ediff-current-diff-Ancestor ((t (:background "dark red"))))
 '(ediff-current-diff-B ((t (:background "chocolate4"))))
 '(ediff-current-diff-C ((t (:background "sea green"))))
 '(ediff-even-diff-A ((t (:background "gray33"))))
 '(ediff-even-diff-Ancestor ((t (:background "gray40"))))
 '(ediff-even-diff-B ((t (:background "gray35"))))
 '(ediff-even-diff-C ((t (:background "gray49"))))
 '(ediff-fine-diff-A ((t (:background "cadet blue"))))
 '(ediff-fine-diff-Ancestor ((t (:background "sienna1"))))
 '(ediff-fine-diff-B ((t (:background "SlateGray4"))))
 '(ediff-fine-diff-C ((t (:background "saddle brown"))))
 '(ediff-odd-diff-A ((t (:background "gray49"))))
 '(ediff-odd-diff-B ((t (:background "gray50"))))
 '(ediff-odd-diff-C ((t (:background "gray30"))))
 '(erc-command-indicator-face ((t (:background "Purple" :weight bold))))
 '(erc-direct-msg-face ((t (:foreground "Yellow"))))
 '(erc-header-line ((t (:background "GreenYellow" :foreground "Gold"))))
 '(erc-input-face ((t (:foreground "Cyan2"))))
 '(erc-my-nick-face ((t (:foreground "Goldenrod" :weight bold))))
 '(erc-nick-default-face ((t (:foreground "Chartreuse" :weight bold))))
 '(erl-fdoc-name-face ((t (:foreground "green" :weight bold))))
 '(error ((t (:foreground "red" :weight bold))))
 '(flymake-errline ((t (:inherit error :foreground "red"))))
 '(font-lock-builtin-face ((t (:foreground "#F8F8F8"))))
 '(font-lock-comment-face ((t (:foreground "#AEAEAE"))))
 '(font-lock-constant-face ((t (:foreground "#D8FA3C"))))
 '(font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
 '(font-lock-done-face ((t (:foreground "Green" :box (:line-width 2 :color "grey75" :style released-button) :height 1.2))) t)
 '(font-lock-function-name-face ((t (:foreground "#FF6400"))))
 '(font-lock-keyword-face ((t (:foreground "#FBDE2D"))))
 '(font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
 '(font-lock-reference-face ((t (:foreground "SlateBlue"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "red"))))
 '(font-lock-string-face ((t (:foreground "light salmon"))))
 '(font-lock-todo-face ((t (:foreground "Red" :box (:line-width 2 :color "grey75" :style released-button) :height 1.2))) t)
 '(font-lock-type-face ((t (:foreground "#8DA6CE"))))
 '(font-lock-variable-name-face ((t (:foreground "#40E0D0"))))
 '(font-lock-warning-face ((t (:foreground "Pink"))))
 '(gui-element ((t (:background "#D4D0C8" :foreground "black"))))
 '(header-line ((t (:background "gray30" :distant-foreground "gray" :inverse-video nil))))
 '(helm-buffer-directory ((t (:background "Blue4" :foreground "gray"))))
 '(helm-ff-directory ((t (:background "Blue4" :foreground "gray"))))
 '(helm-grep-file ((t (:foreground "cyan1" :underline t))))
 '(helm-match ((t (:foreground "gold1"))))
 '(helm-selection ((t (:background "darkolivegreen" :underline t))))
 '(helm-source-header ((t (:background "gray46" :foreground "yellow" :weight bold :height 1.3 :family "Sans Serif"))))
 '(helm-visible-mark ((t (:background "gray43" :foreground "orange1"))))
 '(highlight ((t (:background "darkolivegreen"))))
 '(highline-face ((t (:background "SeaGreen"))))
 '(hl-paren-face ((t (:overline t :underline t :weight extra-bold))) t)
 '(isearch ((t (:background "seashell4" :foreground "green1"))))
 '(ivy-current-match ((t (:background "cyan4"))))
 '(ivy-minibuffer-match-face-1 ((t (:inherit comment))))
 '(ivy-minibuffer-match-face-2 ((t (:foreground "green" :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:foreground "green" :weight bold))))
 '(ivy-minibuffer-match-face-4 ((t (:foreground "green" :weight bold))))
 '(lazy-highlight ((t (:background "ivory4"))))
 '(line-number ((t (:foreground "DarkGoldenrod2"))))
 '(line-number-current-line ((t (:foreground "green"))))
 '(link ((t (:foreground "cyan" :underline t))))
 '(linum ((t (:inherit (shadow default) :foreground "green"))))
 '(linum-relative-current-face ((t (:inherit linum :foreground "#FBDE2D" :weight bold))))
 '(linum-relative-face ((t (:inherit linum :foreground "dark gray"))))
 '(log-view-file ((t (:foreground "DodgerBlue" :weight bold))))
 '(log-view-message ((t (:foreground "Goldenrod" :weight bold))))
 '(magit-branch ((t (:foreground "Green" :weight bold))))
 '(magit-branch-local ((t (:foreground "coral1"))))
 '(magit-branch-remote ((t (:foreground "green1"))))
 '(magit-diff-added ((t (:inherit diff-added))))
 '(magit-diff-added-highlight ((t (:background "gray26" :foreground "green4"))))
 '(magit-diff-context ((t (:inherit diff-context))))
 '(magit-diff-file-heading ((t (:inherit diff-file-header))))
 '(magit-diff-hunk-heading ((t (:inherit diff-hunk-header :inverse-video t))))
 '(magit-diff-removed ((t (:inherit diff-removed))))
 '(magit-header ((t (:foreground "DodgerBlue"))))
 '(magit-log-author ((t (:foreground "Green"))))
 '(magit-log-date ((t (:foreground "cyan"))))
 '(magit-section-heading ((t (:background "gray29" :weight bold))))
 '(minibuffer-prompt ((t (:foreground "salmon1"))))
 '(mode-line ((t (:background "grey75" :foreground "black"))))
 '(mode-line-buffer-id ((t (:background "dark olive green" :foreground "beige"))))
 '(mode-line-highlight ((((class color) (min-colors 88)) nil)))
 '(mode-line-inactive ((t (:background "dark olive green" :foreground "dark khaki" :weight light))))
 '(org-agenda-date ((t (:inherit org-agenda-structure))))
 '(org-agenda-date-today ((t (:inherit org-agenda-date :underline t))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :foreground "green"))))
 '(org-agenda-done ((t (:foreground "#269926"))))
 '(org-agenda-restriction-lock ((t (:background "#FFB273"))))
 '(org-agenda-structure ((t (:foreground "gold1" :weight bold))))
 '(org-date ((t (:foreground "medium sea green" :underline t))))
 '(org-document-info ((t (:foreground "tomato1"))))
 '(org-document-title ((t (:foreground "orchid1" :weight bold))))
 '(org-done ((t (:foreground "green" :weight bold))))
 '(org-drawer ((t (:foreground "purple1"))))
 '(org-ellipsis ((t (:foreground "#FF7400" :underline t))))
 '(org-footnote ((t (:foreground "#1240AB" :underline t))))
 '(org-hide ((t (:foreground "gray20"))))
 '(org-level-1 ((t (:inherit outline-1 :box nil))))
 '(org-level-2 ((t (:inherit outline-2 :box nil))))
 '(org-level-3 ((t (:inherit outline-3 :box nil))))
 '(org-level-4 ((t (:inherit outline-4 :box nil))))
 '(org-level-5 ((t (:inherit outline-5 :box nil))))
 '(org-level-6 ((t (:inherit outline-6 :box nil))))
 '(org-level-7 ((t (:inherit outline-7 :box nil))))
 '(org-level-8 ((t (:inherit outline-8 :box nil))))
 '(org-scheduled-previously ((t (:foreground "#FF7400"))))
 '(org-table ((t (:foreground "cyan"))))
 '(org-tag ((t (:weight bold))))
 '(org-todo ((t (:foreground "#FF6961" :weight bold))))
 '(region ((t (:background "DarkSlateGray"))))
 '(term-color-blue ((t (:background "#85aed9" :foreground "#85aed9"))))
 '(term-color-green ((t (:background "#ceffa0" :foreground "#ceffa0"))))
 '(term-color-magenta ((t (:background "#ff73fd" :foreground "#ff73fd"))))
 '(term-color-red ((t (:background "#ff6d60" :foreground "#ff6d60"))))
 '(term-color-yellow ((t (:background "#d1f13c" :foreground "#d1f13c"))))
 '(text-cursor ((t (:background "yellow" :foreground "black"))))
 '(tooltip ((t (:inherit variable-pitch :background "systeminfowindow" :foreground "DarkGreen" :height 2.5))))
 '(underline ((nil (:underline nil))))
 '(vhl/default-face ((t (:background "DarkSlateGray"))))
 '(vmacs-scroll-highlight-line-face ((t (:background "cadetblue4" :foreground "white" :weight bold))))
 '(warning ((t (:foreground "Salmon" :weight bold))))
 '(web-mode-html-tag-bracket-face ((t (:inherit web-mode-html-tag-face))))
 '(window-divider ((t (:foreground "gray"))))
 '(window-divider-first-pixel ((t (:foreground "yellow"))))
 '(window-divider-last-pixel ((t (:foreground "yellow"))))
 '(woman-addition ((t (:inherit font-lock-builtin-face :foreground "Tan2"))))
 '(woman-bold ((t (:inherit bold :foreground "yellow2"))))
 '(woman-italic ((t (:inherit italic :foreground "green"))))
 '(woman-unknown ((t (:inherit font-lock-warning-face :foreground "Firebrick")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-visited-interval 5)
 '(auto-save-visited-mode t)
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/cache/backup_files/"))))
 '(blink-cursor-mode t)
 '(bookmark-default-file "~/.emacs.d/cache/bookmarks")
 '(column-number-mode nil)
 '(custom-group-tag-faces (quote (default)))
 '(electric-pair-mode t)
 '(global-auto-revert-mode t)
 '(helm-minibuffer-history-key "C-r")
 '(magit-commit-ask-to-stage t)
 '(magit-no-confirm
   (quote
    (reverse rename abort-merge resect-bisect kill-process stage-all-changes unstage-all-changes)))
 '(magit-push-arguments (quote ("--force-with-lease")))
 '(magit-save-repository-buffers (quote dontask))
 '(menu-bar-mode nil)
 '(org-agenda-files nil t)
 '(package-selected-packages
   (quote
    (iedit go-imports sane-term shell-toggle ctable dash deferred osx-dictionary dired-narrow smex ivy-dired-history counsel flx magit gitconfig-mode company-jedi dired-filetype-face company-go auto-compile golden-ratio-scroll-screen company evil-textobj-anyblock exec-path-from-shell applescript-mode async bm crontab-mode dockerfile-mode erlang ethan-wspace evil evil-leader evil-magit flycheck git-commit go-eldoc go-mode golden-ratio goto-chg helm-core logstash-conf lua-mode markdown-mode protobuf-mode thrift web-mode wgrep with-editor yaml-mode yasnippet)))
 '(recentf-save-file "~/.emacs.d/cache/recentf")
 '(safe-local-variable-values
   (quote
    ((eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep
                (quote package-build))
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require
                (quote package-build))))
           (package-build-minor-mode)
           (set
            (make-local-variable
             (quote package-build-working-dir))
            (expand-file-name "../working/"))
           (set
            (make-local-variable
             (quote package-build-archive-dir))
            (expand-file-name "../packages/"))
           (set
            (make-local-variable
             (quote package-build-recipes-dir))
            default-directory))
     (eval progn
           (setq jedi:environment-root
                 (expand-file-name "./virtual/"
                                   (locate-dominating-file default-directory "Makefile")))
           (setq jedi:server-args
                 (\`
                  ("--virtual-env"
                   (\,
                    (expand-file-name "./virtual/"
                                      (locate-dominating-file default-directory "Makefile")))
                   "--virtual-env"
                   (\,
                    (expand-file-name "~/python/"))
                   "--virtual-env" "/System/Library/Frameworks/Python.framework/Versions/2.7/" "--sys-path"
                   (\,
                    (expand-file-name
                     (expand-file-name "./src/"
                                       (locate-dominating-file default-directory "Makefile"))))
                   "--sys-path"
                   (\,
                    (expand-file-name
                     (expand-file-name "./src/db"
                                       (locate-dominating-file default-directory "Makefile"))))
                   "--sys-path" "/System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7" "--sys-path" ".")))
           (setq exec-path
                 (delete-dups
                  (cons
                   (expand-file-name "./virtual/bin/"
                                     (locate-dominating-file default-directory "Makefile"))
                   exec-path)))
           (setenv "PATH"
                   (concat
                    (expand-file-name "./virtual/bin/"
                                      (locate-dominating-file default-directory "Makefile"))
                    ":"
                    (getenv "PATH")))
           (setenv "PYTHONPATH"
                   (expand-file-name "./src/"
                                     (locate-dominating-file default-directory "Makefile")))
           (setenv "PYTHONPATH"
                   (expand-file-name "./db/"
                                     (locate-dominating-file default-directory "Makefile")))))))
 '(save-place-file "~/.emacs.d/cache/place")
 '(save-place-mode t)
 '(savehist-file "~/.emacs.d/cache/history")
 '(scroll-bar-mode nil)
 '(tramp-persistency-file-name "~/.emacs.d/cache/tramp" nil (tramp))
 '(tramp-syntax (quote default) nil (tramp))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(warning-suppress-types (quote ((yasnippet backquote-change))))
 '(window-divider-default-bottom-width 1)
 '(window-divider-default-places (quote bottom-only))
 '(window-divider-default-right-width 1)
 '(window-divider-mode t))
