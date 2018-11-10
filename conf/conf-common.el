(eval-when-compile (require 'org))
(eval-when-compile (require 'cc-mode))
(eval-when-compile (require 'wgrep))
(defvar  dropbox-dir (expand-file-name "~/Documents/dropbox"))
(when (equal system-type 'darwin)
  (when (or (not (file-exists-p dropbox-dir))
            (not (file-symlink-p dropbox-dir)))
    (start-process "lndropbox" "*Messages*" "ln"  "-f" "-s" (expand-file-name "~/Library/Mobile Documents/com~apple~CloudDocs/") dropbox-dir)))

;; (when (not (file-exists-p dropbox-dir)) (make-directory dropbox-dir t))

(setq-default
 inhibit-startup-screen t;隐藏启动显示画面
 initial-scratch-message nil;关闭scratch消息提示
 initial-major-mode 'emacs-lisp-mode ;scratch init mode
 ;; initial-buffer-choice t                ;默认打开scratch buffer
 initial-buffer-choice "~/"


 use-dialog-box nil		      ;不使用对话框进行（是，否 取消） 的选择，而是用minibuffer
 ;; frame-title-format "%b  [%I] %f  GNU/Emacs" ;标题显示文件名，而不是默认的username@localhost
 frame-title-format '("%e " evil-mode-line-format "「"mode-line-buffer-identification "」("  (:propertize ("" mode-name) ) ") "   mode-line-misc-info   "%f  GNU/Emacs")

 ;;  mode-line 上显示当前文件是什么系统的文件(windows 的换行符是\n\r)
 eol-mnemonic-dos "[w32]"
 eol-mnemonic-unix "[unix]"
 eol-mnemonic-mac "[mac]"
 eol-mnemonic-undecided "[?]"
 ;;(setq track-eol t) ;; 当光标在行尾上下移动的时候，始终保持在行尾。
 ;; (setq-default cursor-type 'bar);;光标显示为一竖线

 ;;中键点击时的功能
 ;;不要在鼠标中键点击的那个地方插入剪贴板内容。
 ;;而是光标在什么地方,就在哪插入(这个时候光标点击的地方不一定是光标的所在位置)

 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
 sentence-end-double-space nil         ;;设置 sentence-end 可以识别中文标点。不用在 fill 时在句号后插入两个空格。


 ;;(require 'tramp)
 tramp-adb-prompt "^\\(?:[[:digit:]]*|?\\)?\\(?:[[:alnum:]-]*@[[:alnum:]]*[^#\\$]*\\)?[#\\$][[:space:]]" ;加了一个  "-"
 remote-file-name-inhibit-cache 60 ;60s default 10s
 backup-by-copying t    ;自动备份
 delete-old-versions t ; 自动删除旧的备份文件
 kept-new-versions 10   ; 保留最近的6个备份文件
 kept-old-versions 10   ; 保留最早的2个备份文件
 version-control t    ; 多次备份
 ;; 备份文件统一放在 ~/.emacs.d/cache/backup_files,避免每个目录生成一些临时文件
 pulse-iterations 3
 large-file-warning-threshold nil       ;打开大文件时不必警告

 ;;find-file-visit-truename t

 send-mail-function 'sendmail-send-it
 mail-addrbook-file (expand-file-name "mail_address" dropbox-dir)

 ;; after this shell-command can use zsh alias
 ;; shell-file-name "zsh"
 ;; shell-command-switch "-ic"

 ;;注意这两个变量是与recentf相关的,把它放在这里,是因为
 ;;觉得recentf与filecache作用有相通之处,
 ;;匹配这些表达示的文件，不会被加入到最近打开的文件中
 recentf-exclude  `("\\.elc$" ,(regexp-quote (concat user-emacs-directory "cache/" ))
                    "/cache/recentf"
                    "/TAGS$" "java_base.tag" ".erlang.cookie" "xhtml-loader.rnc" "COMMIT_EDITMSG")
 recentf-max-saved-items 300
 ring-bell-function 'ignore

 savehist-additional-variables '(ivy-dired-history-variable helm-dired-history-variable magit-repository-directories mew-passwd-alist kill-ring sqlserver-connection-info mysql-connection-4-complete sql-server sql-database sql-user)
 ;;when meet long line ,whether to wrap it
 truncate-lines t ;一行过长时 是否wrap显示
 save-place t                           ;记录上次打开文件的光标位置
 display-line-numbers 'absolute
 )

(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                              "xelatex -interaction nonstopmode %f"))
;; (with-eval-after-load 'org
;;   ;; #+LATEX_HEADER: \usepackage{fontspec}
;;   ;; #+LATEX_HEADER: \setmainfont{Songti SC}
;;   ;; (add-to-list 'org-latex-default-packages-alist '(""     "fontspec" nil))
;;   ;; (setq org-format-latex-header (concat org-format-latex-header "\n\\setmainfont{Songti SC}"))

;;   )

;; (defun pretty-chinese-char()
;;   (push '("“" . "「") prettify-symbols-alist)
;;   (push '("”" . "」") prettify-symbols-alist)
;;   (prettify-symbols-mode 1)
;;   )
;; ;; (add-hook 'org-mode-hook 'pretty-chinese-char)
;; (add-hook 'text-mode-hook 'pretty-chinese-char)




(fset 'yes-or-no-p 'y-or-n-p) ;; 把Yes用y代替

;;(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil);; 启用narrow-to-region ,不再警告
(put 'erase-buffer 'disabled nil)

;; 因为经常按出c-x c-u，总是出upcase region的警告，
;; (put 'upcase-region 'disabled nil)
;; (put 'downcase-region 'disabled nil)
;; (add-to-list 'byte-compile-not-obsolete-vars 'font-lock-beginning-of-syntax-function)
;; (add-to-list 'byte-compile-not-obsolete-vars 'font-lock-syntactic-keywords)

;; after-init-hook 所有配置文件都加载完之后才会运行此hook
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(setq-default auto-mode-alist
      (append
       '(("\\.pyx" . python-mode)
         ("SConstruct" . python-mode)
         ("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)
         ("\\.lua$" . lua-mode)
         ("\\.scpt\\'" . applescript-mode)
         ("\\.applescript$" . applescript-mode)
         ("crontab\\'" . crontab-mode)
         ("\\.cron\\(tab\\)?\\'" . crontab-mode)
         ("cron\\(tab\\)?\\."    . crontab-mode)
         ("\\.mxml" . nxml-mode)
         ("\\.as" . actionscript-mode)
         ("\\.proto\\'" . protobuf-mode)
         ("\\.thrift" . thrift-mode)
         ("\\.md" . markdown-mode)
         ("\\.\\(frm\\|bas\\|cls\\|vba\\|vbs\\)$" . visual-basic-mode)

         ("\\.go.txt$" . go-mode)
         ("\\.yaws$" . nxml-mode)
         ("\\.html$"  . web-mode)
         ("\\.htm$"   . web-mode)
         ("\\.phtml$" . web-mode)
         ("\\.php3$"  . web-mode)
         ("\\.jsp$" . web-mode)

         ("\\.hrl$" . erlang-mode)
         ("\\.erl$" . erlang-mode)
         ("\\.rel$" . erlang-mode)
         ("\\.app$" . erlang-mode)
         ("\\.app.src$" . erlang-mode)
         ("\\.ahk$\\|\\.AHK$" . xahk-mode)
         ("\\.bat$"   . batch-mode)
         ("\\.cmd$"   . batch-mode)
         ("\\.pl$"   . cperl-mode)
         ("\\.pm$"   . cperl-mode)
         ("\\.perl$" . cperl-mode)
         ("\\.sqlo$"  . oracle-mode)
         ("\\.sqlm$"  . mysql-mode)
         ("\\.sqlms$"  . sqlserver-mode)
         ("\\.js$"  . js-mode)
         ("\\.pac$" . js-mode)
         ;; ("\\.js$"  . js3-mode)
         ("\\.txt$" . novel-mode)
         ("\\.mm$" . objc-mode)
         )
       auto-mode-alist))
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and buffer-file-name
                       (string= (file-name-extension buffer-file-name) "h")
                       (or (re-search-forward "@\\<interface\\>"
                                              magic-mode-regexp-match-limit t)
                           (re-search-forward "@\\<protocol\\>"
                                          magic-mode-regexp-match-limit t))))
               . objc-mode))

(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (looking-at "[ \t\n]*{[ \t\n]*\""))
               . json-mode))
;; (add-to-list 'magic-mode-alist
;;              `(,(lambda ()
;;                   (and
;;                    (or
;;                     ;; (re-search-forward "insert "
;;                     ;;                           magic-mode-regexp-match-limit t)
;;                     ;; (re-search-forward "delete "
;;                     ;;                           magic-mode-regexp-match-limit t)
;;                     (re-search-forward "alter "
;;                                               magic-mode-regexp-match-limit t)
;;                     (re-search-forward "grant"
;;                                               magic-mode-regexp-match-limit t)
;;                            )))
;;                . mysql-mode))

;; (setq-default ace-jump-mode-case-fold nil
;;               ace-jump-mode-scope 'window
;;               ;; 59==; ,97=a
;;               ace-jump-mode-move-keys '(97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 59))

;;wgrep
;; (add-hook 'grep-setup-hook 'grep-mode-fun)
(setq-default wgrep-auto-save-buffer t
              wgrep-enable-key "i"
              wgrep-change-readonly-file t)
(with-eval-after-load 'wgrep
  (define-key wgrep-mode-map (kbd "C-g") 'wgrep-abort-changes))

;; (defun enable-wgrep-when-entry-insert()
;;   (when (member major-mode '(ivy-occur-mode helm-grep-mode))
;;     (wgrep-change-to-wgrep-mode)))

;; (defun disable-wgrep-when-exit-insert()
;;   (when (member major-mode '(ivy-occur-mode helm-grep-mode))
;;     (wgrep-abort-changes)))
;; (autoload 'wgrep-change-to-wgrep-mode "wgrep" "enable wgrep" nil)
;; (autoload 'wgrep-abort-changes "wgrep" "disable wgrep" nil)
;; (add-hook 'evil-insert-state-entry-hook 'enable-wgrep-when-entry-insert)
;; (add-hook 'evil-insert-state-exit-hook 'disable-wgrep-when-exit-insert)

(setq-default
 enable-recursive-minibuffers t        ;在minibuffer 中也可以再次使用minibuffer
 history-delete-duplicates t          ;minibuffer 删除重复历史
 minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)) ;;;;minibuffer prompt 只读，且不允许光标进入其中
 resize-mini-windows t
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 completion-cycle-threshold 8)
;; (add-hook 'minibuffer-setup-hook 'minibuf-define-key-func )


;; (setq-default hippie-expand-try-functions-list
;;               '(
;;                 yas-hippie-try-expand
;;                 try-expand-dabbrev
;;                 ;; try-vmacs-dabbrev-substring
;;                 try-expand-dabbrev-visible
;;                 try-expand-dabbrev-all-buffers
;;                 try-expand-dabbrev-from-kill
;;                 try-expand-list
;;                 try-expand-list-all-buffers
;;                 try-expand-line
;;                 try-expand-line-all-buffers
;;                 try-complete-file-name-partially
;;                 try-complete-file-name
;;                 try-expand-whole-kill
;;                 )
;;               )
;; (global-set-key [(meta return)] 'hippie-expand)
;;  ;meta return on terminal
;; (global-set-key (kbd "C-M-m") 'hippie-expand) ;meta return for terminal


;; (push '("lambda" . #x1d77a) prettify-symbols-alist)
;; (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)



(global-set-key "\C-j" 'open-line-or-new-line-dep-pos)
(define-key lisp-interaction-mode-map "\C-j" 'open-line-or-new-line-dep-pos)

(global-set-key (kbd "C-a") 'smart-beginning-of-line)
(global-set-key (kbd "C-e") 'smart-end-of-line)
(global-set-key "\C-k" 'vmacs-kill-region-or-line)
(with-eval-after-load 'org
  (define-key org-mode-map "\C-k" 'vmacs-kill-region-or-org-kill-line)
  (define-key org-mode-map "\C-a" 'org-mode-smart-beginning-of-line)
  (define-key org-mode-map "\C-e" 'org-mode-smart-end-of-line))

(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "s-w") 'kill-this-buffer)

(global-set-key "\M-;" 'vmacs-comment-dwim-line)

(global-set-key "\C-x\C-v" 'switch-to-scratch-buffer)

(global-set-key (kbd "C-c w") 'browse-url-at-point)


;;; goto-last change
;;快速跳转到当前buffer最后一次修改的位置 利用了undo定位最后一次在何处做了修改
;; (autoload 'goto-last-change "goto-last-change" "Set point to the position of the last change." t)
(autoload 'goto-last-change-reverse "goto-chg.el" "goto last change reverse" t)
(vmacs-leader "x/" 'goto-last-change)
(vmacs-leader "x," 'goto-last-change-reverse)

(with-eval-after-load 'cc-mode (define-key c-mode-base-map ";" 'vmacs-append-semicolon-at-eol))

(setq-default iedit-toggle-key-default (kbd "C-;"))
(autoload 'iedit-mode-from-isearch "iedit" "enable iedit-mode when in isearch mode")

(define-key evil-normal-state-map "s;" 'iedit-mode)
;; (with-eval-after-load 'iedit
;;   (define-key iedit-mode-keymap (kbd "M-;") 'iedit-toggle-selection))


(define-key global-map iedit-toggle-key-default 'iedit-mode)
;; (define-key global-map (kbd "C-[ [ 1 f") 'iedit-mode) ;iterm map C-; to this
(define-key isearch-mode-map iedit-toggle-key-default 'iedit-mode-from-isearch)

(global-set-key (kbd "C-x C-e") 'eval-print-last-sexp)
;; (with-eval-after-load 'elisp-mode
;;   (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'eval-print-last-sexp))



;; (global-set-key "\M-n"  'vmacs-forward-4-line)
;; (global-set-key "\M-p"  'vmacs-backward-4-line)


;; ;; 因为evil的 jk 用于上下移动，C-n C-p基本上用不到
;; ;; 故把C-n C-p绑定到上下移动4 行，以便小范围的快速移动
;; ;; Faster point movement,一次前进后退5行
;; (global-set-key "\C-n"  'vmacs-forward-4-line)
;; (global-set-key "\C-p"  'vmacs-backward-4-line)
;; (global-set-key "\M-n"  'next-line)
;; (global-set-key "\M-p"  'previous-line)



(provide 'conf-common)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-common.el ends here.
