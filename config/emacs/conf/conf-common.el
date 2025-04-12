;;; -*- lexical-binding: t -*-
(defvar dropbox-dir (expand-file-name "~/Documents/jianguo/jianguo"))

;; (when (not (file-exists-p dropbox-dir)) (make-directory dropbox-dir t))

(when (boundp 'pixel-scroll-precision-mode) (pixel-scroll-precision-mode 1))
(setq-default
 inhibit-startup-screen t;隐藏启动显示画面
 initial-scratch-message nil;关闭 scratch 消息提示
 initial-major-mode 'emacs-lisp-mode ;scratch init mode
 initial-buffer-choice t                ;默认打开 scratch buffer
 ;; initial-buffer-choice "~/"

 treesit-max-buffer-size 107374182   ;100m
 gnus-directory "~/maildir/news"
 message-directory "~/maildir/"


 use-dialog-box nil           ;不使用对话框进行（是，否 取消） 的选择，而是用 minibuffer
 ;; frame-title-format "%b  [%I] %f  GNU/Emacs" ;标题显示文件名，而不是默认的 username@localhost
 frame-title-format '((:eval (meow-indicator)) "「"mode-line-buffer-identification "」("  (:propertize ("" mode-name) ) ") "   mode-line-misc-info   " GNU/Emacs<" (:eval (expand-file-name default-directory)) ">")
 xterm-set-window-title t
 xterm-extra-capabilities '( modifyOtherKeys reportBackground  )
 xterm-tmux-extra-capabilities xterm-extra-capabilities
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

 remote-file-name-inhibit-cache 60 ;60s default 10s
 backup-by-copying t    ;自动备份
 delete-old-versions t ; 自动删除旧的备份文件
 kept-new-versions 10   ; 保留最近的 6 个备份文件
 kept-old-versions 10   ; 保留最早的 2 个备份文件
 version-control t    ; 多次备份
 ;; create-lockfiles nil
 vc-make-backup-files t
 ;; 备份文件统一放在 ~/.emacs.d/cache/backup_files,避免每个目录生成一些临时文件
 pulse-iterations 3
 large-file-warning-threshold (* 1024 1024 50)       ;打开大文件时不必警告

 ;; 改成 true package 生成 autoload 有问题
 ;; find-file-visit-truename t

 send-mail-function 'sendmail-send-it
 ;; mail-addrbook-file (expand-file-name "mail_address" dropbox-dir)

 ;; after this shell-command can use zsh alias
 ;; 设置成zsh M-x:man 似乎无法用
 ;; shell-file-name "zsh"
 ;; shell-command-switch "-ic"

 ;;注意这两个变量是与 recentf 相关的,把它放在这里,是因为
 ;;觉得 recentf 与 filecache 作用有相通之处,
 ;;匹配这些表达示的文件，不会被加入到最近打开的文件中
 recentf-exclude  `("\\.elc$" ,(regexp-quote (concat user-emacs-directory "cache/" ))
                    ,(regexp-quote "~/.cache/")
                    "/cache/recentf"
                    "/TAGS$" "java_base.tag" ".erlang.cookie" "xhtml-loader.rnc" "COMMIT_EDITMSG")
 recentf-max-saved-items 1000
 ring-bell-function 'ignore
 savehist-additional-variables '(corfu-history magit-repository-directories kill-ring)
 ;;when meet long line ,whether to wrap it
 truncate-lines t ;一行过长时 是否 wrap 显示
 bidi-display-reordering  nil
 bidi-inhibit-bpa t
 long-line-threshold 1000
 large-hscroll-threshold 1000
 syntax-wholeline-max 1000
 ;; display-line-numbers 'absolute
 fill-column 100
 tramp-adb-prompt "^\\(?:[[:digit:]]*|?\\)?\\(?:[[:alnum:]-:/]*@?[[:alnum:]-:/]*[^#\\$]*\\)?[#\\$][[:space:]]" ;加了一个  "-"
 tramp-shell-prompt-pattern (concat "\\(?:^\\|\r\\)" "[^]#@$%>\n]*#?[]#$@%>] *\\(\e\\[[0-9;]*[a-zA-Z-.] *\\)*")
 comint-prompt-regexp "^[^#$%\n]*[#$%] *"  ;默认 regex 相当于没定义，term-bol 无法正常中转到开头处
 shell-prompt-pattern "^[^#$%\n]*[#$%] *"  ;默认 regex 相当于没定义，term-bol 无法正常中转到开头处
 tramp-default-method "ssh" ;Faster than the default scp
 tramp-verbose 1
 find-function-C-source-directory "~/repos/emacs/src/"
 Man-notify-method 'bully
 )
;; (global-visual-line-mode)
(setq show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t
      show-paren-context-when-offscreen t
      show-paren-delay 0.2)
;; Increase undo limits. Why?
;; .. ability to go far back in history can be useful, modern systems have sufficient memory.
;; Limit of 64mb.
(setq undo-limit 6710886400)
;; Strong limit of 1.5x (96mb)
(setq undo-strong-limit 100663296)
;; Outer limit of 10x (960mb).
;; Note that the default is x100), but this seems too high.
(setq undo-outer-limit 1006632960)
(with-eval-after-load 'vundo
  (setq vundo-roll-back-on-quit nil)
  (setq vundo-glyph-alist vundo-unicode-symbols))

;; (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/cache/undo")))
;; (require 'display-fill-column-indicator nil t)
;; (when (featurep 'display-fill-column-indicator)
;; (add-hook 'find-file-hook #'display-fill-column-indicator--turn-on))
;; (when (boundp 'global-so-long-mode) (global-so-long-mode))

(fset 'yes-or-no-p 'y-or-n-p) ;; 把 Yes 用 y 代替

;;(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil);; 启用 narrow-to-region ,不再警告
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(with-eval-after-load 'markdown-mode  (add-hook 'markdown-mode-hook #'auto-fill-mode))
;; after-init-hook 所有配置文件都加载完之后才会运行此 hook
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(setq-default auto-mode-alist
              (append
               '(("\\.pyx" . python-mode)
                 ("zsh" . sh-mode)
                 ("SConstruct" . python-mode)
                 ("\\.yml$" . yaml-ts-mode)
                 ("authinfo.gpg" . authinfo-mode)
                 ("\\.yaml$" . yaml-ts-mode)
                 ("\\.lua$" . lua-mode)
                 ("\\.scpt\\'" . applescript-mode)
                 ("\\.applescript$" . applescript-mode)
                 ;; ("crontab\\'" . crontab-mode)
                 ;; ("\\.cron\\(tab\\)?\\'" . crontab-mode)
                 ;; ("cron\\(tab\\)?\\."    . crontab-mode)
                 ("\\.mxml" . nxml-mode)
                 ("\\.proto\\'" . protobuf-mode)
                 ("\\.thrift" . thrift-mode)
                 ("\\.md$" . markdown-mode)
                 ("\\.\\(frm\\|bas\\|cls\\|vba\\|vbs\\)$" . visual-basic-mode)

                 ("\\.rs$" . rust-ts-mode)
                 ("\\.go.txt$" . go-ts-mode)
                 ("\\.go\\'" . go-ts-mode)
                 ("\\.java\\'" . java-ts-mode)

                 ("\\.yaws$" . nxml-mode)

                 ("\\.hrl$" . erlang-mode)
                 ("\\.erl$" . erlang-mode)
                 ("\\.rel$" . erlang-mode)
                 ("\\.app$" . erlang-mode)
                 ("\\.app.src$" . erlang-mode)
                 ("\\.ahk$\\|\\.AHK$" . ahk-mode)
                 ("\\.bat$"   . batch-mode)
                 ("\\.cmd$"   . batch-mode)
                 ("\\.pl$"   . cperl-mode)
                 ("\\.pm$"   . cperl-mode)
                 ("\\.perl$" . cperl-mode)
                 ("\\.sqlo$"  . oracle-mode)
                 ("\\.sqlm$"  . mysql-mode)
                 ("\\.sqlms$"  . sqlserver-mode)
                 ("\\.js$"  . js-mode)
                 ("\\.json$"  . json-ts-mode)
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
               . json-ts-mode))
(global-set-key "\C-j" 'open-line-or-new-line-dep-pos)
(define-key lisp-interaction-mode-map "\C-j" 'open-line-or-new-line-dep-pos)

(global-set-key (kbd "C-a") 'smart-beginning-of-line)
(global-set-key (kbd "C-e") 'smart-end-of-line)
;; (define-key evil-motion-state-map (kbd "C-a") 'smart-beginning-of-line)
;; (define-key evil-motion-state-map (kbd "C-e") 'smart-end-of-line)

(global-set-key "\C-k" 'vmacs-kill-region-or-line)
(global-set-key "\M-;" 'vmacs-comment-dwim-line)

(global-set-key "\C-x\C-v" 'scratch-buffer)

(global-set-key (kbd "C-c w w") 'browse-url-at-point)


;;; goto-last change
;;快速跳转到当前 buffer 最后一次修改的位置 利用了 undo 定位最后一次在何处做了修改
;; (autoload 'goto-last-change "goto-last-change" "Set point to the position of the last change." t)
(autoload 'goto-last-change-reverse "goto-chg.el" "goto last change reverse" t)

(with-eval-after-load 'cc-mode (define-key c-mode-base-map ";" 'vmacs-append-semicolon-at-eol))
;; Quick edit (for use with hammerspoon quick edit)
(defun meain/quick-edit-end ()
  "Util function to be executed on qed completion."
  (interactive)
  (mark-whole-buffer)
  (call-interactively 'kill-ring-save)
  (kill-current-buffer))
(defun meain/quick-edit ()
  "Util function for use with hammerspoon quick edit functionality."
  (interactive)
  (let ((qed-buffer-name (concat "*scratch*" )))
    (switch-to-buffer (generate-new-buffer qed-buffer-name t))
    (sit-for 0.01)
    ;; (evil-paste-after 1)
    (gfm-mode)))

;; (global-set-key (kbd "C-x C-e") 'eval-print-last-sexp)
(defun vmacs-pager(&optional file)
  (require 'ansi-color)
  (require 'ansi-osc)
  (let ((buf (generate-new-buffer "*pager")))
    (with-current-buffer buf
      (insert-file-contents file)
      (goto-char (point-min))
      (setq default-directory (buffer-substring (point-min)( point-at-eol)))
      (save-place-local-mode -1)
      (setq comint-use-prompt-regexp t)
      (shell-mode)
      ;; (compilation-minor-mode 1)
      (ansi-osc-apply-on-region  (point-min)(point-max))
      (define-key comint-mode-map (kbd "M-p")  'comint-previous-prompt)
      (define-key comint-mode-map (kbd "M-n")  'comint-next-prompt)
      (ansi-color-apply-on-region (point-min)(point-max))
      (set-buffer-modified-p nil)
      (read-only-mode)
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (delete-file file)
      (tab-line-mode -1)
      (forward-char 1)
      (meow-normal-mode))
    (switch-to-buffer buf)))

(defun vmacs-calc-hook()
  (require 'calc-bin)
  (define-key calc-mode-map (kbd "C-c M/") 'calc-divide)
  ;; 默认 calc 的移位移位操作是接 32 位的， 可以 bw(calc-word-size) 来改成 64 位
  (calc-word-size 128))

(add-hook 'calc-mode-hook 'vmacs-calc-hook)

(defun vmacs-backup-enable-predicate (name)
  "Disable backups for remote files (with Tramp)."
  (and (normal-backup-enable-predicate name)
       (not
        (let ((method (file-remote-p name 'method)))
          (when method (member method '("ssh" "scp" "sudo")))))))

(setq backup-enable-predicate 'vmacs-backup-enable-predicate)

(autoload 'gt-do-translate "go-translate" "go-translate" t)
(with-eval-after-load 'go-translate
  ;; https://github.com/lorniu/go-translate/blob/master/README-zh.org
  (add-to-list 'plz-curl-default-args "--dns-servers"  t)
  (add-to-list 'plz-curl-default-args "1.1.1.1"  t)
  (setq gt-default-http-client
        (lambda (host)
          (if (string-match-p "google\\|deepl\\|openai" host)
              (gt-plz-http-client :args '("--proxy" "socks5://192.168.124.24:8088"))
            (gt-plz-http-client))))
  
(add-hook 'gt-buffer-render-init-hook #'(lambda()(setq truncate-lines nil)))
  (setq gt-langs '(en zh))
  (setq gt-buffer-render-window-config
        '((display-buffer-same-window)))
  (setq gt-chatgpt-system-prompt "You are a highly skilled translation engine with expertise in the technology sector. maintaining the original format, technical terms, and abbreviations. Do not add any explanations or annotations to the translated text.")
  (setq gt-default-translator
        (gt-translator
         :taker   (list (gt-taker :pick nil :if 'selection) ;有选中则使用选中的内这
                        ;; 以下mode 默认翻译单个段落
                        ;; (gt-taker :text 'paragraph :if '(Info-mode help-mode markdown-mode text-mode org-mode novel-mode))
                        (gt-taker :text 'paragraph)
                        ;;read-only： fresh-word 只翻译生词
                        ;; (gt-taker :text 'buffer :pick 'fresh-word :if 'read-only)
                        ;; (gt-taker :text 'word)
                        )
         :engines (list (gt-google-engine :if 'word)
                        (gt-youdao-dict-engine :if 'word)
                        (gt-chatgpt-engine :if '(or parts read-only selection)) ;多段落 不支持  :stream t
                        (gt-chatgpt-engine :if '(and no-word no-parts no-read-only no-selection) :stream t) ; 非单词 非多段落 （即单个段落时） 使用stream
                        )
         :render  (list
                   ;; (gt-overlay-render :if 'selection)
                   ;; (gt-render :if 'selection) ;minibuffer
                   ;; (gt-insert-render :if 'selection) ;minibuffer
                   (gt-overlay-render :if '(and read-only no-word))
                   ;; (gt-insert-render :if (lambda (translator) (member (buffer-name) '("COMMIT_EDITMSG"))))
                   (gt-insert-render :if  '((and (or org-mode text-mode markdown-mode novel-mode) not-word)) :type 'after)
                   ;; (gt-alert-render :if '(and xxx-mode (or not-selection (and read-only parts))))
                   (gt-buffer-render))))
  (define-key gt-overlay-render-map (kbd "C-g") #'gt-delete-render-overlays)
  (define-key gt-overlay-render-map (kbd "<escape>") #'gt-delete-render-overlays)
  (define-key gt-overlay-render-map (kbd "M-w") #'gt-overlay-render-save-to-kill-ring))

(with-eval-after-load 'conf-mode  (keymap-unset conf-mode-map "C-c SPC" t))
(provide 'conf-common)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-common.el ends here.
