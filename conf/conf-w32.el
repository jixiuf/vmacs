;; some code for windows system
(setq exec-path (delete-dups  (cons (expand-file-name "~/.emacs.d/binw32") exec-path)))
(setenv "PATH" (concat (convert-standard-filename (expand-file-name  "~/.emacs.d/binw32/")) ";" (getenv "PATH") ))

;;tramp 远程编译服务器上的文件（通过ssh /plink等）
;; ssh://user@server:path/to/file
;; /host:/filename
;; http://blog.donews.com/pluskid/archive/2006/05/06/858306.aspx
;; (require 'tramp)
(eval-after-load 'tramp
  '(progn
     (setq tramp-default-method "plink")
     ;; (add-to-list 'tramp-default-method-alist '("localhost" "root" "su"))
     (setq tramp-default-user "root")
     ;; 你也可以对于不同的 方法/主机 组合使用不同的用户名。例如，如果你总是想在域 10.10.10.211 上使用用户名 root ，你可以用如下方法指定：
     ;; (add-to-list 'tramp-default-user-alist '("plink" "10.10.10.211" "root"))
     ;; (add-to-list 'tramp-default-user-alist '("ssh" "jf.org" "jixiuf"))
     ;; (setq tramp-default-host "10.10.10.211") ;那么 /ssh:: 将连接到 10.10.10.211 上
     (setq password-cache-expiry nil)         ;密码缓存永不过期 ,default 16seconds
     ))


;;进行server认证的目录,
;; (setq server-auth-dir (expand-file-name "cache/" user-emacs-directory))
;; (setq server-name "emacs-server-file")
;;上面两个值连起来即为emacsclient --server-file后面跟的参数
;;为方便计只需要设置EMACS_SERVER_FILE,值为emacs-server-file的绝对路径名称
;;如我的"d:\.emacs.d\cache\emacs-server-file"
;;注意在windows 上我把环境变量HOME设成了D:\,所以"~"就代表"D:\"了.
;; (require 'server)
;; (when (not (server-running-p))
;;   (server-force-delete)
;;   (server-start))

;; ;;这台机器用是日文系统 ,所以一些配置,采用日文编码
;; ;; (when (equal system-name "SB_QINGDAO")
;; ;;   (setq buffer-file-coding-system 'utf-8) ;;写文件时使用什么编码
;; ;;                                         ;  (setq file-name-coding-system 'shift_jis-dos) ;;文件名所用的编码,不过这样,中文文件名就有问题了
;; ;;   (setq file-name-coding-system 'undecided-unix)
;; ;;   (prefer-coding-system 'utf-8)
;; ;;   )
;; (prefer-coding-system 'cp936) ;;默认使用cp936
;; (setq process-coding-system-alist (cons '("git" . (utf-8 . utf-8)) process-coding-system-alist));;对git 的输入输入的编辑使用utf-8
;; ;; (setq process-coding-system-alist (cons '("grep" . (cp936 . cp936)) process-coding-system-alist));;对git 的输入输入的编辑使用utf-8
;; (setq process-coding-system-alist (cons '("bash" . (utf-8 . utf-8)) process-coding-system-alist));对bash 的输入输入的编辑使用cp936
;; (setq process-coding-system-alist (cons '("diff" . (cp936 . cp936)) process-coding-system-alist));对bash 的输入输入的编辑使用cp936
;; (set-file-name-coding-system 'cp936) ;;文件名的编辑 dired 中会用到
;; (setq-default buffer-file-coding-system 'utf-8) ;;buffer写文件时使用什么编码
;; ;; 以下两个测试中。
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; ;; (setq buffer-file-coding-system 'utf-8) ;;写文件时使用什么编码

(provide 'conf-w32)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-w32.el ends here.
