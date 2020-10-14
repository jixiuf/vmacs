;;; -*- coding:utf-8 -*-
;;; 一些命令注释

;;q        quit
;; f <RET> open file 打开文件
;; o       open file other window 在另一个窗口中打开文件
;;C-o      open file other window (point in this window) ,在另个窗口打开文件,焦点仍在当前window
;;v        view-file 只读打开(q 退出)
;; ^       上层目录 ,我改成了u,以方便操作

;;关于mark,将文件标记之后,一些处理文件的命令会对mark的所有文件
;;采取一致的行动,如删除等.

;; m与*m 标记为"*"
;; **    标记所有可执行文件
;; *@    标记所有软连接文件
;; */    标记所有目录(.与..除外)
;; *s    标记当前目录的所有
;; u与*u     删除标记 (u被我重定义为回到上层)
;; U与*!  删除所有标记
;; %d REGEXP  将所有文件匹配的文件标记为D(删除)
;; %m REGEXP  将所有文件匹配的文件标记为*
;; %g REGEXP <RET>  如果文件中的内容匹配正则表达示则标记之
;; *C-n 移动到下一个标记的文件
;; *C-p .....上...........
;;

;;操作文件的命令,有以下规则
;;如果有前缀参数N 则对从当前文件开始的N个文件执行操作,负数则反向
;;否则对标记mark为*的文件,
;;否则 当前文件

;;C  copy
;; (setq dired-copy-preserve-time t),copy时保留原文件的修改时间 如果cp -p

;;D  delete  ,经确认后马上删除
;;d  detele 实际只是标记此文件为删除,执行x 才真正删除

;;R   mv rename 重命名,移动文件
;;H  硬链接
;;M  chmod  ,如M 755 修改为755
;;G  change group
;;O   change owner
;;T  touch file
;;Z   compress or uncompress
;;L   load lisp
;;A REGEXP  ,在文件中search   ,M-, 跳到下一个
;;Q REGEXP  ,query-replace-regexp(`dired-do-query-replace-regexp').

;;子目录
;;i 同时打开目录
;;$ hide or show 当前目录
;;指定隐藏哪些文件 : C-o, toggle


;; g refresh all
;; l  refresh mark的文件
;; k 隐藏指定文件
;; s 排序 字母,或date 间toggle


;;编辑,输入C-x C-q 切换到writable 模式,此时修改文件名,然后C-c c-c 提交
;;也或以移动文件,将文件名写成相应的路径名即可
;;设置在writable 模式下允许修改权限
;;(setq wdired-allow-to-change-permissions t)
;;C-td 会将目录中所有标记的文件生成缩略图,图片预览

;;w  copy the file name
;;C-u0w copy 全路径名

;;dired 支持拖放,你可以在pcmanfm nautils 中将文件,拖动到dired buffer 中

;;正则表达式的应用
;;% R FROM <RET> TO <RET>'   ;rename
;;% C FROM <RET> TO <RET>'   ; copy
;;% H FROM <RET> TO <RET>'   ; Hard link
;;% S FROM <RET> TO <RET>'   ; soft link
;;FROM TO 是一个正则,另外在TO 中可以用\&引用FROM整个的匹配组,\数字匹配某一个组
;;如 % R ^.*$ <RET> \&.tmp <RET>' 重命名所有标记的文件为*.tmp

;;= diff
;;M-=  diff 与backup

;;!与X 执行shell 命令,"*" 代表选中的文件名
;;如我想把abc 文件移动到/tmp
;; !cp * /tmp
;; !tar -cf a.tar *
;; !for file in * ; do mv "$file" "$file".tmp; done
;;
;;与*类似但不相同的"?" 表示对mark的文件"分别" 运行这个命令
;;; image-dired

(setq-default
 image-dired-db-file (concat user-emacs-directory "cache/image-dired/image-dired_db" )
 image-dired-dir (concat user-emacs-directory "cache/image-dired/image-dired" )
 image-dired-gallery-dir (concat user-emacs-directory "cache/image-dired/image-dired-gallery")
 image-dired-main-image-directory (concat user-emacs-directory "cache/image" )
 image-dired-temp-image-file (concat user-emacs-directory "cache/image-dired-tmp")
 thumbs-thumbsdir (concat user-emacs-directory "cache/thumbs-dir")

 dired-recursive-copies 'always         ;让 dired 可以递归的拷贝和删除目录。
 dired-recursive-deletes 'always       ;always表示不加询问
 dired-dwim-target t                   ;Dired试着猜处默认的目标目录
 ivy-dired-history-max 200
 dired-listing-switches "-alht"
 )

;; (if (equal system-type 'gnu/linux)
;;     (setq dired-listing-switches "--time-style=+%y-%m-%d/%H:%M  --group-directories-first -alhG")
;;   (setq dired-listing-switches "-alhG"))
;; (when (eq system-type 'darwin)
;;   ;; macos 使用emacs自带的ls-lisp来展示文件目录，
;;   ;; 下文用到lazy-dired-sort 进行排序时，mac自带的ls有些参数不支持
;;   (require 'ls-lisp)
;;   (setq-default ls-lisp-use-insert-directory-program nil))

;;(setq directory-free-space-args "-Pkh")
;;u原来绑定为unmark ,可以使用它的另一个绑定"*u"来完成
(require 'dired)
(define-key dired-mode-map "u" 'dired-up-directory) ;上层目录
(define-key dired-mode-map "y" nil)     ;给evil-mode 的y让位
;;change to another directory
;; (define-key dired-mode-map "c" 'dired)
;;(define-key dired-mode-map "q" 'kill-buffer-and-window)
(define-key dired-mode-map "g" nil)     ;给evil-mode 的gg让位
(define-key dired-mode-map "r" 'revert-buffer)


(define-key dired-mode-map (kbd "<C-m>" ) 'dired-find-file)
(define-key dired-mode-map (kbd "C-[ [ 1 m") 'dired-find-file)
;; 只显示匹配的文件 do filter  "/" 只显示匹配的文件
(define-key dired-mode-map  "/" 'dired-narrow) ;dired-narrow-fuzzy
(define-key dired-mode-map  (kbd "C-s") 'dired-narrow) ;dired-narrow-fuzzy
(with-eval-after-load 'dired-narrow
  (define-key dired-narrow-map (kbd "<C-m>") 'exit-minibuffer)
  (define-key dired-narrow-map (kbd "C-[ [ 1 m") 'exit-minibuffer)
  )
;; (dired-mark-unmarked-files "init" nil nil )
;; 临时忽略某些文件,用正则表达示  "z"跟/的作用相反
(define-key dired-mode-map (kbd "z")  'dired-omit-expunge)
;; (define-key dired-mode-map (kbd "M-=") 'dired-ediff)

;; 第一次跳到文件名处，C-aC-a才跳到行首，再次则跳回
(define-key dired-mode-map (kbd "C-a") 'dired-smart-beginning-of-line)
;;; wdired的配置
;; (define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)

;; wdired == writable dired
;; i后 进入可以对dired文件名 权限等可以修改的mode，同时evil-mode 可进行evil-insert-state
(setq-default wdired-allow-to-change-permissions t);; writable 时,不仅可以改文件名,还可以改权限
;; C-gC-g 退出编辑或C-cC-c保存修改
(define-key dired-mode-map "i" 'wdired-change-to-wdired-mode)


(with-eval-after-load 'wdired
  (define-key wdired-mode-map (kbd "C-a") 'dired-smart-beginning-of-line)
  (define-key wdired-mode-map (kbd "C-g") 'wdired-abort-changes))

(with-eval-after-load 'dired-x
  (define-key dired-mode-map "\M-o" 'dired-omit-mode)) ;不显示一些不重要的文件

;;; dired-x 增强的dired功能
(with-eval-after-load 'dired-x
  (add-hook 'dired-mode-hook 'dired-omit-mode);;M-o toggle 是否显示忽略的文件
  ;; 默认这些后缀的文件 不显示，M-o后才显示
  (setq dired-omit-files (concat dired-omit-files "\\|^.*~$\\|^#.*#$\\|^\\.svn$\\|.DS_Store\\|.go-imports-packages.el\\|\\.ccls-cache"))
  (setq dired-omit-extensions
        (append dired-omit-extensions (list ".beam" ".swp"  "CVS/")))

  )
;;in dired mode ,C-s works like "M-s f C-s" ,only search filename in dired buffer
;; (setq dired-isearch-filenames t )
;;不知道出什么原因,如果delete-by-moving-to-trash 设成t ,emacs --daemon 会启动失败
;; (setq delete-by-moving-to-trash t);;using trash
;; 如果buffer中有一个路径如/home/jixiuf/,光标移动到其上C-u C-x C-f,会以此路径默认路径
;; Make sure our binding preference is invoked.
;;(setq dired-x-hands-off-my-keys nil) (dired-x-bind-find-file)
;; Set dired-x global variables here.  For example:
;;定义哪些文件会忽略如.git


;;; np下下运动的时候光标始终在文件名上（evil-mode 的j k 不受此限制）
(defadvice dired-next-line (around dired-keep-point-on-filename-next activate)
  "Replace current buffer if file is a directory."
  ad-do-it
  (while (and  (not  (eobp)) (not ad-return-value))
    (forward-line)
    (setq ad-return-value (dired-move-to-filename)))
  (when (eobp)
    (forward-line -1)
    (setq ad-return-value(dired-move-to-filename))))

;; 把目录加入到load-path中，或load el文件
(define-key dired-mode-map "L" 'dired-add-to-load-path-or-load-it)
(define-key dired-mode-map "v" 'add-dir-local-variable)
;; (define-key dired-mode-map (kbd "C-o") 'golden-ratio-scroll-screen-down)


(define-key dired-mode-map "," 'dired)  ;
(define-key dired-mode-map "f" 'open-in-filemanager)  ;

;; 删除 copy 文件目录时 以异步的形式进行，以避copy大文件时emacs卡位无法进行其他操作
(with-eval-after-load 'dired-aux
  (add-to-list 'dired-compress-files-alist '("\\.tgz\\'" . "tar -c %i | gzip -c9 > %o")))
;; 根据后缀名对文件进行着色
(require 'dired-filetype-face)

;; 实现按文件大小 时间 扩展名 名称排序，默认绑定在s上如ss 按size排序
;;; 排序
;;;do sorting
;; 1. s s 按照文件大小排序。
;; 2. s x 按照文件扩展名排序。
;; 3. s t 按照文件访问时间排序。
;; 4. s n 按照文件名称的字母顺序排序。
;; 5. s C-s 原来的s 功能 ,C=u s C-s 可手动编辑ls 的命令
(require 'lazy-dired-sort)
(require 'vmacs-dired-single)           ;确保只有一个dired buffer的存在

;; 绑定之后，你访问过的dired都会被记录住，当你copy rename 及打开dired时，可以从这些
;; 已访问的目录中筛选以方便快速访问
(require 'vmacs-dired-history)



(provide 'conf-dired-dump)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-dired.el ends here.
