;;; -*- lexical-binding: t; -*-
;;; Code:
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
 dired-recursive-copies 'always         ;让 dired 可以递归的拷贝和删除目录。
 dired-recursive-deletes 'always       ;always表示不加询问
 dired-dwim-target t                   ;Dired试着猜处默认的目标目录
 dired-listing-switches "-alht"
 dired-kill-when-opening-new-dired-buffer t
 dired-vc-rename-file t)

;;u原来绑定为unmark ,可以使用它的另一个绑定"*u"来完成
(require 'dired)
(define-key dired-mode-map  "l"  #'forward-char)
(define-key dired-mode-map  "h"  #'backward-char)
(define-key dired-mode-map  "u" 'dired-up-directory );上层目录
  ;; 只显示匹配的文件 do filter  "/" 只显示匹配的文件
;; (define-key dired-mode-map  "C" 'dired-rsync)

(define-key dired-mode-map  (kbd "C-c M G") #'(lambda()(interactive) (end-of-buffer) (dired-previous-line 1)) )
(define-key dired-mode-map  (kbd "C-c M /") 'consult-focus-lines)
(define-key dired-mode-map  (kbd "C-c M z") 'consult-hide-lines)
(define-key dired-mode-map  (kbd "C-c M j") 'dired-next-line)
;; 第一次跳到文件名处，C-aC-a才跳到行首，再次则跳回
;; C-gC-g 退出编辑或C-cC-c保存修改
;; "i" 'wdired-change-to-wdired-mode
(define-key dired-mode-map  "\M-o" 'dired-omit-mode );不显示一些不重要的文件
(define-key dired-mode-map  "L" 'dired-add-to-load-path-or-load-it )
(define-key dired-mode-map  "V" 'add-dir-local-variable)
(define-key dired-mode-map  "i"  #'wdired-change-to-wdired-mode);上层目录
(define-key dired-mode-map  "o" 'consult-dir)
  (keymap-unset dired-mode-map "y" t)
(define-key dired-mode-map  "f" 'open-in-filemanager)
(define-key dired-mode-map  (kbd "<mouse-2>") #'dired-mouse-find-file)
(with-eval-after-load 'image-mode
  (define-key image-mode-map  (kbd "C-f") #'image-scroll-up)
  (define-key image-mode-map  (kbd "C-b") #'image-scroll-down)
(define-key image-mode-map  (kbd "C-c Mn") #'image-next-file))

;; wdired == writable dired
;; i后 进入可以对dired文件名 权限等可以修改的mode，同时evil-mode 可进行evil-insert-state
(setq-default wdired-allow-to-change-permissions t);; writable 时,不仅可以改文件名,还可以改权限


(with-eval-after-load 'wdired
  (define-key wdired-mode-map (kbd "C-g") 'wdired-abort-changes))

;;; dired-x 增强的dired功能
(with-eval-after-load 'dired-x
  (add-hook 'dired-mode-hook 'dired-omit-mode);;M-o toggle 是否显示忽略的文件
  ;; 默认这些后缀的文件 不显示，M-o后才显示
  (setq dired-omit-files (concat dired-omit-files "\\|^.*~$\\|^#.*#$\\|^\\.svn$\\|.DS_Store\\|\\.ccls-cache"))
  )

(with-eval-after-load 'dired-aux (add-to-list 'dired-compress-files-alist '("\\.tgz\\'" . "tar -c %i | gzip -c9 > %o")))
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

(when (require 'dired-async nil t)
(dired-async-mode 1))

(defun dired-next-line (arg)
  (interactive "^p")
  (let ((old-line (line-number-at-pos)))
    (beginning-of-line)
    (forward-line arg)
    (while (and (not (eobp)) (not (bobp)) (invisible-p (point)))
      (forward-line arg))
    (dired-move-to-filename)
    ;; (when (= 2 (line-number-at-pos))
    ;;   (if (> arg 0)
    ;;       (dired-next-line 1)
    ;;     (dired-next-line -1)
    ;;     (back-to-indentation)
    ;;     ))
    (when (and (= 1 old-line) (< arg 0))
      (goto-char (1- (point-max)))
      (dired-move-to-filename))

    (when (and (eobp) (> arg 0))
      (goto-char (point-min))
      (back-to-indentation))))

(with-eval-after-load 'doc-view
  (define-key doc-view-mode-map    (kbd "C-v") #'doc-view-next-page)
  (define-key doc-view-mode-map    (kbd "M-v") #'doc-view-previous-page))

(defun xdg-open (&optional filename)
  (interactive)
  (let ((process-connection-type))
    (start-process
     "" nil (cond ((eq system-type 'gnu/linux) "xdg-open")
                  ((eq system-type 'darwin) "open")
                  ((eq system-type 'windows-nt) "start")
                  (t "")) (expand-file-name
                  (or filename (dired-file-name-at-point))))))

(defun find-file-auto (orig-fun &rest args)
  (let ((filename (car args)))
    (if (cl-find-if
         (lambda (regexp) (string-match regexp filename))
         '("\\.pdf\\'" "\\.mp3\\'" "\\.mp4\\'" "\\.wav\\'" "\\.avi\\'" "\\.gif\\'" "\\.docx?\\'"))
        (xdg-open filename)
      (apply orig-fun args))))

(advice-add 'find-file :around 'find-file-auto)

(defun consult-focus-lines-dired()
  (run-with-timer
   0.001 nil #'(lambda()
                 (when (and consult--focus-lines-overlays
                            (eq major-mode 'dired-mode))
                   (if (dired-get-filename t t)
                       (dired-move-to-filename)
                     (dired-previous-line 1))
                   ))))
(add-hook 'minibuffer-exit-hook 'consult-focus-lines-dired)

(provide 'conf-dired)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-dired.el ends here.
