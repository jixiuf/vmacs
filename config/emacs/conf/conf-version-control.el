;;; -*- lexical-binding: t; -*-
;; smerge
;; cn next
;; cu （upper） 选择上面的部分，cl (lower) 选下部分
;; ca 两部分都要
;; cRET 选择光标下的

;; magit 可以通过s u 来只选中某一部分 hunk来提交
;; vc 也可以实现类似的功能，先产生一个diff-mode 的buffer ,然后通过编辑diff-mode
;; 如通过M-k 删除一个hunk ,C-cC-s 分隔一个hunk 从而更精细的只提交某一部分hunk
;; 然后在这个diff-mode 中进行C-xvv 即可只提交diff-mode 中的变更

(setq-default
 ;; 只让vc支持git svn , 可以加快vc的一些处理
 vc-handled-backends '(Git )         ;default '(RCS CVS SVN SCCS Bzr Git Hg Mtn Arch)
 vc-command-messages 'log
 vc-find-revision-no-save t
 vc-use-short-revision t
 log-edit-hook nil
 vc-follow-symlinks t
 vc-allow-rewriting-published-history 'ask
 vc-dir-hide-up-to-date-on-revert t
 diff-font-lock-prettify t
 vc-annotate-background-mode nil
 vc-suppress-confirm t                  ;;;自动保存当前buffer后进行操作 除非进行一个危险的操作,如回滚
 ;; vc-git-print-log-follow t ; 似乎与 vc-log-short-style 有冲突 C-xvl 展示异常
 ;; vc--inhibit-async-window t             ;vc-pull vc-push 不显示window
 vc-log-short-style '(directory file)   ;C-xvl列出当前文件的历史版本时 不展示详情
 ;; --textconv 以支持 gpg 文件的diff
 ;; 去除 "--ignore-space-change" 否则会导致在 diff-mode 中提交部分hunk的时候失败
 vc-git-diff-switches '("--ignore-space-at-eol" "--ignore-blank-lines"  "--textconv")
 ;; vc-git-revision-complete-only-branches
 ;; svn diff --help
 ;; -b (--ignore-space-change): 忽略空白数量的修改。
 ;; -w (--ignore-all-space): 忽略所有的空白。
 ;; --ignore-eol-style: 忽略行尾样式的改变。
 vc-svn-diff-switches '("-x --ignore-eol-style")
 diff-switches "-ubB"
 ;; diff-jump-to-old-file t 控制 diff-mode 中RET C-u 的行为，
 )

(defvar-keymap  vc-fetch-map
  "v" #'vc-push
  "o" #'vcgit-push-other
  (kbd "f") #'vcgit-pull-default
  (kbd "a") #'vcgit-fetch-all
  (kbd "t") #'vcgit-fetch-tags)
(defvar-keymap  vc-push-map
  "v" #'vc-push
  "o" #'vcgit-push-other
  "t" #'vcgit-push-tags)
(defvar-keymap  vc-tag-map
  "t" #'vc-create-tag
  "v" #'vcgit-push-tags
  "d" #'vcgit-git-delete)

(defvar-keymap  vc-r-map
  "i" #'vcgit-rebase-i                 ;in log view mode
  "r" #'vcgit-rebase                   ;  ;in log view mode
  "d" #'vc-revert                       ;丢弃当前未提交的修改
  "v" #'vcgit-revert-commit)           ;  ;in log view mode revert 某个commit
;; for rebase/cherry-pick/am/revert/merge --continue/--skip/--abort
(defvar-keymap  vc-action-map
  "c" #'vcgit-continue                 ;
  "a" #'vcgit-abort
  "s" #'vcgit-skip)

(defalias 'vc-create-plain-mail-patch 'vc-prepare-patch)

(defvar-keymap  vc-cherry-pick-map
  "a" #'vcgit-cherry-pick-commit       ;将其他分支的commit拿到当前分支
  "A" #'vcgit-am-apply-patches         ;apply git format-patch 产生的git 专有patch
  "y" #'vcgit-apply-plain-patches      ;apply 普通的diff patch
  "p" #'vc-create-plain-mail-patch                ;create plain patch by mail
  )

(defvar-keymap  vc-log-map
  "." #'vc-print-root-log
  "b" #'vc-print-branch-log
  "s" #'vc-log-search
  "e" #'vcgit-reflog
  "m" #'vc-log-mergebase
  "i" #'vc-log-incoming ;unpulled, 很少用的，已经将其插入到vc-dir中的
  "o" #'vc-log-outgoing ;unpushed, 很少用的，已经将其插入到vc-dir中的
  "r" #'vcgit-print-remote-branch)     ;log 肖前追踪的remote branch
(defvar-keymap  vc-branch-map
  "b" #'vc-switch-branch
  "d" #'vcgit-git-delete                   ;delete tag/branch/remote-branch
  "c" #'vc-create-branch
  "m" #'vc-merge)
;; ;git machine 的实现，顺序的遍历当前文件的历史
(global-set-key (kbd "M-p") 'vc-annotate)
(global-set-key (kbd "C-c vg") #'vc-annotate)
;; (global-set-key (kbd "C-c vd") #'magit-status)
(global-set-key (kbd "C-c vj") #'vc-dir-root) ;like dired-jump
(global-set-key (kbd "C-c vk") #'vc-dir-root) ;like dired-jump
(global-set-key (kbd "C-c vf") vc-fetch-map)
(global-set-key (kbd "C-c vc") #'vc-next-action)
(global-set-key (kbd "C-c vv") vc-push-map)
(global-set-key (kbd "C-c vp") #'vc-push)
(global-set-key (kbd "C-c vr") #'vc-revert)
(global-set-key (kbd "C-c vl") #'vc-print-log)
;; (global-set-key (kbd "C-c vl") #'magit-log-buffer-file)
(global-set-key (kbd "C-c v.") vc-log-map)
(global-set-key (kbd "C-c v=") #'vc-diff)
(global-set-key (kbd "C-c vh") #'vc-root-diff)
(global-set-key (kbd "C-c =") #'vc-diff-on-current-hunk)
;; (global-set-key (kbd "C-c +") #'vc-ediff)
(global-set-key (kbd "C-c vb") vc-branch-map)
(global-set-key (kbd "C-c vt") vc-tag-map)

(global-set-key (kbd "C-c vs") #'vcgit-stage)
(global-set-key (kbd "C-c vu") #'vcgit-unstage)
;; (global-set-key (kbd "C-c ve") #'magit-commit-extend)
;; (global-set-key (kbd "C-c va") #'magit-commit-amend)

(global-set-key (kbd "C-c vz") #'vc-git-stash)
(with-eval-after-load 'vc-git
  (define-key vc-git-stash-shared-map (kbd "C-c Ma") #'vc-git-stash-apply) ;my a for meow-motion
  (define-key vc-git-stash-shared-map (kbd "d") #'vc-git-stash-show-at-point)
  (define-key vc-git-stash-shared-map "z" #'vc-git-stash)
  (define-key vc-git-stash-shared-map "a" #'vc-git-stash-apply-at-point)
  (define-key vc-git-stash-map "A" #'vc-git-stash-pop-at-point)
  (define-key vc-git-stash-shared-map "x" #'vc-git-stash-delete-at-point))

(with-eval-after-load 'vc-dir
  ;; (setq outline-minor-mode-cycle t) ;tab
 ;; (setq outline-minor-mode-cycle-filter 'bolp) ; tab only work when at bol
 ;; (setq outline-minor-mode-use-buttons 'in-margins)
  (define-key vc-dir-mode-map (kbd "M-n") #'outline-next-visible-heading)
  (define-key vc-dir-mode-map (kbd "M-p") #'outline-previous-visible-heading)
  (define-key vc-dir-mode-map (kbd "C-i") #'vc-diff)
  (define-key vc-dir-mode-map (kbd "d") #'vc-diff)
  (define-key vc-dir-mode-map (kbd "a") vc-cherry-pick-map)
  (define-key vc-dir-mode-map (kbd "C-c Mq") vc-action-map) ;my q for meow-motion
  (define-key vc-dir-mode-map (kbd ".") vc-log-map)
  (define-key vc-dir-mode-map (kbd "f") vc-fetch-map)
  (define-key vc-dir-mode-map (kbd "s") #'vcgit-stage) ;for --continue
  (define-key vc-dir-mode-map (kbd "S") #'vcgit-unstage)
  (define-key vc-dir-mode-map (kbd "b") vc-branch-map)
  (keymap-unset vc-dir-mode-map "e" t)
  (define-key vc-dir-mode-map (kbd "r") vc-r-map)
  (define-key vc-dir-mode-map (kbd "x") #'vc-revert)            ;丢弃当前未提交的修改
  (define-key vc-dir-mode-map (kbd "C-d") #'vc-dir-clean-files) ;delete un added file
  (define-key vc-dir-mode-map (kbd "X") #'vc-dir-delete-file)   ;git rm
  (define-key vc-dir-mode-map (kbd "C-c Gd") #'vc-root-diff) ;gd for meow-motion
  (define-key vc-dir-mode-map (kbd "o") #'vc-switch-project)
  (define-key vc-dir-mode-map (kbd "i") #'vc-dir-ignore)
  (define-key vc-dir-mode-map (kbd "c") #'vc-next-action)
  (keymap-unset vc-dir-mode-map "v" t)
  (define-key vc-dir-mode-map (kbd "v") vc-push-map)
  (define-key vc-dir-mode-map (kbd "t") vc-tag-map)
  (define-key vc-dir-mode-map (kbd "C-c Gr") ;gr for meow-motion
              #'(lambda()(interactive) (revert-buffer) (vc-dir-hide-state)))
  (require 'vc-git)
  (define-key vc-dir-mode-map (kbd "C-c Mz") vc-git-stash-shared-map)) ;z for meow-motion

(define-advice vc-next-action (:around (orig-fun &rest args) default-mark-all)
  "Default mark all in *vc-dir*"
  (when (and (eq major-mode 'vc-dir-mode)
             (not (vc-dir-marked-files)))
    (vc-dir-mark-all-files nil))
  (apply orig-fun args))


(define-advice vc-dir-headers (:around (orig-fun &rest args) progress)
  (interactive)
  (let ((msg (apply orig-fun args)))
    (when (eq vc-dir-backend 'Git)
      (let ((gitdir (vc-git--git-path)))
        (setq msg (string-trim-left msg "VC backend : Git\n")) ;
        ;; (setq msg (string-trim-left msg "Working dir:.+?\n"))
        (setq msg (string-trim-right msg "Stash      : Nothing stashed\n"))
        (when (< emacs-major-version 31)
          (when (file-exists-p
	             (expand-file-name "rebase-apply/applying" gitdir))
            (setq msg (concat msg (propertize  "\nApply     : in progress"
                                               'face 'vc-dir-status-warning))))
          (when (file-exists-p (expand-file-name "MERGE_HEAD" gitdir))
            (setq msg (concat msg (propertize  "\nMerge     : in progress"
                                               'face 'vc-dir-status-warning))))
          (when (file-exists-p (expand-file-name "REVERT_HEAD" gitdir))
            (setq msg (concat msg (propertize  "\nRevert     : in progress"
                                               'face 'vc-dir-status-warning))))
          (when (file-exists-p (expand-file-name "CHERRY_PICK_HEAD" gitdir))
            (setq msg (concat msg (propertize  "\nCherry-Pick     : in progress"
                                               'face 'vc-dir-status-warning)))))
        ))
    msg))
(with-eval-after-load 'outline
  (keymap-unset outline-overlay-button-map "RET" t))


(with-eval-after-load 'log-view
  (require 'vc-dir)
  (define-key log-view-mode-map (kbd "a") vc-cherry-pick-map)
  (define-key log-view-mode-map (kbd "C-c Ga") #'log-view-annotate-version) ;ga
  (define-key log-view-mode-map (kbd "f") vc-fetch-map)
  (define-key log-view-mode-map (kbd "M-w") #'log-view-kill-revision)
  (define-key log-view-mode-map (kbd ".") vc-log-map)
  (define-key log-view-mode-map (kbd "b") vc-branch-map)
  (define-key log-view-mode-map (kbd "v") vc-push-map)
  (define-key log-view-mode-map (kbd "o") #'vc-switch-project)
  (define-key log-view-mode-map (kbd "r") vc-r-map)
  (define-key log-view-mode-map (kbd "x") #'vcgit-reset)
  (define-key log-view-mode-map (kbd "t") vc-tag-map)
  (define-key log-view-mode-map (kbd "SPC") nil)
  (define-key log-view-mode-map (kbd "g") nil)
  (define-key log-view-mode-map (kbd "C-i") #'log-view-diff)
  (define-key log-view-mode-map (kbd "i") #'log-view-toggle-entry-display)

  (define-key log-view-mode-map (kbd "RET") #'log-view-find-revision)
  )
(with-eval-after-load 'diff-mode
  ;; (define-key diff-mode-shared-map (kbd "s") #'vcgit-stage)
  ;; (define-key diff-mode-shared-map (kbd "u") #'vcgit-unstage)
  (add-hook 'diff-mode-hook #'outline-minor-mode)
  (define-key diff-mode-map (kbd "c") #'vc-next-action)
  (define-key diff-mode-map (kbd "d") #'outline-cycle)
  (define-key diff-mode-map (kbd "v") vc-push-map)
  (define-key diff-mode-map (kbd "x") #'diff-hunk-kill)
  (define-key diff-mode-map (kbd "X") #'diff-file-kill)
  )

;;;; `C-xvg' vc-annotate 查看某个特定文件自始至终的变化
;;位于info 的Emacs>>Maintaining>>Version Control>>Old Revisions
;; C-x v g     vc-annotate -- show when each line in a tracked file was added and by whom
;;`C-uC-xvg' 则不是对默认的当前buffer进行操作,让你选择?
;;某一个特定版本文件的内容在不同的版本都有增减,而vc-annotate 用不同的颜色表示文件中不同
;;代码的历史, 红色的部分是最近才添加的,蓝色的则是最初就加入的内容,中间过程添加的代码也会用不同的颜色进行标记

;;*Annotate* buffer 的格式是:右边是代码,左边则是右边每一行代码所对应的版本,也就是代表了这一行代码是在哪个版本
;;的时候添加进来的.
;;进入Annotate mode 后还可以进行其他操作

;;p  对此文件的上一个版本进行vc-annotate操作
;;n  ........下........................
;;j 对`当前行' 所对应的版本的当前文件进行vc-annotate操作,比如当前行的代码是在版本号为3的时候添加进来的,
;;  则此操作会对此文件版本为3时的内容进行vc-annotate操作
;;w  通过p n j 操作后有可能你忘记了当前buffer中的内容到底是哪个版本的,可以用w 回到最初运行`C-xvg' 时的版本
;;   w 表示working revision 其实就是最新的一个版本

;;a `当前行' 则相当于先进行j操作,然后进行p操作,其作用是查看还没有加入当前行的内容时的前一个版本对应的文件是什么样子的
;;f `当前行' file跟j类似,不过不进行vc-annotate操作,仅显示当前行对应版本的文件内容
;;d `当前行' diff操作,当前行对应一个版本,用此版本与它的前一个版本进行diff操作,即查看到底这一次的版本变化有哪些变化
;;D `当前行' diff操作,与d类似,不过此次显示的不仅是当前文件的diff,而是此次提交所有文件的变化.
;;l `当前行' log 显示日志 ,显示当前行所对应的版本 相应的日志
;;v 默认右边代码左边版本号,v 则toggle 是否显示版本号,用处不大.

(with-eval-after-load 'vc-annotate
  (define-key vc-annotate-mode-map (kbd "c") #'vc-annotate-revision-at-line) ;old j
  (define-key vc-annotate-mode-map (kbd "M-n") #'vc-annotate-next-revision)
  (define-key vc-annotate-mode-map (kbd "M-p") #'vc-annotate-prev-revision)
  (define-key vc-annotate-mode-map (kbd ".") #'vc-annotate-show-log-revision-at-line)
  (define-key vc-annotate-mode-map (kbd "C-c C-c") #'vcgit-save-revision)
  (define-key vc-annotate-mode-map [return] #'vc-annotate-goto-revision-line)
  ;; copy 的时候 忽略invisible 的文字，即 忽略 每行开头的 commit date author 等信息
  ;; 只copy 实际内容
  (defun vc-annotate-filter-visibile ()
    (require 'backtrace)
    (if (memq 'vc-annotate-annotation buffer-invisibility-spec)
        (setq-local filter-buffer-substring-function #'backtrace--filter-visible)
      (setq-local filter-buffer-substring-function #'buffer-substring--filter)))
  (advice-add 'vc-annotate-toggle-annotation-visibility :after #'vc-annotate-filter-visibile)

  (defun vc-annotate-annotation-invisibility(&rest args)
    ;; 默认隐藏每行开头的 commit date author 等信息,可通过v 来toggle之
    (add-to-invisibility-spec 'vc-annotate-annotation)
    (vc-annotate-filter-visibile)
          (vc-run-delayed
            (message "RET:goto rev,c:annotation rev at line v: toggle visible M-n/p: next/prev C-cC-c:reset to current rev"))
    (when vc-parent-buffer
      (rename-buffer (format "%s-%s" (buffer-name vc-parent-buffer)
                             (or (bound-and-true-p vc-buffer-revision)
                                 (bound-and-true-p vc-annotate-parent-rev)))
                     t)))
  (advice-add 'vc-annotate :after #'vc-annotate-annotation-invisibility)
  (advice-add 'vc-annotate-next-revision :after #'vc-annotate-annotation-invisibility)
  (advice-add 'vc-annotate-prev-revision :after #'vc-annotate-annotation-invisibility))
(defun vc-find-revision-func (file revision &optional backend)
  (local-set-key (kbd "C-c C-c") #'vcgit-save-revision))
(advice-add 'vc-find-revision :after #'vc-find-revision-func)

(add-hook 'vc-dir-mode-hook #'vc-remember-project)
(add-hook 'vc-dir-mode-hook #'vcgit-global-minor-mode)

(with-eval-after-load 'git-link
  (defun git-link-gitlab (hostname dirname filename branch commit start end)
    (format "https://%s/%s/blob/%s/%s"
            hostname dirname
            (or branch commit)
            (concat filename
                    (when start
                      (concat "#"
                              (if end
                                  (format "L%s-%s" start end)
                                (format "L%s" start))))))))


;;有一个旧的文件a , 你编辑了a将这个编辑后的文件命令为b
;;现在想生成一个补丁文件,将这个补丁文件应用到a 上,就会变成b
;;生成这个补丁文件的命令是diff
;; diff -ubB a b>a.patch  (-u指定生成的格式,-b忽略空格-B忽略空格引起的差异)
;;这样在当前目录下会生成a.patch的文件,
;;这样你可以将你的补丁文件发布到网上,别人拿到你的补丁及a文件 放在同一个目录
;;patch -p0 <a.patch a  这样打上补丁后,a中的内容就与b中的内容无异
;;可是你后悔了,不起打这个补丁,想就a恢复原样
;;patch -R <a.patch a  这样a文件就变成了最初的模样了.
;;diff mode 像Compilation mode 一样,可以用C-x` `C-cC-c' 在各个条目间跳转


;; Alternatively, if you’d rather stick with Subversion’s built-in diff tool,
;; you can pass Subversion-specific diff switches by setting
;; `vc-svn-diff-switches` to a string or list of strings.
;; For example, to tell `svn diff` to ignore EOL conventions and other whitespace, use
;; (when (equal system-type 'windows-nt) (setq-default vc-git-program (expand-file-name "~/.emacs.d/binw32/gitsh.exe")))


;;;; comments
;; C-x v v     vc-next-action -- perform the next logical control operation on file 会根据当前文件状态决定该做什么
;; 1.如果当前的文件(work file)不在任何一个version control 管理下,则询问你创建什么样的仓库,如svn git等.
;; 2.如果在管理下,则register the file. 即git add filename.
;; 3.如果work file 与库中的文件一样,do nothing.
;; 4.若不一样,则进行merge (checkout 或update) 操作. 即更新(好像并不更新,更新需要`C-xvu')
;; 5.如果你对work file 进行的修改则进行checkin(即commit)操作,它会打开一个*VC-LOG*buffer让你输入日志,关于*VC-LOG* 见下面的注释
;; 6.如果有冲突则先merge 最新的文件到work file,此时work file 处于冲突状态,需要解决冲突,继续`C-xvv'后说明冲突已解决此后再`c-xvv'则提交
;;`C-uC-xvv' 可以选择进入哪个分支,reversion,

;; C-x v i     vc-register -- add a new file to version control  ;;相当于git add .将文件加入到版本管理当中

;; C-x v ~     vc-version-other-window -- look at other revisions
;;             查看此文件以前的版本对应的内容,需要输入版本号,git 操作不太方便,因为版本号不是递增的数字,而是SHA1值
;; C-x v =     vc-diff -- diff with other revisions
;;             对未提交的文件与最新的版本对应的文件进行diff操作,C-u可以选择用哪两个版本,不仅可以单文件diff,
;;             可以是fileset,如何对多文件进行操作看vc-dir mode `C-xvd' 类似于dired, ibuffer.
;; C-x v D     同`C-xv=' ,不过是对所有的文件进行与最新的版本进行diff操作(`C-xv='需要选择操作哪些文件),即显示最近进行了哪些未提交的修改
;; C-x v u     vc-revert-buffer -- undo checkout  放弃对文件的修改,即重新update 一下.
;;;; 查看日志 `*vc-change-log*' buffer
;; C-x v l     vc-print-log -- show log (not in ChangeLog format) 显示日志,只显示当前文件有关的日志
;; 这个打开的日志buffer 功能绝对不止显示日志这么简单，
;; 你可以按下C-hb 查看一下它的键绑定，
;; 比如= 是进行diff比较，默认是最新的版本，与你光标下的版本进行比较
;; 也可以用m ,mark 两个版本后，然后= ,将其进行比较
;; C-x v L     `vc-print-root-log' 显示日志,显示所有日志
;; 在*vc-change-log*buffer 中可以进行以下操作
;; `p' 跳转到前一个日志条目
;; `n' 跳转到下一个日志条目
;; `P'
;;      Move to the log of the previous file, when the logs of multiple
;;      files are in the log buffer (*note VC Directory Mode::).
;;      Otherwise, just move to the beginning of the log.  A numeric
;;      prefix argument is a repeat count, so `C-u 10 P' would move
;;      backward 10 files.
;; `N'
;;      Move to the log of the next file, when the logs of multiple files
;;      are in the log buffer (*note VC Directory Mode::).  It also takes a
;;      numeric prefix argument as a repeat count.

;;`a'  对当前对应的版本进行annotate 操作,详见`C-xvg' ,下面有注释
;; e   重新编辑当前的日志内容,并不是所有的管理工具都支持
;; f   查看引版本的文件对应的内容,相当于`C-xv~' 然后输入版本号的操作,对git 来说比`C-xv~'方便
;; d   diff 对此版本与前一个版本的当前文件diff操作
;; D   diff ,同d ,不过是所有的文件,
;;;; ChangeLog 文件
;;`C-x4a' 在ChangeLog文件中添加一个条目,关于当前文件的修改的,当前日期的.
;;`C-xva' 根据version control 日志自动生成ChangeLog,不过svn git 目录还不支持.
;;在编辑ChangeLog时
;;`C-j' 自动缩进
;;C-x`  打开此条目对应的文件
;;;; `C-xvd' vc-dir  多文件操作
;; C-x v d     vc-directory -- show all files which are not up to date
;;             操作有点类似dired ,它是VC 支持多文件操作的方式,在*vc-dir* buffer 中会显示处于version control管理下的文件
;;             不过默认up-to-date 的文件及相应的子目录会被隐藏,例外是这个up-to-date 的文件是刚刚被你提交导致的,则不隐藏.
;;            其格式如下
;;           ./
;;           modified           file1.c
;;           needs-update       file2.c
;;           needs-merge        file3.c
;;           unregistered        g.c

;;           其中很多命令类似于dired
;;           n p TAB SPC 上下箭头进行导航
;;           `RET' 和f 打开相应文件 ,o 在另外一个窗口打开
;;           q 退出
;;           x 隐藏所有up-to-date的文件
;;           m 对文件进行标记,然后可以对标记的文件进行操作,如commit提交
;;          `M' 标记所有与当前文件状态相同的文件
;;           u 与U则是m M相反的操作
;;           对标记的文件或者当前文件的内容进行搜索替换
;;           `S' searches the marked files.
;;           `Q' does a query replace on the marked files.
;;           `M-s a C-s' does an incremental search on the marked files.
;;           `M-s a C-M-s' does an incremental search on the marked files.
;;
;;           另外以`C-xv'为前缀的命令在vc-dir buffer中都有对应的短的键绑定
;;           如l 对应 `C-xvl' 查看日志
;;            `=', `+'`l', `i',`v'
;;           对多文件进行操作时,文件必须处于相同的状态,或者兼容态
;;            (added, modified and removed states 为兼容态

;; (C-x C-q    by default, C-x C-q is no longer bound, so it's better to use the above binding)
;; C-x v c     vc-cancel-version -- delete the latest revision (often it makes more sense to look at an old revision
;;             and check that in again!) 回滚操作
;;             git svn 现在还不支持,

;; C-x v s     vc-create-snapshot -- tag all the files with a symbolic name ,
;;             创建标签tag ,git 相当于git tag newTAGname
;; C-x v r     vc-retrieve-snapshot -- undo checkouts and return to a snapshot with a symbolic name
;;             git 相当于git checkout newTAGname ,会处于一个无名的branch 此时work dir中的文件都是tagName时的版本

;; C-x v a     vc-update-change-log -- update ChangeLog

;; C-x v m     vc-merge
;; C-x v h     vc-insert-headers

;;;; VC-LOG

;;关于*VC-LOG*  进入这个buffer 后
;; `C-cC-c' 完成日志的填写,commit.
;; `C-cC-f' 显示这次提交有哪些文件作了修改,不过如果是在直接编辑某个work file时执行`C-xvv' 则显示的仅是当前work file
;;          用处不大,如果是在`C-xvd'进入vc-dir模式进行多文件操作后,执行`C-xvv'进入vc-log则`C-cC-f'显示的才是操作的多文件
;; `C-cC-d' 显示diff.
;;  在minibuffer中我们可以用`M-p'前一个 `M-n'下一个 `M-r'向后搜索 `M-s'向前搜索 等查看以往的历史,
;;  同样在*VC-LOG*中也可以查看以往的提交历史.操作相同.


;;;; diff

;;;;; 关于diff ,patch 补丁的使用


;;注意linux下的diff a b ,其中a 是旧文件,b是新文件
;;在Emacs中`M-x' diff  先就你选择的是b然后才是a
;; 一个hunk 就是一处: @@ -130,7 +130,7 @@

;; `M-n' 跳到下一个差异处(hunk)
;; `M-p' 跳到上一个差异处(hunk)
;; `M-}' 跳到下一个文件 (在多文件补丁中)
;; `M-{'
;; `M-k' 删除这个(hunk)
;; `M-K' 删除关于这个文件的(hunk)
;;`C-cC-a' 将当前的hunk打到旧文件中 `diff-apply-hunk'
;;          `C-u' 则进行相反的操作,注意如果这个hunk已经打过
;;          再运行`C-cC-a'会问你是否reverse反向操作

;;`C-cC-b' 高亮显示到时底有哪些删减`diff-refine-hunk'

;;`C-cC-c' 查看旧的文件`diff-goto-source'
;;`C-cC-e' 起一个Ediff会话`diff-ediff-patch'
;;`C-cC-n' `diff-restrict-view' 就是Narrowing ,只显示当前hunk的内容`C-xnw' 相反操作widen之
;;         `C-u',则对文件而非hunk
;;`C-cC-r'  `diff-reverse-direction' 交换新老文件(diff a b 变成diff b a)
;;

;; `C-c C-s'
;;      Split the hunk at point (`diff-split-hunk').  This is for manually
;;      editing patches, and only works with the "unified diff format"
;;      produced by the `-u' or `--unified' options to the `diff' program.
;;      If you need to split a hunk in the "context diff format" produced
;;      by the `-c' or `--context' options to `diff', first convert the
;;      buffer to the unified diff format with `C-c C-u'.

;; `C-c C-d'
;;      Convert the entire buffer to the "context diff format"
;;      (`diff-unified->context').  With a prefix argument, convert only
;;      the text within the region.
;; `C-c C-u'
;;      Convert the entire buffer to unified diff format
;;      (`diff-context->unified').  With a prefix argument, convert
;;      unified format to context format.  When the mark is active, convert
;;      only the text within the region.

;; `C-c C-w' 重新生成diff文件,此次忽略空格
;;      Refine the current hunk so that it disregards changes in whitespace
;;      (`diff-refine-hunk').

;; `C-x 4 A'
;;      Generate a ChangeLog entry, like `C-x 4 a' does (*note Change
;;      Log::), for each one of the hunks
;;      (`diff-add-change-log-entries-other-window').  This creates a
;;      skeleton of the log of changes that you can later fill with the
;;      actual descriptions of the changes.  `C-x 4 a' itself in Diff mode
;;      operates on behalf of the current hunk's file, but gets the
;;      function name from the patch itself.  This is useful for making
;;      log entries for functions that are deleted by the patch.
;; `M-x diff-show-trailing-whitespaces RET'
;;      Highlight trailing whitespace characters, except for those used by
;;      the patch syntax (*note Useless Whitespace::).
;; (eval-after-load 'diff-mode
;;   ;;为*Diff* mode 设置高度face
;;   '(require 'diff-mode-)
;;  )
;;;; Ediff
;;Ediff常用的命令
;; `ediff-files' `ediff-current-file' `ediff-directories'
;; `edir-revisions' `edir-merge-revisions' `ediff-show-registry'
;; `edir-merge-revisions-with-ancestor'
;; `ediff-revision' `ediff-patch-file'
;; `ediff-merge-files' `ediff-merge-files-with-ancestor'
;; `ediff-merge-directories' `ediff-merge-revisions'

;; `v'            scroll A and B
;; `V'            scroll the buffers down
;; `wd'           write diff to a file
;; `wb' `wc'`wa'  Saves buffer A, if it was modified.
;; `a'  `b' `c'   把A中相前的的difference region copy到B中相应相应位置,`rb' 可以恢复B到原状
;; `ab'           同`a'不过是在3个文件对比的时候
;; `p' `n'         选中上一个(下一个)difference region
;; `j' `-j' `Nj';; `n' 与'p'是相对跳转,此为绝对跳转,N是数字,表示跳到第N个difference region,
;; -j表示跳到跳后
;; `ga'              将A中跳(point)最近的difference region选中
;; `!'               Recomputes the difference regions ,防止因为修改导致高亮出错
;;  `m'             调整窗口在大小尽量大(toggle
;;   `|'            toggle 是水平还是垂直摆放两个window
;;   'r'            重置merge中的内容到未修改前(只在merge会话中有用)
;;   `ra' `rb' `rc' ::: `a' `b' `c' undo操作 (只在compare会话中有用)
;;   `##'          跳过只因 空格TAB不同引起的difference
;;   `#c'          跳过只因 大小写不同引起的difference
;;`#h' `#f'       处理因为大量相同的变量替换引起的difference
;;`A' `B' `C'    toggle Read-Only in A(B,C)
;; ~             交换A B窗口
;; i            显示当前进行的Ediff Session的信息,如在对哪两个文件进行对比等
;; D             显示diff命令的输出结果,生成diff文件
;; R             显示所有可用的Ediff Session,基本就是历史浏览`ediff-show-registry'
;; z            暂时挂起,(关闭相关窗口,) 可以`R' 进行恢复会话
;; q               quit.
;;`/' Displays the ancestor file during merges.
;; s            收缩merge窗口(toggle) ,`4s' 则增大4行
;; +            合并A B 的当前 difference region
;; =           启用一个新的子会话对当前difference region进行对比

;; log-view-diff 默认绑定在=上
;; c-xvl列出当前文件的历史版本
;; 此函数可以对各个历史版本进行比较
;; 使用方法在你在比较的两个版本中分别用m标记一下
;; 然后调用此函数即可
;;;; log-view-diff  "如果mark了两个entity ,则对此mark的进行对比"
;; (define-advice log-view-diff (:around (orig-fun &rest args) diff-marked-two-entity)
;;   (let (pos1 pos2 (marked-entities (log-view-get-marked)))
;;     (if (= (length marked-entities) 2)
;;         (progn
;;           (setq pos1 (progn (log-view-goto-rev (car marked-entities)) (point)))
;;           (setq pos2 (progn (log-view-goto-rev (nth 1 marked-entities)) (point)))
;;           (apply orig-fun (if (< pos1 pos2) (list pos1 pos2) (list pos2 pos1)) )
;;           )
;;       (apply orig-fun args))))
;; 下面的vc-run-delayed 用法挺好的，可以等待buffer中的进程结束后再执行代码
;; (define-advice vc-dir-root (:after (&rest _) commits-behind)
;;   (let ((buffer (get-buffer-create "*vc-commits-behind*")))
;;     (vc-call-backend (vc-deduce-backend) 'log-incoming  buffer "")
;;     (vc-run-delayed
;;       (with-current-buffer buffer
;;         (let ((lines (count-lines (point-min) (point-max))))
;;           (unless (zerop lines)
;;             (display-warning :warning (format "%s commits behind" lines))))))))

(provide 'conf-version-control)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; conf-version-control.el ends here.
