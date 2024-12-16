;; 目前我采用的方案是 mbsync 同步邮件到本地
;; 使用notmuch 对邮件进行索引,gnus使用notmuch的搜索功能
;; 从而利用gnus 的nnselect+gnus-search-notmuch功能对邮件进行分组
;; 可以实现以下分组:
;; 1. unread (需要利用 notmuch 的tag）
;; 2. 根据cc to bcc 等header 对mail list 的邮件进行分组
;; 这种方式 因邮件在本地,notmuch索引搜索也很快，几乎无卡顿
;;
;; 我的多个邮箱 通过自动转发功能 汇总到一个邮箱，mbsync同步时只同步这一个，速度会快一些

;; https://forums.freebsd.org/threads/do-you-use-emacs-gnus.41969/
;; https://ericabrahamsen.net/tech/2014/oct/gnus-dovecot-lucene.html
(when (member (system-name) '("jxfhome" "jxfluoji"))
  (load  (concat user-emacs-directory "conf/conf-private.el.gpg") t))

;; 转发:C-c C-f forward ,可在邮件列表中用 # 选多个 合并转发
;; C-c C-a – Attach file
;; C-c C-d – Save message as draft
;; C-c C-b: goto body
;; C-c C-f C-t: goto To:
;; C-c C-f C-o: goto From:
;; C-c C-f C-c: goto Cc:
;; C-c C-c :send
;; C-c C-k : 丢弃
;; nndraft:drafts 中的草稿可以e 后编辑
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-user user-mail-address
      smtpmail-smtp-server "smtp.qq.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      smtpmail-debug-info t
      smtpmail-debug-verb t)

;; Message 根据from: 自动设置 "X-Message-SMTP-Method" 功能。
;; 当已经有X-Message-SMTP-Method header时 此方法无效
(defun vmacs-message-server-alist-function ()
  "guess smpt server by From: header"
  (let* ((from (cadr (mail-extract-address-components
                      (save-restriction
                        (widen)
                        (message-narrow-to-headers-or-head)
                        (message-fetch-field "From")))))
         (auth (auth-source-search :user from :protocol "smtp" :require '(:host))))
    (when auth
      (let* ((entry (car auth))
             (host (plist-get entry :host))
             (port (plist-get entry :port)))
        (format "smtp %s %s %s" host (or port "465") from)))))
;; machine smtp.qq.com port 465 login yourmail@qq.com  protocol smtp  password yourpasswd
(setq message-server-alist '((vmacs-message-server-alist-function)))


;; https://www.bounga.org/tips/2020/05/03/multiple-smtp-accounts-in-gnus-without-external-tools/
;; 回复消息时自动设置X-Message-SMTP-Method
(setq gnus-posting-styles               ;C-h S check info of gnus-posting-styles
      `((".*" ; Matches all groups of messages with default qq
         (address (concat (format "%s <%s>" ,user-full-name ,user-mail-address) ))
         ("X-Message-SMTP-Method" (concat "smtp smtp.qq.com 465 " ,user-mail-address)))
        ((header "to" ,user-mail-address-2)
         (address ,user-mail-address-2)
         ("X-Message-SMTP-Method" (concat "smtp smtp.qq.com 465 " ,user-mail-address-2)))
        ((header "to" ,user-mail-address-3)
         (address ,user-mail-address-3)
         ("X-Message-SMTP-Method" (concat "smtp smtp.gmail.com 465 " ,user-mail-address-3)))
        ("vmacs" ; Matches Gnus group called "vmacs"
         (address ,user-mail-address-2)
         ("X-Message-SMTP-Method" (concat "smtp smtp.qq.com 465 " ,user-mail-address-2)))))

;; (setq gnus-search-ignored-newsgroups "nndraft:drafts")
;; (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
(with-eval-after-load 'message
  ;; From: 后 tab 键 切换 发送邮箱
  (add-to-list 'message-completion-alist (cons "^\\([^ :]*-\\)?\\(From\\):" #'message-toggle-from)))

;; user-work-mail-address user-mail-address-2 等变量为我其他两个邮箱地址
(defun message-toggle-from()
  "Toggle addresses in the From: field of the message buffer."
  (interactive)
  (save-excursion
    (message-goto-from)
    (cond
     ((string-match (concat "^from:\s-*.*" user-mail-address) (thing-at-point 'line))
	  (beginning-of-line)
	  (message-delete-line)
      (insert (concat "From: " user-full-name " <" user-work-mail-address ">\n")))
     ((string-match (concat "^from:\s-*.*" user-work-mail-address) (thing-at-point 'line))
	  (beginning-of-line)
	  (message-delete-line)
      (insert (concat "From: " user-mail-address-2 "\n")))
     (t
	  (beginning-of-line)
	  (message-delete-line)
	  (insert (concat "From: " user-full-name " <" user-mail-address ">\n"))))))

;;; sending mail end here.

;; Make all mails visible:
;; Select a group and press C-u RET in “Group Buffer”. Or C-u M-g in “Summary Buffer”.
(with-eval-after-load 'gnus-group
  ;; L or l (meow:caplock+l) 显示所有group
  ;; 这些配置是因我是meow 用户，我对"g" "G" 做了一些定制
  (define-key gnus-group-mode-map (kbd "C-c Mn") #'gnus-group-next-unread-group)     ;old  n
  (define-key gnus-group-mode-map (kbd "C-c MG") gnus-group-group-map)     ;old  G
  (define-key gnus-group-mode-map (kbd "C-c M/") #'gnus-group-read-ephemeral-search-group)     ;old  GG now /
  (define-key gnus-group-mode-map (kbd "C-c Gu") #'mbsync)                 ;gu
  (define-key gnus-group-mode-map (kbd "C-c Gr") #'gnus-group-get-new-news))     ;old  g, now gr
(with-eval-after-load 'gnus-sum
  ;; d:标记为已读  C-k:整个subject 已读
  ;; r 回复
  ;; 交换t T 后 tk:标记当前thread子分支为已读
  (define-key gnus-summary-mode-map  "t" #'gnus-summary-thread-map)     ;old T
  (define-key gnus-summary-mode-map  "T" #'gnus-summary-toggle-header)     ;old t
  (define-key gnus-summary-thread-map "t" #'gnus-summary-toggle-threads)   ;tt :切换是否thread old:TT
  (define-key gnus-summary-mode-map (kbd "C-c Gu") #'mbsync)                 ;gu
  (define-key gnus-summary-mode-map (kbd "C-c Gr") #'gnus-summary-rescan-group);gr old M-g
  (define-key gnus-summary-mode-map (kbd "C-c M/") #'gnus-summary-limit-map);/
  ;; 见下面 关于gnus-widen-article-window 的注释，用于实现类似于 mu4e 查看article时隐藏summary的样式
  (define-key gnus-summary-mode-map  (kbd "C-m") #'(lambda()(interactive)
                                                     (call-interactively #'gnus-summary-scroll-up)             ;old RET
                                                     (gnus-summary-select-article-buffer) ;old h
                                                     ))

  (define-key gnus-summary-mode-map  "r" #'gnus-summary-mark-as-read-forward)     ;old d mark readed
  (define-key gnus-summary-mode-map  "d" #'gnus-summary-mark-as-expirable)     ;old E delete mail
  (define-key gnus-summary-mode-map  "v" #'gnus-summary-next-page)     ;old space
  (define-key gnus-summary-mode-map (kbd "C-c MG") gnus-summary-goto-map)     ;old gnus G
  (define-key gnus-summary-mode-map (kbd "C-c Gr") #'gnus-summary-show-article) ;gr
  ) ;old gnus g ,now gr
(with-eval-after-load 'gnus-art
  (define-key gnus-article-mode-map (kbd "C-c MG") gnus-summary-goto-map)     ;old gnus G
  ;; 下面几个key 通过在article buffer 中直接实现next/prev article
  ;; 需要gnus-widen-article-window=t
  (define-key gnus-article-mode-map  (kbd "C-m") #'gnus-article-show-summary)     ;old h/s
  (define-key gnus-article-mode-map  (kbd "C-j") (kbd "C-m GN C-m"))     ;next article
  (define-key gnus-article-mode-map  (kbd "C-k") (kbd "C-m GP C-m"))     ;prev article
  (define-key gnus-article-mode-map  (kbd "M-n") (kbd "C-m Gn C-m"))     ;next unread article
  (define-key gnus-article-mode-map  (kbd "M-p") (kbd "C-m Gp C-m"))     ;prev unread article
  )

(setq gnus-interactive-exit nil)     ;退出时不必确认
(setq gnus-expert-user t)
;; GG 搜索支持 subject:keyword from:keyword body:keyword cc:keyword tag:notmuch
;; 可以 #选多个group 进行搜索
;; 搜索后可以 C-cC-p 将这个搜索结果保存为一个nnselect:组
;; Gg 则后将搜索结果保存为一个virtual group
;; 另外可以 GV 创建空的virtuall group ,然后 Gv 将其他group加入到那个空组
;; 如将所有邮箱的inbox 加到这个虚组中
(setq gnus-search-use-parsed-queries t) ;GG search group, and / :  limit in summary buffer

;; nndraft 会导致在group 上GG搜索时失败，
;; 我不想显示这个nndraft 目前没找到办法，可以使用u subscribed
;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Drafts.html
;; (with-eval-after-load 'gnus-search ; 这段没生效
;;   (add-to-list 'gnus-search-default-engines '((nndraft . gnus-search-notmuch))))

(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods
      `((nnmaildir ,user-full-name
                   (directory "~/maildir/qq")
                   (gnus-search-engine gnus-search-notmuch
                                       (remove-prefix ,(expand-file-name"~/maildir/qq"))
                                       (config-file ,(expand-file-name "~/.notmuch-config"))))
        (nnmaildir "vmacs"  (directory "~/maildir/vmacs")
                   ;; (nnmaildir-directory "~/maildir/vmacs")
                   (gnus-search-engine gnus-search-notmuch
                                       (remove-prefix ,(expand-file-name"~/maildir/vmacs"))
                                       (config-file ,(expand-file-name "~/.notmuch-config"))))
        ;; (nntp "news.gmane.io")
        ;; (nntp "news.gwene.org")
        ;; (nntp "news.gmane.org")
        ;; (nnimap ,user-mail-address
        ;;         (nnimap-address "localhost")
        ;;         ;; (nnimap-inbox "inbox")
        ;;         (nnimap-expunge t)
        ;;         ;; (nnimap-user ,user-full-name)
        ;;         (nnimap-server-port 143)
        ;;         (nnimap-stream network))
        ;; (nnimap ,user-full-name
        ;;         (nnimap-address "imap.qq.com")
        ;;         (nnimap-inbox "INBOX")
        ;;         (nnimap-expunge t)
        ;;         (nnimap-server-port 993)
        ;;         (nnimap-stream ssl))
        ))
;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Group-Parameters.html
(setq gnus-parameters
      `(("nnvirtual:.*"
         (gnus-show-threads t)
         (gnus-article-sort-functions '((not gnus-article-sort-by-number))) ;not 是倒序的意思
         (gnus-use-scoring nil)
         (display . 500))
        ("nnselect:.*"
         (gnus-show-threads t)
         ;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Selection-Groups.html
         ;; 如果不加(nnselect-rescan t)
         ;; g: gnus-group-get-new-news 的时候 并不会重新搜索以刷新nnselect 的group内容
         ;; 但我的nnselect 是notmuch本地搜索返回的结果相对较快，故打开此开关
         (nnselect-rescan t)
         ;; 默认情况下newsrc 会缓存搜索的结果(nnselect-always-regenerate t) 后则不缓存
         ;; 每次都重新生成
         (nnselect-always-regenerate t)
         ;; C-c C-s C-a 排序 author, C-c C-s C-d:date
         (gnus-article-sort-functions '((not gnus-article-sort-by-number))) ;not 是倒序的意思
         (gnus-use-scoring nil)
         (display . 500))
        (,(format "nnmaildir.*%s:.*" user-full-name)
         (gnus-show-threads nil)
         (gnus-article-sort-functions '((not gnus-article-sort-by-date))) ;not 是倒序的意思
         ;;expiry-wait expire-group 对gnus-secondary-select-methods中配的maildir 似乎未生效
         ;; (expire-group .  "nnmaildir+jixiuf:Deleted Messages")
         ;; (expiry-wait . immediate)              ;E的邮件，多久后真正删除 see nnmail-expiry-wait
         (gnus-use-scoring nil)
         (display . 500)  ;C-u ret 可指定别的数量big enouch without confirm
         ;; (display . all)
         )
        ("inbox"
         ;; "nnmaildir.*jixiuf:.*"中配置的expiry-wait expire-group
         ;; 对gnus-secondary-select-methods中配的maildir 似乎未生效
         ;; 比如说我的group是nnmaildir+jixiuf:inbox 它应该匹配gnus-parameters中"nnmaildir.*jixiuf:.*"
         ;; 中配置的parameters,但并没有,所以我下面又单独加了 "inbox",Sent Messages等的规则
         ;; 删除后移动哪个组  nnmail-expiry-wait  nnmail-expiry-target
         ;; 也可以使用Bm 移动邮件的操作，来实现挪到“已删除邮件箱的功能”
         (expiry-wait . immediate)              ;E的邮件，多久后真正删除 see nnmail-expiry-wait
         ;;  ;;; 删除后移动哪个组 nnmail-expiry-target
         (expire-group .  ,(format "nnmaildir+%s:Deleted Messages" user-full-name))
         )
        ("Sent Messages\\|Drafts"
         (gnus-show-threads nil)
         (gnus-article-sort-functions '((not gnus-article-sort-by-number))) ;not 是倒序的意思
         (expiry-wait . immediate)              ;E的邮件，多久后真正删除 see nnmail-expiry-wait
         (expire-group . ,(format "nnmaildir+%s:Deleted Messages" user-full-name))
         (display . 500)  ;C-u ret 可指定别的数量big enouch without confirm
         (gnus-use-scoring nil))
        ("Deleted Messages\\|Junk"
         (gnus-show-threads nil)
         (gnus-article-sort-functions '((not gnus-article-sort-by-date))) ;not 是倒序的意思
         (expiry-wait . immediate)              ;E的邮件，多久后真正删除 see nnmail-expiry-wait
         (display . 500)  ;C-u ret 可指定别的数量big enouch without confirm
         (gnus-use-scoring nil))))
;; (setq mm-discouraged-alternatives '( "text/html" "text/richtext"))
(when window-system
  (setq gnus-sum-thread-tree-indent " ")
  (setq gnus-sum-thread-tree-root "") ;; "● ")
  (setq gnus-sum-thread-tree-false-root "") ;; "◯ ")
  (setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)))
;; (setq gnus-thread-sort-functions (quote ((not gnus-thread-sort-by-most-recent-number))))
(setq gnus-use-dribble-file nil)
(setq gnus-always-read-dribble-file nil)
(setq gnus-save-newsrc-file nil)
(setq gnus-read-newsrc-file nil)
(setq gnus-fetch-old-headers t)
(setq gnus-article-date-headers '(local))
;; 默认展示article 上，最上方25%为summary 所占，
;; M-x:gnus-summary-select-article-buffer (default:h) 此选项则仅展示article
;; summary buffer 中 s/h 是一对命令，h 相当于hide summary,s 则show summary
;; 会根据gnus-widen-article-window 的值 来决定 summary buffer是否展示
(setq gnus-widen-article-window t)
(setq gnus-single-article-buffer t)
(setq gnus-summary-mode-hook 'hl-line-mode)
(setq gnus-group-mode-hook  '(gnus-topic-mode hl-line-mode))
;; (setq gnus-message-archive-method '(nnmaildir "archive" (directory "~/maildir/archive")))
;; gnus-level-subscribed:5
(setq gnus-group-line-format "Lv%L\ %M\ %S\ %p\ %P\ %5y/%-5t:%B%(%g%)\n")
;; https://www.math.utah.edu/docs/info/gnus_5.html#SEC51
(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       ;; "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
       "%3{│%}" "%3{%-16,16&user-date;%}" "%3{│%}" ;; date
       ""
       "%4{%-12,12f%}"               ;; name
       " "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%S\n"))
;; (setq gnus-activate-level 4)
;; gnus-summary-line-format 内通过%&user-date; 自定义时间格式
(setq gnus-user-date-format-alist (quote (
					  ((gnus-seconds-today) . "%a%b%d %H:%M今")
					  ((+ 86400 (gnus-seconds-today)) . "%a%b%d %H:%M昨")
					  ((gnus-seconds-year) . "%a%b%d %H:%M")
					  (t . "%a%Y%b%d %H:%M"))))
(setq gnus-permanently-visible-groups;不管有没有未读，都展示
      "qq$\\|gmail$\\|emacs$\\|inbox$")
;; Gnus的默认配置, 生成 "sent.%Y-%m" 格式的 Send-Mail存档, 这与imap的Send-Messages重复, 因此关闭改功能
(setq gnus-message-archive-group nil)

(delete 'gnus-topic-alist gnus-variable-list)
(delete 'gnus-topic-topology gnus-variable-list)
(setq gnus-topic-topology `(("Gnus" visible)
                                 (("misc" visible))
                                 ((,user-full-name visible))))
(setq gnus-topic-alist
      `((,user-full-name ; the key of topic
         ,(format "nnmaildir+%s:inbox" user-full-name)
         ,(format "nnmaildir+%s:Deleted Messages" user-full-name)
         ,(format "nnmaildir+%s:Junk" user-full-name)
         ,(format "nnmaildir+%s:Drafts" user-full-name)
         ,(format "nnmaildir+%s:Sent Messages" user-full-name))
        ("misc" ; the key of topic
         ;; 通过 GV 创建"nnvirtual:inbox",后 再通过 Gv 依次将各邮箱的inbox 加入到这个virtual group 后
         ;; 目前只用到一个邮箱，暂时用不上 ,nnvirtual的另一个缺点上 不能在其上继续使用GG  进行搜索
         "nnvirtual:inbox"
         ;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Selection-Groups.html
         ;;"nnselect:unread"    通过 Gg 后输入groupname:unread,然后用 tag:unread 作关键词搜索后的结果
         ;; (query . "tag:unread")
         ;; 创建完"nnselect:unread" 后需要通过 Gp 编辑这个group ,在(nnselect-specs 的上一行添加
         ;; (nnselect-rescan t)  (nnselect-always-regenerate t)
         ;; 确保重新进入会刷新
         ;; 保存后，然后重新打开gnus 能其将这段配置写入 .newsrc.eld 文件后即可
         ;; 这个搜索需要信赖gnus-search-notmuch 的支持 notmuch 的配置文件中
         ;; 我有配unread 这个tag ,即新邮件会打上unread的tag
         ;; [new]
         ;; tags=unread;inbox;
         "nnselect:unread"
         ;; like "nnselect:unread" but with
         ;; address: 发件人+收件人 包括密送 抄送等,
         ;; recipient: 只包括收件人+密送 抄送等
         ;; (query . "address:emacs-devel@gnu.org or address:debbugs.gnu.org")
         ;; (query . "recipient:emacs-devel@gnu.org")
         ;;gnus 的语法 https://www.gnu.org/software/emacs/manual/html_node/gnus/Search-Queries.html
         ;; notmuch 的语法 https://notmuchmail.org/doc/latest/man7/notmuch-search-terms.html#notmuch-search-terms-7
         ;; notmuch 会使用如下命令查询，因duplicate 去重，若自己给自己发邮件，发件箱也会存一份完全一样的，
         ;; 去重后导致只返回发件箱的，但过滤条件就会再次过滤只显示发件箱就会导致少显示几条
         ;; notmuch  search --output=files --duplicate=1 "to:mail2@qq.com or  or to:mail1@formail.com"
         "nnselect:emacs"
         "nnselect:emacs-info"
         "nnselect:qq"
         "nnselect:gmail"
         ;; 我不想显示这个nndraft 目前没找到办法，可以使用u unsubscribed
         ;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Drafts.html
         "nndraft:drafts"
         ;; "nnfolder+archive:sent.2024-12" // imap server 端会有发件箱，用不到sent了
         )
        ("Gnus")))

;; (setq gnus-summary-line-format "%U%R%([%-30,30f]:%) %-50,40s(%&user-date;)\n")
;; (setq gnus-summary-display-arrow t)
;; (setq gnus-exit-gnus-hook (quote (mm-destroy-postponed-undisplay-list)))
(setq gnus-after-exiting-gnus-hook #'mbsync)

(defun mbsync()
  (interactive)
  (let ((process (start-process "mbsync" "*Messages*" "sh" "-c" "mbsync -aq;notmuch new")))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel
     process
     (lambda (proc _)
       (when (eq (process-status proc) 'exit)
         (when (eq major-mode 'gnus-group-mode)
           (gnus-group-get-new-news))
         (when (eq major-mode 'gnus-summary-mode)
           (gnus-summary-rescan-group)))))))

(add-hook 'gnus-group-mode-hook #'init-my-gnus-group)
(defun init-my-gnus-group()
  ;; 这段代码是将以下手工创建group 的操作固化，以便我换电脑的时候
  ;; 不用再需要重新创建，而是通过代码自动化了
  ;;  下面用到的 user-mail-address-3 是我的gamil邮箱地址，没在此文件中定义
  ;; 如果是手工创建nnselect 组：流程如下:
  ;; 在groupbuffer中 将光标移动到你要搜索的那个group上 或用 # 选多个组
  ;; 然后 Gg 后输入groupname: gmail ,然后输入搜索用的关键字: recipient:yourmailaddress
  ;; gnus 的搜索语法 https://www.gnu.org/software/emacs/manual/html_node/gnus/Search-Queries.html
  ;; 我用到的搜索实现是 gnus-search-notmuch
  ;; 此时刷新group 就会出现一个名为 nnselect:gmail 的新组
  ;; 然后光标移动到这个组上 按下GE 编辑这个组 在 nnselect-specs的上一行
  ;; 添加   (nnselect-rescan t) (nnselect-always-regenerate t)
  ;; 只有这样 下次再进入这个组的时候 才会重新搜索，否则这个组就只是一个快照
  ;; 当然也可以通过 gnus-parameters 为这个组设置 这两个属性
  ;; 我的这个函数 就是将 GE编辑时的部分内容 copy出来通过gnus-group-make-group 实现的
  ;; 编写过程中如果有部Kg 可以通过C-k 删掉某group，以便重建
  (unless (gnus-group-entry "nnselect:gmail")
    (gnus-group-make-group
     "gmail"
     (list 'nnselect "nnselect")
     nil
     (list
      `(nnselect-specs (nnselect-function . gnus-search-run-query)
                       (nnselect-args (search-query-spec (query . ,(format "recipient:%s" user-mail-address-3))
                                                         (raw))
                                      (search-group-spec (,(format "nnmaildir:%s" user-full-name)
                                                          ,(format "nnmaildir+%s:inbox" user-full-name)))))
      '(nnselect-rescan t)
      '(nnselect-always-regenerate t)
      (cons 'nnselect-artlist nil))))

  ;; 下面是创建 nnselect:emacs 这个emacs相关邮件定阅分组
  (unless (gnus-group-entry "nnselect:emacs")
    (gnus-group-make-group
     "emacs"
     (list 'nnselect "nnselect")
     nil
     (list
      `(nnselect-specs (nnselect-function . gnus-search-run-query)
                       (nnselect-args
                        (search-query-spec
                         (query
                          . "address:emacs-devel@gnu.org or address:debbugs.gnu.org or address:emacs-tangents@gnu.org or address:info-gnu-emacs@gnu.org")
                         (raw))
                        (search-group-spec (,(format "nnmaildir:%s" user-full-name)
                                            ,(format "nnmaildir+%s:inbox" user-full-name)))))
      '(nnselect-rescan t)
      '(nnselect-always-regenerate t)
      (cons 'nnselect-artlist nil))))

(unless (gnus-group-entry "nnselect:emacs-info")
    (gnus-group-make-group
     "emacs-info"
     (list 'nnselect "nnselect")
     nil
     (list
      `(nnselect-specs (nnselect-function . gnus-search-run-query)
                       (nnselect-args
                        (search-query-spec
                         (query
                          . "address:emacs-tangents@gnu.org or address:info-gnu-emacs@gnu.org")
                         (raw))
                        (search-group-spec (,(format "nnmaildir:%s" user-full-name)
                                            ,(format "nnmaildir+%s:inbox" user-full-name)))))
      '(nnselect-rescan t)
      '(nnselect-always-regenerate t)
      (cons 'nnselect-artlist nil))))

  ;; 未读邮件单独一个分组 nnselect:unread
  (unless (gnus-group-entry "nnselect:unread")
    (gnus-group-make-group
     "unread"
     (list 'nnselect "nnselect")
     nil
     (list
      `(nnselect-specs (nnselect-function . gnus-search-run-query)
                       (nnselect-args
                        (search-query-spec
                         ;; 这个搜索需要信赖 gnus-search-notmuch 的支持 notmuch 的配置文件中
                         ;; 我有配unread 这个tag ,即新邮件会打上unread的tag
                         ;; [new]
                         ;; tags=unread;inbox;
                         (query . "tag:unread")
                         (raw))
                        (search-group-spec (,(format "nnmaildir:%s" user-full-name)
                                            ,(format "nnmaildir+%s:inbox" user-full-name)))))
      '(nnselect-rescan t)
      '(nnselect-always-regenerate t)
      (cons 'nnselect-artlist nil))))

  ;; 直发我个人邮箱的分组 nnselect:qq
  (unless (gnus-group-entry "nnselect:qq")
    (gnus-group-make-group
     "qq"
     (list 'nnselect "nnselect")
     nil
     (list
      `(nnselect-specs (nnselect-function . gnus-search-run-query)
                       (nnselect-args
                        (search-query-spec
                         (query . ,qq-mail-query) ;如 recipient:yourmail@qq.com
                         (raw))
                        (search-group-spec (,(format "nnmaildir:%s" user-full-name)
                                            ,(format "nnmaildir+%s:inbox" user-full-name)))))
      '(nnselect-rescan t)
      '(nnselect-always-regenerate t)
      (cons 'nnselect-artlist nil))))
  )


(provide 'gnus)
