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
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-user user-mail-address
      smtpmail-smtp-server "smtp.qq.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      smtpmail-debug-info t
      smtpmail-debug-verb t)

;; https://www.bounga.org/tips/2020/05/03/multiple-smtp-accounts-in-gnus-without-external-tools/
(setq gnus-posting-styles
      `((".*" ; Matches all groups of messages with default qq
         (address (concat (format "%s <%s>" ,user-full-name ,user-mail-address) )))
        ("vmacs" ; Matches Gnus group called "vmacs"
         (address ,user-mail-address-2)
         ("X-Message-SMTP-Method" (concat "smtp smtp.qq.com 587 " ,user-mail-address-2)))))

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
  (define-key gnus-group-mode-map (kbd "C-c Gu") #'mbsync)                 ;gu
  (define-key gnus-group-mode-map (kbd "C-c Gr") #'gnus-group-get-new-news))     ;old  g, now gr
(with-eval-after-load 'gnus-sum
  ;; d:标记为已读  C-k:整个subject 已读
  ;; r 回复
  ;; 交换t T 后 tk:标记当前thread分支为已读
  (define-key gnus-summary-mode-map  "t" #'gnus-summary-thread-map)     ;old T
  (define-key gnus-summary-mode-map  "T" #'gnus-summary-toggle-header)     ;old t
  (define-key gnus-summary-thread-map "t" #'gnus-summary-toggle-threads)   ;tt :切换是否thread old:TT
  (define-key gnus-summary-mode-map (kbd "C-c Gu") #'mbsync)                 ;gu
  (define-key gnus-summary-mode-map (kbd "C-c Gr") #'gnus-summary-rescan-group);gr old M-g
  (define-key gnus-summary-mode-map (kbd "C-c M/") #'gnus-summary-limit-map);/

  (define-key gnus-summary-mode-map  "v" #'gnus-summary-next-page)     ;old space
  (define-key gnus-summary-mode-map (kbd "C-c MG") gnus-summary-goto-map)     ;old gnus G
  (define-key gnus-summary-mode-map (kbd "C-c Gr") #'gnus-summary-show-article)) ;old gnus g ,now gr

(setq gnus-interactive-exit nil)     ;退出时不必确认
(setq gnus-expert-user t)
(setq gnus-search-use-parsed-queries t) ;GG search group, and / :  limit in summary buffer
(setq message-directory "~/maildir/")
;; (with-eval-after-load 'gnus-search
;;   (add-to-list 'gnus-search-default-engines '((nnmaildir:vmacs . gnus-search-find-grep))))
;; (setq gnus-search-mu-remove-prefix (expand-file-name "~/maildir/vmacs"))
(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods
      `((nnmaildir ,user-full-name (directory "~/maildir/qq"))
        ;; (nntp "news.gmane.io")
        ;; (nntp "news.gwene.org")
        (nnmaildir "vmacs"  (directory "~/maildir/vmacs")
                   (nnmaildir-directory "~/maildir/vmacs")
                   (gnus-search-engine gnus-search-find-grep)
                   )
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
  '(("nnmaildir.*vmacs:.*"
     (gnus-show-threads t)
     ;; (gnus-article-sort-functions '((not gnus-article-sort-by-date)))
     (gnus-use-scoring nil)
     ;; (expiry-wait . 2)
     (display . 500))  ;C-u ret 可指定别的数量               ;big enouch without confirm
    ("nnmaildir.*jixiuf:.*"
     (gnus-show-threads
      nil)
     (gnus-article-sort-functions '((not gnus-article-sort-by-date)))
     (gnus-use-scoring nil)
     ;; (expiry-wait . 2)
     (display . all))))
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
      "nnmaildir\\+jixiuf:Sent Messages\\|inbox$\\|nnmaildir\\+jixiuf:Drafts")
(delete 'gnus-topic-alist gnus-variable-list)
(delete 'gnus-topic-topology gnus-variable-list)
(setq gnus-topic-topology '(("Gnus" visible)
                                 (("misc" visible))
                                 (("jixiuf" visible))
                                 (("vmacs" visible ))))
(setq gnus-topic-alist
      '(("jixiuf" ; the key of topic
         "nnmaildir+jixiuf:inbox"
         "nnmaildir+jixiuf:Deleted Messages"
         "nnmaildir+jixiuf:Junk"
         "nnmaildir+jixiuf:Drafts"
         "nnmaildir+jixiuf:Sent Messages")
        ("vmacs" ; the key of topic
         "nnmaildir+vmacs:inbox"
         "nnmaildir+vmacs:Deleted Messages"
         "nnmaildir+vmacs:Junk"
         "nnmaildir+vmacs:Drafts"
         "nnmaildir+vmacs:Sent Messages")
        ("misc" ; the key of topic
         ;; "nnfolder+archive:sent.2015-12"
         ;; "nnfolder+archive:sent.2016"
         "nndraft:drafts")
        ("Gnus")))

;; (setq gnus-summary-line-format "%U%R%([%-30,30f]:%) %-50,40s(%&user-date;)\n")
;; (setq gnus-summary-display-arrow t)
;; (setq gnus-exit-gnus-hook (quote (mm-destroy-postponed-undisplay-list)))
(setq gnus-exit-gnus-hook #'mbsync)

(defun mbsync()
  (interactive)
  (let ((process (start-process "mbsync" "*Messages*" "mbsync" "-aq")))
    (set-process-sentinel
     process
     (lambda (proc _)
       (when (eq (process-status proc) 'exit)
         (when (eq major-mode 'gnus-group-mode)
           (gnus-group-get-new-news))
         (when (eq major-mode 'gnus-summary-mode)
           (gnus-summary-rescan-group)))))))

(provide 'gnus)
