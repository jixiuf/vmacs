;; https://forums.freebsd.org/threads/do-you-use-emacs-gnus.41969/
;; https://ericabrahamsen.net/tech/2014/oct/gnus-dovecot-lucene.html
(when (member (system-name) '("jxfhome" "jxfluoji"))
  (load  (concat user-emacs-directory "conf/conf-private.el.gpg") t))

;; C-c C-m f – Attach file
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

(with-eval-after-load 'gnus-group
;; 这些配置是因我是meow 用户，我对"g" "G" 做了一些定制
  (define-key gnus-group-mode-map (kbd "C-c MG") gnus-group-group-map)     ;old  G
  (define-key gnus-group-mode-map (kbd "C-c Gr") #'gnus-group-get-new-news))     ;old  g, now gr
(with-eval-after-load 'gnus-sum
  ;; 交换t T 后 tk:标记当前thread分支为已读
  (define-key gnus-summary-mode-map  "t" #'gnus-summary-thread-map)     ;old T
  (define-key gnus-summary-mode-map  "T" #'gnus-summary-toggle-header)     ;old t
  (define-key gnus-summary-thread-map "t" #'gnus-summary-toggle-threads)   ;tt :切换是否thread old:TT

  (define-key gnus-summary-mode-map  "v" #'gnus-summary-next-page)     ;old space
  (define-key gnus-summary-mode-map (kbd "C-c MG") gnus-summary-goto-map)     ;old gnus G
  (define-key gnus-summary-mode-map (kbd "C-c Gr") #'gnus-summary-show-article)) ;old gnus g ,now gr

(setq gnus-select-method `(nnmaildir ,user-full-name (directory "~/maildir/qq"))) ;(nnnil "")
(setq gnus-secondary-select-methods
      `((nnmaildir "vmacs"  (directory "~/maildir/vmacs"))
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
(setq gnus-parameters
  '(("nnmaildir.*"
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
(setq gnus-article-date-headers (quote (local)))
(setq gnus-summary-mode-hook 'hl-line-mode)
(setq gnus-group-mode-hook (quote (gnus-topic-mode hl-line-mode)))

;; https://www.math.utah.edu/docs/info/gnus_5.html#SEC51
(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       ;; "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
       "%3{│%}" "%3{%-18,18&user-date; %}" "%3{│%}" ;; date
       "  "
       "%4{%-20,20f%}"               ;; name
       "  "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%S\n"))
;; (setq gnus-activate-level 4)
;; gnus-summary-line-format 内通过%&user-date; 自定义时间格式
(setq gnus-user-date-format-alist (quote (
					  ((gnus-seconds-today) . "%a %b%d %H:%M 今")
					  ((+ 86400 (gnus-seconds-today)) . "%a %b%d %H:%M 昨")
					  ((gnus-seconds-year) . "%a %b%d %H:%M")
					  (t . "%a %Y%b%d %H:%M"))))
;; (setq gnus-summary-line-format "%U%R%([%-30,30f]:%) %-50,40s(%&user-date;)\n")
;; set gnus-parameter

;; (setq gnus-summary-display-arrow t)
;; (setq gnus-topic-alist '(("vmacs" ; the key of topic
;;                           "nnmaildir+vmacs:inbox"
;;                           )
;;                          ))
;; (gnus-topic-set-parameters "vmacs" '((display . 200)))
;; (gnus-topic-set-parameters "jixiuf" '((display . 200))))


;; (setq gnus-summary-highlight)


(provide 'gnus)
