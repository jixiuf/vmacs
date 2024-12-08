;; https://forums.freebsd.org/threads/do-you-use-emacs-gnus.41969/
(setq message-send-mail-function 'smtpmail-send-it)
(when (member (system-name) '("jxfhome" "jxfluoji"))
  (load  (concat user-emacs-directory "conf/conf-private.el.gpg") t))

(setq smtpmail-smtp-user user-mail-address
      smtpmail-smtp-server "smtp.qq.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)

;;Debug
(setq smtpmail-debug-info t)
(setq smtpmail-debug-verb t)

;;; sending mail end


(setq gnus-select-method '(nnnil ""))

(setq gnus-secondary-select-methods
      `(
        ;; (nnmaildir "jixiuf"
        ;;           (directory "~/maildir/qq")
        ;;           )
        ;; (nnmaildir ""
        ;;            (directory "~/maildir/qq/&UXZO1mWHTvZZOQ-/")
        ;;           )
        ;; (nnml "vmacs"
        ;;           (directory "~/maildir/vmacs")
        ;;           )
        (nnimap ,user-full-name
                (nnimap-address "imap.qq.com")
                (nnimap-inbox "INBOX")
                (nnimap-expunge t)
                (nnimap-server-port 993)
                (nnimap-stream ssl))
        ))
(setq mail-sources
      '((maildir :path "~/maildir/")))
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "") ;; "● ")
  (setq gnus-sum-thread-tree-false-root "") ;; "◯ ")
  (setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))
;; https://www.math.utah.edu/docs/info/gnus_5.html#SEC51
(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "%3{│%}" "%1{%&user-date; %}" "%3{│%}" ;; date
       "  "
       "%4{%-20,20f%}"               ;; name
       "  "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%s\n"))
;; (setq gnus-activate-level 4)
(setq gnus-article-date-headers (quote (local)))
(setq gnus-summary-mode-hook 'hl-line-mode)
(setq gnus-group-mode-hook (quote (gnus-topic-mode hl-line-mode)))
;; gnus-summary-line-format 内通过%&user-date; 自定义时间格式
(setq gnus-user-date-format-alist (quote (
					  ((gnus-seconds-today) . "%H:%M")
					  ((+ 86400 (gnus-seconds-today)) . "昨%H:%M")
					  ((gnus-seconds-year) . "%a %b %d %H:%M")
					  (t . "%a %b %d %Y %H:%M"))))
;; (setq gnus-summary-line-format "%U%R%([%-30,30f]:%) %-50,40s(%&user-date;)\n")
;; set gnus-parameter

;; (setq nnml-directory "~/gmail")
;; (setq message-directory "~/gmail")
;; (setq gnus-summary-display-arrow t)
;; (setq gnus-topic-alist '(("vmacs" ; the key of topic
;;                           "nnmaildir+vmacs:inbox"
;;                           )
;;                          ))
;; (gnus-topic-set-parameters "vmacs" '((display . 200)))
;; (gnus-topic-set-parameters "jixiuf" '((display . 200))))
(setq gnus-parameters
  '(("nnimap.*"
     (gnus-use-scoring nil)
     (expiry-wait . 2)
     (display . all))))

;; https://www.bounga.org/tips/2020/05/03/multiple-smtp-accounts-in-gnus-without-external-tools/
(setq gnus-posting-styles
      `((".*" ; Matches all groups of messages with default qq
         (address (concat (format "%s <%s>" ,user-full-name ,user-mail-address) )))
        ("vmacs" ; Matches Gnus group called "vmacs"
         (address ,user-mail-address-2)
         ("X-Message-SMTP-Method" (concat "smtp smtp.qq.com 587 " ,user-mail-address-2)))))

;; (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(defun toggle-from()
  "Toggle addresses in the From: field of the message buffer."
  (interactive)
  (save-excursion
    (message-goto-from)
    (if (string-match (concat "^From:\s-*.*" user-work-mail-address) (thing-at-point 'line))
	(progn 
	  (beginning-of-line)
	  (message-delete-line)
	  (insert (concat "From: " user-full-name " <" user-mail-address ">\n")))
      (message-goto-from)
      (when (string-match (concat "^from:\s-*.*" user-mail-address) (thing-at-point 'line))
	(beginning-of-line)
	(message-delete-line)
    (insert (concat "From: " user-full-name " <" user-work-mail-address ">\n"))))))
(provide 'gnus)
