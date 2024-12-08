(setq message-send-mail-function 'smtpmail-send-it)
(setq user-mail-address (concat "jixiuf" "@" "qq.com"))
(setq user-full-name "jixiuf")

(setq smtpmail-smtp-user (concat "jixiuf" "@" "qq.com")
      smtpmail-smtp-server "smtp.qq.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)

;;Debug
(setq smtpmail-debug-info t)
(setq smtpmail-debug-verb t)

;;; sending mail end


(setq gnus-select-method '(nnnil ""))

(setq gnus-secondary-select-methods
      '((nnmaildir "jixiuf"
                  (directory "~/maildir/qq")
                  )
        (nnmaildir ""
                   (directory "~/maildir/qq/&UXZO1mWHTvZZOQ-/")
                  )
        ;; (nnml "vmacs"
        ;;           (directory "~/maildir/vmacs")
        ;;           )
        ;; (nnimap "jixiuf"
        ;;         (nnimap-address "imap.qq.com")
        ;;         (nnimap-inbox "INBOX")
        ;;         (nnimap-expunge t)
        ;;         (nnimap-server-port 993)
        ;;         (nnimap-stream ssl))
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
(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
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
;; (setq gnus-user-date-format-alist (quote (
;; 					  ((gnus-seconds-today) . "今%H:%M")
;; 					  ((+ 86400 (gnus-seconds-today)) . "昨%H:%M")
;; 					  ((gnus-seconds-year) . "%a %b %d %H:%M")
;; 					  (t . "%a %b %d %Y %H:%M"))))
;; (setq gnus-summary-line-format "%U%R%([%-30,30f]:%) %-50,40s(%&user-date;)\n")

;; (setq nnml-directory "~/gmail")
;; (setq message-directory "~/gmail")
;; (setq gnus-summary-display-arrow t)
;; (setq gnus-topic-alist '(("vmacs" ; the key of topic
;;                           "nnmaildir+vmacs:inbox"
;;                           )
;;                          ))
;; (gnus-topic-set-parameters "vmacs" '((display . 200)))
;; (gnus-topic-set-parameters "jixiuf" '((display . 200))))
;; https://www.bounga.org/tips/2020/05/03/multiple-smtp-accounts-in-gnus-without-external-tools/
(setq gnus-posting-styles
      '((".*" ; Matches all groups of messages with default qq
         (address (concat (format "jixiuf <jixiuf%s%s>" "@qq" ".com") )))
        ("vmacs" ; Matches Gnus group called "vmacs"
         (address (concat "vmacs" "@" "qq" ".com"))
         ("X-Message-SMTP-Method" (concat "smtp smtp.qq.com 587 vmacs" "@" "qq" ".com")))))

;; (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(provide 'gnus)
