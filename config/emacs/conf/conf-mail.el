;; brew install isync # for mbsync
;; brew install mu  terminal-notifier
;;
;; https://rakhim.org/fastmail-setup-with-emacs-mu4e-and-mbsync-on-macos/
;; mu init --maildir=~/maildir
;; mu index
;; mu find hello

;; ~/.authinfo
;; machine smtp.exmail.qq.com login jixiufeng@luojilab.com password mypass
;; machine smtp.qq.com login jixiuf@qq.com password mypass
;; machine smtp.139.com login jixiuf@139.com password mypass
(defun vmacs-mu4e-update-mail-and-index ()
  (interactive)
  (if current-prefix-arg (mu4e-update-mail-and-index t) (mu4e-update-index)))

(define-key mu4e-view-mode-map (kbd "C-j") #'mu4e-view-headers-next)
(define-key mu4e-view-mode-map (kbd "C-k") #'mu4e-view-headers-prev)
(define-key mu4e-view-mode-map (kbd "M-n") #'mu4e-view-headers-next-unread)
(define-key mu4e-view-mode-map (kbd "M-p") #'mu4e-view-headers-prev-unread)

(define-key mu4e-headers-mode-map "r" #'mu4e-headers-mark-for-read)
(define-key mu4e-headers-mode-map "t" #'(lambda nil (interactive) (mu4e-headers-mark-thread nil '(read)) (mu4e-mark-execute-all t) (mu4e-view-headers-prev)(mu4e-headers-next-unread)))
(define-key mu4e-headers-mode-map "!" #'mu4e-headers-mark-for-refile)
(define-key mu4e-headers-mode-map (kbd "M-n") #'mu4e-headers-next-unread)
(define-key mu4e-headers-mode-map (kbd "M-p") #'mu4e-headers-prev-unread)
(define-key mu4e-headers-mode-map (kbd "C-c C-w") #'vmacs-mu4e-brower)
(define-key mu4e-headers-mode-map "i" #'vmacs-read-all)
(define-key mu4e-headers-mode-map (kbd "C-c Gu") #'vmacs-mu4e-update-mail-and-index);gu
(define-key mu4e-headers-mode-map (kbd "C-c Gl") #'mu4e-search-next);gl
(define-key mu4e-headers-mode-map (kbd "C-c Gh") #'mu4e-search-prev);gh

(defun vmacs-mu4e-brower (&optional msg)
  (interactive)
  (let ((browse-url-browser-function browse-url-browser-function))
    (when (fboundp 'xwidget-webkit-browse-url)
      (setq browse-url-browser-function 'xwidget-webkit-browse-url))
    (mu4e-action-view-in-browser (or msg (mu4e-message-at-point)))
    ))
;; mu4e-view-actions by `a'
(setq mu4e-view-actions                 ;
      '(("apply git patches" . mu4e-action-git-apply-patch)
        ("mgit am patch" . mu4e-action-git-apply-mbox)
        ("capture message"  . mu4e-action-capture-message)
        ("view in browser"  . vmacs-mu4e-brower)
        ("show this thread" . mu4e-action-show-thread)))


(defun vmacs-xwidget-hook()
  ;; (set-frame-parameter nil 'alpha 100)
  (dolist(x (get-buffer-xwidgets (current-buffer)))
    (set-xwidget-query-on-exit-flag x nil)))
(add-hook 'xwidget-webkit-mode-hook 'vmacs-xwidget-hook)

;; as 查看当前thread 的列表

;; 配置环境变量 XAPIAN_CJK_NGRAM 为 1，
;; 这样使用 mu find 可以搜索任意单个中文字符。
(setenv "XAPIAN_CJK_NGRAM" "yes")
(setq
 mu4e-headers-skip-duplicates t
 mu4e-search-results-limit 5000
 mu4e-compose-keep-self-cc t
 mu4e-index-lazy-check t
 mu4e-view-show-images t
 ;; mu4e-view-image-max-width 800
 mu4e-view-show-addresses t
 mu4e-date-format "%y-%m-%d"
 mu4e-headers-date-format "%y-%m-%d"
 mu4e-change-filenames-when-moving t
 mu4e-attachment-dir "~/Downloads/"
 mu4e-update-interval nil
 mu4e-index-update-in-background t
 mu4e-split-view 'single-window
 mu4e-completing-read-function #'completing-read
 mu4e-compose-context-policy 'ask
 mu4e-context-policy 'pick-first
 ;; this setting allows to re-sync and re-index mail by pressing U
 mu4e-get-mail-command  "mbsync -a"
 mu4e-confirm-quit nil
;; Why would I want to leave my message open after I've sent it?
 ;; mu4e-maildir-shortcuts '(("/luojilab/inbox" . ?l)
 ;;                          ("/139/inbox" . ?1)
 ;;                          ("/qq/inbox" . ?q))
 )
(add-to-list 'mu4e-bookmarks '(:name "inbox" :query "(maildir:/vmacs/&UXZO1mWHTvZZOQ-/emacs-bug or maildir:/vmacs/&UXZO1mWHTvZZOQ-/emacs-dev or maildir:/qq/inbox or maildir:/vmacs/inbox or maildir:/luojilab/inbox or maildir:/139/inbox) AND NOT flag:trashed" :key ?i))
(add-to-list 'mu4e-bookmarks '(:name "qq" :query "maildir:/qq/inbox  AND NOT flag:trashed" :key ?q))
(add-to-list 'mu4e-bookmarks '(:name "vmacs" :query "maildir:/vmacs/inbox  AND NOT flag:trashed" :key ?v))
(add-to-list 'mu4e-bookmarks '(:name "emacs" :query "maildir:/vmacs/&UXZO1mWHTvZZOQ-/emacs-dev  AND NOT flag:trashed" :key ?e))
(add-to-list 'mu4e-bookmarks '(:name "emacs-bug" :query "maildir:/vmacs/&UXZO1mWHTvZZOQ-/emacs-bug  AND NOT flag:trashed" :key ?b))
;; (add-to-list 'mu4e-bookmarks '(:name "luojilab" :query "maildir:/luojilab/inbox  AND NOT flag:trashed" :key ?l))
;; (add-to-list 'mu4e-bookmarks '(:name "139(10086)" :query "maildir:/139/inbox  AND NOT flag:trashed" :key ?1))
(add-to-list 'mu4e-bookmarks '(:name "sent" :query "(maildir:/qq/\"Sent Messages\" or maildir:/vmacs/\"Sent Messages\"  or maildir:/139/&XfJT0ZAB-) AND NOT flag:trashed" :key ?s))
(add-to-list 'mu4e-bookmarks '(:name "removed" :query "(maildir:/qq/\"Deleted Messages\" or maildir:/vmacs/\"Deleted Messages\"  or maildir:/139/&XfJSIJZk-) AND NOT flag:trashed" :key ?r))
(add-to-list 'mu4e-bookmarks '(:name "drafts" :query "(maildir:/qq/Drafts or maildir:/vmacs/Drafts  or maildir:/139/&g0l6P3ux-) AND NOT flag:trashed" :key ?d))
(add-to-list 'mu4e-bookmarks '(:name "trash" :query "(maildir:/qq/Junk or maildir:/vmacs/Junk or  maildir:/139/&XfJSIJZk-)" :key ?j))

(when (fboundp 'imagemagick-register-types) (imagemagick-register-types))


;; 通过msmtp 支持多账号发送
(setq mail-user-agent 'mu4e-user-agent
      message-send-mail-function 'smtpmail-send-it
      message-sendmail-f-is-evil t
      message-kill-buffer-on-exit t
      message-mail-user-agent t
      )

(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "qq"
          :enter-func (lambda () (mu4e-message "Entering qq context"))
          ;; :leave-func (lambda () (mu4e-message "Leaving qq context"))
          ;; we match based on the contact-fields of the message
          :match-func (lambda (msg)
                        (when msg (mu4e-message-contact-field-matches msg '(:to :from :cc :bcc) (concat "jixiuf" "@" "qq.com"  ))))
          :vars '((smtpmail-smtp-user    . "jixiuf@qq.com")
                  (smtpmail-smtp-server  . "smtp.qq.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (user-full-name        . "jixiuf@qq.com")
                  (user-mail-address     . "jixiuf@qq.com")
                  (mu4e-sent-folder      . "/qq/Sent Messages")
                  (mu4e-drafts-folder    . "/qq/Drafts")
                  (mu4e-trash-folder     . "/qq/Deleted Messages")))
        ,(make-mu4e-context
          :name "vmacs"
          :enter-func (lambda () (mu4e-message "Entering vmacs context"))
          ;; :leave-func (lambda () (mu4e-message "Leaving qq context"))
          ;; we match based on the contact-fields of the message
          :match-func (lambda (msg)
                        (when msg (mu4e-message-contact-field-matches msg '(:to :from :cc :bcc) (concat "vmacs" "@" "qq.com"  ))))
          :vars '((smtpmail-smtp-user    . "vmacs@qq.com")
                  (smtpmail-smtp-server  . "smtp.qq.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (user-full-name        . "vmacs@qq.com")
                  (user-mail-address     . "vmacs@qq.com")
                  (mu4e-sent-folder      . "/vmacs/Sent Messages")
                  (mu4e-drafts-folder    . "/vmacs/Drafts")
                  (mu4e-trash-folder     . "/vmacs/Deleted Messages")))
        ,(make-mu4e-context
          :name "10086(139)"
          :enter-func (lambda () (mu4e-message "Entering 10086(139) context"))
          ;; :leave-func (lambda () (mu4e-message "Leaving qq context"))
          ;; we match based on the contact-fields of the message
          :match-func (lambda (msg)
                        (when msg (mu4e-message-contact-field-matches msg '(:to :from :cc :bcc) (concat "jixiuf" "@139.com" ))))
          :vars '((smtpmail-smtp-user    . "jixiuf@139.com")
                  (smtpmail-smtp-server  . "smtp.139.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (user-mail-address     . "jixiuf@139.com")
                  (user-full-name        . "jixiuf@139.com")
                  (mu4e-sent-folder      . "/139/&XfJT0ZAB-")
                  (mu4e-drafts-folder    . "/139/&g0l6P3ux-")
                  (mu4e-trash-folder     . "/139/&XfJSIJZk-")))
        ;; ,(make-mu4e-context
        ;;   :name "luojilab"
        ;;   :enter-func (lambda () (mu4e-message "Switch to the luojilab context"))
        ;;   :match-func (lambda (msg)
        ;;                 (when msg (mu4e-message-contact-field-matches msg '(:to :from :cc :bcc)  "@luojilab.com")))
        ;;   :vars '(
        ;;           (smtpmail-smtp-user    . "jixiufeng@luojilab.com")
        ;;           (smtpmail-smtp-server  . "smtp.feishu.cn")
        ;;           (user-full-name        . "jixiufeng@luojilab.com")
        ;;           (smtpmail-smtp-service . 465)
        ;;           (smtpmail-stream-type  . ssl)
        ;;           (user-mail-address     . "jixiufeng@luojilab.com")
        ;;           (mu4e-sent-folder      . "/luojilab/Sent Messages")
        ;;           (mu4e-refile-folder    . "/luojilab/Archive")
        ;;           (mu4e-drafts-folder    . "/luojilab/Drafts")
        ;;           (mu4e-trash-folder     . "/luojilab/Deleted Messages")))
        ))

(defun mu4e-goodies~break-cjk-word (word)
  "Break CJK word into list of bi-grams like: 我爱你 -> 我爱 爱你"
  (if (or (<= (length word) 2)
          (equal (length word) (string-bytes word))) ; only ascii chars
      word
    (let ((pos nil)
          (char-list nil)
          (br-word nil))
      (if (setq pos (string-match ":" word))     ; like: "s:abc"
          (concat (substring word 0 (+ 1 pos))
                  (mu4e-goodies~break-cjk-word (substring word (+ 1 pos))))
        (if (memq 'ascii (find-charset-string word)) ; ascii mixed with others like: abc你好
            word
          (progn
            (setq char-list (split-string word "" t))
            (while (cdr char-list)
              (setq br-word (concat br-word (concat (car char-list) (cadr char-list)) " "))
              (setq char-list (cdr char-list)))
            br-word))))))

(defun mu4e-goodies~break-cjk-query (expr)
  "Break CJK strings into bi-grams in query."
  (let ((word-list (split-string expr " " t))
        (new ""))
    (dolist (word word-list new)
      (setq new (concat new (mu4e-goodies~break-cjk-word word) " ")))))

(setq mu4e-query-rewrite-function 'mu4e-goodies~break-cjk-query)

;; (require 'mu4e-alert)
;; (mu4e-alert-set-default-style 'notifier)
;; (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

;; 注释掉，暂时因为开了这 无法用C-xm
;; https://github.com/jeremy-compostella/org-msg
;; (require 'org-msg)
;; (setq org-msg-options "html-postamble:nil  H:2 num:t ^:{} toc:nil author:nil email:nil \\n:t"
;;       org-msg-startup " inlineimages"
;;       org-msg-default-alternatives '((new		. (text html))
;;                                      (reply-to-html	. (text html))
;;                                      (reply-to-text	. (text)))
;;       org-msg-posting-style nil
;;       org-msg-convert-citation t)
;; (org-msg-mode)

(fset 'vmacs-read-all
   (kmacro-lambda-form [?% ?! ?s ?. ?* return ?x ?y] 0 "%d"))

(provide 'conf-mail)

  ;; Local Variables:
  ;; coding: utf-8
  ;; End:

;;; conf-mail.el ends here.
