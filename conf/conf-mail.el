;; brew install isync # for mbsync
;; brew install mu msmtp terminal-notifier
;;
;; https://rakhim.org/fastmail-setup-with-emacs-mu4e-and-mbsync-on-macos/
;; mu index --maildir=~/maildir
;; mu find hello

;; ~/.authinfo
;; machine smtp.exmail.qq.com login jixiufeng@luojilab.com password mypass
;; machine smtp.qq.com login jixiuf@qq.com password mypass
;; machine smtp.139.com login jixiuf@139.com password mypass

(evil-collection-define-key 'normal 'mu4e-headers-mode-map
  "gu" #'mu4e-update-mail-and-index
  "," #'mu4e~headers-jump-to-maildir)
;; 配置环境变量 XAPIAN_CJK_NGRAM 为 1，
;; 这样使用 mu find 可以搜索任意单个中文字符。
(setenv "XAPIAN_CJK_NGRAM" "yes")
(setq
 mu4e-headers-skip-duplicates t
 mu4e-view-show-images t
 ;; mu4e-view-image-max-width 800
 mu4e-view-show-addresses t
 mu4e-date-format "%Y-%m-%d"
 mu4e-headers-date-format "%Y-%m-%d"
 mu4e-change-filenames-when-moving t
 mu4e-attachment-dir "~/Downloads/"
 mu4e-update-interval 300
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
(add-to-list 'mu4e-bookmarks '(:name "inbox" :query "(maildir:/qq/inbox or maildir:/luojilab/inbox or maildir:/139/inbox) AND NOT flag:trashed" :key ?i))
(add-to-list 'mu4e-bookmarks '(:name "qq" :query "maildir:/qq/inbox  AND NOT flag:trashed" :key ?q))
(add-to-list 'mu4e-bookmarks '(:name "luojilab" :query "maildir:/luojilab/inbox  AND NOT flag:trashed" :key ?l))
(add-to-list 'mu4e-bookmarks '(:name "139(10086)" :query "maildir:/139/inbox  AND NOT flag:trashed" :key ?1))
(add-to-list 'mu4e-bookmarks '(:name "sent" :query "(maildir:/qq/\"Sent Messages\" or maildir:/luojilab/\"Sent Messages\" or maildir:/139/&XfJT0ZAB-) AND NOT flag:trashed" :key ?s))
(add-to-list 'mu4e-bookmarks '(:name "removed" :query "(maildir:/qq/\"Deleted Messages\" or maildir:/luojilab/\"Deleted Messages\" or maildir:/139/&XfJSIJZk-) AND NOT flag:trashed" :key ?r))
(add-to-list 'mu4e-bookmarks '(:name "drafts" :query "(maildir:/qq/Drafts or maildir:/luojilab/Drafts or maildir:/139/&g0l6P3ux-) AND NOT flag:trashed" :key ?d))

(when (fboundp 'imagemagick-register-types) (imagemagick-register-types))

;; Choose account label to feed msmtp -a option based on From header
;; in Message buffer; This function must be added to
;; message-send-mail-hook for on-the-fly change of From address before
;; sending message since message-send-mail-hook is processed right
;; before sending message.
(defun vmacs-choose-msmtp-account ()
  (when (message-mail-p)
	(save-excursion
      (let* ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond ((string-match (concat "jixiufeng" "@luojilab.com") from) "luojilab")
                    ((string-match "@139.com" from) "139")
                    ((string-match (concat "jixiuf" "@qq.com") from) "qq"))))
        (setq message-sendmail-extra-arguments (list '"-a" account))))))

;; 通过msmtp 支持多账号发送
(setq mail-user-agent 'mu4e-user-agent
      sendmail-program "/usr/local/bin/msmtp"
      send-mail-function 'smtpmail-send-it
      message-sendmail-f-is-evil t
      message-kill-buffer-on-exit t
      message-sendmail-extra-arguments '("--read-envelope-from")
       message-sendmail-envelope-from 'header
      message-send-mail-function 'message-send-mail-with-sendmail)
  (add-hook 'message-send-mail-hook #'vmacs-choose-msmtp-account)



;; (setq message-send-mail-function  #'smtpmail-send-it)
;; (setq smtpmail-stream-type        'ssl)
;; (setq smtpmail-auth-credentials   "~/.authinfo")

(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "qq"
          :enter-func (lambda () (mu4e-message "Entering qq context"))
          ;; :leave-func (lambda () (mu4e-message "Leaving qq context"))
          ;; we match based on the contact-fields of the message
          :match-func (lambda (msg)
                        (when msg (mu4e-message-contact-field-matches msg '(:to :from :cc :bcc) (concat "jixiuf" "@" "qq.com"  ))))
          :vars '((smtpmail-smtp-user    . "jixiuf@qq.com")
                  (user-mail-address     . "jixiuf@qq.com")
                  (mu4e-sent-folder      . "/qq/Sent Messages")
                  (mu4e-drafts-folder    . "/qq/Drafts")
                  (mu4e-trash-folder     . "/qq/Deleted Messages")))
        ,(make-mu4e-context
          :name "10086(139)"
          :enter-func (lambda () (mu4e-message "Entering 10086(139) context"))
          ;; :leave-func (lambda () (mu4e-message "Leaving qq context"))
          ;; we match based on the contact-fields of the message
          :match-func (lambda (msg)
                        (when msg (mu4e-message-contact-field-matches msg '(:to :from :cc :bcc) (concat "jixiuf" "@139.com" ))))
          :vars '((smtpmail-smtp-user         . "jixiuf@139.com")
                  (user-mail-address          . "jixiuf@139.com")
                  (mu4e-sent-folder      . "/139/&XfJT0ZAB-")
                  (mu4e-drafts-folder    . "/139/&g0l6P3ux-")
                  (mu4e-trash-folder     . "/139/&XfJSIJZk-")))
        ,(make-mu4e-context
          :name "luojilab"
          :enter-func (lambda () (mu4e-message "Switch to the luojilab context"))
          :match-func (lambda (msg)
                        (when msg (mu4e-message-contact-field-matches msg '(:to :from :cc :bcc)  "@luojilab.com")))
          :vars '(
                  (smtpmail-smtp-user    . "jixiufeng@luojilab.com")
                  (user-mail-address     . "jixiufeng@luojilab.com")
                  (mu4e-sent-folder      . "/luojilab/Sent Messages")
                  (mu4e-refile-folder    . "/luojilab/Archive")
                  (mu4e-drafts-folder    . "/luojilab/Drafts")
                  (mu4e-trash-folder     . "/luojilab/Deleted Messages")))))

;; (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

;; (setq
;;  smtpmail-default-smtp-server "smtp.exmail.qq.com"
;;  smtpmail-smtp-service 465
;;  smtpmail-smtp-user "jixiufeng@luojilab.com"
;;  user-mail-address  "jixiufeng@luojilab.com"
;;  smtpmail-stream-type 'ssl
;;  smtpmail-smtp-server         "smtp.exmail.qq.com")
(require 'mu4e-alert)
(mu4e-alert-set-default-style 'notifier)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)


(provide 'conf-mail)

  ;; Local Variables:
  ;; coding: utf-8
  ;; End:

;;; conf-mail.el ends here.
