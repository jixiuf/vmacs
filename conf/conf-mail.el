;; brew install isync # for mbsync
;; brew install mu
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
(setq
 mu4e-headers-skip-duplicates t
 mu4e-view-show-images t
 mu4e-view-show-addresses t
 mu4e-date-format "%Y-%m-%d"
 mu4e-headers-date-format "%Y-%m-%d"
 mu4e-change-filenames-when-moving t
 mu4e-attachment-dir "~/Downloads/"
 mu4e-update-interval 300
 mu4e-split-view 'single-window
 mu4e-completing-read-function #'completing-read
 ;; this setting allows to re-sync and re-index mail by pressing U
 mu4e-get-mail-command  "mbsync -a"
 mu4e-maildir-shortcuts '(("/luojilab/inbox" . ?l)
                          ("/139/inbox" . ?1)
                          ("/drafts" . ?d)
                          ("/sent" . ?s)
                          ("/trash" . ?t)
                          ("/qq/inbox" . ?q)))


(setq message-send-mail-function  #'smtpmail-send-it)
(setq smtpmail-stream-type        'ssl)
(mu4e-bookmarks)
(setq mu4e-contexts
      `(,(make-mu4e-context
           :name "qq"
           :enter-func (lambda () (mu4e-message "Entering qq context"))
           ;; :leave-func (lambda () (mu4e-message "Leaving qq context"))
           ;; we match based on the contact-fields of the message
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               '(:to :from :cc :bcc) (concat "jixiuf" "@" "qq.com"  ))))
           :vars '(( user-mail-address      . (concat "jixiuf" "@" "qq.com"  ))
                   (user-full-name     . "jixiuf" )
                   ;; (message-send-mail-function . #'smtpmail-send-it)
                   (smtpmail-smtp-user         . "jixiuf@qq.com")
                   (user-mail-address          . "jixiuf@qq.com")
                   (smtpmail-smtp-service      . 465)
                   ;;  ;; machine smtp.exmail.qq.com login jixiufeng@luojilab.com password mypassword
                   ;;  ;; machine smtp.qq.com login jixiuf@qq.com password mypassword
                   (smtpmail-auth-credentials  . "~/.authinfo")
                   (smtpmail-smtp-server       . "smtp.qq.com")
                   ))
        ,(make-mu4e-context
           :name "10086(139)"
           :enter-func (lambda () (mu4e-message "Entering 10086(139) context"))
           ;; :leave-func (lambda () (mu4e-message "Leaving qq context"))
           ;; we match based on the contact-fields of the message
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               '(:to :from :cc :bcc) (concat "jixiuf" "@139.com" ))))
           :vars '(( user-mail-address      . (concat "jixiuf" "@" "139.com"  ))
                   (user-full-name     . "jixiuf10086" )
                   ;; (message-send-mail-function . #'smtpmail-send-it)
                   (smtpmail-smtp-user         . "jixiuf@139.com")
                   (user-mail-address          . "jixiuf@139.com")
                   (smtpmail-smtp-service      . 465)
                   ;;  ;; machine smtp.exmail.qq.com login jixiufeng@luojilab.com password mypassword
                   ;;  ;; machine smtp.qq.com login jixiuf@qq.com password mypassword
                   (smtpmail-auth-credentials  . "~/.authinfo")
                   (smtpmail-smtp-server       . "smtp.139.com")
                   ))
         ,(make-mu4e-context
           :name "luojilab"
           :enter-func (lambda () (mu4e-message "Switch to the luojilab context"))
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               '(:to :from :cc :bcc)  "@luojilab.com")))
           :vars '((user-full-name          . "jixiufeng" )
                   (smtpmail-smtp-user         . "jixiufeng@luojilab.com")
                   (user-mail-address          . "jixiufeng@luojilab.com")
                   (smtpmail-smtp-service      . 465)
                   (smtpmail-stream-type       . 'ssl)
                   ;;  ;; machine smtp.exmail.qq.com login jixiufeng@luojilab.com password mypassword
                   ;;  ;; machine smtp.qq.com login jixiuf@qq.com password mypassword
                   (smtpmail-auth-credentials  . "~/.authinfo")
                   (smtpmail-smtp-server       . "smtp.exmail.qq.com")
                   ))))


;; (setq
;;  smtpmail-default-smtp-server "smtp.exmail.qq.com"
;;  smtpmail-smtp-service 465
;;  smtpmail-smtp-user "jixiufeng@luojilab.com"
;;  user-mail-address  "jixiufeng@luojilab.com"
;;  smtpmail-stream-type 'ssl
;;  smtpmail-smtp-server         "smtp.exmail.qq.com")

  (provide 'conf-mail)

  ;; Local Variables:
  ;; coding: utf-8
  ;; End:

;;; conf-mail.el ends here.
