;; brew install isync # for mbsync
;; brew install mu
;; https://rakhim.org/fastmail-setup-with-emacs-mu4e-and-mbsync-on-macos/
;; mu index --maildir=~/maildir
;; mu find hello
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(when (require 'mu4e nil t)
  (setq
   mu4e-headers-skip-duplicates t
   mu4e-view-show-images t
   mu4e-view-show-addresses t
   mu4e-date-format "%Y-%m-%d"
   mu4e-headers-date-format "%Y-%m-%d"
   mu4e-change-filenames-when-moving t
   mu4e-attachment-dir "~/Downloads/mail"
   )

  ;; this setting allows to re-sync and re-index mail
  ;; by pressing U
  (setq mu4e-get-mail-command  "mbsync -a")
  )
;; (setq
;;    message-send-mail-function   'smtpmail-send-it
;;    smtpmail-default-smtp-server "smtp.exmail.qq.com"
;;    smtpmail-smtp-server         "smtp.exmail.qq.com")

(provide 'conf-mail)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-mail.el ends here.
