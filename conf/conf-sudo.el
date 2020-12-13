;; 用sudo 打开当前编辑的文件或目录
(global-set-key (kbd "C-c o") 'toggle-read-only-file-with-sudo)
;; su: or /sudo: on remote hosts
;; You can also use this syntax to sudo/su to root (or of course any other use) on a remote host:
;; C-xC-f /ssh:you@remotehost|sudo:remotehost:/path/to/file RET
;; /-: 表示使用默认的method
;; C-xC-f /-:you@remotehost|sudo:remotehost:/path/to/file RET

;;; 当切换到root 用户时，为作区别 ，外观显红色
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path "/usr/local/go/bin")


  ;;  跳板机 堡垒机脚本
;; ~/bin/jump
;; #!/usr/bin/expect
;; # 支持 ./jump user@host:port  此种情况下，其实只host有用，但为了emacs tramp 使用支持此种格式
;; # 支持 ./jump host
;; # 支持 ./jump  交互模式
;; spawn ssh jixiufeng@agw.luojilab.com
;; expect "jixiufeng@agw.luojilab.com's password:"
;; send "yourpassword\r"
;; expect "Input:*"
;; # user@host:port,这里真正有用的是hosst
;; set address [lindex $argv 0]
;; if { $address != ""} {
;;     set tokens [split $address  "@"]
;;     set hostport  [lindex $tokens 1]
;;     # 没有user@ 只有hostport的情况
;;     if { $hostport == "" } {
;;         set hostport  [lindex $tokens 0]
;;     }
;;     set host  [lindex [split $hostport  ":"] 0]
;;     send "$host\r"
;;     expect "1\]$host*Input:*"
;;     send "1\r"
;;     expect "*"
;;     interact
;; } else {
;;     expect "*"
;;     interact
  ;; }

  (add-to-list 'tramp-default-method-alist '("BJ-APM-PROD-04" "admin" "jump"))

  (add-to-list 'tramp-methods
               '("jump"
                 (tramp-login-program "jump")
                 (tramp-login-args
                  (("%h")))
                 (tramp-async-args
                  (("%h")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-login
                  ("-l"))
                 (tramp-remote-shell-args
                  ("-c")))))

(defface toggle-to-root-header-face
  '((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root."
    :group 'emacs)

;;when open a file with sudo ,then change the face of the file to waring
(defun toggle-to-root-header-warning ()
  "*Display a warning in header line of the current buffer.
   This function is suitable to add to `toggle-to-root-hook'."
  (let* ((warning "WARNING: EDITING FILE AS ROOT!")
         (space (+ 6 (- (window-width) (length warning))))
         (bracket (make-string (/ space 5) ?-))
         (warning (concat bracket bracket warning bracket bracket bracket )))
    (setq header-line-format
          (propertize  warning 'face 'toggle-to-root-header-face))))

;; ;;; 加载一个新文件时，如果是sudo 开头的文件 ，也加上红色的外观
(defun my-sudo-find-file-hook ()
  (if (string-match "^/sudo:\\||sudo" (or (buffer-file-name)  (and (stringp dired-directory) dired-directory) "")) (toggle-to-root-header-warning))
  ;; (when (or (string-match "^/etc" (or (buffer-file-name)  dired-directory))
  ;;           (string-match "^/private/etc" (or (buffer-file-name)  dired-directory)))
  ;;   (find-alternate-file (concat "/sudo:root@" (get-localhost-name) ":" (or (buffer-file-name)  dired-directory))))
  )

(add-hook 'find-file-hooks 'my-sudo-find-file-hook);; find-file-hooks 是加载完file 之后调用的一个hook
(add-hook 'dired-mode-hook 'my-sudo-find-file-hook) ;;




(provide 'conf-sudo)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-sudo.el ends here.
