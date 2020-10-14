;;; Code:

(setq sql-input-ring-file-name "~/.emacs.d/cache/sql-cmd-hist")
(defun vmacs-try-write-sql-hist()
  "kill-buffer方式退出时,不会自动写hist, 此处修复之."
  (when (equal major-mode 'sql-interactive-mode)
    (comint-write-input-ring)))

(add-hook 'kill-buffer-hook 'vmacs-try-write-sql-hist)
;;(setq comint-input-ring-size 500)

;; ;; demo
(setq sql-connection-alist
      `(("mysql-localhost-test"
         (sql-product 'mysql)
         (sql-user "root")
         (sql-server "localhost")
         (sql-password "root")
         (sql-database "test")
         (sql-port 3306))
        ("oracle"
         (sql-product 'oracle)
         (sql-user "scott")
         (sql-server "localhost")
         ;; (sql-port 3306)
         (sql-database "scott"))))


;;; sqlserver custom
(setq sql-ms-options (quote ("-w" "65535" "-h" "20000" ))) ;长度设的长一点，免折行。分页20000行一页
(setq sql-ms-program "sqlcmd")                ; 不使用默认的osql.exe ,似乎sqlcmd 比osql快。,并且osql有被微软弃用的可能。
;; mysql optional
;; Make mysql not buffer sending stuff to the emacs-subprocess-pipes
;; -n unbuffered -B batch(tab separated) -f force(go on after error) -i ignore spaces -q no caching -t table format
;; (setq-default sql-mysql-options (quote ("-n" "-B" "-f" "-i" "-q" "-t")))
(setq sql-mysql-options '("-C" "-t" "-f" "-n")) ;; MS 上，mysql 不回显
;;; 在普通的sql mode 中以上命令的前提是当前buffer与*SQL* 进行了关联
;; `C-cC-b' send buffer content to *SQL* buffer中去执行。
;; `C-cC-r' send 选中区域到 *SQL* buffer中去执行。
;; `C-cC-c' (sql-send-paragraph)
;;下面这个hook，如果在启用*SQL* 时已经有sql-mode 的buffer了，则将其与*SQL* 进行关联
(add-hook 'sql-interactive-mode-hook 'vmacs-sql-set-buffer)
(defun vmacs-sql-set-buffer ()
  "Sets the SQLi buffer for all unconnected SQL buffers.
Called from `sql-interactive-mode-hook'."
  (let ((new-buffer (current-buffer)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (unless (buffer-live-p sql-buffer)
          (setq sql-buffer new-buffer)
          (run-hooks 'sql-set-sqli-hook))))))

(defun vmacs-sql-mode-hook()
  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end) "*/")
  (toggle-truncate-lines t)
  (setq comint-input-ignoredups t)      ;避免hist命令重复
  )
(add-hook 'sql-mode-hook 'vmacs-sql-mode-hook)
(add-hook 'sql-interactive-mode-hook 'vmacs-sql-mode-hook)

(defadvice sql-mysql (around start-mysql-complete-minor-mode activate)
  "enable `mysql-complete-minor-mode' minor mode."
  ad-do-it
  (mysql-complete-minor-mode))
(define-derived-mode mysql-mode sql-mode "mysql"
  "mysql mode"
  (mysql-complete-minor-mode))

(require 'sqlparser-mysql-complete)
(define-key sql-mode-map (quote [M-return]) 'sqlparser-mysql-complete)
(define-key sql-interactive-mode-map  (quote [M-return]) 'sqlparser-mysql-complete)


(provide 'conf-sql)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-sql.el ends here.
