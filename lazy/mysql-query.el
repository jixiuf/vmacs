;;; mysql-query.el --- execute sql select using mysql. -*- coding:utf-8 -*-

;; Copyright (C) 2011~2012 纪秀峰(Joseph)

;; Last Updated: Joseph 2012-01-15 21:33:25 星期日
;; Created: 2012-01-12 10:52
;; Version: 0.1.0
;; Author: 纪秀峰(Joseph)  jixiuf@gmail.com
;; Keywords: mysql sql emacs
;; Filename: mysql-query.el
;; Description:execute sql select using mysql.
;; URL:http://www.emacswiki.org/emacs/download/mysql-query.el
;; https://github.com/jixiuf/sqlparser
;; Compatibility: Test on Linux,Windows

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; execute sql using mysql and return as list .
;;  (mysql-query "select 1"  mysql-connection-info)
;;  (mysql-query "select user ,sql-password from mysql.user limit 0,2"  mysql-connection-info)
;;  (mysql-query-with-heading "select user ,sql-password from mysql.user limit 0,2"  mysql-connection-info)
;;
;; recommended
;;
;; the normal way to use mysql-query.el is :
;; 1:
;; (defvar mysql-connection (mysql-query-create-connection connection-info))
;; or
;; (defvar mysql-connection nil)
;; (unless (mysql-query-connection-alive-p  mysql-connection)
;;   (setq mysql-connection (call-interactively 'mysql-query-create-connection)))
;;
;; 2:
;;   (mysql-query              "select user from mysql.user" mysql-connection)
;;   (mysql-query-with-heading "select user from mysql.user" mysql-connection)
;;

;;; Code:

(require 'sql)

(defgroup mysql-query nil
  "mysql query."
  :group 'SQL)

(defcustom mysql-connection-info
  '((sql-server  "localhost")
    (sql-user  "root")
    (sql-password  "root")
    (sql-dababase  "test")
    (sql-port  3306)
    )
  "default mysql connection info ."
  :group 'mysql-query
  :type 'alist)

(defcustom mysql-command "mysql"
  "default mysql command ,."
  :group 'mysql-query
  :type 'string)


(defcustom mysql-command-other-options
  '("--column-names" "--unbuffered")
  "default mysql connection info ."
  :group 'mysql-query)


(defun mysql-query-read-connect-string()
  "set hostname sql-dababase sql-user sql-password interactive"
  (let ((connection-info (copy-alist mysql-connection-info)))
    (setcdr  (assoc 'sql-user connection-info)
             (list(read-string (format  "sql-user(default:%s):"  (cadr (assoc 'sql-user connection-info)))
                          "" nil   (cadr (assoc 'sql-user connection-info)))))
    (setcdr  (assoc 'sql-password connection-info)
             (list (read-passwd "password:"  nil )))
    (setcdr  (assoc 'sql-dababase connection-info)
             (list (read-string (format  "sql-dababase(default:%s):"  (cadr (assoc 'sql-dababase connection-info)))
                          "" nil (cadr (assoc 'sql-dababase connection-info)))))
    (setcdr  (assoc 'sql-server connection-info)
             (list (read-string (format  "sql-server(default:%s):"  (cadr (assoc 'sql-server connection-info)))
                          "" nil (cadr (assoc 'sql-server connection-info)))))
    (setcdr  (assoc 'sql-port connection-info)
             (list (read-string (format  "sql-port(default:%s):"  (cadr (assoc 'sql-port connection-info)))
                          "" nil   (cadr (assoc 'sql-port connection-info)))))
    (setq-default mysql-connection-info connection-info) ;;update default info
    connection-info))

(defun mysql-query-create-connection (&optional connection-info)
  (interactive (list (mysql-query-read-connect-string)))
  (print connection-info)
  connection-info)

(defun mysql-query-complete-create-connection (&optional connection-info)
  (interactive)
  (if sql-connection-alist
      (let ((connection (sql-read-connection "Connection: ")))
        (setq connection-info (cdr (assoc connection sql-connection-alist)))
        )
    (call-interactively 'mysql-query-create-connection))
  connection-info)

(defun mysql-query-connection-alive-p (connection)
  connection)

;; ("-h" "localhost" "-u" "root" "-proot" "-P" "3306" "--database=mysql" "--column-names" "-s" "--unbuffered")
(defun mysql-format-command-args (connection-info)
  "Returns a list of all the arguments for the mysql  program.
  default: mysql -h localhost -u root -proot -s  --database=mysql "
  (apply 'list "-h" (cadr (assoc 'sql-server connection-info))
         "-u" (cadr (assoc 'sql-user connection-info))
         (concat "-p" (cadr (assoc 'sql-password connection-info)))
         "-P" (number-to-string (cadr (assoc 'sql-port connection-info)))
         ;;use tab as column separator char
         (if (cadr (assoc 'sql-dababase connection-info))
           (concat "--database=" (cadr (assoc 'sql-dababase connection-info))) "")
         mysql-command-other-options))

(defun mysql-query-parse(raw-result-buf)
  "parse the result of mysql -e 'sql' ,the separator char is \t."
  (let  (result row line-count)
    (when raw-result-buf
      (with-current-buffer raw-result-buf
        (setq line-count (count-lines (point-min) (point-max)))
        (goto-char  (point-min))
        (while (< (line-number-at-pos) line-count)
          (setq row (split-string (buffer-substring-no-properties
                                   (point-at-bol) (point-at-eol)) "\t" nil))
          (setq result (append result (list row)))
          (forward-line))
        ;;the last line always \n on windows,but not no linux
        (unless  (string= "" (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
          (setq row (split-string (buffer-substring-no-properties
                                   (point-at-bol) (point-at-eol)) "\t" nil))
          (setq result (append result (list row)))))
      (kill-buffer raw-result-buf))
    result))

(defun mysql-query-raw ( sql connection-info )
  "Returns a list of all the arguments for the mysql  program.
  default: mysql -h localhost -u root -proot -s  --database=test -e"
  (let((result-buf-name  (concat " *mysql-query-" (number-to-string (random)) "*"))
       (result))
    (setq result
          (apply 'call-process mysql-command nil result-buf-name nil
                 (append (mysql-format-command-args connection-info) (list "--batch" "-e" sql))))
    (if (= result 0)                    ;success
        result-buf-name nil)))

;; (mysql-query-with-heading "select user ,sql-password from mysql.user"  mysql-connection-info)
(defun mysql-query-with-heading (sql &optional connection-info)
  "execute query ,using `connection-info' if `connection-info' is nil,
using `mysql-connection-info' instead"
  (unless connection-info (setq connection-info (mysql-query-read-connect-string)))
  (let* ((raw-result-buf (mysql-query-raw sql connection-info ))
         (result (mysql-query-parse raw-result-buf)))
    result))


;; (mysql-query "select user ,sql-password from mysql.user"  mysql-connection-info)
;; (mysql-query "select 1"  mysql-connection-info)

(defun mysql-query (sql &optional connection-info)
  (let ((result (mysql-query-with-heading sql connection-info)))
    (when (and result (listp result))
      (cdr result))))

(provide 'mysql-query)
;;; mysql-query.el ends here
