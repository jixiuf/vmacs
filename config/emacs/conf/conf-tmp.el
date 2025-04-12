;;; -*- lexical-binding: t -*-
;; (add-to-list 'load-path "~/repos/emacs-application-framework/")
;; (setq eaf-python-command "/Users/jixiuf/.pyenv/versions/3.9.7/bin/python3")

;; (require 'eaf nil t)
;; (require 'eaf-browser)
;; (require 'eaf-evil)
;; (setq eaf-evil-leader-keymap vmacs-space-leader-mode-map)
;; (setq eaf-evil-leader-key "C-SPC")

 ;; (defalias 'browse-web #'eaf-open-browser)
;; (global-set-key (kbd "C-M-s-l" ) #'eaf-open-browser-with-history)

;; (add-to-list 'load-path "~/repos/emacs-libvterm")
(setq sql-connection-alist
      `(("mysql-localhost-test"
         (sql-product 'mysql)
         (sql-user "luojilab")
         (sql-server "localhost")
         (sql-password "uXAqvbX5LR")
         (sql-database "test")
         (sql-port 3306))
        ("oracle"
         (sql-product 'oracle)
         (sql-user "scott")
         (sql-server "localhost")
         ;; (sql-port 3306)
         (sql-database "scott"))))



(provide 'conf-tmp)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-tmp.el ends here.
