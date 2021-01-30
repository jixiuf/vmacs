;;确保需要的package 在本地都存在
(require 'package)
(setq package-name-column-width 30)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; mirror
  ;; (setq package-archives
  ;;       '(("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
  ;;         ("org-cn"   . "http://mirrors.cloud.tencent.com/elpa/org/")
  ;;         ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")))
  (setq package-archives
        '(("melpa-cn" . "http://mirrors.163.com/elpa/melpa/")
          ("gnu-cn"   . "http://mirrors.163.com/elpa/gnu/")))

;; (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
;;                          ("melpa" . "http://elpa.emacs-china.org/melpa/")))

;; (add-to-list 'package-archives '("melpa" . "http://www.mirrorservice.org/sites/melpa.org/packages/"))

;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository
;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir) (package-refresh-contents))
;; (package-initialize)

(defun ensure-package-installed (packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; 国内有人做了个melpa 的镜像，
;; (add-to-list 'package-archives '("popkit" . "http://elpa.popkit.org/packages/"))

(ensure-package-installed package-selected-packages)

;; 为evil-mode 的gg 让位
(define-key package-menu-mode-map "g" nil)
(define-key package-menu-mode-map "/" nil)
;; 为evil-mode 的n让位
(define-key tabulated-list-mode-map "n" nil)
(define-key tabulated-list-mode-map "0" nil)
(provide 'conf-package)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-common.el ends here.
