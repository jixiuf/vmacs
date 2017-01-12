;;确保需要的package 在本地都存在
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
;; 为evil-mode 的n让位
(define-key tabulated-list-mode-map "n" nil)
(define-key tabulated-list-mode-map "0" nil)
(provide 'conf-package)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-common.el ends here.
