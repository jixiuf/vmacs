(require 'package)
  (setq package-archives
        ;; '(("melpa-cn" . "http://mirrors.163.com/elpa/melpa/")
        '(("melpa-cn" .  "http://melpa.org/packages/")
          ("nognu-cn" . "http://mirrors.163.com/elpa/nongnu/")
          ("gnu-cn"   . "http://mirrors.163.com/elpa/gnu/")))

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


(add-hook 'after-init-hook (lambda() (ensure-package-installed package-selected-packages)))

(provide 'conf-package)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-common.el ends here.
