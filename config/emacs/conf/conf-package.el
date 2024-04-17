(require 'package)
(setq package-archives
      '(("melpa-cn" .  "https://melpa.org/packages/")
        ("nognu-cn" .  "https://elpa.nongnu.org/nongnu/")
        ("gnu-cn"   .  "https://elpa.gnu.org/packages/")))

(or (file-exists-p package-user-dir) (package-refresh-contents))
;; (package-initialize)
(add-hook 'after-init-hook (lambda()
                             (package-install-selected-packages t)))

(provide 'conf-package)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-common.el ends here.
