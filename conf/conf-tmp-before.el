(when (file-directory-p "~/repos/emacs-libvterm")
  (add-to-list 'load-path "~/repos/emacs-libvterm"))

(when (file-directory-p "~/repos/magit/lisp")
  (add-to-list 'load-path "~/repos/magit/lisp"))
(require 'magit nil t)

(when (file-directory-p "~/repos/vterm-toggle")
  (add-to-list 'load-path "~/repos/vterm-toggle"))



(provide 'conf-tmp-before)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-tmp-before.el ends here.
