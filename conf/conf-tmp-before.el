;; (when (file-directory-p "~/repos/emacs-libvterm")
;;   (add-to-list 'load-path "~/repos/emacs-libvterm"))

(when (file-directory-p "~/repos/magit/lisp")
  (add-to-list 'load-path "~/repos/magit/lisp"))
(when (file-directory-p "~/repos/libegit2")
  (add-to-list 'load-path "~/repos/libegit2"))
(when (file-directory-p "~/repos/libegit2/build")
  (add-to-list 'load-path "~/repos/libegit2/build"))
(when (vmacs-not-dumping-p)
  (require 'magit nil t))

;; (when (file-directory-p "~/repos/vterm-toggle")
;;   (add-to-list 'load-path "~/repos/vterm-toggle"))



(provide 'conf-tmp-before)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-tmp-before.el ends here.
