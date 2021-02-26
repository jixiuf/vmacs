 (when (file-directory-p "~/repos/emacs-libvterm")
   (add-to-list 'load-path "~/repos/emacs-libvterm"))
 (when (file-directory-p "~/repos/Emacs-wgrep")
   (add-to-list 'load-path "~/repos/Emacs-wgrep"))

(when (file-directory-p "~/repos/magit/lisp")
  (add-to-list 'load-path "~/repos/magit/lisp"))
(when (file-directory-p "~/repos/libegit2")
  (add-to-list 'load-path "~/repos/libegit2"))
(when (file-directory-p "~/repos/libegit2/build")
  (add-to-list 'load-path "~/repos/libegit2/build"))

 (when (file-directory-p "~/repos/vterm-toggle")
   (add-to-list 'load-path "~/repos/vterm-toggle"))


;; (when (file-directory-p "~/repos/marginalia")
;;   (add-to-list 'load-path "~/repos/marginalia"))

;; (when (file-directory-p "~/repos/embark")
;;   (add-to-list 'load-path "~/repos/embark"))
;; (when (file-directory-p "~/repos/consult")
;;   (add-to-list 'load-path "~/repos/consult")
;;   (require 'consult)
;;   )
;; (when (file-directory-p "~/repos/icomplete-vertical")
;;   (add-to-list 'load-path "~/repos/icomplete-vertical"))
;; (when (file-directory-p "~/repos/evil")
;;   (add-to-list 'load-path "~/repos/evil"))

(provide 'conf-tmp-before)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-tmp-before.el ends here.
