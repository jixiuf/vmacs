(add-hook 'js-mode-hook 'vmacs-js-mode-hook)
(require 'hideshow)

;; evil 的za用来toggle hiddle
(defun vmacs-js-mode-hook()
  (modify-syntax-entry ?- "_" (syntax-table))  ;还是让 - 作为symbol，
  (modify-syntax-entry ?_ "_" (syntax-table))  ;还是让 _ 作为symbol，
  (hs-minor-mode 1)
  (evil-define-key 'normal 'local (kbd "<return>") 'evil-toggle-fold)
  )

(provide 'conf-program-js)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-jslang.el ends here.
