(add-hook 'lua-mode-hook 'vmacs-lua-mode-hook)

(defun vmacs-lua-mode-hook()
  (modify-syntax-entry ?:  "_" (syntax-table)) ;还是让 ":" 作为symbol，还不是word
  )

(provide 'conf-program-lua)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-lualang.el ends here.
