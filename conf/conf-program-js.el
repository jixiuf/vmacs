(add-hook 'js-mode-hook 'vmacs-js-mode-hook)
(add-hook 'js-json-mode-hook 'vmacs-js-mode-hook)
(require 'hideshow)

;; evil 的za用来toggle hiddle
(defun vmacs-js-mode-hook()
  (modify-syntax-entry ?_ "_" (syntax-table))  ;还是让 _ 作为symbol，
  (hs-minor-mode 1)
  (local-set-key (kbd "<return>") 'hs-toggle-hiding)
  (local-set-key (kbd "C-=") 'json-unescape)
  (local-set-key (kbd "C-M-\\") (lambda()
                                  (interactive)
                                  (if (region-active-p)
                                      (call-interactively #'json-pretty-print)
                                    (call-interactively #'json-pretty-print-buffer) ) ))
  )

(provide 'conf-program-js)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-jslang.el ends here.
