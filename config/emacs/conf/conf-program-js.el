;; -*- lexical-binding: t; -*-

;; (add-hook 'js-mode-hook 'vmacs-js-mode-hook)
(add-hook 'json-ts-mode-hook 'vmacs-js-mode-hook)
;; (add-hook 'js-json-mode-hook 'vmacs-js-mode-hook)
(require 'hideshow)

;; evil 的za用来toggle hiddle
(defun vmacs-js-mode-hook()
  (modify-syntax-entry ?_ "_" (syntax-table))  ;还是让 _ 作为symbol，
  (hs-minor-mode 1)
  (meep-local-set-key  "<tab>" 'hs-toggle-hiding)
  (local-set-key (kbd "C-=") 'json-unescape)
  (meep-local-set-key  "=" (lambda()
                             (interactive)
                             (if (region-active-p)
                                 (call-interactively #'json-pretty-print)
                               (call-interactively #'json-pretty-print-buffer) ))))
;; (add-to-list 'hs-special-modes-alist
;;              '(js-mode
;;                "\\[\\|{" "\\]\\|}" "/[*/]" nil nil))
(add-to-list 'hs-special-modes-alist
             '(json-ts-mode
               "\\[\\|{" "\\]\\|}" "/[*/]" nil nil))

(provide 'conf-program-js)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-jslang.el ends here.
