;; -*- lexical-binding: t; -*-

(with-eval-after-load 'json-ts-mode
  (bray-state-map-set 'normal json-ts-mode-map "C-c C-p" #'json-ts-jq-path-at-point)
  (bray-state-map-set 'normal json-ts-mode-map "z" #'hs-toggle-hiding)
  (bray-state-map-set 'normal json-ts-mode-map "<tab>" #'(lambda()
                                                           (interactive)
                                                           (if (region-active-p)
                                                               (call-interactively #'json-pretty-print)
                                                             (call-interactively #'json-pretty-print-buffer) ))))
;; (add-hook 'js-mode-hook 'vmacs-js-mode-hook)
(add-hook 'json-ts-mode-hook 'vmacs-js-mode-hook)
;; (add-hook 'js-json-mode-hook 'vmacs-js-mode-hook)
(require 'hideshow)

;; evil 的za用来toggle hiddle
(defun vmacs-js-mode-hook()
  (modify-syntax-entry ?_ "_" (syntax-table))  ;还是让 _ 作为symbol，
  (hs-minor-mode 1))
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
