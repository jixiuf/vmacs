(defconst my-protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil)))

(defun vmacs-protobuf-mode-hook()
  (c-add-style "my-style" my-protobuf-style t)
  (local-set-key (kbd "C-M-h") 'mark-defun) ;evil-mode bind on mf
  (local-set-key (kbd "M-h") 'mark-paragraph) ;evil-mode bind on mh
  (setq indent-region-function 'protobuf-indent-align))

(add-hook 'protobuf-mode-hook 'vmacs-protobuf-mode-hook)

(provide 'conf-program-protobuf)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-program-protobuf.el ends here.
