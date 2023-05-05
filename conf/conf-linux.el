;; 获取当前输入法
;; ibus engine
;; xkb:us::eng  or rime
;; ibus engine rime  # 将输入法更改为
(defun switch-to-english-input-method ()
  "Switch to English input method."
  (interactive)
  (call-process "ibus" nil nil nil "engine" "xkb:us::eng"))
(defun switch-to-rime-input-method ()
  "Switch to English input method."
  (interactive)
  (call-process "ibus" nil nil nil "engine" "rime"))
(defun get-input-method-state()
  (string-trim (shell-command-to-string "ibus engine")))

(add-hook 'evil-normal-state-entry-hook #'switch-to-english-input-method)
(defun on-toggle-input-method()
  (interactive)
  (if (string-equal (get-input-method-state) "rime")
      (switch-to-english-input-method)
    (switch-to-rime-input-method)
    (evil-insert-state)))
(global-set-key (kbd "<f15>") 'vmacs-toggle-input-method)



(provide 'conf-linux)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-linux.el ends here.
