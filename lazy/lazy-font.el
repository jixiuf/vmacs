;;;###autoload
(defun create-frame-font-big-mac()          ;emacs 若直接启动 启动时调用此函数似乎无效
  (interactive)
  (set-face-attribute
   'default nil :font "Menlo 16")
  ;; Chinese Font
  (dolist (charset '( han symbol cjk-misc bopomofo)) ;script 可以通过C-uC-x=查看当前光标下的字的信息
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "PingFang SC" :size 20)))

  (set-fontset-font (frame-parameter nil 'font)
                    'kana                 ;script ｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺ
                    (font-spec :family "Hiragino Sans" :size 20))
  (set-fontset-font (frame-parameter nil 'font)
                    'hangul               ;script 까까까까까까까까까까까까까까까까까까까까
                    (font-spec :family "Apple SD Gothic Neo" :size 23)))

;;;###autoload
(defun create-frame-font-mac()
  (interactive)
  (set-face-attribute
   'default nil :font "Menlo 14")
  ;; Chinese Font
  (dolist (charset '( han symbol cjk-misc bopomofo)) ;script 可以通过C-uC-x=查看当前光标下的字的信息
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "PingFang SC" :size 16)))

  (set-fontset-font (frame-parameter nil 'font)
                    'kana                 ;script ｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺ
                    (font-spec :family "Hiragino Sans" :size 16))
  (set-fontset-font (frame-parameter nil 'font)
                    'hangul               ;script 까까까까까까까까까까까까까까까까까까까까
                    (font-spec :family "Apple SD Gothic Neo" :size 18)))
(provide 'lazy-font)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-font.el ends here.
