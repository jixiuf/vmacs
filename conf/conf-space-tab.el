(setq-default
 indent-tabs-mode nil                   ;用空格代替tab
 tab-width 4
 indicate-empty-lines t                 ;如果文件末尾有空行， 以可视地形式提醒
 ;; x-stretch-cursor nil                  ;如果设置为t，光标在TAB字符上会显示为一个大方块
 mode-require-final-newline nil
 require-final-newline t ; 文档末尾插入空行
                                        ;(setq next-line-add-newlines t);到达最后一行后继续C-n将添加空行
 ;;(setq-default line-spacing 1);;设置行距
 whitespace-style   '(face
                      trailing space-before-tab newline
                      empty space-after-tab
                      missing-newline-at-eof)
 whitespace-action '(auto-cleanup))

;; 只对特定的major mode 启用ethan-wspace-mode,因为在makefile 中启用会有bug
(dolist (hook '(java-mode-hook c++-mode-hook python-mode-hook c-mode-hook org-mode-hook perl-mode-hook
                               ojbc-mode-hook sh-mode-hook yaml-mode-hook
                               makefile-mode-hook makefile-bsdmake-mode-hook
                               protobuf-mode-hook objc-mode-hook lua-mode-hook nxml-mode-hook
                               gitconfig-mode-hook go-mode-hook js-mode-hook
                               cperl-mode-hook emacs-lisp-mode-hook erlang-mode-hook))
  (add-hook hook 'whitespace-mode))


(provide 'conf-space-tab)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-space.el ends here.
