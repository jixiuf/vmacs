;;; -*- lexical-binding: t -*-
(with-eval-after-load 'compile
  (setq-default
   compilation-ask-about-save nil         ;编译之前自动保存buffer
   compilation-auto-jump-to-first-error nil ;编译完成后自动跳到第一个error处
   compilation-disable-input nil
   compilation-scroll-output t
   compilation-read-command nil;默认emacs提供M-x:compile 用于编译，编译前 不提示用户输入命令
   compilation-always-kill t    ;启动新的compile命令前，自动杀掉之前未完成的compile命令
   )


  (define-key compilation-mode-map "g" nil)
  (define-key compilation-mode-map "r" 'recompile)
  (define-key compilation-mode-map "C-o" nil)
  ;; (require 'ansi-color)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  )
;;                                ;



(provide 'conf-compile)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-compile.el ends here.
