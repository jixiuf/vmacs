;; 自动编译额el文件，如果el文件比elc文件新，
;; 这段配置要尽可能得早，但要在(package-initialize)之后

(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(provide 'conf-auto-compile)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-auto-compile.el ends here.
