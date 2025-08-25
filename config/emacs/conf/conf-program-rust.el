;; https://kurotych.com/posts/rust_neovim_debugger/  -*- lexical-binding: t; -*-
;; https://github.com/vadimcn/codelldb
(use-package eglot
  :config (add-to-list 'eglot-server-programs
                       `(rust-ts-mode . ("rust-analyzer"
                                         :initializationOptions
                                         ( :procMacro (:enable t)
                                           :cargo ( :buildScripts (:enable t)
                                                    :features "all"))))))

(provide 'conf-program-rust)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-program-rust.el ends here.
