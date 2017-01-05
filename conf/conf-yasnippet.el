;; config for yasnippet

;; 使用自己定义的snippet
;; 如果你想用yasnippet带的snippet

;; (require 'yasnippet) ;;
(autoload 'yas-global-mode "yasnippet")
(require 'yasnippet)
(unless noninteractive (yas-global-mode 1)) ;batch 模式下不启用yas
(setq-default yas-prompt-functions '(yas-completing-prompt))
(define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)

(provide 'conf-yasnippet)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-yasnippet.el ends here.
