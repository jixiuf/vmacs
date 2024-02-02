(when (eq system-type 'darwin)
  (setq chatgpt-shell-openai-key
        (lambda ()
          ;; (auth-source-pass-get 'secret "openai-key") ; alternative using pass support in auth-sources
          (nth 0 (process-lines "security" "find-internet-password" "-g" "-a" "luojilab.com" "-s" "openai.com" "-w")))))
(when (eq system-type 'gnu/linux)
  (setq-default  chatgpt-shell-api-url-base "http://192.168.124.26:8081")
  (setq chatgpt-shell-openai-key
        (lambda ()
        (auth-source-pick-first-password :host "ddai-proxy.luojilab.com"))))
;; 我是一个时间有限的程序员，如无特别说明请优先使用go/elisp相关的语言进行回答，请回答时不要把我当成白痴，不要回答显而易见的事情，包括我输入给你的内容, 请回答尽量简洁，如果你不会，请回答“不会”，不要胡乱回答，请尽量使用markdown语法来组织答案，代码可以使用markdown blocks语法，不用给我解释代码。 我的OS是 gentoo with systemd
(setq chatgpt-shell-system-prompts  '( ("Code" . "我是一名go程序言，我的OS是 gentoo with systemd & hyprland wm ,平时使用emacs编辑器,回答问题时，请你尽量简洁，如果你不会，请回答“不会”，" )))
(setq chatgpt-shell-system-prompt 0)


(setq exec-path (delete-dups  (cons "/usr/local/opt/curl/bin" exec-path)))
(setenv "PATH" (concat "/usr/local/opt/curl/bin" ":" (getenv "PATH") ))
(setq shell-maker-prompt-before-killing-buffer nil)
(provide 'conf-ai)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-ai.el ends here.
