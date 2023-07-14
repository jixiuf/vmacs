(when (eq system-type 'darwin)
  (setq chatgpt-shell-openai-key
        (lambda ()
          ;; (auth-source-pass-get 'secret "openai-key") ; alternative using pass support in auth-sources
          (nth 0 (process-lines "security" "find-internet-password" "-g" "-a" "luojilab.com" "-s" "openai.com" "-w")))))
(when (eq system-type 'gnu/linux)
  (setq-default  chatgpt-shell-api-url-base "http://ddai-proxy.luojilab.com:8081")
  (setq chatgpt-shell-openai-key
        (lambda ()
        (auth-source-pick-first-password :host "ddai-proxy.luojilab.com"))))

(setq exec-path (delete-dups  (cons "/usr/local/opt/curl/bin" exec-path)))
(setenv "PATH" (concat "/usr/local/opt/curl/bin" ":" (getenv "PATH") ))
(require 'ob-chatgpt-shell)
(ob-chatgpt-shell-setup)

(provide 'conf-ai)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-ai.el ends here.
