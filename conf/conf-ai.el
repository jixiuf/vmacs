(require 'gptel)
(setq-default
 gptel-prompt-prefix-alist '((markdown-mode . "# ") (org-mode . "* "))
 gptel-response-prefix-alist '((markdown-mode . "# ") (org-mode . "** "))
 gptel-default-mode 'org-mode
 ;; gptel-max-tokens 500
 gptel-directives
 '((default . "我是一名go程序言，我的OS是 gentoo with systemd & hyprland wm ,平时使用emacs编辑器,回答问题时，不要重复我的提问，回答尽量简洁，如果你不会，请回答“不会”，")
   (writing . "你是一个写作助手，请不要带有任何偏见，以客观公正的态度回答问题,现在需要你来帮我协助完善相关方案、提供写作思路")
   )
 gptel-model "gpt-4-1106-preview"
 gptel-backend (gptel-make-openai
                   "openai"
                 :models '("gpt-3.5-turbo" "gpt-3.5-turbo-16k" "gpt-4"
                           "gpt-4-turbo-preview" "gpt-4-32k" "gpt-4-1106-preview"
                           "gpt-4-0125-preview")
                 :stream t
                 :key 'gptel-api-key
                 :protocol "http"
                 :host "192.168.124.26:8081"
                 ))

(defun vmacs-gptel-save(_ _ &optional arg)
  (unless buffer-file-name
    (setq buffer-file-name
          (format "%s-%s"
                  (string-trim (buffer-name) "*" "*")
                  (format-time-string "%Y%m%d_%H%M%S.org" (current-time)))))
  (write-file buffer-file-name))

(add-hook 'gptel-post-response-functions 'gptel-end-of-response)
(add-hook 'gptel-post-response-functions 'vmacs-gptel-save 1)
(define-key gptel-mode-map (kbd "<RET>") 'gptel-send)
(define-key gptel-mode-map (kbd "C-c C-c") 'gptel-send)
;; (keymap-global-set "<f6>" "C-u C-c <return> <return>")

(provide 'conf-ai)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-ai.el ends here.
