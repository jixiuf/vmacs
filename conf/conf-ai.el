(setq org-ai-openai-chat-endpoint "http://192.168.124.26:8081/v1/chat/completions")
(setq org-ai-openai-completion-endpoint "http://192.168.124.26:8081/v1/completions")
(setq org-ai-openai-image-generation-endpoint "http://192.168.124.26:8081/v1/images/generations")
(setq org-ai-openai-image-variation-endpoint "http://192.168.124.26:8081/v1/images/variations")

(setq org-ai-default-chat-model "gpt-4-1106-preview")
(setq ai-system-prompts '(
                          " 我是一名go程序员，我的OS是 gentoo with systemd & hyprland wm ,平时使用emacs编辑器,回答问题时，不要重复我的提问，回答尽量简洁，如果你不会，请回答“不会”，"
                          ))
(setq org-ai-default-chat-system-prompt (car ai-system-prompts))
(setq org-ai-image-directory "~/Documents/jianguo/jianguo/ai/images/")
(add-hook 'org-mode-hook #'org-ai-mode)
(defun vmacs-ai()
  (interactive)
  (let ((default-directory "~/Documents/jianguo/jianguo/ai/")
        (name "*ai*"))
    (with-current-buffer (generate-new-buffer name)
      (org-mode)
      (yas-minor-mode 1)
      (insert "ai")
      (yas-expand-from-trigger-key)
      (display-buffer (current-buffer) '(pop-to-buffer)))))

(defun vmacs-ai-after-chat-insertion-hook (&optional arg arg2 )
  (unless buffer-file-name
    (setq buffer-file-name
          (format-time-string "ai-%Y%m%d_%H%M%S.org" (current-time))))
  (write-file buffer-file-name))

(add-hook 'org-ai-after-chat-insertion-hook #'vmacs-ai-after-chat-insertion-hook)
(vmacs-leader  "fg" #'vmacs-ai)
;; (keymap-global-set "<f6>" "C-u C-c <return> <return>")

(provide 'conf-ai)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-ai.el ends here.
