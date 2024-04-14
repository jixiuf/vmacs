(setq org-ai-default-chat-model "gpt-4-1106-preview")
(setq org-ai-default-chat-system-prompt (car ai-system-prompts))
(setq org-ai-image-directory "~/Documents/jianguo/jianguo/ai/images/")
(require 'org-ai)
(add-hook 'org-mode-hook #'org-ai-mode)

(defun vmacs-ai-after-chat-insertion-hook (&optional type arg2 )
  (unless buffer-file-name
    (setq buffer-file-name
          (expand-file-name (format-time-string "ai-%Y%m%d_%H%M%S.org" (current-time))
                            default-directory)))
  (setq-local epa-file-encrypt-to (default-value 'epa-file-encrypt-to))
  (when (eq type 'end)
    (run-with-timer 0.01 nil #'(lambda() (write-file buffer-file-name)))
    ;; (epa-file-write-region (point-min) (point-max) buffer-file-name)
    ))

(add-hook 'org-ai-after-chat-insertion-hook #'vmacs-ai-after-chat-insertion-hook)
;; (keymap-global-set "<f6>" "C-u C-c <return> <return>")

(provide 'conf-ai)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-ai.el ends here.
