;;; org-ai-useful.el --- A few useful functions and commands -*- lexical-binding: t; -*-
(setq org-ai-default-chat-model "gpt-4o-mini")
(setq org-ai-default-chat-system-prompt (car ai-system-prompts))
(setq org-ai-image-directory "~/Documents/jianguo/jianguo/ai/images/")
(setq org-ai-openai-api-token nil)
;; (setq org-ai-auto-fill nil)
(require 'org-ai)
(add-hook 'org-mode-hook #'org-ai-mode)

(defun vmacs-ai-after-chat-insertion-hook (&optional type arg2 )
  (when (string-equal default-directory "~/Documents/jianguo/jianguo/ai/")
    (unless buffer-file-name
      (setq buffer-file-name
            (expand-file-name (format-time-string "ai-%Y%m%d_%H%M%S.org" (current-time))
                              default-directory)))
    (when (eq type 'end)
      (run-with-timer 0.01 nil #'(lambda() (write-file buffer-file-name)))
      ;; (epa-file-write-region (point-min) (point-max) buffer-file-name)
      ))
  ;; (setq-local epa-file-encrypt-to (default-value 'epa-file-encrypt-to))
  )

(add-hook 'org-ai-after-chat-insertion-hook #'vmacs-ai-after-chat-insertion-hook)
;; (keymap-global-set "<f6>" "C-u C-c <return> <return>")
(with-eval-after-load 'org-ai
  (cl-defun org-ai--output-to-buffer (start end text-prompt-fn output-buffer &optional &key show-output-buffer callback)
    "Get the currently selected text, create a prompt, insert the response.
`OUTPUT-BUFFER' is the buffer to insert the response in.
`TEXT-PROMPT-FN' is a function that takes the selected text as
argument and returns a prompt.
`START' is the buffer position of the region.
`END' is the buffer position of the region.
`OUTPUT-BUFFER' is the name or the buffer to insert the response in.
`CALLBACK' is a function to call after the response is inserted."
    (let* ((text (buffer-substring-no-properties start end))
           (full-prompt (funcall text-prompt-fn text))
           (output-buffer (get-buffer-create output-buffer)))
      (with-current-buffer output-buffer
        (read-only-mode -1)
        (erase-buffer)
        (toggle-truncate-lines -1)
        (when show-output-buffer
          (display-buffer output-buffer)))
      (org-ai-prompt full-prompt :output-buffer output-buffer :callback callback))))


;; (setq aider-args '("--model" "gpt-4o-mini"))
;; ;; (setq aider-args nil)
;; ;; https://aider.chat/docs/git.html
;; (require 'conf-program-python)
;; (setenv "AIDER_AUTO_COMMITS" "false")
;; ;; (setenv "AIDER_DEEPSEEK" "true")
(provide 'conf-ai)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-ai.el ends here.
