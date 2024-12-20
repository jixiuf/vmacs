;;; -*- lexical-binding: t; -*-
(require 'gptel)
(setq gptel-model 'gpt-4o)
(setq gptel-default-mode 'org-mode)
(setq gptel-display-buffer-action  '(pop-to-buffer-same-window))
(add-hook 'gptel-post-response-functions 'gptel-end-of-response)
(keymap-set gptel-mode-map "C-c C-c" #'vmacs-gptel-send)
(keymap-set gptel-mode-map "C-c C-v" #'gptel-menu)
(keymap-set gptel-mode-map "C-c C-d" #'vmacs-gptel-prompt)

(defun vmacs-gptel-send(&optional arg)
  (interactive)
  (gptel--restore-state)
  (gptel-send arg))

(defun vmacs-gptel-prompt(&optional arg)
  (interactive)
  (let* ((keys (mapcar #'car gptel-directives))
         (select (completing-read "Select directive key: " keys)))
    (when select
      (when-let* ((val (alist-get (intern select) gptel-directives)))
        (setq select val))
      (setq-local gptel--system-message select)
      (when gptel-mode
        (gptel-org-set-properties (point-min))
        (when (gptel--get-buffer-bounds)
          (gptel-org--save-state))
        (vmacs-ai-after-chat-insertion-hook)
        ))))

(defun vmacs-ai-after-chat-insertion-hook (&optional beg end)
  (when gptel-mode
    (unless buffer-file-name
      (setq buffer-file-name
            (expand-file-name (format-time-string "ai-%Y%m%d_%H%M%S.org" (current-time))
                              "~/Documents/jianguo/jianguo/ai/")))
    (write-file buffer-file-name)))

(add-hook 'gptel-post-response-functions #'vmacs-ai-after-chat-insertion-hook)

(provide 'conf-ai)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-ai.el ends here.
