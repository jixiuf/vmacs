;; ;;; Code:

;; (defadvice eshell-send-input (around change-buffer-name activate)
;;   "change-buffer-name."
;;   (let ((input (eshell-get-old-input))
;;         (eshell-buffer)
;;         )
;;     ad-do-it
;;     (setq eshell-buffer (vmacs-eshell--generate-buffer-name "*esh* " input default-directory))
;;     (when (equal major-mode 'eshell-mode)
;;       ;; 有可能exit之后，当前buffer就不是eshell了
;;       (unless (string-equal eshell-buffer (buffer-name))
;;         (rename-buffer eshell-buffer t)))))

;; ;; (defun vmacs-term-exec-hook(&optional cmd )
;; ;;   (rename-buffer (vmacs-eshell--generate-buffer-name "*term* " (or cmd "") default-directory)))

;; ;; ;; eshell里启动term的时候rename 之
;; ;; (with-eval-after-load 'term
;; ;;   (add-hook 'term-exec-hook 'vmacs-term-exec-hook))


;; ;;;###autoload
;; (defun eshell-insert-last-cmd-argument()
;;   "like Alt-. in bash"
;;   (interactive)
;;   (let* ((last-hist (eshell-get-history 0))
;;         (last-argv (last (split-string last-hist "[ \t]+"))))
;;     (when last-argv (insert (car last-argv)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; Shared history.
;; (defvar eshell-history-global-ring nil
;;   "The history ring shared across Eshell sessions.")

;; ;;;###autoload
;; (defun eshell-hist-use-global-history ()
;;   "Make Eshell history shared across different sessions."
;;   (unless eshell-history-global-ring
;;     (let (eshell-history-ring)
;;       (when eshell-history-file-name
;;         (eshell-read-history nil t))
;;       (setq eshell-history-global-ring eshell-history-ring))
;;     (unless eshell-history-ring (setq eshell-history-global-ring (make-ring eshell-history-size))))
;;   (setq eshell-history-ring eshell-history-global-ring))


;; (defun vmacs-esh-parse-zsh-history ()
;;   "Parse the bash history."
;;   (if (file-exists-p "~/.zsh_history")
;;       (let (collection zsh_history)
;;         ;; (vmacs-esh-reload-shell-history)
;;         (setq collection
;;               (nreverse
;;                (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.zsh_history"))
;;                                                (replace-regexp-in-string "^:[^;]*;" "" (buffer-string)))
;;                              "\n"
;;                              t)))
;;         (when (and collection (> (length collection) 0)
;;                    (setq zsh_history collection))
;;           zsh_history))
;;     nil))

;; (defun vmacs-esh-parse-shell-history ()
;;   "Parse history from eshell/bash/zsh/ ."
;;   (delete-dups
;;    (mapcar
;;     (lambda (str)
;;       (string-trim (substring-no-properties str)))
;;     (append
;;      (ring-elements eshell-history-ring)
;;      (vmacs-esh-parse-zsh-history)))))

;; ;;;###autoload
;; (defun vmacs-esh-history ()
;;   "Browse Eshell/zsh history."
;;   (interactive)
;;   (require 'em-hist)
;;   (let ((cands (vmacs-esh-parse-shell-history)))
;;     (setq ivy-completion-beg (eshell-beginning-of-input))
;;     (setq ivy-completion-end (point))
;;     (ivy-read "cmd history: " cands
;;               :initial-input (eshell-get-old-input)
;;               :action #'ivy-completion-in-region-action
;;               :caller 'counsel-shell-history)))


;; ;; (defun vmacs-esh-reload-shell-history ()
;; ;;   (with-temp-message ""
;; ;;     (let* ((shell-command (getenv "SHELL")))
;; ;;       (cond ((string-equal shell-command "/bin/bash")
;; ;;              (shell-command "history -r"))
;; ;;             ((string-equal shell-command "/bin/zsh")
;; ;;              (shell-command "fc -W; fc -R"))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun vmacs-cat-syntax-highlight (filename)
;;   "Like cat(1) but with syntax highlighting."
;;   (let ((existing-buffer (get-file-buffer filename))
;;         (buffer (find-file-noselect filename)))
;;     (eshell-print
;;      (with-current-buffer buffer
;;        (if (fboundp 'font-lock-ensure)
;;            (font-lock-ensure)
;;          (with-no-warnings
;;            (font-lock-fontify-buffer)))
;;        (buffer-string)))
;;     (unless existing-buffer
;;       (kill-buffer buffer))
;;     nil))

;; (advice-add 'eshell/cat :override #'vmacs-cat-syntax-highlight)



(provide 'lazy-eshell)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-eshell.el ends here.
