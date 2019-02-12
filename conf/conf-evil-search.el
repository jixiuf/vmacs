(define-key evil-ex-search-keymap "\C-w" 'evil-search-yank-word)
;; Yank text at point.
(defvar evil-ex-search-yank-point nil)
;;;###autoload
(defun evil-search-yank-word (&optional arg)
  "Pull next word from buffer into search string."
  (interactive)
  (let ((fwd-fn #'forward-word)
        word)
    (with-current-buffer evil-ex-current-buffer
      ;; Start to initial point if C-w have never been hit.
      (unless evil-ex-search-yank-point
        (setq evil-ex-search-yank-point evil-ex-search-start-point))

      (save-excursion
        (goto-char evil-ex-search-yank-point)
        (funcall fwd-fn 1)
        (setq word (buffer-substring-no-properties
                    evil-ex-search-yank-point (point)))
        (setq evil-ex-search-yank-point (point))))
    (when (string-prefix-p (minibuffer-contents) word)
      (setq word (substring-no-properties word (length (minibuffer-contents)))))
    (evil-set-register ?s word)
    (evil-paste-from-register ?s)))

(defadvice evil-ex-search-stop-session(after clean-yank-work activate)
  (setq evil-ex-search-yank-point nil))


(provide 'conf-evil-search)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-evil-search.el ends here.
