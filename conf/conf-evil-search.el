
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key evil-ex-search-keymap "\C-w" 'evil-search-yank-word)
;; 默认情况下，对于光标下的内容，/的时候会跳过，此处不跳过
(defadvice evil-ex-start-search (around  go-back-one-char activate)
  "replace evil-ex-start-search"
  ;; store buffer and window where the search started
  (let ((evil-ex-current-buffer (current-buffer)))
    (setq evil-ex-search-count count
          evil-ex-search-direction direction
          evil-ex-search-start-point (point)
          evil-ex-last-was-search t)
    (progn
      ;; ensure minibuffer is initialized accordingly
      (add-hook 'minibuffer-setup-hook #'evil-ex-search-start-session)
      ;; read the search string
      (let* ((minibuffer-local-map evil-ex-search-keymap)
             (search-string
              (condition-case err
                  (minibuffer-with-setup-hook
                      #'evil-ex-search-setup
                    (read-string (if (eq evil-ex-search-direction 'forward)
                                     "/" "?")
                                 (and evil-ex-search-history
                                      (propertize
                                       (car evil-ex-search-history)
                                       'face 'shadow))
                                 'evil-ex-search-history))
                (quit
                 (evil-ex-search-stop-session)
                 (evil-ex-delete-hl 'evil-ex-search)
                 (goto-char evil-ex-search-start-point)
                 (signal (car err) (cdr err))))))
        ;; pattern entered successful
        (goto-char evil-ex-search-start-point) ;------changed only here
        ;; (goto-char (1+ evil-ex-search-start-point))
        (let* ((result
                (evil-ex-search-full-pattern search-string
                                             evil-ex-search-count
                                             evil-ex-search-direction))
               (success (pop result))
               (pattern (pop result))
               (offset (pop result)))
          (setq evil-ex-search-pattern pattern
                evil-ex-search-offset offset)
          (cond
           ((memq success '(t wrap))
            (goto-char (match-beginning 0))
            (setq evil-ex-search-match-beg (match-beginning 0)
                  evil-ex-search-match-end (match-end 0))
            (evil-ex-search-goto-offset offset)
            (evil-push-search-history search-string (eq direction 'forward))
            (unless evil-ex-search-persistent-highlight
              (evil-ex-delete-hl 'evil-ex-search)))
           (t
            (goto-char evil-ex-search-start-point)
            (evil-ex-delete-hl 'evil-ex-search)
            (signal 'search-failed (list search-string)))))))))

;; ;; 当搜索开始的时候，黑夜光标会跳过当前字符，此时后退一个字符，以便光标下的内容可以被搜到
;; 以方便evil-search-yank-word yank 光标下的内容
(defadvice evil-ex-search-update-pattern (around  go-back-one-char activate)
  (let ((evil-ex-search-start-point evil-ex-search-start-point))
    (when (equal evil-search-module 'evil-search)
      (if (equal evil-ex-search-direction 'forward)
          (setq evil-ex-search-start-point (1- evil-ex-search-start-point))
        ;; (setq evil-ex-search-start-point (1+ evil-ex-search-start-point))
        ))
    ad-do-it))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
