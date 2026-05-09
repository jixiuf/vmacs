;;; conf-term.el  -*- lexical-binding: t; -*-

;;; Code:

;; (package-vc-install '(ghostel :url "https://github.com/dakra/ghostel.git" :branch "main"))

(setq-default term-prompt-regexp "^[^#$%>\n]*[#$%>] *") ;默认 regex 相当于没定义，term-bol 无法正常中转到开头处
(setq ghostel-enable-osc52 t)
(setq ghostel-ignore-cursor-change t)
(setq ghostel-copy-mode-auto-load-scrollback t)

(defun ghostel-latest ()
  "Switch to the next ghostel terminal buffer, or create one."
  (interactive)
  (let* ((bufs (cl-remove-if-not
                (lambda (b)
                  (with-current-buffer b
                    (derived-mode-p 'ghostel-mode)))
                (buffer-list)))
         )
    (if bufs
        (pop-to-buffer (car bufs) (append display-buffer--same-window-action
                                          '((category . comint))))
      (ghostel))))


(defun ghostel-reset-cursor-point ()
  "Move Emacs point to the terminal cursor position.
`ghostel--cursor-position' returns row relative to the viewport
\(the last `ghostel--term-rows' lines of the buffer), so the row
must be offset by the scrollback line count.  Mirrors the
placement math the native module performs in `src/render.zig'."
  (when (and ghostel--term ghostel--term-rows)
    (let ((pos (ghostel--cursor-position ghostel--term)))
      (when pos
        (let ((scrollback (max 0 (- (count-lines (point-min) (point-max))
                                    ghostel--term-rows))))
          (goto-char (point-min))
          (forward-line (+ scrollback (cdr pos)))
          (move-to-column (car pos)))))))

(defun vmacs-ghostel-disable-copy()
  (when (member major-mode '(ghostel-mode))
    (ghostel-readonly-exit)
    (ghostel-reset-cursor-point)
    (ghostel--set-cursor-style 0 t)))

(defun vmacs-ghostel-enable-copy()
  (when (member major-mode '(ghostel-mode))
    (setq display-line-numbers nil)
    (when (member this-command '(keyboard-quit
                                 bray-state-stack-pop
                                 ghostel-latest
                                 ghostel
                                 ghostel-other))
      (ghostel-line-mode)
      (ghostel--set-cursor-style 1 t))))
(with-eval-after-load 'ghostel
  (define-key  ghostel-mode-map (kbd "C-s-v") #'ghostel-yank)
  (helixel-define-key 'insert (kbd "C-s-v") #'ghostel-yank 'ghostel-mode)
  (helixel-define-key 'normal (kbd "C-s-v") #'ghostel-yank 'ghostel-mode)
  (add-hook 'helixel-normal-mode-hook 'vmacs-ghostel-enable-copy)
  (add-hook 'helixel-insert-mode-hook 'vmacs-ghostel-disable-copy))

(provide 'conf-term)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-term.el ends here.
