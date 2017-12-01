(eval-when-compile
  (require 'em-term)
  (require 'em-hist)
  (require 'eshell))

;; 在当前目录打开eshell
;; ctrl-u 可打开多个eshell
;;;###autoload
(defun toggle-eshell-cd(&optional arg dir)
  (interactive "P")
  (let ((dest-dir-cd (or dir default-directory))
        (shell-buffer-name (toggle-shell-completing-read-buffer-name arg "*eshell*")))
    (toggle-eshell-internal shell-buffer-name)
    (with-current-buffer shell-buffer-name
      (goto-char (point-max))
      ;; (eshell-next-prompt 1)            ;
      (eshell-bol)
      (delete-region (point) (line-end-position))
      (cd dest-dir-cd)
      (insert (concat "cd \"" dest-dir-cd "\""))
      (eshell-send-input))))

;;;###autoload
(defun toggle-eshell(&optional arg dir)
  (interactive "P")
  (toggle-eshell-internal  (toggle-shell-completing-read-buffer-name arg "*eshell*")))


(defvar eshll-toggle-commands '(toggle-eshell-cd toggle-eshell  toggle-shell))

(defun toggle-eshell-internal (&optional  shell-buffer-name)
  (interactive "sshell name:\nsshell buffer name:")
  (if (and (get-buffer shell-buffer-name)
           (buffer-live-p (get-buffer shell-buffer-name)))
      (cond
       ( (not (string= (buffer-name) shell-buffer-name))
         (switch-to-buffer shell-buffer-name))
       ;; ((and (string= (buffer-name) shell-buffer-name)
       ;;       (> (length (window-list)) 1)
       ;;       (member last-command eshll-toggle-commands))
       ;;  (delete-other-windows)
       ;;  )
       ((and (string= (buffer-name) shell-buffer-name)
             (> (length (window-list)) 1))
        (delete-window)
        )
       ((and
         (string= (buffer-name) shell-buffer-name)
         (equal (length (window-list)) 1))
        (bury-buffer)
        ))
    (let((old-window-config (current-window-configuration)))
      (setq eshell-buffer-name shell-buffer-name)
      (eshell)
      (goto-char (point-max))
      ;; (insert (concat "cd " (concat "\""default-directory "\""))) ;;make sure current directory is default-directory
      ;; (eshell-send-input)
      (set-window-configuration old-window-config)
      (switch-to-buffer-other-window shell-buffer-name))))


(defvar shell-buffer-hist nil)

(defun toggle-shell-completing-read-buffer-name(arg &optional default-buffer-name-when-no-hist )
  (let* ((default-shell-buffer
           (if (and shell-buffer-hist (listp shell-buffer-hist) (car shell-buffer-hist))
               (car shell-buffer-hist) default-buffer-name-when-no-hist ))
         (buffer-name default-shell-buffer))
    (when arg
      (setq buffer-name (completing-read (concat "shell buffer name(default:"
                                                 (if (string-match "^\\*" default-shell-buffer)
                                                     default-shell-buffer
                                                   (concat "*"  default-shell-buffer "*"))
                                                 "):")
                                         shell-buffer-hist nil nil nil nil default-shell-buffer ))
      (unless (string-match "^\\*" buffer-name)
        (setq buffer-name (concat "*"  buffer-name "*"))) )
    (setq shell-buffer-hist (delete buffer-name shell-buffer-hist))
    (push buffer-name shell-buffer-hist)
    buffer-name))

(defun eshell-insert-last-cmd-argument()
  "like Alt-. in bash"
  (interactive)
  (let* ((last-hist (eshell-get-history 0))
        (last-argv (last (split-string last-hist "[ \t]+"))))
    (when last-argv (insert (car last-argv)))))


(provide 'lazy-toggle-eshell)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-toggle-eshell.el ends here.
