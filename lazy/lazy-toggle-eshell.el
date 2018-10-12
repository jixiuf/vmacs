(eval-when-compile
  (require 'em-term)
  (require 'em-hist)
  (require 'eshell))

;; ;; 在当前目录打开eshell
;; ;; ctrl-u 可打开多个eshell
;; ;;;###autoload
;; (defun toggle-eshell-cd(&optional arg dir)
;;   (interactive "P")
;;   (let ((dest-dir-cd (or dir default-directory))
;;         (shell-buffer-name (toggle-shell-completing-read-buffer-name
;;                             arg (generate-new-buffer-name (format "*eshell*  (%s)"  default-directory)))))
;;     (toggle-eshell-internal shell-buffer-name)
;;     (with-current-buffer shell-buffer-name
;;       (goto-char (point-max))
;;       ;; (eshell-next-prompt 1)            ;
;;       (eshell-bol)
;;       (delete-region (point) (line-end-position))
;;       (cd dest-dir-cd)
;;       (insert (concat "cd \"" dest-dir-cd "\""))
;;       (eshell-send-input))))


(defvar vmacs-window-configration nil)
(defvar vmacs-shell-last-buffer nil)    ;记录切到eshell/term前 激活的buffer,以便恢复现场

;;;###autoload
(defun vmacs-eshell-new ()
  (interactive)
  (let* ((pwd (abbreviate-file-name default-directory))
         (shell-buffer-name (generate-new-buffer-name (format "*eshell*  (%s)" pwd))))
    (unless (derived-mode-p 'eshell-mode 'term-mode 'shell-mode)
      (setq vmacs-window-configration (current-window-configuration))
      (setq vmacs-shell-last-buffer (current-buffer)))
    (setq eshell-buffer-name shell-buffer-name)
    (eshell)
    (goto-char (point-max))
    ;; (insert (concat "cd " (concat "\""default-directory "\""))) ;;make sure current directory is default-directory
    ;; (eshell-send-input)
    (delete-other-windows)
    (pop-to-buffer shell-buffer-name)))

;;;###autoload
(defun vmacs-eshell-toggle()
  (interactive)
  (cond
   ((derived-mode-p 'eshell-mode 'term-mode 'shell-mode) ;当前在eshell中
    (switch-to-buffer (vmacs-eshell--recent-other-buffer))
    (set-window-configuration vmacs-window-configration))
   (t                                   ; ;当前不在eshell中
    (setq vmacs-shell-last-buffer (current-buffer))
    (let ((shell-buffer (vmacs-eshell--recent-buffer)))
      (if shell-buffer                 ;存在eshell，直接切到这个 eshell buffer
          (progn
            (setq vmacs-window-configration (current-window-configuration))
            (pop-to-buffer shell-buffer)
            (delete-other-windows)
            )
        ;; 不存在已打开的eshell buffer
        (vmacs-eshell-new))))))

;; 返回最近打开过的eshell term mode的buffer
(defun vmacs-eshell--recent-buffer()
  (let ((shell-buffer ))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'eshell-mode 'term-mode 'shell-mode)
          (unless shell-buffer
                 (setq shell-buffer buf)))))
    shell-buffer))
;; 返回最近打开过的buffer,即，切换到eshell buffer之前的那个buffer
(defun vmacs-eshell--recent-other-buffer()
  (let ((list (buffer-list))
        (index 0)
        shell-buffer buf )
    (cl-loop until shell-buffer do
             (setq buf (nth index list))
             (with-current-buffer buf
               (unless (derived-mode-p 'eshell-mode 'term-mode 'shell-mode)
                 (setq shell-buffer buf)))
             (setq index (1+ index)))
    shell-buffer))

(defun vmacs-eshell--generate-buffer-name(prefix cmd default-directory)
  (let* ((cmd (car (split-string cmd "[ |\t]" t " ")))
         (pwd (abbreviate-file-name default-directory))
         (dir-tokens (split-string pwd "[/|\\]" t " ")))
    (when (> (length dir-tokens) 2)
      (setq pwd (mapconcat  'identity (last dir-tokens 2)  "/")))
    (generate-new-buffer-name (format "%s %s (%s)"  prefix cmd pwd))
    ))

(vmacs-eshell--generate-buffer-name "*eshell*" "pwd ls" "~")



(defadvice eshell-send-input (around change-buffer-name activate)
  "change-buffer-name."
  (let ((input (eshell-get-old-input))
        (eshell-buffer)
        )
    ad-do-it
    (setq eshell-buffer (generate-new-buffer-name (format "*eshell* %s (%s)"  input default-directory)))
    (when (equal major-mode 'eshell-mode)
      ;; 有可能exit之后，当前buffer就不是eshell了
      (rename-buffer eshell-buffer))))


(defun eshell-insert-last-cmd-argument()
  "like Alt-. in bash"
  (interactive)
  (let* ((last-hist (eshell-get-history 0))
        (last-argv (last (split-string last-hist "[ \t]+"))))
    (when last-argv (insert (car last-argv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shared history.
(defvar eshell-history-global-ring nil
  "The history ring shared across Eshell sessions.")

;;;###autoload
(defun eshell-hist-use-global-history ()
  "Make Eshell history shared across different sessions."
  (unless eshell-history-global-ring
    (let (eshell-history-ring)
      (when eshell-history-file-name
        (eshell-read-history nil t))
      (setq eshell-history-global-ring eshell-history-ring))
    (unless eshell-history-ring (setq eshell-history-global-ring (make-ring eshell-history-size))))
  (setq eshell-history-ring eshell-history-global-ring))


(provide 'lazy-toggle-eshell)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-toggle-eshell.el ends here.
