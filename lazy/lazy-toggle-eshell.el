(eval-when-compile
  (require 'term)
  (require 'em-term)
  (require 'em-hist)
  (require 'eshell))


;; nil表示默认打开eshell ,t表示默认打开term
(defcustom vmacs-eshell-default-term t
  ""
  :type 'boolean
  :group 'vmacs-eshell)
(defvar vmacs-window-configration nil)
(defvar vmacs-shell-last-buffer nil)    ;记录切到eshell/term前 激活的buffer,以便恢复现场


;;;###autoload
(defun vmacs-eshell-new (&optional term-mode)
  (interactive "P")
    (if (or term-mode vmacs-eshell-default-term)
        (term  (getenv "SHELL") )
      (let* ((shell-buffer-name
              (generate-new-buffer-name
               (vmacs-eshell--generate-buffer-name "*esh* " "" default-directory))))
        (unless (derived-mode-p 'eshell-mode 'term-mode 'shell-mode)
          (setq vmacs-window-configration (current-window-configuration))
          (setq vmacs-shell-last-buffer (current-buffer)))
        (setq eshell-buffer-name shell-buffer-name)
        (eshell)
        (goto-char (point-max))
        ;; (insert (concat "cd " (concat "\""default-directory "\""))) ;;make sure current directory is default-directory
        ;; (eshell-send-input)
        (delete-other-windows)
        (pop-to-buffer shell-buffer-name))))

;;;###autoload
(defun vmacs-term-new ()
  (interactive "P")
  (vmacs-eshell-new t))


;;;###autoload
(defun vmacs-eshell-hide()
  (interactive)
  (switch-to-buffer (vmacs-eshell--recent-other-buffer))
  (set-window-configuration vmacs-window-configration))
;;;###autoload
(defun vmacs-eshell-show()
  (interactive)
  (setq vmacs-shell-last-buffer (current-buffer))
  (let ((shell-buffer (vmacs-eshell--recent-buffer)))
    (if shell-buffer                 ;存在eshell，直接切到这个 eshell buffer
        (progn
          (setq vmacs-window-configration (current-window-configuration))
          (pop-to-buffer shell-buffer)
          (delete-other-windows)
          )
      ;; 不存在已打开的eshell buffer
      (vmacs-eshell-new vmacs-eshell-default-term))))

;;;###autoload
(defun vmacs-eshell-toggle()
  (interactive)
  (cond
   ((derived-mode-p 'eshell-mode 'term-mode 'shell-mode) ;当前在eshell中
    (vmacs-eshell-hide))
   (t                                   ; ;当前不在eshell中
    (vmacs-eshell-show))))

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
    (format "%s%s(%s)"  prefix (or cmd "") pwd)))

(provide 'lazy-toggle-eshell)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-toggle-eshell.el ends here.
