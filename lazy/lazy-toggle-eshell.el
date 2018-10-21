(eval-when-compile
  (require 'term)
  (require 'ange-ftp)
  (require 'em-term)
  (require 'em-hist)
  (require 'eshell))


;; nil表示默认打开eshell ,t表示默认打开term
(defcustom vmacs-eshell-default-term t
  ""
  :type 'boolean
  :group 'vmacs-eshell)
(defvar vmacs-window-configration nil)

;;;###autoload
(defun vmacs-eshell-term-new (&optional args)
  (interactive "P")
  (if (>  (prefix-numeric-value args) 1)
      (if vmacs-eshell-default-term
          (vmacs-eshell-new)
        (vmacs-term-new))
    (if vmacs-eshell-default-term
        (vmacs-term-new)
      (vmacs-eshell-new))))

;;;###autoload
(defun vmacs-eshell-new()
  (interactive )
  (let* ((shell-buffer-name
          (generate-new-buffer-name
           (vmacs-eshell--generate-buffer-name "*esh* " "" default-directory))))
    (unless (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode )
      (setq vmacs-window-configration (current-window-configuration)))
    (setq eshell-buffer-name shell-buffer-name)
    (eshell)
    (goto-char (point-max))
    ;; (insert (concat "cd " (concat "\""default-directory "\""))) ;;make sure current directory is default-directory
    ;; (eshell-send-input)
    (delete-other-windows)
    (pop-to-buffer shell-buffer-name)))

;;;###autoload
(defun vmacs-term-new()
  (interactive )
  (term  (or explicit-shell-file-name
			 shell-file-name) ))


;;;###autoload
(defun vmacs-eshell-term-hide()
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)
        (bury-buffer))))
  (message "%s" (vmacs-eshell--recent-other-buffer))
  (switch-to-buffer (vmacs-eshell--recent-other-buffer))
  (when vmacs-window-configration
    (set-window-configuration vmacs-window-configration)))

;;;###autoload
(defun vmacs-eshell-term-show()
  (interactive)
  (let ((shell-buffer (vmacs-eshell--recent-buffer)))
    (if shell-buffer                 ;存在eshell，直接切到这个 eshell buffer
        (progn
          (unless (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)
            (setq vmacs-window-configration (current-window-configuration))
            )
          (pop-to-buffer shell-buffer)
          (delete-other-windows)
          )
      ;; 不存在已打开的eshell buffer
      (vmacs-eshell-term-new ))))

;;;###autoload
(defun vmacs-eshell-term-toggle()
  (interactive)
  (cond
   ((derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode) ;当前在eshell中
    (vmacs-eshell-term-hide))
   (t                                   ; ;当前不在eshell中
    (vmacs-eshell-term-show))))

;; 返回最近打开过的eshell term mode的buffer
(defun vmacs-eshell--recent-buffer()
  (let ((shell-buffer ))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)
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
               (unless (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)
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

(defun vmacs-kill-buffer-hook()
  (when (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)
    (let ((proc (get-buffer-process (current-buffer))))
      (when (process-live-p proc)
        (when (derived-mode-p 'term-mode)
          (term-send-raw-string "\^C")
          (term-send-raw-string "\^D")
          (term-send-raw-string "\^\\"))
        (kill-process proc)))))

(add-hook 'kill-buffer-hook 'vmacs-kill-buffer-hook)

;; emacs26 之后tramp 的路径里必须指定连接的method(ssh等)必须包含
;; https://www.reddit.com/r/emacs/comments/8ghdxu/make_dired_show_correct_directory_after_ssh/

;; If you're using Emacs 26 then it looks like this needs a bug fix,
;; as tramp now mandates that a method component is included in tramp
;; file names, and the associated elisp does not supply one. You can
;; fix that by editing the term-handle-ansi-terminal-messages function
;; (M-x find-function) and changing instances of (concat "/"
;; term-ansi-at-host into (concat "/-:" term-ansi-at-host (where "-"
;; is a pseudo-method with equivalent behaviour).
;; For hosts to which I connect

(defadvice term-handle-ansi-terminal-messages(after fix-tramp-default-directory last activate)
  ;; Is there a command here?
  (while (string-match "\eAnSiT.+\n" message)
    ;; Extract the command code and the argument.
    (let* ((start (match-beginning 0))
	   (command-code (aref message (+ start 6)))
	   (argument
	    (save-match-data
	      (substring message
			 (+ start 8)
			 (string-match "\r?\n" message
				       (+ start 8)))))
	   ignore)
      ;; Delete this command from MESSAGE.
      (setq message (replace-match "" t t message))

      ;; If we recognize the type of command, set the appropriate variable.
      (cond ((= command-code ?c)
	     (setq term-ansi-at-dir argument))
	    ((= command-code ?h)
	     (setq term-ansi-at-host argument))
	    ((= command-code ?u)
	     (setq term-ansi-at-user argument))
	    ;; Otherwise ignore this one.
	    (t
	     (setq ignore t)))

      ;; Update default-directory based on the changes this command made.
      (if ignore
	  nil
	(setq default-directory
	      (file-name-as-directory
	       (if (and (string= term-ansi-at-host (system-name))
					(string= term-ansi-at-user (user-real-login-name)))
		   (expand-file-name term-ansi-at-dir)
		 (if (string= term-ansi-at-user (user-real-login-name))
		     (concat "/" term-ansi-at-host ":" term-ansi-at-dir)
           ;; 改动了这里
		   ;;(concat "/" term-ansi-at-user "@" term-ansi-at-host ":"
		   (concat "/-:" term-ansi-at-user "@" term-ansi-at-host ":"
			   term-ansi-at-dir)))))


	;; I'm not sure this is necessary,
	;; but it's best to be on the safe side.
	(if (string= term-ansi-at-host (system-name))
	    (progn
	      (setq ange-ftp-default-user term-ansi-at-save-user)
	      (setq ange-ftp-default-password term-ansi-at-save-pwd)
	      (setq ange-ftp-generate-anonymous-password term-ansi-at-save-anon))
	  (setq term-ansi-at-save-user ange-ftp-default-user)
	  (setq term-ansi-at-save-pwd ange-ftp-default-password)
	  (setq term-ansi-at-save-anon ange-ftp-generate-anonymous-password)
	  (setq ange-ftp-default-user nil)
	  (setq ange-ftp-default-password nil)
	  (setq ange-ftp-generate-anonymous-password nil)))))
  message)



(provide 'lazy-toggle-eshell)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-toggle-eshell.el ends here.
