;; brew install libvterm
;; https://github.com/akermu/emacs-libvterm
;; mkdir -p build
;; cd build
;; cmake -DEMACS_SOURCE=~/repos/emacs ..
;; make
;; (setq-default vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-v" "M-v"))
(eval-when-compile (require 'evil))
(setq-default vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-y"  "M-y"))
(setq-default vterm-max-scrollback 50000)
(setq vterm-toggle-prompt-regexp
  (concat "\\(?:^\\|\r\\)"
	      "[^]#$%>\n]*#?[]#$%➜⇒»☞\[@λ] *\\(\e\\[[0-9;]*[-_a-zA-Z] *\\)*"))

(require 'vterm)
(require 'vterm-toggle)


(defun vterm-bol()
  "Go to the beginning of line, then skip past the prompt, if any.
The prompt skip is done by skipping text matching the regular expression
`vterm-toggle-prompt-regexp', a buffer local variable."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line)
    (vterm-toggle--skip-prompt)
    (if (equal pt (point))
        (beginning-of-line)
      (vterm--self-insert))))


(defun vterm-send-ctrl-x-ctrl-e ()
  "edit with editor"
  (interactive)
  (vterm-send-key "x" nil nil t)
  (vterm-send-key "e" nil nil t))

(defun vterm-send-return ()
  "Sends C-m to the libvterm."
  (interactive)
   (process-send-string vterm--process "\C-m"))

(defun vmacs-vterm-toggle-cd(&optional args)
  (interactive "P")
  (cond
   ((equal (prefix-numeric-value args) 1)
    (vterm-toggle-cd))
   ((equal (prefix-numeric-value args) 4)
    (vterm))
   ((equal (prefix-numeric-value args) 16)
    (vterm-other-window))))


(defun vterm-compile ()
  (interactive)
  (vterm-send-key "a" nil nil t)
  (vterm-send-string "c ")
  (vterm-send-key "<return>" nil nil nil))

(defun vterm-ctrl-g ()
  "vterm ctrl-g"
  (interactive)
  (if (or (save-excursion (goto-char (point-at-bol))(search-forward-regexp "filter»" nil t))
          (and (save-excursion (search-forward-regexp "[^\n \t]+" nil t))
               (save-excursion (not (search-forward-regexp vterm-toggle-prompt-regexp nil t)))))
      (call-interactively 'vmacs-vterm-self-insert)
    (if (equal last-command 'keyboard-quit)
        (call-interactively 'vmacs-vterm-self-insert)
      (setq this-command 'keyboard-quit)
      (evil-normal-state)
      (call-interactively 'keyboard-quit))))

(defun vmacs-vterm-self-insert()
  (interactive)
  (unless (evil-insert-state-p)
    (evil-insert-state))
  (call-interactively 'vterm--self-insert))

(defun vterm-disable-output ()
  (interactive)
  (unless (evil-normal-state-p)
    (evil-normal-state))
  (vterm-send-key "s" nil nil t))

(defun vmacs-vterm-disable-output()
  (when (member major-mode '(vterm-mode))
    (vterm-send-key "s" nil nil t)))
(defun vmacs-enable-disable-output()
  (when (member major-mode '(vterm-mode))
    (vterm-send-key "q" nil nil t)))


(add-hook 'evil-insert-state-entry-hook 'vmacs-enable-disable-output)
;; (add-hook 'evil-normal-state-entry-hook 'vmacs-vterm-disable-output)


(defun vterm-eob()
  (interactive)
  (goto-char (point-max))
  (forward-char -1))

(define-key vterm-mode-map (kbd "s-t")   #'vterm)
(define-key vterm-mode-map (kbd "C-x C-e")   #'vterm-send-ctrl-x-ctrl-e)
(define-key vterm-mode-map (kbd "M-.")   #'vterm--self-insert)
(define-key vterm-mode-map (kbd "C-g")   #'vterm-ctrl-g)
(define-key vterm-mode-map (kbd "C-c C-g")   #'vterm--self-insert)
(define-key vterm-mode-map (kbd "s-v")   #'vterm-yank)
(define-key vterm-mode-map (kbd "C-k")   #'vterm-kill-line)
(define-key vterm-mode-map [(control return)]   #'vterm-compile)
(define-key vterm-mode-map [f2]   nil)
(define-key vterm-mode-map [f3]   nil)
(define-key vterm-mode-map (kbd "C-a")   #'vterm-bol)
;; C－s 停止滚屏 C-q恢复滚屏
(define-key vterm-mode-map (kbd "C-s")   #'vterm-disable-output)
(define-key vterm-mode-map (kbd "s-s")   #'vterm-disable-output)

;; (define-key vterm-mode-map (kbd "s-t")   #'vterm)
(define-key vterm-mode-map [return] #'vterm-send-return)
(define-key vterm-mode-map (kbd "C-m") #'vterm-send-return)
(defun vmacs-vterm-hook()
  (evil-define-key 'insert 'local [escape] 'vterm--self-insert)
  (evil-define-key 'motion'local (kbd "C-r") 'vmacs-vterm-self-insert)
  (evil-define-key 'insert 'local (kbd "C-g") 'vterm-ctrl-g)
  (evil-define-key 'motion 'local (kbd "C-a") 'vterm-bol)
  (evil-define-key 'normal 'local "y" 'evil-yank-join)
  (evil-define-key 'motion'local "y" 'evil-yank-join)
  (evil-define-key 'visual 'local "y" 'evil-yank-join)
  (evil-define-key 'normal 'local (kbd "C-p") 'vmacs-vterm-self-insert)
  (evil-define-key 'normal 'local (kbd "C-n") 'vmacs-vterm-self-insert)
  (evil-define-key 'normal 'local (kbd "C-r") 'vmacs-vterm-self-insert)
  (evil-define-key 'normal 'local (kbd "p") 'vterm-yank)
  (evil-define-key 'normal 'local (kbd "u") 'vterm-undo)
  (evil-define-key 'normal 'local (kbd "G") 'vterm-eob)

  (let ((p (get-buffer-process (current-buffer))))
    (when p
      (set-process-query-on-exit-flag p nil))))

(add-hook 'vterm-mode-hook 'vmacs-vterm-hook)
(defun vmacs-auto-exit(buf)
  (when buf (kill-buffer buf)))

(add-hook 'vterm-exit-functions #'vmacs-auto-exit)


(defvar vterm-user "")
(make-variable-buffer-local 'vterm-user)
(defvar vterm-host "")
(make-variable-buffer-local 'vterm-host)
(defvar vterm-pwd "")
(make-variable-buffer-local 'vterm-pwd)
(defvar vterm-cmd "")
(make-variable-buffer-local 'vterm-cmd)


;; # http://zsh.sourceforge.net/Doc/Release/Functions.html
;; # 刚打开shell时，也执行一次更新title
;; for zsh

;; lastcmd=""
;; print -Pn "\e]2;${USER}@${HOSTNAME}@${lastcmd}:%~\a" #set title user@host@cmd:path
;; preexec () {
;;     cmd="$1"
;;     tokens=(${(s/ /)cmd}) # split by space
;;     lastcmd=$tokens[1]
;;     # # 标题栏、任务栏样式
;;     # 在执行命令前执行，所以此时打印的pwd可能不准,故还需要在chpwd里，刚更新一次
;;     print -Pn "\e]2;${USER}@${HOSTNAME}@${lastcmd}:%~\a" #set title user@host@cmd:path
;; }
;; chpwd() {
;;     # ESC]0;stringBEL — Set icon name and window title to string
;;     # ESC]1;stringBEL — Set icon name to string
;;     # ESC]2;stringBEL — Set window title to string
;;     print -Pn "\e]2;${USER}@${HOSTNAME}@${lastcmd}:%~\a" #set title user@host@cmd:path  chpwd里取不到当前cmd
;; }
(defun vterm-set-title-hook (title) ;title = user@host@lastcmd:path  or user@host:path
  (let* ((tokens (split-string title ":" ))
         dir)
    (when (equal 2 (length tokens))
      (setq vterm-pwd (string-trim-left (nth 1 tokens)))
      (setq tokens (split-string (nth 0 tokens) "@" ))
      (when (>  (length tokens) 1)
        (setq vterm-user (nth 0 tokens))
        (setq vterm-host (nth 1 tokens))
        (when (and (nth 2 tokens)
                   (not (string-empty-p (or (nth 2 tokens) ""))))
          (setq vterm-cmd (nth 2 tokens))))

      (setq dir
	        (file-name-as-directory
	         (if (and (string= vterm-host (system-name))
                      (string= vterm-user (user-real-login-name)))
		         (expand-file-name vterm-pwd)
               (concat "/-:" vterm-user "@" vterm-host ":"
                       vterm-pwd))))
      (when (ignore-errors (file-directory-p dir))
        (cd-absolute dir))
      ;; (message "title=%s\n pwd=%s\n user=%s\nhost=%s\ncmd=%s\n dir=%s\n"
      ;;          title vterm-pwd vterm-user vterm-host vterm-cmd dir)
      (rename-buffer (vmacs-generate-dir-name "term " vterm-cmd vterm-pwd
                                              (- centaur-tabs-label-fixed-length
                                                 (length vterm-cmd) 1)) t))))

(defun vmacs-generate-dir-name(prefix cmd dir &optional max-dir-len)
  (let* ((cmd (car (split-string cmd "[ |\t]" t " ")))
         (pwd (abbreviate-file-name dir))
         (dir-tokens (split-string pwd "[/|\\]" t " ")))
    (when (> (length dir-tokens) 2)
      (setq pwd (mapconcat  'identity (last dir-tokens 2)  "/")))
    (when (and max-dir-len
               (> max-dir-len 0)
               (> (length pwd) max-dir-len))
      (setq pwd (substring pwd (- (length pwd) max-dir-len))))
    (string-trim (format "%s%s %s"  prefix (or cmd "") pwd))))

(add-hook 'vterm-set-title-functions 'vterm-set-title-hook)

(defun vterm-get-line( &optional skip-prompt)
  (save-excursion
    (let ((start (point-at-bol))
          (end (point-at-eol))
          (width (window-body-width))
          text)
      (while (and (<= width (- (point-at-eol) (point-at-bol)))
                  (not (eobp)))
        (forward-line)
        (end-of-line)
        (setq end (point-at-eol)))
      (goto-char start)
      (forward-line -1)
      (while (and (<= width (string-width (buffer-substring-no-properties (point-at-eol) (point-at-bol) )))
                  (not (bobp)))
        (setq start (point-at-bol))
        (forward-line -1)
        (beginning-of-line))
      (setq text (buffer-substring start end))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (when (looking-at vterm-toggle-prompt-regexp)
          (delete-region (point) (match-end 0)))
        (while  (search-forward-regexp "\n" nil t )
          (replace-match ""))
        (buffer-string)))))


(defun vterm-get-old-input-default ()
  "Default for `term-get-old-input'.
Take the current line, and discard any initial text matching
`term-prompt-regexp'."
  (vterm-get-line t))

(defun vterm-kill-line()
  (interactive)
  (save-excursion
    (let ((pos (point))
          (end (point-at-eol))
          (width (window-body-width))
          text)
      (while (and (<= width (string-width (buffer-substring-no-properties  (point-at-bol)(point-at-eol) )))
                  (not (eobp)))
        (forward-line)
        (end-of-line)
        (setq end (point-at-eol)))
      (setq text (buffer-substring pos end))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while  (search-forward-regexp "\n" nil t )
          (replace-match ""))
        (kill-ring-save (point-min) (point-max)))))
  (vterm-send-key "k" nil nil t))



;; ;; vterm内，会有原本是一行的内容，会硬折行为多行，
;; ;; 此时yank 的时候，自动将其join为一行
(evil-define-operator evil-yank-join (beg end type register yank-handler)
  "try join wrapped lines then yank."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (if current-prefix-arg
      (evil-yank beg end type register yank-handler)
    (let ((text (buffer-substring beg end))
          (width (window-body-width))
          break pt)
      (when (and (equal type 'line)
                 (equal (line-number-at-pos beg)
                        (line-number-at-pos (1- end))))
        (setq text (vterm-get-line)))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (setq pt (point))
        (while (not break)
          (end-of-line)
          (if (eobp)
              (setq break t)
            (if (and (<= width (string-width (buffer-substring-no-properties pt (point))))
                     (looking-at-p "\n"))
                (progn
                  (delete-char 1)         ;delete \n
                  (setq pt (point)))
              (forward-char 1)            ;goto next bol
              (setq pt (point)))))
        (goto-char (point-min))
        (while (looking-at vterm-toggle-prompt-regexp)
          (delete-region (point) (match-end 0))
          (forward-line))
        (evil-yank (point-min) (point-max) type register yank-handler)))))

(defun vmacs-kill-buffer-hook()
  (when (funcall vterm-toggle--vterm-buffer-p-function)
    (let ((proc (get-buffer-process (current-buffer))))
      (when (process-live-p proc)
        (when (derived-mode-p 'term-mode)
          (term-send-raw-string "\^C")
          (term-send-raw-string "\^D")
          (term-send-raw-string "\^\\"))
        (when (derived-mode-p 'vterm-mode)
          (vterm-send-ctrl-c))
        (kill-process proc)))))

(add-hook 'kill-buffer-hook 'vmacs-kill-buffer-hook)

(defun vterm-toggle-after-ssh-login (user host port localdir)
  (when (member host '("BJ-DEV-GO" "dev.com"))
    (vterm-send-string "zsh" t)
    (vterm-send-key "<return>" nil nil nil)
    (vterm-send-string "j;clear" t)
    (vterm-send-key "<return>" nil nil nil)))

(add-hook 'vterm-toggle-after-ssh-login-function 'vterm-toggle-after-ssh-login)

(setq vterm-toggle--vterm-buffer-p-function 'vmacs-term-mode-p)
(defun vmacs-term-mode-p(&optional ignore-scratch)
  (or (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode 'tsmterm-mode)
      (if ignore-scratch
          nil
        (string-match-p "\\*scratch-.*" (buffer-name)))))


(provide 'conf-vterm)
