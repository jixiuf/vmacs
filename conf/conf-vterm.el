;; brew install libvterm
;; https://github.com/akermu/emacs-libvterm
;; mkdir -p build
;; cd build
;; cmake -DEMACS_SOURCE=~/repos/emacs ..
;; make
;; (setq-default vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-v" "M-v"))
(eval-when-compile (require 'evil))
(setq-default vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-y"  "M-y"))
(setq-default vterm-max-scrollback (- 20000 40))
(setq-default vterm-kill-buffer-on-exit t)
(setq-default vterm-clear-scrollback t)
(setq-default term-prompt-regexp "^[^#$%>\n]*[#$%>] *") ;默认regex 相当于没定义，term-bol无法正常中转到开头处
;; (setq vterm-toggle-prompt-regexp
;;   (concat "\\(?:^\\|\r\\)"
;; 	      "[^]#$%>\n]*#?[]#$%➜⇒»☞\[@λ] *\\(\e\\[[0-9;]*[-_a-zA-Z] *\\)*"))

(require 'vterm)
(require 'vterm-toggle)

(add-hook 'vterm-toggle-show-hook #'evil-insert-state)
;; (add-hook 'vterm-toggle-hide-hook #'(lambda()(compilation-shell-minor-mode -1)))
(add-hook 'vterm-toggle-hide-hook #'evil-insert-state)
(setq vterm-toggle-fullscreen-p t)

(defun vterm-send-ctrl-x-ctrl-e ()
  "edit with editor"
  (interactive)
  (vterm-send-key "x" nil nil t)
  (vterm-send-key "e" nil nil t))

(defun vmacs-vterm-toggle-cd(&optional args)
  (interactive "P")
  (cond
   ((equal (prefix-numeric-value args) 1)
    (vterm-toggle-cd))
   ((equal (prefix-numeric-value args) 16)
    (vterm))
   ((equal (prefix-numeric-value args) 4)
    (let ((vterm-toggle-fullscreen-p nil))
      (vterm-toggle-cd)))))

(defun vmacs-vterm-toggle(&optional args)
  (interactive "P")
  (cond
   ((equal (prefix-numeric-value args) 1)
    (vterm-toggle))
   ((equal (prefix-numeric-value args) 16)
    (vterm))
   ((equal (prefix-numeric-value args) 4)
    (let ((vterm-toggle-fullscreen-p nil))
      (vterm-toggle)))))

(defun vterm-ctrl-g ()
  "vterm ctrl-g"
  (interactive)
  (if (or (save-excursion (goto-char (point-at-bol))(search-forward-regexp "filter»" nil t))
          ;; (and (save-excursion (search-forward-regexp "[^\n \t]+" nil t))
          ;;      (save-excursion (not (vterm-next-prompt))))
          )
      (if (equal last-command 'vterm-ctrl-g)
          (evil-normal-state)
        (call-interactively 'vmacs-vterm-self-insert))
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

(defun vmacs-vterm-disable-output()
  (when (member major-mode '(vterm-mode))
    (vterm-copy-mode 1)))
(defun vmacs-vterm-enable-output()
  (when (member major-mode '(vterm-mode))
    (vterm-copy-mode -1)))

(defun vmacs-vterm-copy-mode-hook()
  (if vterm-copy-mode
      (unless (evil-normal-state-p)
        (evil-normal-state))
    (unless (evil-insert-state-p)
      (evil-insert-state))))

(add-hook 'vterm-copy-mode-hook #'vmacs-vterm-copy-mode-hook)


(add-hook 'evil-insert-state-entry-hook 'vmacs-vterm-enable-output)
(add-hook 'evil-normal-state-entry-hook 'vmacs-vterm-disable-output)


(defun vterm-eob()
  (interactive)
  (goto-char (point-max))
  (skip-chars-backward "\n[:space:]"))

(define-key vterm-mode-map (kbd "s-t")   #'vterm-toggle-cd-show)
(define-key vterm-mode-map (kbd "C-x C-e")   #'vterm-send-ctrl-x-ctrl-e)
(define-key vterm-mode-map (kbd "C-g")   #'vterm-ctrl-g)
(define-key vterm-mode-map (kbd "C-c C-g")   #'vterm--self-insert)
(define-key vterm-mode-map (kbd "s-v")   #'vterm-yank)
(define-key vterm-mode-map [f2]   nil)
(define-key vterm-mode-map [f3]   nil)

(define-key vterm-mode-map (kbd "C-.")   #'vterm-reset-cursor-point)
(define-key vterm-copy-mode-map (kbd "C-.")   #'vterm-reset-cursor-point)

;; C－s 停止滚屏 C-q恢复滚屏
(define-key vterm-mode-map (kbd "C-s")   #'vterm-copy-mode)
(define-key vterm-mode-map [(control return)]   #'vterm-toggle-insert-cd)

(define-key vterm-mode-map (kbd "C-q")   #'vterm-copy-mode)
(define-key vterm-copy-mode-map (kbd "C-s")   #'vterm-copy-mode)
(define-key vterm-copy-mode-map (kbd "C-c C-c")   #'vterm-send-C-c)
(define-key vterm-copy-mode-map (kbd "C-c C-c")   #'vterm-send-C-c)
;; (define-key vterm-mode-map (kbd "C-l")   #'vterm-clear)
(define-key vterm-mode-map (kbd "C-c C-k")   #'compilation-shell-minor-mode)
(define-key vterm-copy-mode-map [remap self-insert-command] #'vterm--self-insert)


;; (define-key vterm-mode-map (kbd "s-t")   #'vterm)
(defun vmacs-vterm-hook()
  (evil-define-key 'insert 'local [escape] 'vterm--self-insert)
  (evil-define-key 'motion'local (kbd "C-r") 'vmacs-vterm-self-insert)
  (evil-define-key 'insert 'local (kbd "C-g") 'vterm-ctrl-g)
  ;; (evil-define-key 'normal 'local "y" 'evil-yank-join)
  ;; (evil-define-key 'motion'local "y" 'evil-yank-join)
  ;; (evil-define-key 'visual 'local "y" 'evil-yank-join)
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

(defun vterm-set-title-hook (title) ;title = user@host@lastcmd:path  or user@host:path
  (unless (string-equal "term compile" (buffer-name))
    (rename-buffer (vmacs-generate-dir-name "term " "" title
                                            (- centaur-tabs-label-fixed-length 1 5)) t)))

(add-hook 'vterm-set-title-functions 'vterm-set-title-hook)

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





(defun vmacs-kill-buffer-hook()
  (when (funcall vterm-toggle--vterm-buffer-p-function)
    (let ((proc (get-buffer-process (current-buffer))))
      (when (process-live-p proc)
        (when (derived-mode-p 'term-mode)
          (term-send-raw-string "\^C")
          (term-send-raw-string "\^D")
          (term-send-raw-string "\^\\"))
        (when (derived-mode-p 'vterm-mode)
          (vterm-send-C-c))
        (kill-process proc)))))

(add-hook 'kill-buffer-hook 'vmacs-kill-buffer-hook)

(defun vterm-toggle-after-ssh-login (method user host port localdir)
  (when (string-equal "docker" method)
    (vterm-send-string "bash")
    (vterm-send-return))
  (when (member host '("BJ-DEV-GO" "dev.com"))
    (vterm-send-string "zsh")
    (vterm-send-return)
    (vterm-send-string "j;clear" )
    (vterm-send-return)))

(add-hook 'vterm-toggle-after-remote-login-function 'vterm-toggle-after-ssh-login)

(setq vterm-toggle--vterm-buffer-p-function 'vmacs-term-mode-p)
(defun vmacs-term-mode-p(&optional args)
  (or (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode 'tsmterm-mode)))



;; (defun vterm-get-line ()
;;   (save-excursion
;;     (let ((start (point-at-bol))
;;           (end (point-at-eol))
;;           (width (- (window-body-width) (vterm--get-margin-width)))
;;           text)
;;       (while (and (<= width (- (point-at-eol) (point-at-bol)))
;;                   (not (eobp)))
;;         (forward-line)
;;         (end-of-line)
;;         (setq end (point-at-eol)))
;;       (goto-char start)
;;       (forward-line -1)
;;       (while (and (<= width (string-width (buffer-substring-no-properties (point-at-eol) (point-at-bol) )))
;;                   (not (bobp)))
;;         (setq start (point-at-bol))
;;         (forward-line -1)
;;         (beginning-of-line))
;;       (setq text (buffer-substring start end))
;;       (with-temp-buffer
;;         (insert text)
;;         (goto-char (point-min))
;;         (while  (search-forward-regexp "\n" nil t )
;;           (replace-match ""))
;;         (buffer-string)))))
;; ;; ;; vterm内，会有原本是一行的内容，会硬折行为多行，
;; ;; ;; 此时yank 的时候，自动将其join为一行
;; (evil-define-operator evil-yank-join (beg end type register yank-handler)
;;   "try join wrapped lines then yank."
;;   :move-point nil
;;   :repeat nil
;;   (interactive "<R><x><y>")
;;   (if current-prefix-arg
;;       (evil-yank beg end type register yank-handler)
;;     (let ((text (buffer-substring beg end))
;;           (width (- (window-body-width) (vterm--get-margin-width)))
;;           break pt)
;;       (when (and (equal type 'line)
;;                  (equal (line-number-at-pos beg)
;;                         (line-number-at-pos (1- end))))
;;         (setq text (vterm-get-line)))
;;       (with-temp-buffer
;;         (insert text)
;;         (goto-char (point-min))
;;         (setq pt (point))
;;         (while (not break)
;;           (end-of-line)
;;           (if (eobp)
;;               (setq break t)
;;             (if (and (<= width (string-width (buffer-substring-no-properties pt (point))))
;;                      (looking-at-p "\n"))
;;                 (progn
;;                   (delete-char 1)         ;delete \n
;;                   (setq pt (point)))
;;               (forward-char 1)            ;goto next bol
;;               (setq pt (point)))))
;;         (goto-char (point-min))
;;         (evil-yank (point-min) (point-max) type register yank-handler)))))

(defun vterm-open-other-window (path)
  (if-let* ((buf (find-file-noselect path)))
      (pop-to-buffer buf )
    (message "Failed to open file: %s" path)))

(push (list "vterm-open-other-window" 'vterm-open-other-window) vterm-eval-cmds)

(provide 'conf-vterm)
