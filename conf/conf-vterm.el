;; brew install libvterm
;; https://github.com/akermu/emacs-libvterm
;; mkdir -p build
;; cd build
;; cmake -DEMACS_SOURCE=~/repos/emacs ..
;; make
;; (setq-default vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-v" "M-v"))
(setq-default vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-y"))
(require 'vterm)
(define-key vterm-mode-map (kbd "s-t")   #'vterm)
(define-key vterm-mode-map (kbd "C-/")   #'(lambda()(interactive) (vterm-send-key "_" nil nil t)))
(define-key vterm-mode-map (kbd "M-.")   #'vterm--self-insert)
(define-key vterm-mode-map (kbd "C-g")   #'vterm-ctrl-g)
(define-key vterm-mode-map (kbd "s-v")   #'vterm-yank)

(defun vterm-ctrl-g ()
  "vterm ctrl-g"
  (interactive)
  (if (evil-insert-state-p)
      (progn
        (evil-normal-state)
        (setq this-command 'evil-normal-state))
    (if (equal last-command 'vterm-ctrl-g)
        (call-interactively 'keyboard-quit)
      (call-interactively 'vterm--self-insert))))


(defun vmacs-vterm-hook()
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
;; lastcmd=""
;; print -Pn "\e]0;%~@${USER}@${HOSTNAME}@${lastcmd}\a" #set title path@user@host@cmd
;; preexec () {
;;     lastcmd="$1"
;;     # # 标题栏、任务栏样式
;;     # 在执行命令前执行，所以此时打印的pwd可能不准,故还需要在chpwd里，刚更新一次
;;     print -Pn "\e]0;%~@${USER}@${HOSTNAME}@${lastcmd}\a" #set title path@user@host@cmd
;; }
;; chpwd() {
;;     # ESC]0;stringBEL — Set icon name and window title to string
;;     # ESC]1;stringBEL — Set icon name to string
;;     # ESC]2;stringBEL — Set window title to string
;;     print -Pn "\e]0;%~@${USER}@${HOSTNAME}@${lastcmd}\a" #set title path@user@host  chpwd里取不到当前cmd
;; }
(defun vterm-vterm-set-title-hook (title)
  (let ((tokens (split-string title "@" )))
    (setq vterm-pwd (nth 0 tokens))
    (setq vterm-user (nth 1 tokens))
    (setq vterm-host (nth 2 tokens))
    (when (and (nth 3 tokens)
               (not (string-empty-p (or (nth 3 tokens) ""))))
      (setq vterm-cmd (nth 3 tokens)))
    (setq default-directory
	      (file-name-as-directory
	       (if (and (string= vterm-host (system-name))
                    (string= vterm-user (user-real-login-name)))
		       (expand-file-name vterm-pwd)
             (concat "/-:" vterm-user "@" vterm-host ":"
                     vterm-pwd))))
    ;; (message "pwd=%s,user=%s,host=%s,cmd=%s d=%s"
    ;;          vterm-pwd vterm-user vterm-host vterm-cmd (or default-directory ""))
    (rename-buffer (vmacs-eshell--generate-buffer-name "vterm " (or vterm-cmd "") vterm-pwd ) t)))

(add-hook 'vterm-set-title-functions 'vterm-vterm-set-title-hook)

(provide 'conf-vterm)
