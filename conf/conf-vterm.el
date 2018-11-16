;; brew install libvterm
;; https://github.com/akermu/emacs-libvterm
;; mkdir -p build
;; cd build
;; cmake -DEMACS_SOURCE=~/repos/emacs ..
;; make
(require 'vterm)
(define-key vterm-mode-map (kbd "s-t")   #'vterm)
(define-key vterm-mode-map (kbd "C-c C-c")   #'(lambda()(interactive) (vterm-send-key "c" nil nil t)))
(define-key vterm-mode-map (kbd "C-/")   #'(lambda()(interactive) (vterm-send-key "_" nil nil t)))
(define-key vterm-mode-map (kbd "M-.")   #'vterm--self-insert)

(defun vmacs-vterm-hook()
  (let ((p (get-buffer-process (current-buffer))))
    (when p
      (set-process-query-on-exit-flag p nil))))

(add-hook 'vterm-mode-hook 'vmacs-vterm-hook)
(add-hook 'vterm-exit-hook 'kill-buffer)

(defvar vterm-user "")
(make-variable-buffer-local 'vterm-user)
(defvar vterm-host "")
(make-variable-buffer-local 'vterm-host)
(defvar vterm-pwd "")
(make-variable-buffer-local 'vterm-pwd)
(defvar vterm-cmd "")
(make-variable-buffer-local 'vterm-cmd)
;; preexec () {
;;     # # 标题栏、任务栏样式
;;     print -Pn "\e]0;%~@${USER}@${HOSTNAME}@$1\a" #set title path@user@host@cmd
;; }
;; chpwd() {
;;     print -Pn "\e]0;%~@${USER}@${HOSTNAME}@\a" #set title path@user@host  chpwd里取不到当前cmd
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

(add-hook 'vterm-set-title-hook 'vterm-vterm-set-title-hook)

(provide 'conf-vterm)
