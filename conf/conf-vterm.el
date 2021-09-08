;; brew install libvterm
;; https://github.com/akermu/emacs-libvterm
;; mkdir -p build
;; cd build
;; cmake -DEMACS_SOURCE=~/repos/emacs ..
;; make
;; (setq-default vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-v" "M-v"))
(setq-default vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-y"  "M-y"))
(setq-default vterm-max-scrollback (- 20000 42))
(setq-default vterm-enable-manipulate-selection-data-by-osc52 t)
(setq-default vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=no ")
(setq vterm-toggle-cd-auto-create-buffer t)
(setq-default vterm-clear-scrollback-when-clearing t)
(setq-default term-prompt-regexp "^[^#$%>\n]*[#$%>] *") ;默认regex 相当于没定义，term-bol无法正常中转到开头处
(setq vterm-buffer-name-string "*vterm* %s")

(require 'vterm)
(require 'vterm-toggle)

(add-hook 'vterm-toggle-show-hook #'evil-insert-state)
(add-hook 'vterm-toggle-hide-hook #'evil-normal-state)
(setq vterm-toggle-fullscreen-p t)
(setq vterm-toggle-reset-window-configration-after-exit 'kill-window-only)

(defun vterm-ctrl-g ()
  "vterm ctrl-g"
  (interactive)
  (if (save-excursion (goto-char (point-at-bol))(search-forward-regexp "filter>" nil t))
      (if (equal last-command 'vterm-ctrl-g)
          (evil-normal-state)
        (call-interactively 'vmacs-vterm-self-insert))
    (if (equal last-command 'vterm-copy-mode)
        (call-interactively 'vmacs-vterm-self-insert)
      (if (equal last-command 'evil-normal-state)
          (progn
            (vterm-copy-mode 1)
            (setq this-command 'vterm-copy-mode)
            )
        (setq this-command 'evil-normal-state)
        (evil-normal-state)))))


(defun vmacs-vterm-self-insert()
  (interactive)
  (unless (evil-insert-state-p)
    (evil-insert-state))
  (call-interactively 'vterm--self-insert))

(defun vmacs-vterm-enable-output()
  (when (member major-mode '(vterm-mode))
    (vterm-copy-mode -1)))

(defun vmacs-vterm-copy-mode-hook()
  (if vterm-copy-mode
      (progn
        (message "vterm-copy-mode enabled")
        (unless (evil-normal-state-p)
          (evil-normal-state)))
    (unless (evil-insert-state-p)
      (evil-insert-state))))

(add-hook 'vterm-copy-mode-hook #'vmacs-vterm-copy-mode-hook)
(add-hook 'evil-insert-state-entry-hook 'vmacs-vterm-enable-output)

(defun vterm-eob()
  (interactive)
  (goto-char (point-max))
  (skip-chars-backward "\n[:space:]"))

(define-key vterm-mode-map (kbd "s-C-M-u") 'vterm-toggle)
(define-key vterm-mode-map (kbd "C-c C-g")   #'vterm--self-insert)
(define-key vterm-mode-map (kbd "s-v")   #'vterm-yank)

;; C－s 停止滚屏 C-q恢复滚屏
(define-key vterm-mode-map [(control return)]   #'vterm-toggle-insert-cd)

(define-key vterm-copy-mode-map (kbd "C-c C-c")   #'vterm-send-C-c)
;; (define-key vterm-mode-map (kbd "C-l")   #'vterm-clear)
(define-key vterm-mode-map (kbd "C-c C-e")   #'compilation-shell-minor-mode)
(define-key vterm-copy-mode-map [remap self-insert-command] #'vterm--self-insert)

(evil-collection-define-key 'insert 'vterm-mode-map
  (kbd "C-r") 'vmacs-vterm-self-insert
  (kbd "C-u") 'universal-argument
  (kbd "C-\\") 'toggle-input-method
  (kbd "C-l") #'vterm-clear
  (kbd "C-g") 'vterm-ctrl-g)

(evil-collection-define-key 'normal 'vterm-mode-map
  (kbd "C-g") 'vterm-ctrl-g
  (kbd "C-p") 'vmacs-vterm-self-insert
  (kbd "C-n") 'vmacs-vterm-self-insert
  (kbd "C-r") 'vmacs-vterm-self-insert
  (kbd "C-y") 'vterm-yank
  (kbd "C-/") 'vterm-undo
  "G" 'vterm-eob)

(defun vmacs-vterm-hook()
  (let ((p (get-buffer-process (current-buffer))))
    (when p (set-process-query-on-exit-flag p nil))))

(add-hook 'vterm-mode-hook 'vmacs-vterm-hook)



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


(provide 'conf-vterm)
