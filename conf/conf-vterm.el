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
(setq vterm-toggle-cd-auto-create-buffer t)
(setq-default vterm-clear-scrollback-when-clearing t)
(setq-default term-prompt-regexp "^[^#$%>\n]*[#$%>] *") ;默认regex 相当于没定义，term-bol无法正常中转到开头处

(require 'vterm)
(require 'vterm-toggle)

(add-hook 'vterm-toggle-show-hook #'evil-insert-state)
(add-hook 'vterm-toggle-hide-hook #'evil-insert-state)
(setq vterm-toggle-fullscreen-p nil)

(defun vterm-ctrl-g ()
  "vterm ctrl-g"
  (interactive)
  (if (or (save-excursion (goto-char (point-at-bol))(search-forward-regexp "filter>" nil t))
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

(define-key vterm-mode-map (kbd "s-C-M-u") 'vterm-toggle)
;; (define-key vterm-mode-map (kbd "s-t")   #'vterm-toggle-cd-show)
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
(define-key vterm-mode-map (kbd "C-c C-e")   #'compilation-shell-minor-mode)
(define-key vterm-copy-mode-map [remap self-insert-command] #'vterm--self-insert)


(defun vmacs-vterm-hook()
  (let ((p (get-buffer-process (current-buffer))))
    (when p (set-process-query-on-exit-flag p nil)))
  (evil-local-mode 1)
  (evil-define-key 'insert 'local [escape] 'vterm--self-insert)
  (evil-define-key 'motion'local (kbd "C-r") 'vmacs-vterm-self-insert)
  (evil-define-key 'insert 'local (kbd "C-g") 'vterm-ctrl-g)
  (evil-define-key 'normal 'local (kbd "C-p") 'vmacs-vterm-self-insert)
  (evil-define-key 'normal 'local (kbd "C-n") 'vmacs-vterm-self-insert)
  (evil-define-key 'normal 'local (kbd "C-r") 'vmacs-vterm-self-insert)
  (evil-define-key 'normal 'local (kbd "p") 'vterm-evil-paste-after)
  (evil-define-key 'normal 'local (kbd "P") 'vterm-evil-paste-before)
  (evil-define-key 'normal 'local (kbd "C-y") 'vterm-yank)
  (evil-define-key 'normal 'local (kbd "C-/") 'vterm-undo)
  (evil-define-key 'normal 'local "a" 'vterm-evil-append)
  (evil-define-key 'normal 'local "d" 'vterm-evil-delete)
  (evil-define-key 'normal 'local "i" 'vterm-evil-insert)
  (evil-define-key 'normal 'local "c" 'vterm-evil-change)

  (evil-define-key 'normal 'local (kbd "u") 'vterm-undo)
  (evil-define-key 'normal 'local (kbd "G") 'vterm-eob))

(add-hook 'vterm-mode-hook 'vmacs-vterm-hook)
;; (add-hook 'vterm-mode-hook  'with-editor-export-editor)
(setq vterm-buffer-name-string "vterm %s")

(defun vmacs-kill-buffer-hook()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (and (derived-mode-p 'vterm-mode)
               (process-live-p proc))
      (vterm-send-C-c)
      (kill-process proc))))

(add-hook 'kill-buffer-hook 'vmacs-kill-buffer-hook)

(defun vterm-evil-insert ()
  (interactive)
  (vterm-goto-char (point))
  (call-interactively #'evil-insert))

(defun vterm-evil-append ()
  (interactive)
  (vterm-goto-char (point))
  (call-interactively #'evil-append))

(defun vterm-evil-delete ()
  "Provide similar behavior as `evil-delete'."
  (interactive)
  (let ((inhibit-read-only t))
    (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
      (call-interactively #'evil-delete))))

(defun vterm-evil-change ()
  "Provide similar behavior as `evil-change'."
  (interactive)
  (let ((inhibit-read-only t))
    (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
      (call-interactively #'evil-change))))

(defvar origin-evil-paste-after (symbol-function #'evil-paste-after))
(defvar origin-evil-paste-before (symbol-function #'evil-paste-before))

(evil-define-command vterm-evil-paste-after
  (count &optional register yank-handler)
  "Pastes the latest yanked text behind point.
The return value is the yanked text."
  :suppress-operator t
  (interactive "P<x>")
  (vterm-goto-char (1+ (point)))
  (let ((inhibit-read-only t)
        (buffer-read-only nil))
    (cl-letf* (((symbol-function #'insert) #'vterm-insert)
               ((symbol-function #'delete-region) #'vterm-delete-region))
      (funcall origin-evil-paste-after count register yank-handler))))

(evil-define-command vterm-evil-paste-before
  (count &optional register yank-handler)
  "Pastes the latest yanked text behind point.
The return value is the yanked text."
  :suppress-operator t
  (interactive "P<x>")
  (vterm-goto-char (point))
  (let ((inhibit-read-only t)
        (buffer-read-only nil))
    (cl-letf* (((symbol-function #'insert) #'vterm-insert)
               ((symbol-function #'delete-region) #'vterm-delete-region))
      (funcall origin-evil-paste-before count register yank-handler))))


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

(defun vmacs-term-mode-p(&optional args)
  (or (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode 'tsmterm-mode)))

(provide 'conf-vterm)
