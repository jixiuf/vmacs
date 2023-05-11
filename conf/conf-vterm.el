;; brew install libvterm
;; https://github.com/akermu/emacs-libvterm
;; mkdir -p build
;; cd build
;; cmake -DEMACS_SOURCE=~/repos/emacs ..
;; make
;; (setq-default vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-v" "M-v"))
(setq-default vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-y"  "M-y"))
(setq-default vterm-max-scrollback (- 20000 42))
(setq-default vterm-min-window-width 10)
(setq-default vterm-copy-mode-remove-fake-newlines t)
(setq-default vterm-enable-manipulate-selection-data-by-osc52 t)
(setq-default vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes ")
(setq vterm-toggle-cd-auto-create-buffer t)
(setq-default vterm-clear-scrollback-when-clearing t)
(setq-default term-prompt-regexp "^[^#$%>\n]*[#$%>] *") ;默认 regex 相当于没定义，term-bol 无法正常中转到开头处
(setq vterm-buffer-name-string "*vterm* %s")

(require 'vterm)
(require 'vterm-toggle)

(add-hook 'vterm-toggle-show-hook #'evil-insert-state)
(add-hook 'vterm-toggle-hide-hook #'evil-normal-state)
(setq vterm-toggle-fullscreen-p t)
(setq vterm-toggle-reset-window-configration-after-exit 'kill-window-only)
;; (setq vterm-toggle-hide-method 'bury-all-vterm-buffer)
;; 使用 swith-to-buffer 来 hide vterm,以确保使用共同的 window,与 tabline 更好的兼容
;; 主要是维护 buffer-list,以确保下次切回来，仍是最近使用的 vterm
(add-hook 'vterm-toggle-hide-hook #'(lambda() (switch-to-buffer (current-buffer))))
(setq vterm-toggle-hide-method nil)


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


(defun vmacs-vterm-kill-line()
  (interactive)
  (let ((succ (vterm-goto-char (point)))
        (beg (point))
        (end (vterm--get-end-of-line)))
    (save-excursion
      (goto-char end)
      (when (looking-back "[ \t\n]+" beg t)
        (setq end (match-beginning 0)))
      (when (> end beg) (kill-ring-save beg end)))
    (vterm-send-key "k" nil nil :ctrl)))

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

(define-key vterm-mode-map (kbd "s-C-u") 'vterm-toggle)
(define-key vterm-mode-map (kbd "s-u") 'vterm-toggle)
(define-key vterm-mode-map (kbd "C-c C-g")   #'vterm--self-insert)
(define-key vterm-mode-map (kbd "s-v")   #'vterm-yank)
(define-key vterm-mode-map (kbd "C-k")   #'vmacs-vterm-kill-line)
(define-key vterm-mode-map (kbd "C-q")   #'vterm-send-next-key)
(define-key vterm-mode-map (kbd "M-:")   #'eval-expression)


;; C－s 停止滚屏 C-q 恢复滚屏
(define-key vterm-mode-map [(control return)]   #'vterm-toggle-insert-cd)

(define-key vterm-copy-mode-map (kbd "C-c C-c")   #'vterm--self-insert)
;; (define-key vterm-mode-map (kbd "C-l")   #'vterm-clear)
(define-key vterm-mode-map (kbd "C-c C-e")   #'compilation-shell-minor-mode)
(define-key vterm-copy-mode-map [remap self-insert-command] #'vterm--self-insert)
(evil-define-operator evil-vterm-delete-char (beg end type register)
  "Delete previous character."
  :motion evil-forward-char
  (interactive "<R><x>")
  (evil-collection-vterm-delete beg end type register))


(evil-collection-define-key 'insert 'vterm-mode-map
  (kbd "C-r") 'vmacs-vterm-self-insert
  (kbd "C-u") 'universal-argument
  (kbd "C-\\") 'toggle-input-method
  (kbd "C-l") #'vterm-clear
  (kbd "C-g") 'vterm-ctrl-g)

(evil-collection-define-key 'normal 'vterm-mode-map
  "s" nil                               ;unbind s
  (kbd "C-g") 'vterm-ctrl-g
  (kbd "C-p") 'vmacs-vterm-self-insert
  (kbd "C-n") 'vmacs-vterm-self-insert
  (kbd "C-k")   #'vmacs-vterm-kill-line
  (kbd "C-r") 'vmacs-vterm-self-insert
  (kbd "C-y") 'vterm-yank
  (kbd "C-/") 'vterm-undo
  "x" 'evil-vterm-delete-char
  "G" 'vterm-eob)
(evil-collection-define-key 'visual 'vterm-mode-map "x" 'evil-vterm-delete-char)

(defun vmacs-vterm-hook()
  (evil-define-key 'insert 'local   (kbd "<escape>") 'vterm--self-insert)
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

(define-key vterm-mode-map (kbd "C-x C-e") 'vterm-edit-command-action)
(define-key vterm-mode-map (kbd "C-c e") 'vterm-edit-command-action)

(defun vterm-edit-command-action ()
  (interactive)
  (let* ((delete-trailing-lines t)
         (vtermbuf (current-buffer))
         (begin (vterm--get-prompt-point))
         (buffer (get-buffer-create "vterm-edit-command"))
         (n (length (filter-buffer-substring begin (point))))
         foreground
         (content (filter-buffer-substring
                   begin (point-max))))
    (with-current-buffer buffer
      (setq vterm-edit-vterm-buffer vtermbuf)
      (erase-buffer)
      (insert content)
      (delete-trailing-whitespace)
      (goto-char (1+ n))
      ;; delete zsh auto-suggest candidates
      (setq foreground (plist-get (get-text-property (point) 'font-lock-face) :foreground ))
      (when (equal foreground  (face-background 'vterm-color-black nil 'default))
        (delete-region (point) (point-max)))
      (sh-mode)
      (vterm-edit-command-mode)
      (evil-insert-state)
      (setq-local header-line-format
                  (substitute-command-keys
                   (concat "Edit, then "
                           (mapconcat
                            'identity
                            (list "\\[vterm-edit-command-commit]: Finish"
                                  "\\[vterm-edit-command-abort]: Abort"
                                  )
                            ", "))))
      (split-window-sensibly)
      (switch-to-buffer-other-window buffer)))
  )

(defun vterm-edit-command-commit ()
  (interactive)
  (let ((delete-trailing-lines t)
        content)
    (delete-trailing-whitespace)
    (goto-char (point-max))
    (when (looking-back "\n") (backward-delete-char 1))
    (setq content (buffer-string))
    (with-current-buffer vterm-edit-vterm-buffer
      (vterm-send-key "a" nil nil t)
      (vterm-send-key "k" nil nil t t)
      (unless (vterm--at-prompt-p)
        (vterm-send-key "c" nil nil t))
      (vterm-send-string content)))
  (vterm-edit-command-abort))

(defun vterm-edit-command-abort ()
  (interactive)
  (kill-buffer-and-window))

(defvar vterm-edit-command-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-c") #'vterm-edit-command-commit)
    (define-key keymap (kbd "C-c C-k") #'vterm-edit-command-abort)
    keymap))

(define-minor-mode vterm-edit-command-mode
  "Vterm Edit Command Mode")

;; (defun zsh-find-file-hook()
;;   (when (string-prefix-p "/tmp/zsh" (buffer-file-name))
;;     (setq truncate-lines nil)
;;     (local-set-key (kbd "C-c C-k") #'server-edit-abort)
;;     (local-set-key (kbd "C-c C-c") #'vmacs-kill-buffer-dwim)))

;; (add-hook #'sh-mode-hook #'zsh-find-file-hook)

;; (define-key vterm-mode-map (kbd "C-x C-e") #'(lambda () (interactive) (vterm-send-string (kbd "C-x C-e"))))
;; (define-key vterm-mode-map (kbd "C-c e")  #'(lambda () (interactive) (vterm-send-string (kbd "C-x C-e"))))
;; (define-key vterm-mode-map (kbd "C-c '")  #'(lambda () (interactive) (vterm-send-string (kbd "C-x C-e"))))

(provide 'conf-vterm)
