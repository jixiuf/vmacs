;; (global-set-key (kbd "s-,") 'vterm-toggle)
;; (global-set-key (kbd "s-C-,") 'vterm-toggle)
;; (global-set-key  (kbd "s-t") 'vterm-toggle-cd)
;; (global-set-key  (kbd "s-C-t") 'vterm-toggle-cd)
(setq widen-automatically nil)         ;for goto-line
(defvar-keymap  g-mode-map
 "g" #'vmacs-goto-line
 "/" #'consult-focus-lines
 "z" #'consult-hide-lines
 "r" #'revert-buffer
 "i" #'meow-insert
 "n" #'next-error
 "p" #'previous-error
 "b" #'pop-global-mark
 "k" #'vmacs-meow-grab
 "u" #'upcase-dwim
 "U" #'downcase-dwim
 "m" #'push-mark-command
 "P" #'project-or-external-find-file
 "d" #'xref-find-definitions
 "," 'goto-last-change
 "." 'goto-last-change-reverse
 )
(with-eval-after-load 'smerge-mode
  (define-key g-mode-map "v" smerge-basic-map))

(global-set-key (kbd "C-c G") g-mode-map)
(defvar-keymap  vmacs-normal-mode-map
   "="  #'meow-indent
   "G"  #'vmacs-goto-line
   "g" g-mode-map
   "n"  #'meow-search
   "N"  #'meow-search-reverse
   "/"  #'isearch-forward
   "z"   #'meow-pop-selection)

(global-set-key (kbd "C-c N") vmacs-normal-mode-map)
(defvar-keymap  vmacs-motion-mode-map
   "j"  #'meow-next
   "k"  #'meow-prev
   "G"  #'vmacs-goto-line
   "g"  g-mode-map
   "/"  #'isearch-forward
   "z"   #'meow-pop-selection
   "<escape>"  #'keyboard-quit)
(global-set-key (kbd "C-c M") vmacs-motion-mode-map)


;; (global-set-key [(tab)]       'smart-tab)
;; (global-set-key (kbd "TAB")   'smart-tab)
;; (global-set-key (kbd "<C-i>") 'counsel-git) ;Ctrl-i not tab
(global-set-key (kbd "C-9")   #'(lambda() (interactive)  (vmacs-insert-pair "(" ")")))
(global-set-key (kbd "C-0")   #'(lambda() (interactive)(insert ")")))
(global-set-key (kbd "C--")   #'(lambda() (interactive)(insert "_")))
(defun vmacs-isearch-insert_()
  (interactive)
  (isearch-printing-char ?_))
(define-key isearch-mode-map  (kbd "C--")   'vmacs-isearch-insert_)
(defun vmacs-isearch-insert-quote()
  (interactive)
  (isearch-printing-char ?\"))

(define-key isearch-mode-map  (kbd "C-'")   'vmacs-isearch-insert-quote)
(global-set-key (kbd "C-'") #'(lambda() (interactive)  (vmacs-insert-pair "\"" "\"")))
(define-key isearch-mode-map  (kbd "M-n")   'isearch-forward-thing-at-point)
(define-key isearch-mode-map  (kbd "M-p")   'consult-isearch-history)
(define-key isearch-mode-map  (kbd "C-f")   'isearch-yank-word-or-char)
(define-key isearch-mode-map  (kbd "C-,")   'isearch-beginning-of-buffer)
(define-key isearch-mode-map  (kbd "C-.")   'isearch-end-of-buffer)
(define-key isearch-mode-map  (kbd "C-t")   'isearch-toggle-regexp)
(define-key isearch-mode-map  (kbd "C-e")   'isearch-edit-string)
(add-hook 'isearch-mode-hook #'push-mark)
(setq isearch-lazy-count t)
(setq lazy-highlight-cleanup nil)
(setq isearch-wrap-pause 'no)
(vmacs-leader "," #'pop-global-mark)
(advice-add 'pop-global-mark :before #'deactivate-mark)

(global-set-key (kbd "C-7")   #'(lambda() (interactive)(insert "&")))
(setq shell-command-buffer-name "*Messages*")
(setq shell-command-default-error-buffer "*Messages*")
(setq shell-command-dont-erase-buffer nil)
(defun vmacs-isearch-insert-shift1()
  (interactive)
  (isearch-printing-char ?\!))
(global-set-key (kbd "C-1")   (lambda()
                                (interactive)
                                (let ((shell-file-name "zsh")
                                      (shell-command-switch "-ic"))
                                  (cond
                                   ((eq major-mode 'dired-mode)
                                    (call-interactively 'dired-do-shell-command))
                                   (t
                                    (if current-prefix-arg
                                        (if (equal current-prefix-arg '(16))
                                            (emamux:send-command)
                                          (setq current-prefix-arg nil)
                                          (emamux:send-command))
                                      (if (region-active-p)
                                          (shell-command-on-region (region-beginning) (region-end)
                                                                   (read-shell-command "Shell command on region: ")
                                                                   t t)
                                        (call-interactively 'shell-command))))))))

(define-key isearch-mode-map  (kbd "C-1")   'vmacs-isearch-insert-shift1)

(with-eval-after-load 'isearch (define-key isearch-mode-map [escape] 'isearch-abort))


;; (global-set-key  (kbd "s-a") 'evil-mark-whole-buffer) ;mac Cmd+a
;; (global-set-key  (kbd "s-t") 'shell-toggle-cd) ;mac Cmd+a
;; (global-set-key  (kbd "s-s") 'evil-write-all)
(global-set-key  (kbd "s-z") 'undo)
(global-set-key  (kbd "s-r") 'compile-dwim-compile)
(global-set-key  (kbd "C-\\") 'hippie-expand)
(global-set-key  (kbd "s-1") 'delete-other-windows)
(global-set-key  (kbd "s-C-1") 'delete-other-windows) ;hyper-1
(global-set-key  (kbd "s-C-2") 'vmacs-split-window-vertically) ;hyper-2
(global-set-key  (kbd "s-2") 'vmacs-split-window-vertically)
(global-set-key  (kbd "s-C-3") 'vmacs-split-window-horizontally) ;hyper-2
(global-set-key  (kbd "s-3") 'vmacs-split-window-horizontally)
(with-eval-after-load 'cus-edit (define-key custom-mode-map "n" nil))
(global-set-key (kbd "s-C-i")  'project-find-file)
(global-set-key (kbd "s-i")  'project-find-file)
(global-set-key (kbd "s-C-I")  'project-or-external-find-file)
(global-set-key (kbd "s-I")  'project-or-external-find-file)
(global-set-key [S-insert] 'mouse-yank-primary)

;; (global-set-key  (kbd "s-C-u") 'vmacs-prev-buffer)
;; (global-set-key  (kbd "s-u") 'vmacs-prev-buffer)
(global-set-key  (kbd "s-C-u") 'vmacs-undo-kill-buffer)

(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "s-w") 'vmacs-kill-buffer-dwim)
(global-set-key (kbd "C-x K") 'vmacs-kill-buffer-dwim)
(global-set-key (kbd "C-c C-v") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-C-w") 'vmacs-kill-buffer-dwim)
(global-set-key (kbd "M-o") 'toggle-camelize)
(autoload 'golden-ratio-scroll-screen-up "golden-ratio-scroll-screen" "" t)
(autoload 'golden-ratio-scroll-screen-down "golden-ratio-scroll-screen" "" t)
(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up) ;C-v
;; (global-set-key "\C-u" 'gold-ratio-scroll-screen-up)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down) ;M-v
(global-set-key  (kbd "C-2") 'meow-set-mark)
; (global-set-key  (kbd "C-3") 'rectangle-mark-mode)
(global-set-key  (kbd "C-4") 'vmacs-meow-grab)
(global-set-key  (kbd "C-M-d") 'backward-kill-sexp)
(global-set-key (kbd "C-c C-k") 'compile-dwim-compile)
(global-set-key (kbd "C-s-r") 'compile-dwim-run)
(global-set-key  (kbd "M-,") 'scroll-other-window-down)
(global-set-key  (kbd "M-.") 'scroll-other-window)

(vmacs-leader "s" 'save-buffer)
(vmacs-leader "b" 'meow-last-buffer)
(vmacs-leader "q" 'meow-start-kmacro-or-insert-counter)
(vmacs-leader  "fg" #'vmacs-ai)

(vmacs-leader  "fm" (vmacs-defun switch-to-message
                      (if (equal (buffer-name) "*Messages*")
                          (switch-to-buffer (other-buffer))
                        (switch-to-buffer "*Messages*")
                        (goto-char (point-max)))))


(global-set-key (kbd "C-x C-u") #'vundo)
(vmacs-leader (kbd "l") 'ibuffer)
(vmacs-leader (kbd "j") 'dired-jump)
(vmacs-leader (kbd "nw") (vmacs-defun vmacs-widen
                           (meow--cancel-second-selection)
                           (meow--cancel-selection)
                           (widen)))
(vmacs-leader (kbd "nn") 'narrow-to-region)
(vmacs-leader (kbd "nf") 'narrow-to-defun)
(vmacs-leader (kbd "nb") 'vmacs-meow-grab)
(vmacs-leader (kbd "ng") 'meow-grab)
(vmacs-leader (kbd "(") 'kmacro-start-macro) ;C-x(
(vmacs-leader (kbd ")") 'kmacro-end-macro) ;C-x
(vmacs-leader (kbd "u") 'backward-up-list)
(vmacs-leader (kbd "$") 'toggle-truncate-lines)
(vmacs-leader (kbd "m") 'execute-extended-command)
(vmacs-leader (kbd "wc") 'toggle-case-fold)
(vmacs-leader (kbd "wl") 'git-link)
;; (global-set-key  (kbd "s-h") 'vmacs-undo-kill-buffer)
(vmacs-leader "p" 'list-packages)
(vmacs-leader "k" ctl-x-r-map)
(vmacs-leader (kbd "wd") 'fanyi-dwim2)

(defun vmacs-bury-boring-windows ()
  (when (equal last-command 'keyboard-quit)
    (bury-boring-windows)))

(advice-add 'keyboard-quit :before #'vmacs-bury-boring-windows)

(global-set-key (kbd "C-;") #'vmacs-meow-iedit)
(with-eval-after-load 'man
  (set-keymap-parent Man-mode-map meow-normal-state-keymap)
  (define-key Man-mode-map "q" #'save-buffers-kill-terminal)
  (define-key Man-mode-map (kbd "C-c Mq") 'save-buffers-kill-terminal)
  (define-key Man-mode-map (kbd "n") 'meow-search)
  (define-key Man-mode-map (kbd "N") 'meow-search-reverse)
  (define-key Man-mode-map (kbd "s-C-w") 'save-buffers-kill-terminal)
  (define-key Man-mode-map (kbd "C-x K") 'save-buffers-kill-terminal))

;; iterm2 下实同一些 终端下本没有的按键
;;参见 这个链接中含中文  http://jixiuf.github.io/blog/emacs-在 mac 上的安装及一些相应配置/#orgheadline15
;;   (global-set-key (kbd "C-[ [ 1 a") (key-binding(kbd "C-<backspace>"))) ;== "M-[ a a" iterm2 map to ctrl-backspace
(defun vmacs-tui(&optional f)
  ;; https://github.com/benjaminor/kkp for kitty keyboard protocol
  ;; 支持terminal下使用ctrl-;的协议， alacritty/kitty等支持
  ;; (global-kkp-mode +1) ; for
  ;; (define-key input-decode-map "\e[1;1a" (kbd "C-<backspace>"))
  ;; (define-key input-decode-map "\e[1;1c"  [(control return)] )
  ;; (define-key input-decode-map "\e[1;1d" (kbd "C-,"))
  ;; (define-key input-decode-map "\e[1;1e" (kbd "C-."))
  ;; (define-key input-decode-map "\e[1;1f" (kbd "C-;"))
  ;; (define-key input-decode-map "\e[1;1h" (kbd "C-i"))
  (define-key input-decode-map "\e[1;1i" (kbd "C-3"))
  (define-key input-decode-map "\e[1;1j" (kbd "C-4"))
  (define-key input-decode-map "\e[1;1k" (kbd "C-8"))
  (xterm-mouse-mode 1)
  (require 'clipetty)
  (global-clipetty-mode)
  ;; (define-key input-decode-map "\e[1;1l" (kbd "C-1"))
  ;; (define-key input-decode-map "\e[1;1m" (kbd "C-m"))
  )
(add-hook 'tty-setup-hook #'vmacs-tui)


;;   (global-set-key (kbd "C-[ [ 1 l") (key-binding (kbd "C-<f3>") ))   ;iterm2 map to ctrl-f3
;;   (global-set-key (kbd "C-[ [ 1 b") (key-binding (kbd "C-<f2>") )) ; map to ctrl-f2
;;   ) ;iterm2 特有的配置
;; ;; (iterm2-keybind-mapping)
;; (add-hook 'after-init-hook 'iterm2-keybind-mapping)

(provide 'conf-keybind)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-iterm2.el ends here.