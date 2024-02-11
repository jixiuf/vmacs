(global-set-key (kbd "s-,") 'vterm-toggle)
(global-set-key (kbd "s-C-,") 'vterm-toggle)
(global-set-key  (kbd "s-t") 'vterm-toggle-cd)
(global-set-key  (kbd "s-C-t") 'vterm-toggle-cd)
(define-key special-mode-map " " nil)

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
(define-key isearch-mode-map  (kbd "M-.")   'isearch-forward-thing-at-point)
(define-key isearch-mode-map  (kbd "M-n")   'isearch-forward-thing-at-point)
(define-key isearch-mode-map  (kbd "C-f")   'isearch-yank-word-or-char)
(define-key isearch-mode-map  (kbd "C-,")   'isearch-beginning-of-buffer)
(define-key isearch-mode-map  (kbd "C-.")   'isearch-end-of-buffer)
(define-key isearch-mode-map  (kbd "C-e")   'isearch-edit-string)
(setq isearch-lazy-count t)
(setq lazy-highlight-cleanup nil)

(setq lazy-highlight-buffer t)
;; (setq lazy-highlight-initial-delay 0)
;; (setq lazy-highlight-no-delay-length 0)
(defadvice keyboard-quit (before lazy-highlight-cleanup activate)
  (require 'isearch)
  (call-interactively #'lazy-highlight-cleanup)
  ;; (meow--remove-search-indicator)
  (meow-cancel-selection))


(global-set-key (kbd "C-7")   #'(lambda() (interactive)(insert "&")))
(defun vmacs-isearch-insert-shift1()
  (interactive)
  (isearch-printing-char ?\!))
(global-set-key (kbd "C-1")   (lambda()
                                (interactive)
                                (if (region-active-p)
                                    (call-interactively 'shell-command)
                                  (if (eq major-mode 'dired-mode)
                                      (call-interactively 'dired-do-shell-command)
                                      (call-interactively 'shell-command)))))

(define-key isearch-mode-map  (kbd "C-1")   'vmacs-isearch-insert-shift1)

(with-eval-after-load 'isearch (define-key isearch-mode-map [escape] 'isearch-abort))


;; (global-set-key  (kbd "s-a") 'evil-mark-whole-buffer) ;mac Cmd+a
;; (global-set-key  (kbd "s-t") 'shell-toggle-cd) ;mac Cmd+a
;; (global-set-key  (kbd "s-s") 'evil-write-all)
(global-set-key  (kbd "s-z") 'undo)
(global-set-key  (kbd "C-z") 'undo)
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


;; (global-set-key  (kbd "s-C-u") 'vmacs-prev-buffer)
;; (global-set-key  (kbd "s-u") 'vmacs-prev-buffer)
(global-set-key  (kbd "s-C-u") 'vmacs-undo-kill-buffer)

(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "s-w") 'vmacs-kill-buffer-dwim)
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


(global-set-key (kbd "C-x C-u") #'vundo)
(vmacs-leader (kbd "l") 'ibuffer)
(vmacs-leader (kbd "j") 'dired-jump)
(vmacs-leader (kbd "nw") (vmacs-defun vmacs-widen
                           (meow--cancel-second-selection)
                           (meow--cancel-selection)
                           (widen)))
(vmacs-leader (kbd "nn") 'narrow-to-region)
(vmacs-leader (kbd "ng") 'vmacs-meow-grab)
(vmacs-leader (kbd "(") 'kmacro-start-macro) ;C-x(
(vmacs-leader (kbd ")") 'kmacro-end-macro) ;C-x
(vmacs-leader (kbd "u") 'backward-up-list)
(vmacs-leader (kbd "$") 'toggle-truncate-lines)
(vmacs-leader (kbd "m") 'execute-extended-command)
(vmacs-leader (kbd "wc") 'toggle-case-fold)
(vmacs-leader (kbd "wl") 'git-link)
;; (global-set-key  (kbd "s-h") 'vmacs-undo-kill-buffer)
(vmacs-leader "s" 'save-buffer)
(vmacs-leader "q" 'kill-other-buffers)
(vmacs-leader "p" 'list-packages)
(vmacs-leader "k" ctl-x-r-map)
(vmacs-leader (kbd "wd") 'fanyi-dwim2)

(defadvice keyboard-quit (before bury-boring-windows activate)
  (when (equal last-command 'keyboard-quit)
    (require 'lazy-buffer)
    (bury-boring-windows)))

(global-set-key (kbd "C-;") #'vmacs-meow-iedit)

;; C-x esc esc 可以查看 global-set-key 究竟 bind 哪个 key
;; 默认 Emacs 把 TAB==`C-i'
;;            RET==`C-m'
;;            ESC==`C-['
;;这样可以进行绑定的键好像少了一些,
;;下面的方法可以实现将`C-i' `C-m'绑定与`TAB' `RET'不同的 func
;; (defun vmacs-translate-keybind(&optional f) ;
;;   (with-selected-frame (or f (selected-frame))
;;     (when (display-graphic-p)
;;       (define-key input-decode-map [?\C-i] [C-i]) ;(global-set-key (kbd "<C-i>") 'counsel-git)
;;       (define-key input-decode-map [?\C-m] [C-m]) ; (global-set-key (kbd "<C-m>") 'counsel-git)
;;       )))

;; (add-hook 'after-make-frame-functions 'vmacs-translate-keybind)
;; (add-hook 'after-init-hook 'vmacs-translate-keybind) ;this is need for windows
;; (require 'bind-map)

;; iterm2 下实同一些 终端下本没有的按键
;;参见 这个链接中含中文  http://jixiuf.github.io/blog/emacs-在 mac 上的安装及一些相应配置/#orgheadline15
;; (defun iterm2-keybind-mapping()
;;   (global-set-key (kbd "C-[ [ 1 a") (key-binding(kbd "C-<backspace>"))) ;== "M-[ a a" iterm2 map to ctrl-backspace
;;   ;; (global-set-key (kbd "C-[ [ 1 c") 'hippie-expand)   ; iterm map to ctrl-return
;;   (global-set-key (kbd "C-[ [ 1 d") (key-binding (kbd "C-,") ))   ;iterm2 map to ctrl-,

;;   (global-set-key (kbd "C-[ [ 1 e") (key-binding (kbd "C-.") ))   ;iterm2 map to ctrl-.
;;   (global-set-key (kbd "C-[ [ 1 f") (key-binding (kbd "C-;") ))   ; iterm map to ctrl-;
;;   ;; (global-set-key (kbd "C-[ [ 1 h") (key-binding (kbd "C-i") )) ; iterm map to C-i
;;   (global-set-key  (kbd "C-[ [ 1 h") (key-binding (kbd "<C-i>") )) ;iterm map to C-i
;;   (global-set-key (kbd "C-[ [ 1 i") (key-binding (kbd "C-3") ))   ;iterm2 map to ctrl-3
;;   (global-set-key (kbd "C-[ [ 1 j") 'ignore) ; iterm map to C-4
;;   (global-set-key (kbd "C-[ [ 1 j") (key-binding (kbd "C-4") ))   ;iterm2 map to ctrl-f3
;;   (global-set-key (kbd "C-[ [ 1 m") (key-binding (kbd "<C-m>") ))   ;iterm2 map to C-m
;;   ;; (global-set-key (kbd "C-w C-[ [ 1 h") 'goto-definition) ; C-wC-i

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
