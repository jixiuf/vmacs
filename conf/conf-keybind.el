;;;###autoload
(defmacro define-key-lazy (mode-map key cmd  feature &optional state)
  "define-key in `eval-after-load' block. `feature' is the file name where defined `mode-map'"
  (if state
      `(eval-after-load ,feature '(evil-define-key ,state ,mode-map ,key ,cmd))
    `(eval-after-load ,feature '(define-key ,mode-map ,key ,cmd))))

;; (defmacro vmacs-leader-for-map (map feature)
;;   `(define-key-lazy ,map ,(kbd "SPC") nil ,feature ))


;; (global-set-key [f2] 'vterm-toggle)
;; (global-set-key [C-f2] 'vterm-toggle-cd)
(global-set-key (kbd "s-,") 'vterm-toggle)
;; (global-set-key (kbd "s-d") 'vterm-toggle)
;; (global-set-key (kbd "s-C-M-d") 'vterm-toggle)
(global-set-key  (kbd "s-t") 'vterm-toggle-cd)
(define-key special-mode-map " " nil)

(global-set-key [(tab)]       'smart-tab)
(global-set-key (kbd "TAB")   'smart-tab)
;; (global-set-key (kbd "<C-i>") 'counsel-git) ;Ctrl-i not tab
(global-set-key (kbd "C-9")   #'(lambda() (interactive)(insert "()")(backward-char 1)))
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
(global-set-key (kbd "C-'")   #'(lambda() (interactive)(insert "\"\"")(backward-char 1)))

(global-set-key (kbd "C-7")   #'(lambda() (interactive)(insert "&")))
(global-set-key (kbd "C-8")   #'(lambda() (interactive)(insert "*")))
(defun vmacs-isearch-insert-shift1()
  (interactive)
  (isearch-printing-char ?\!))
(global-set-key (kbd "C-1")   #'(lambda() (interactive)(insert "!")))
(define-key isearch-mode-map  (kbd "C-1")   'vmacs-isearch-insert-shift1)

(with-eval-after-load 'isearch (define-key isearch-mode-map [escape] 'isearch-abort))


(global-set-key (kbd "s-m") 'toggle-frame-maximized) ;cmd-m
(global-set-key  (kbd "s-a") 'evil-mark-whole-buffer) ;mac Cmd+a
;; (global-set-key  (kbd "s-t") 'shell-toggle-cd) ;mac Cmd+a

(global-set-key  (kbd "s-s") 'evil-write-all)

(global-set-key  (kbd "s-z") 'undo)
(global-set-key  (kbd "s-r") 'compile-dwim-compile)

(global-set-key  (kbd "s-l") 'delete-other-windows)
(global-set-key  (kbd "s-o") 'other-window)
(global-set-key  (kbd "s-C-M-o") 'other-window)

(global-set-key  [C-M-s-268632079] 'other-window)

;; (global-set-key  (kbd "s-n") 'vmacs-split-window-or-other-window)
;; (global-set-key  (kbd "s-C-M-n") 'vmacs-split-window-or-other-window)
;; (global-set-key  (kbd "s-p") 'vmacs-split-window-or-prev-window)
;; (global-set-key  (kbd "s-C-M-p") 'vmacs-split-window-or-prev-window)


(global-set-key [C-M-s-return] 'vmacs-window-rotate)




(global-set-key  (kbd "C-,") 'scroll-other-window-down)
(global-set-key  (kbd "C-.") 'scroll-other-window)
(global-set-key  (kbd "C-\\") 'hippie-expand)

;; (global-set-key  (kbd "s-q") 'delete-frame)
;; (global-set-key  (kbd "s-C-S-M-q") 'delete-frame)
;; (global-set-key  (kbd "s-w") 'delete-window)
(global-set-key  (kbd "s-1") 'delete-other-windows)
(global-set-key  (kbd "s-C-M-1") 'delete-other-windows) ;hyper-1
(global-set-key  (kbd "s-C-M-2") 'vmacs-split-window-vertically) ;hyper-2
(global-set-key  (kbd "s-2") 'vmacs-split-window-vertically)
(global-set-key  (kbd "s-C-M-3") 'vmacs-split-window-horizontally) ;hyper-2
(global-set-key  (kbd "s-3") 'vmacs-split-window-horizontally)
(with-eval-after-load 'cus-edit (define-key custom-mode-map "n" nil))

(global-set-key (kbd "s-C-M-i")  'vmacs-git-files)

(autoload 'dap-debug "dap-mode" nil t)
(global-set-key (kbd "<f6>")  'dap-debug)
(global-set-key (kbd "<f7>")  'dap-disconnect)


;; C-x esc esc 可以查看global-set-key 究竟bind哪个key
;; 默认Emacs 把TAB==`C-i'
;;            RET==`C-m'
;;            ESC==`C-['
;;这样可以进行绑定的键好像少了一些,
;;下面的方法可以实现将`C-i' `C-m'绑定与`TAB' `RET'不同的func
;; (defun vmacs-translate-keybind(&optional f) ;
;;   (with-selected-frame (or f (selected-frame))
;;     (when (display-graphic-p)
;;       (define-key input-decode-map [?\C-i] [C-i]) ;(global-set-key (kbd "<C-i>") 'counsel-git)
;;       (define-key input-decode-map [?\C-m] [C-m]) ; (global-set-key (kbd "<C-m>") 'counsel-git)
;;       )))

;; (add-hook 'after-make-frame-functions 'vmacs-translate-keybind)
;; (add-hook 'after-init-hook 'vmacs-translate-keybind) ;this is need for windows
;; (require 'bind-map)

;; iterm2下实同一些 终端下本没有的按键
;;参见 这个链接中含中文  http://jixiuf.github.io/blog/emacs-在mac上的安装及一些相应配置/#orgheadline15
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
;;   ) ;iterm2特有的配置
;; ;; (iterm2-keybind-mapping)
;; (add-hook 'after-init-hook 'iterm2-keybind-mapping)



(provide 'conf-keybind)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-iterm2.el ends here.
