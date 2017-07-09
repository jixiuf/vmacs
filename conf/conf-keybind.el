;; 默认Emacs 把TAB==`C-i'
;;            RET==`C-m'
;;            ESC==`C-['
;;这样可以进行绑定的键好像少了一些,
;;下面的方法可以实现将`C-i' `C-m'绑定与`TAB' `RET'不同的func
(defun vmacs-translate-keybind(&optional f) ;
  (with-selected-frame f
    (when (display-graphic-p)
      (define-key input-decode-map [?\C-i] [C-i]) ;(global-set-key (kbd "<C-i>") 'counsel-git)
      (define-key input-decode-map [?\C-m] [C-m]) ; (global-set-key (kbd "<C-m>") 'counsel-git)
      )))
(add-hook 'after-make-frame-functions 'vmacs-translate-keybind)


;; iterm2下实同一些 终端下本没有的按键
;;参见 这个链接中含中文  http://jixiuf.github.io/blog/emacs-在mac上的安装及一些相应配置/#orgheadline15
(defun iterm2-keybind-mapping()
  (when (eq system-type 'darwin)
    (global-set-key (kbd "C-[ [ a a") (key-binding(kbd "C-<backspace>"))) ;== "M-[ a a" iterm2 map to ctrl-backspace
    ;; (global-set-key (kbd "C-[ [ a c") 'hippie-expand)   ; iterm map to ctrl-return
    (global-set-key (kbd "C-[ [ a d") (key-binding (kbd "C-,") ))   ;iterm2 map to ctrl-,

    (global-set-key (kbd "C-[ [ a e") (key-binding (kbd "C-.") ))   ;iterm2 map to ctrl-.
    (global-set-key (kbd "C-[ [ a f") (key-binding (kbd "C-;") ))   ; iterm map to ctrl-;
    ;; (global-set-key (kbd "C-[ [ a h") (key-binding (kbd "C-i") )) ; iterm map to C-i
    (global-set-key  (kbd "C-[ [ a h") (key-binding (kbd "<C-i>") )) ;iterm map to C-i
    (global-set-key (kbd "C-[ [ a i") (key-binding (kbd "C-3") ))   ;iterm2 map to ctrl-3
    (global-set-key (kbd "C-[ [ a j") 'ignore) ; iterm map to C-4
    (global-set-key (kbd "C-[ [ a j") (key-binding (kbd "C-4") ))   ;iterm2 map to ctrl-f3
    (global-set-key (kbd "C-[ [ a m") (key-binding (kbd "C-m") ))   ;iterm2 map to C-m
    ;; (global-set-key (kbd "C-w C-[ [ a h") 'goto-definition) ; C-wC-i

    (global-set-key (kbd "C-[ [ a l") (key-binding (kbd "C-<f3>") ))   ;iterm2 map to ctrl-f3

    (global-set-key (kbd "C-[ [ a b") (key-binding (kbd "C-<f2>") )) ; map to ctrl-f2
    )) ;iterm2特有的配置
;; (iterm2-keybind-mapping)
(add-hook 'after-init-hook 'iterm2-keybind-mapping)
 ;iterm2特有的配置

(global-set-key (kbd "C-<f3>") 'cd-iterm2)
;; 在minibuffer用C-l用于回到上层目录，通常在打开文件时用的到
(define-key minibuffer-local-completion-map (kbd "C-l") 'minibuffer-up-parent-dir)
(define-key minibuffer-local-map (kbd "C-l") 'minibuffer-up-parent-dir)

(global-set-key [(tab)]       'smart-tab)
(global-set-key (kbd "TAB")   'smart-tab)
;; (global-set-key (kbd "<C-i>") 'counsel-git) ;Ctrl-i not tab


(define-key minibuffer-local-map (kbd "<C-m>") 'exit-minibuffer)
(define-key minibuffer-local-completion-map (kbd "<C-m>") 'exit-minibuffer)
(define-key minibuffer-local-must-match-map (kbd "<C-m>") 'exit-minibuffer)
(define-key minibuffer-local-map (kbd "M-p") 'previous-history-element)
(define-key minibuffer-local-map (kbd "M-n") 'next-history-element)

;; (define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(autoload 'minibuffer-keyboard-quit "delsel" "" t nil)
;; (define-key minibuffer-local-map [escape]  'minibuffer-keyboard-quit)
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

(with-eval-after-load 'isearch (define-key isearch-mode-map [escape] 'isearch-abort))


(provide 'conf-keybind)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-iterm2.el ends here.
