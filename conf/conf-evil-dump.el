;; https://github.com/mbriggs/.emacs.d/blob/master/my-keymaps.el

;; http://dnquark.com/blog/2012/02/emacs-evil-ecumenicalism/
;; https://github.com/cofi/dotfiles/blob/master/emacs.d/config/cofi-evil.el
;; https://github.com/syl20bnr/dotemacs/blob/master/init-package/init-evil.el
;;

;; 如果不想让某些命令jump ,则可以通过这种方式实现
;; You can disable it for %, f, F, t and T with the following:
;; (evil-set-command-property #'evil-jump-item :jump nil)
;; (evil-set-command-property #'evil-find-char :jump nil)
;; (evil-set-command-property #'evil-find-char-backward :jump nil)
;; (evil-set-command-property #'evil-find-char-to :jump nil)
;; (evil-set-command-property #'evil-find-char-to-backward :jump nil)


(setq-default
 ;; evil-search-module 'isearch        ;可以用C-w yank word
 evil-disable-insert-state-bindings t
 evil-search-module 'evil-search        ;可以用gn 命令，需要取舍
;; gn 命令的用法 / search 之后，可以用dgn 或cgn 对search到的第一个内容进行处理，然后用.去重复之
 evil-ex-search-highlight-all t
 evil-ex-search-persistent-highlight nil
 evil-toggle-key "<f15>"                ;用不到了 绑定到一个不常用的键,在emacs与normal间切换
 evil-want-visual-char-semi-exclusive t ; 当v 选择到行尾时是否包含换行符
 evil-want-C-w-delete nil
 evil-want-abbrev-expand-on-insert-exit nil
 evil-want-C-i-jump nil
 evil-cross-lines t
 evil-default-state 'normal
 evil-want-fine-undo t                  ;undo更细化,否则从N->I->N 中所有的修改作为一个undo
 evil-symbol-word-search t              ;# search for symbol not word
 evil-flash-delay 0.5                   ;default 2
 evil-ex-search-case 'sensitive
 ;; C-e ,到行尾时,光标的位置是在最后一个字符后,还是在字符上
 evil-move-beyond-eol  t
 evil-move-cursor-back nil)

(setq-default
 evil-normal-state-tag (propertize "N" 'face '((:background "green" :foreground "black")))
 evil-emacs-state-tag (propertize "E" 'face '((:background "orange" :foreground "black")))
 evil-insert-state-tag (propertize "I" 'face '((:background "red")))
 evil-motion-state-tag (propertize "M" 'face '((:background "blue")))
 evil-visual-state-tag (propertize "V" 'face '((:background "grey80" :foreground "cyan")))
 evil-operator-state-tag (propertize "O" 'face '((:background "purple"))))

;; (setq evil-highlight-closing-paren-at-point-states nil)
(setq-default
 evil-default-cursor '(t "white")
 evil-emacs-state-cursor  '("gray" box)
 evil-normal-state-cursor '("green" box)
 evil-visual-state-cursor '("cyan" hbar)
 evil-insert-state-cursor '("orange" (bar . 3))
 evil-motion-state-cursor '("red" box))

(setq-default evil-buffer-regexps
              '(("**testing snippet:" . insert)
                ("*compile*" . normal)
                ;; ("*Org Src" . insert)
                ("*Org Export Dispatcher*" . insert)
                ("*Async Shell Command*" . normal)
                ("^ \\*load\\*")))
(defun vmacs-calc-hook()
  (require 'calc-bin)
  ;; 默认calc 的移位移位操作是接32位的， 可以bw(calc-word-size) 来改成64位
  (calc-word-size 128)
  (define-key calc-mode-map (kbd "j") 'evil-next-line)
  (define-key calc-mode-map (kbd "k") 'evil-previous-line)
  (define-key calc-mode-map (kbd "SPC") nil)
  (define-key calc-mode-map (kbd "y") 'evil-yank))

(add-hook 'calc-mode-hook 'vmacs-calc-hook)

;; (), {}, [], <>, '', "", ` `, or “” by default
;; 不论是何种 ，都会将最近的配对进行操作
(setq-default evil-textobj-anyblock-blocks
              '(("(" . ")")
                ("{" . "}")
                ("\\[" . "\\]")
                ("<" . ">")
                ("'" . "'")
                ("\"" . "\"")
                ("`" . "`")
                ("“" . "”")
                ("［" . "］")           ;全角
                ("（" . "）")           ;全角
                ("{" . "}")             ;全角
                ))
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq-local evil-textobj-anyblock-blocks
                        '(("(" . ")")
                          ("{" . "}")
                          ("\\[" . "\\]")
                          ("\"" . "\"")))))


(global-set-key (kbd "C-x C-s") 'evil-write-all)
(global-set-key (kbd "C-x s") 'evil-write-all)
;; (define-key-lazy python-mode-map [(meta return)] 'eval-print-last-sexp 'python)

(defun vmacs-leader-after-init-hook(&optional frame)
  (define-key-lazy magit-mode-map (kbd "<SPC>") nil  'magit)
  (define-key-lazy magit-status-mode-map (kbd "<SPC>") nil  'magit)
  (define-key-lazy magit-diff-mode-map (kbd "<SPC>") nil  'magit)
  (define-key-lazy magit-stash-mode-map (kbd "<SPC>") nil  'magit)
  (define-key-lazy log-view-mode-map (kbd "<SPC>") nil  'log-view)
  (define-key-lazy tabulated-list-mode-map (kbd "<SPC>") nil  'tabulated-list)
  (define-key-lazy org-agenda-mode-map (kbd "<SPC>") nil  'org-agenda)
  (define-key-lazy dired-mode-map (kbd "<SPC>") nil  'dired)
  (define-key-lazy custom-mode-map (kbd "<SPC>") nil  'cus-edit)

  ;; (define-key-lazy ivy-occur-grep-mode-map (kbd "<SPC>") nil  'ivy)
  (define-key-lazy calc-mode-map (kbd "<SPC>") nil  'calc)
  (define-key-lazy Info-mode-map (kbd "<SPC>") nil  'info)
  (define-key-lazy Info-mode-map "g" nil 'info)
  (define-key-lazy Info-mode-map "n" nil 'info)
  (define-key-lazy grep-mode-map (kbd "<SPC>") nil  'grep)
  (define-key-lazy help-mode-map (kbd "<SPC>") nil  'help-mode)
  (define-key-lazy ibuffer-mode-map (kbd "<SPC>") nil  'ibuffer)
  (define-key-lazy ert-results-mode-map (kbd "<SPC>") nil  'ert)
  (define-key-lazy compilation-mode-map (kbd "<SPC>") nil  'compile)
  (define-key-lazy debugger-mode-map (kbd "<SPC>") nil  'debug)
  ;; (vmacs-leader-for '(diff-mode debugger-mode) '(insert))
  )
;; emacs27 daemonp 启动的时候 after-init-hook会有问题
(unless (daemonp)
  (add-hook 'after-init-hook 'vmacs-leader-after-init-hook))
(add-hook 'after-make-frame-functions 'vmacs-leader-after-init-hook)


(provide 'conf-evil-dump)

;; Local Variables:
;; coding: utf-8
;; End:
