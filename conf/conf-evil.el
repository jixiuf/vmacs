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
 evil-search-module 'isearch        ;可以用C-w yank word
 ;; evil-search-module 'evil-search        ;可以用gn 命令，需要取舍
;; gn 命令的用法 / search 之后，可以用dgn 或cgn 对search到的第一个内容进行处理，然后用.去重复之
 evil-ex-search-highlight-all nil
 evil-toggle-key "<f16>"                ;用不到了 绑定到一个不常用的键,在emacs与normal间切换
 evil-want-visual-char-semi-exclusive t ; 当v 选择到行尾时是否包含换行符
 evil-want-C-i-jump nil
 evil-cross-lines t
 evil-default-state 'normal
 evil-want-fine-undo t                  ;undo更细化,否则从N->I->N 中所有的修改作为一个undo
 evil-symbol-word-search t              ;# search for symbol not word
 evil-flash-delay 0.5                   ;default 2
 evil-ex-search-case 'sensitive
 ;; C-e ,到行尾时,光标的位置是在最后一个字符后,还是在字符上
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
 evil-insert-state-cursor '("orange" bar)
 evil-motion-state-cursor '("gray" box))



(require 'evil)
;; minor-mode
;; 设置一些mode的初始state
(add-hook 'org-capture-mode-hook 'evil-insert-state)
(setq-default evil-buffer-regexps
              '(("**testing snippet:" . insert)
                ("*compile*" . normal)
                ("*Org Src" . insert)
                ("*Org Export Dispatcher*" . insert)
                ("*Async Shell Command*" . normal)
                ("^ \\*load\\*")))
;; 设置一些mode的初始state
(evil-set-initial-state 'bm-show-mode 'insert)
(evil-set-initial-state 'diff-mode 'insert)
(evil-set-initial-state 'git-rebase-mode 'normal)
(evil-set-initial-state 'package-menu-mode 'normal)
(evil-set-initial-state 'vc-annotate-mode 'normal)
(evil-set-initial-state 'Custom-mode 'normal)
(evil-set-initial-state 'erc-mode 'normal)
(evil-set-initial-state 'ibuffer-mode 'normal)
(evil-set-initial-state 'vc-dir-mode 'normal)
(evil-set-initial-state 'vc-git-log-view-mode 'normal)
(evil-set-initial-state 'vc-svn-log-view-mode 'normal)
(evil-set-initial-state 'erlang-shell-mode 'normal)
(evil-set-initial-state 'org-agenda-mode 'normal)
(evil-set-initial-state 'minibuffer-inactive-mode 'normal)

;; 把所有emacs state  的mode 都转成insert mode
(dolist (mode evil-emacs-state-modes)
  (cond
   ((memq mode evil-normal-state-modes)
    (evil-set-initial-state mode 'normal))
   ((memq mode evil-motion-state-modes)
    (evil-set-initial-state mode 'motion))
   (t
    (evil-set-initial-state mode 'insert))))

(setq evil-emacs-state-modes nil)


;; evil-overriding-maps中的按键绑定 优先级高于evil-mode
(add-to-list 'evil-overriding-maps '(vc-git-log-view-mode-map . nil))
(add-to-list 'evil-overriding-maps '(vc-svn-log-view-mode-map . nil))
(add-to-list 'evil-overriding-maps '(evil-leader--default-map . nil))
(add-to-list 'evil-overriding-maps '(custom-mode-map . nil))
(add-to-list 'evil-overriding-maps '(ediff-mode-map . nil))
(add-to-list 'evil-overriding-maps '(package-menu-mode-map . nil))
(add-to-list 'evil-overriding-maps '(minibuffer-local-map . nil))
(add-to-list 'evil-overriding-maps '(minibuffer-local-completion-map . nil))
(add-to-list 'evil-overriding-maps '(minibuffer-local-must-match-map . nil))
(add-to-list 'evil-overriding-maps '(minibuffer-local-isearch-map . nil))
(add-to-list 'evil-overriding-maps '(minibuffer-local-ns-map . nil))
(add-to-list 'evil-overriding-maps '(epa-key-list-mode-map . nil))




;; 更新 evil-overriding-maps ,因为org-agenda-mode-map 变量初始为空keymap,在org-agenda-mode内才往里添加绑定
(evil-set-custom-state-maps 'evil-overriding-maps
                            'evil-pending-overriding-maps
                            'override-state
                            'evil-make-overriding-map
                            evil-overriding-maps)

(with-eval-after-load 'org-agenda
  (add-to-list 'evil-overriding-maps '(org-agenda-mode-map . nil))
  (evil-set-custom-state-maps 'evil-overriding-maps
                              'evil-pending-overriding-maps
                              'override-state
                              'evil-make-overriding-map
                              evil-overriding-maps))

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
;; (setq-default evil-magit-state 'normal)

(evil-mode 1)

;; (unless (equal system-type 'windows-nt)
;;   (defun vmacs-change-cursor-hook(&optional f)
;;     (with-selected-frame (or f (selected-frame))
;;       (if (display-graphic-p)
;;           (when (featurep 'evil-terminal-cursor-changer)
;;             (evil-terminal-cursor-changer-deactivate))
;;         (evil-terminal-cursor-changer-activate)
;;         (remove-hook 'pre-command-hook 'etcc--evil-set-cursor))))
;;   (add-hook 'after-make-frame-functions 'vmacs-change-cursor-hook)
;;   ;; (add-hook 'focus-in-hook 'vmacs-change-cursor-hook)
;;   ;; (add-hook 'evil-insert-state-entry-hook 'vmacs-change-cursor-hook)
;;   )


;;(global-evil-matchit-mode 1)


;; emacs 自带的repeat 绑定在C-xz上， 这个advice ,奖 repeat 的功能 与evil 里的","功能合
;; 2为1,一起绑定在","紧临evil-repeat"." 如此一来， 跟编辑相关的repeat用"." ,跟光标移动相关的
;; 可以用","
(defadvice repeat(around evil-repeat-find-char-reverse activate)
  "if last-command is `evil-find-char' or
`evil-repeat-find-char-reverse' or `evil-repeat-find-char'
call `evil-repeat-find-char-reverse' if not
execute emacs native `repeat' default binding to`C-xz'"
  (if (member last-command '(evil-find-char
                             evil-repeat-find-char-reverse
                             repeat
                             evil-find-char-backward
                             evil-repeat-find-char))
      (progn
        ;; ;I do not know why need this(in this advice)
        (when (evil-visual-state-p)(unless (bobp) (forward-char -1)))
        (call-interactively 'evil-repeat-find-char-reverse)
        (setq this-command 'evil-repeat-find-char-reverse))
    ad-do-it))

;; ctrl-g 时，回到normal状态
(defadvice keyboard-quit (before evil-insert-to-nornal-state activate)
  "C-g back to normal state"
  (when  (evil-insert-state-p)
    (cond
     ((equal (evil-initial-state major-mode) 'normal)
      (evil-normal-state))
     ((equal (evil-initial-state major-mode) 'insert)
      (evil-normal-state))
     ((equal (evil-initial-state major-mode) 'motion)
      (evil-motion-state))
     ((equal (evil-initial-state-for-buffer-name (buffer-name) 'insert) 'insert)
      (evil-normal-state))
     ((equal (evil-initial-state-for-buffer-name (buffer-name) 'insert) 'motion)
      (evil-motion-state))
     (t
      (if (equal last-command 'keyboard-quit)
          (evil-normal-state)           ;如果初始化state不是normal ，按两次才允许转到normal state
        (evil-change-to-initial-state)) ;如果初始化state不是normal ，按一次 转到初始状态
      ))))





(with-eval-after-load 'org-agenda
  (evil-define-key 'normal org-agenda-mode-map
    "j" 'evil-next-line
    "k" 'evil-previous-line
    ":" 'evil-ex
    "r" 'org-agenda-redo))





;; ;; 清空所有insert-state的绑定,这样 ,insert mode 就是没装evil 前的正常emacs了
(setcdr evil-insert-state-map nil)
(setq evil-window-map nil)
;; (define-key evil-insert-state-map [escape] nil) ;emacs karabiner shift 输入法切换相关
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)

(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-map (kbd "M-p") 'previous-history-element)
(define-key minibuffer-local-map (kbd "M-n") 'next-history-element)
(define-key evil-ex-completion-map (kbd "M-p") 'previous-history-element) ;
(define-key evil-ex-completion-map (kbd "M-n")  'next-history-element)

(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

(with-eval-after-load 'isearch (define-key isearch-mode-map [escape] 'isearch-abort))

(define-key evil-insert-state-map [escape] 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)


;; (define-key evil-normal-state-map "/" 'helm-swoop)

;; (define-key evil-motion-state-map "/" 'evil-search-forward)
;; (define-key evil-normal-state-map "/" 'evil-search-forward)



;; (define-key evil-window-map "1" 'delete-other-windows)
;; (define-key evil-window-map "0" 'delete-window)
;; (define-key evil-window-map "2" 'split-window-func-with-other-buffer-vertically)
;; (define-key evil-window-map "3" 'split-window-func-with-other-buffer-horizontally)

;; (define-key evil-normal-state-map (kbd "f") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "C-z") nil)
;; (define-key evil-normal-state-map (kbd "C-w") 'ctl-w-map)
;; (define-key evil-normal-state-map "\C-n" nil)
;; (define-key evil-normal-state-map "\C-p" nil)
(define-key evil-normal-state-map "\C-v" nil)
(define-key evil-motion-state-map "\C-v" nil)
(define-key evil-normal-state-map "\C-e" nil)

(fset 'evil-next-line 'evil-next-visual-line)
(fset 'evil-previous-line 'evil-previous-visual-line)
;; (define-key evil-motion-state-map "j" 'evil-next-visual-line)
;; (define-key evil-motion-state-map "k" 'evil-previous-visual-line)


;; (define-key evil-motion-state-map (kbd "C-w") nil)
(define-key evil-motion-state-map (kbd "C-i") nil)
(define-key evil-motion-state-map (kbd "C-b") nil)
(define-key evil-motion-state-map (kbd "C-d") nil)
(define-key evil-motion-state-map (kbd "C-e") nil)
(define-key evil-motion-state-map (kbd "C-f") nil)
(define-key evil-motion-state-map (kbd "C-y") nil)
(define-key evil-normal-state-map [remap yank-pop] nil)
(define-key evil-normal-state-map (kbd "M-.") nil)
;; (define-key evil-normal-state-map "q" nil)
(define-key evil-normal-state-map (kbd "DEL") nil) ;backupspace
(define-key evil-motion-state-map  (kbd "RET") nil) ;
(define-key evil-normal-state-map  (kbd "RET") nil) ;
;; (define-key evil-motion-state-map "n" nil)
;; (define-key evil-motion-state-map "N" nil)
;; (define-key evil-normal-state-map "\C-r" nil)
(global-unset-key (kbd "C-s"))
(define-key evil-normal-state-map  (kbd "C-.") nil)
(define-key evil-normal-state-map  (kbd "M-.") nil)
;; (define-key evil-normal-state-map "o" nil)
;; (define-key evil-normal-state-map "\M-o" 'evil-open-below)
;; (define-key evil-normal-state-map "O" nil)
;; (define-key evil-motion-state-map (kbd "C-o") 'evil-open-below)

;; (define-key evil-normal-state-map "m" nil) ;evil-set-marker

(define-key evil-motion-state-map "`" nil) ;'evil-goto-mark


;; (define-key evil-motion-state-map "L" 'vmacs-forward-4-line)
;; (define-key evil-motion-state-map "H" 'vmacs-backward-4-line)
;; (define-key evil-normal-state-map "s" 'conf-forward-symbol-or-isearch-regexp-forward)
;; (define-key evil-normal-state-map "S" 'conf-backward-symbol-or-isearch-regexp-backward)
(define-key evil-normal-state-map "m" nil)
(define-key evil-normal-state-map "mq" 'fill-paragraph)
;; (define-key evil-normal-state-map "m\t" 'novel-fill)


;; C-3 加任一char ,start  and C-3 end ,then @char repeat
(define-key evil-normal-state-map (kbd "C-[ [ a i") 'evil-record-macro) ;C-3 default q

;; g; goto-last-change
;; g,  goto-last-change-reverse
;; (define-key evil-normal-state-map "g/" 'goto-last-change-reverse); goto-last-change

(define-key evil-normal-state-map "gh" 'evil-goto-line) ;default G

(define-key evil-normal-state-map "ga" (kbd "M-a"))
(define-key evil-normal-state-map "ge" (kbd "M-e"))
;; (define-key evil-normal-state-map "gA" (kbd "C-M-a"))
;; (define-key evil-normal-state-map "gE" (kbd "C-M-e"))

(define-key evil-normal-state-map "s" nil)
(define-key evil-normal-state-map "sa" 'evil-begin-of-defun)

;; (define-key evil-normal-state-map "sp" 'evil-paste-pop)
;; (define-key evil-normal-state-map "sP" 'evil-paste-pop)


(define-key evil-normal-state-map "ss" 'evil-end-of-defun)
(define-key evil-normal-state-map "se" 'evil-end-of-defun)

(define-key evil-normal-state-map "me" 'evil-M-e)
(define-key evil-normal-state-map "ma" 'evil-M-a)

(define-key evil-normal-state-map "s/" 'goto-last-change)
(define-key evil-normal-state-map "s," 'goto-last-change-reverse)

;; (define-key evil-normal-state-map "eh" (kbd "C-M-h"))
(define-key evil-normal-state-map "sf" 'evil-C-M-f)
(define-key evil-normal-state-map "sb" 'evil-C-M-b)
(define-key evil-normal-state-map "sd" 'evil-C-M-k)
(define-key evil-normal-state-map "sy" 'evil-copy-sexp-at-point) ;kill-sexp,undo
(define-key evil-normal-state-map "sk" (kbd "C-k"))
(define-key evil-normal-state-map "su" (kbd "C-u 0 C-k")) ;H-i =C-u 删除从光标位置到行首的内容
(evil-leader/set-key "h" 'evil-mark-whole-buffer)

;; (define-key evil-normal-state-map "so" 'helm-occur)




(define-key evil-normal-state-map (kbd "C-j") 'open-line-or-new-line-dep-pos)
;; (define-key evil-normal-state-map (kbd ".") 'repeat)
;; (define-key evil-normal-state-map (d "zx") 'repeat) ;
(define-key evil-normal-state-map "," 'repeat)
(define-key evil-visual-state-map "," 'repeat)
(define-key evil-motion-state-map "," 'repeat) ;

;; (define-key evil-ex-completion-map (kbd "H-m") 'exit-minibuffer)
(define-key evil-ex-completion-map (kbd "<C-m>") 'exit-minibuffer)

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

;; dib dab绑定
(define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
(define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block)



(define-key evil-normal-state-map "s;" 'vmacs-comment-dwim-line)




(evil-leader/set-key "o" 'other-window)
;; (evil-leader/set-key "G" 'helm-do-zgrep)
;; magit



(autoload 'dired-jump "dired-x" "dired-jump" t)
(evil-leader/set-key "j" 'dired-jump)

(evil-leader/set-key "l" 'ibuffer)

(evil-leader/set-key (kbd "C-g") 'keyboard-quit)
(evil-leader/set-key "zd" 'sdcv-to-buffer)

(evil-leader/set-key "s" 'evil-write-all)
(global-set-key (kbd "C-x C-s") 'evil-write-all)
(global-set-key (kbd "C-x s") 'evil-write-all)

;; (evil-leader/set-key "S" 'save-buffer)
;; (evil-leader/set-key "j" 'open-line-or-new-line-dep-pos)
(evil-leader/set-key "rt" 'string-rectangle)
(evil-leader/set-key "rk" 'kill-rectangle)
(evil-leader/set-key "ry" 'yank-rectangle)

(evil-leader/set-key "nw" 'widen)
(evil-leader/set-key "nn" 'narrow-to-region)

(evil-leader/set-key "xu" 'undo-tree-visualize)
(evil-leader/set-key "xv" 'switch-to-scratch-buffer)
(evil-leader/set-key "<RET>r" 'revert-buffer-with-coding-system) ;C-x<RET>r
(evil-leader/set-key "(" 'kmacro-start-macro) ;C-x(
(evil-leader/set-key ")" 'kmacro-end-macro) ;C-x
(evil-leader/set-key "ca" 'org-agenda)
(evil-leader/set-key "cc" 'toggle-case-fold)
(evil-leader/set-key "u" 'backward-up-list)
(evil-leader/set-key "t" 'org-agenda)
(evil-leader/set-key "/" 'undo)
(evil-leader/set-key "$" 'toggle-truncate-lines)
(evil-leader/set-key  "f;" 'ff-find-other-file) ;头文件与源文件间快速切换

;; (evil-leader/set-key "pr" 'publish-my-note-recent)
;; (evil-leader/set-key "pa" 'publish-my-note-all)
;; (evil-leader/set-key "pp" 'publish-my-note-local-preview)

;; (evil-leader/set-key "\\" 'just-one-space-or-delete-horizontal-space)
(define-key evil-normal-state-map "\\" 'just-one-space-or-delete-horizontal-space)


(evil-leader/set-key "y" 'evil-paste-before) ;default P

;; 默认visual选中即复制到剪切版，去掉这个功能
(fset 'evil-visual-update-x-selection 'ignore)


;; (fset 'yank 'evil-paste-before)

(defadvice evil-ex-search-next (after dotemacs activate)
  (recenter))

(defadvice evil-ex-search-previous (after dotemacs activate)
  (recenter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (fset 'evil-visual-update-x-selection 'ignore)





(provide 'conf-evil)

;; Local Variables:
;; coding: utf-8
;; End:
