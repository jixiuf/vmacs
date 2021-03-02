;; https://github.com/mbriggs/.emacs.d/blob/master/my-keymaps.el

;; http://dnquark.com/blog/2012/02/emacs-evil-ecumenicalism/
;; https://github.com/cofi/dotfiles/blob/master/emacs.d/config/cofi-evil.el
;; https://github.com/syl20bnr/dotemacs/blob/master/init-package/init-evil.el
;;

;; 如果不想让某些命令jump ,则可以通过这种方式实现
;; You can disable it for %, f, F, t and T with the following:
;; (evil-set-command-property #'evil-jump-item :jump nil)

(setq-default
 evil-want-keybinding nil

 ;; evil-search-module 'isearch        ;可以用C-w yank word
 evil-undo-system 'undo-tree
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
 evil-want-fine-undo t                  ;undo更细化,否则从N->I->N 中所有的修改作为一个undo
 evil-symbol-word-search t              ;# search for symbol not word
 evil-flash-delay 0.5                   ;default 2
 evil-ex-search-case 'sensitive
 ;; C-e ,到行尾时,光标的位置是在最后一个字符后,还是在字符上
 evil-move-beyond-eol  t
 evil-move-cursor-back nil)


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
                ("COMMIT_EDITMSG" . insert)
                ("*Async Shell Command*" . normal)
                ("^ \\*load\\*")))

(defun vmacs-calc-hook()
  (require 'calc-bin)
  ;; 默认calc 的移位移位操作是接32位的， 可以bw(calc-word-size) 来改成64位
  (calc-word-size 128))

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
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local evil-textobj-anyblock-blocks
                        '(("(" . ")")
                          ("{" . "}")
                          ("\\[" . "\\]")
                          ("\"" . "\"")))))



(require 'evil)
(setq evil-collection-key-blacklist '("SPC"))
(setq evil-collection-company-use-tng nil)
(when (require 'evil-collection nil t) (evil-collection-init))

;; minor-mode
(add-hook 'org-capture-mode-hook 'evil-insert-state)
;; 设置一些mode的初始state
(evil-set-initial-state 'log-edit-mode 'insert)
(evil-set-initial-state 'org-agenda-mode 'normal)


;; evil-overriding-maps中的按键绑定 优先级高于evil-mode
;; (add-to-list 'evil-overriding-maps '(grep-mode-map . nil))
(add-to-list 'evil-overriding-maps '(org-agenda-mode-map . nil))
(evil-set-custom-state-maps 'evil-overriding-maps
                            'evil-pending-overriding-maps
                            'override-state
                            'evil-make-overriding-map
                            evil-overriding-maps)

;; evil-normalize-keymaps forces an update of all Evil keymaps
;; (add-hook 'magit-blob-mode-hook #'evil-normalize-keymaps)
;; 更新 evil-overriding-maps ,因为org-agenda-mode-map 变量初始为空keymap,在org-agenda-mode内才往里添加绑定
(add-hook 'org-agenda-mode-hook #'evil-normalize-keymaps)

(evil-collection-define-key 'normal 'org-agenda-mode-map
      "j" 'evil-next-line
    "k" 'evil-previous-line
    ":" 'evil-ex
    "r" 'org-agenda-redo)


;; (setq display-line-numbers-current-absolute t)
(defun vmacs-change-line-number-abs()
  (if (member major-mode '(term-mode eshell-mode ansi-term-mode  magit-status-mode ))
      (setq display-line-numbers nil)
    (setq display-line-numbers 'absolute)))

(defun vmacs-change-line-number-relative()
  (if (member major-mode '(vterm-mode term-mode eshell-mode ansi-term-mode  magit-status-mode))
      (if (member major-mode '(vterm-mode))
          (setq display-line-numbers 'absolute)
          (setq display-line-numbers nil)
          )
    (setq display-line-numbers 'visual)))


(add-hook 'evil-insert-state-entry-hook 'vmacs-change-line-number-abs)
(add-hook 'evil-normal-state-entry-hook 'vmacs-change-line-number-relative)
(add-hook 'evil-motion-state-entry-hook 'vmacs-change-line-number-relative)

(evil-mode 1)

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



;; (define-key evil-ex-completion-map (kbd "M-p") 'previous-history-element) ;
;; (define-key evil-ex-completion-map (kbd "M-n")  'next-history-element)
;; (define-key evil-normal-state-map "\C-n" nil)
;; (define-key evil-normal-state-map "\C-p" nil)

;; (evil-define-motion vmacs-evil-next-line (count)
;;   "Move the cursor COUNT lines down."
;;   :type line
;;   (let ((line-move-visual (not truncate-lines)))
;;     (evil-line-move (or count 1))))

;; (evil-define-motion vmacs-evil-previous-line (count)
;;   "Move the cursor COUNT lines up."
;;   :type line
;;   (let ((line-move-visual (not truncate-lines)))
;;     (evil-line-move (- (or count 1)))))


;; (fset 'evil-next-line 'vmacs-evil-next-line)
;; (fset 'evil-previous-line 'vmacs-evil-previous-line)

;; ;; (define-key evil-motion-state-map (kbd "C-w") nil)
;; (define-key evil-motion-state-map (kbd "C-i") nil)
(define-key evil-motion-state-map (kbd "C-b") nil)
(define-key evil-motion-state-map (kbd "C-d") nil)
(define-key evil-motion-state-map (kbd "C-f") nil)
(define-key evil-motion-state-map  (kbd "RET") nil) ;
;; (define-key evil-motion-state-map (kbd "C-e") nil)
;; (define-key evil-normal-state-map  (kbd "RET") nil) ;
;; (define-key evil-normal-state-map  (kbd "M-.") nil)

(define-key evil-motion-state-map (kbd "C-y") nil)
(define-key evil-normal-state-map [remap yank-pop] nil)
(define-key evil-normal-state-map  (kbd "C-.") nil)
(define-key evil-normal-state-map (kbd "DEL") nil) ;backupspace
(define-key evil-normal-state-map "m" nil)
(define-key evil-normal-state-map "mq" 'fill-paragraph)
(define-key evil-normal-state-map "mm" 'evil-set-marker) ;`


(define-key evil-normal-state-map (kbd "C-3") #'evil-search-word-backward) ;C-3
(define-key evil-normal-state-map (kbd "C-4") #'evil-search-word-forward) ;C-8
(define-key evil-normal-state-map (kbd "C-8") #'evil-search-word-forward) ;C-8

(define-key evil-normal-state-map "ga" (kbd "M-a"))
(define-key evil-normal-state-map "ge" (kbd "M-e"))
(define-key evil-normal-state-map "s" nil)
(define-key evil-normal-state-map "sa" 'evil-begin-of-defun)
(define-key evil-normal-state-map "sw" 'evil-begin-of-defun)

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
(define-key evil-normal-state-map (kbd "C-j") 'open-line-or-new-line-dep-pos)
(define-key evil-normal-state-map "," 'repeat)
(define-key evil-visual-state-map "," 'repeat)
(define-key evil-motion-state-map "," 'repeat) ;

(define-key evil-ex-completion-map (kbd "<C-m>") 'exit-minibuffer)

;; dib dab绑定
(define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
(define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block)
(vmacs-leader (kbd "o") 'other-window)

(autoload 'dired-jump "dired" "" t)
(vmacs-leader (kbd "j") 'dired-jump)
(global-set-key  (kbd "s-j") 'dired-jump)


(vmacs-leader (kbd "l") 'ibuffer)

(evil-define-key 'normal 'global  (kbd "<SPC>C-g") 'keyboard-quit)
(vmacs-leader (kbd "zd") 'sdcv-to-buffer)

(vmacs-leader (kbd "s") 'evil-write-all)

;; (vmacs-leader (kbd "S") 'save-buffer)
;; (vmacs-leader (kbd "j") 'open-line-or-new-line-dep-pos)
(vmacs-leader (kbd "rt") 'string-rectangle)
(vmacs-leader (kbd "rk") 'kill-rectangle)
(vmacs-leader (kbd "ry") 'yank-rectangle)

(vmacs-leader (kbd "nw") 'widen)
(vmacs-leader (kbd "nn") 'narrow-to-region)

(vmacs-leader (kbd "xu") 'undo-tree-visualize)
(vmacs-leader (kbd "xv") 'switch-to-scratch-buffer)
(vmacs-leader (kbd "<RET>r") 'revert-buffer-with-coding-system) ;C-x<RET>r
(vmacs-leader (kbd "(") 'kmacro-start-macro) ;C-x(
(vmacs-leader (kbd ")") 'kmacro-end-macro) ;C-x
(vmacs-leader (kbd "ca") 'org-agenda)
(vmacs-leader (kbd "cc") 'toggle-case-fold)
(vmacs-leader (kbd "u") 'backward-up-list)
(vmacs-leader (kbd "t") 'org-agenda)
(vmacs-leader (kbd "$") 'toggle-truncate-lines)
(vmacs-leader (kbd "m") 'execute-extended-command)
(vmacs-leader (kbd "wl") 'git-link)


(define-key evil-normal-state-map "\\" 'just-one-space-or-delete-horizontal-space)


(global-set-key (kbd "C-s") 'evil-search-forward)
(global-set-key (kbd "C-r") 'evil-search-backward)
(vmacs-leader (kbd "y") 'evil-paste-before) ;default P

;; 默认visual选中即复制到剪切版，去掉这个功能
(fset 'evil-visual-update-x-selection 'ignore)


(defadvice evil-ex-search (after dotemacs activate)
  ;; (recenter)
  (unless evil-ex-search-persistent-highlight
    (sit-for 0.1)
    (evil-ex-delete-hl 'evil-ex-search)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (fset 'evil-visual-update-x-selection 'ignore)

(defadvice evil-write-all (after save-smart activate)
  (when (string-match-p "*Org Src" (buffer-name))
    (require 'org-src)
    (call-interactively 'org-edit-src-exit)))


;; (global-set-key (kbd "C-c C-c") 'vmacs-smart-double-ctrl-c)
(define-key evil-normal-state-map (kbd "M-o") 'toggle-camelize)
(define-key evil-motion-state-map (kbd "C-o") nil)

(evil-declare-motion 'golden-ratio-scroll-screen-down)
(evil-declare-motion 'golden-ratio-scroll-screen-up)

;; this is need for vterm
(autoload 'golden-ratio-scroll-screen-up "golden-ratio-scroll-screen" "" t)
(autoload 'golden-ratio-scroll-screen-down "golden-ratio-scroll-screen" "" t)
(define-key evil-motion-state-map (kbd "C-v") 'golden-ratio-scroll-screen-up)
(define-key evil-motion-state-map (kbd "M-v") 'golden-ratio-scroll-screen-down)
(define-key evil-motion-state-map (kbd "C-l") 'recenter-top-bottom)
(define-key evil-motion-state-map (kbd "C-k") 'vmacs-kill-region-or-line)


(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up) ;C-v
;; (global-set-key "\C-u" 'gold-ratio-scroll-screen-up)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down) ;M-v

;; 与选中区域有关的配置
;; 因为v 用于向后移动一个symbol的距离
(define-key evil-motion-state-map "sv" 'evil-visual-char) ;==v开始选中区域
(define-key evil-motion-state-map "sm" 'evil-visual-line) ;==V 开始行选中
;; 因为C-v用于滚屏，故mv==原vim的C-v
(define-key evil-normal-state-map "mv" 'evil-visual-block) ;==vim.C-v 开始矩形操作，然后移动位置，就可得到选区

(define-key evil-visual-state-map "n" 'rectangle-number-lines) ;C-xrN

(global-set-key  (kbd "C-2") 'set-mark-command)

;; 选中区域后 交换当前光标点，
(define-key evil-visual-state-map "x" 'exchange-point-and-mark)
(define-key evil-visual-state-map "X" 'evil-visual-exchange-corners)
;; 有一种需要是
;; 当我取消选中后 我希望光标停留在选中前光标所在的位置而不是在选区的开头或结尾处
(define-key evil-normal-state-map "mf" 'evil-mark-defun) ;mark-defun 相当于C-M-h
(define-key evil-normal-state-map "mh" 'evil-M-h)        ;相当于M-h
(define-key evil-normal-state-map "mxh" 'evil-mark-whole-buffer) ;相当于C-xh
(define-key evil-normal-state-map "mb" 'evil-mark-whole-buffer);相当于C-xh

;; http://vimcdoc.sourceforge.net/doc/motion.html
;; vim 里有
;; |w|向前一个word|
;; |b|向后一个word|
;; |W|向前一个WORD|
;; |B|向后一个WORD|
;;|dw|删除光标后的一个word|
;;|daw|删除光标下的一个word,包括空格|  delete a word
;;|diw|删除光标下的一个word,不包括空格| delete inner word

;; WORD是中间没有空格的一串字符
;; 与emacs中的symbol类似但是不同

;; 下面实现向前向后移动一个symbol,
;; |e|向前移动一个symbol,光标停在下个symbol的开始处|
;; |r|向前移动一个symbol,光标停在下个symbol的结束处|

;; |v|向后移动一个symbol,光标停在下个symbol的开始处|
;; |R|向后移动一个symbol,光标停在下个symbol的结束处|

;; |de|删除一个symbol,不包含空格，==die|
;; |dae|删除一个symbol,含空格|
;; |die|删除一个symbol,不含空格|
;;
;;
;; 这里占用了vim 原有的绑定，包括 e r R v
;; 其中原来的e我觉得用下不大
;; emacs更倾向于在word 或symbol的开头后进行操作，
;; 所以基本上移动到word或symbol的开后进行操作就足够了
;; 而我很少用vim的 r R进行替换操作,所以这两个键被占用了对我没有太大的影响

;; 而影响较大的是v键被占用了，v的功能是开始选中一片区域
;; 之所以占用这个功能是，是我觉得向后移动到symbol的操作是个很常用的操作
;; 我如果持续向后移动，只需要一直按住v就可以了
;; 而所有的选中区域的功能我绑定到了别的键上即sv,
;; 我把s键扩展成了一系列功能键


;; e ,r 移动
(define-key evil-normal-state-map "e" 'evil-forward-symbol-begin)
(define-key evil-normal-state-map "r" 'evil-forward-symbol-end)
;; (define-key evil-normal-state-map "E" 'evil-forward-symbol-end)
(define-key evil-normal-state-map "v" 'evil-backward-symbol-begin)
;; (define-key evil-normal-state-map ";" 'evil-repeat-find-char-or-evil-backward-symbol-begin)
;; (define-key evil-normal-state-map "R" 'evil-backward-symbol-end)

(define-key evil-visual-state-map "e" 'evil-forward-symbol-begin)
(define-key evil-visual-state-map "r" 'evil-forward-symbol-end)
;; (define-key evil-visual-state-map "E" 'evil-forward-symbol-end)
(define-key evil-visual-state-map "v" 'evil-backward-symbol-begin)
;; (define-key evil-visual-state-map "R" 'evil-backward-symbol-end)


;; de dr
(define-key evil-motion-state-map "e" 'evil-forward-symbol-end)
(define-key evil-motion-state-map "r" 'evil-backward-symbol-begin)
;; dae die
(define-key evil-outer-text-objects-map "e" 'evil-a-symbol)
(define-key evil-inner-text-objects-map "e" 'evil-inner-symbol)

(global-set-key (kbd "<f17>") 'evil-normal-state) ;mac karabiner用来控制输入法
(define-key isearch-mode-map (kbd "<f17>") 'evil-normal-state) ;详见isearch-pre-command-hook
(global-set-key (kbd "<f18>") 'evil-insert-state) ;mac karabiner用来控制输入法
(define-key isearch-mode-map (kbd "<f18>") 'evil-insert-state) ;详见isearch-pre-command-hook
(global-set-key (kbd "<f16>") 'vmacs-toggle-input-method)
(global-set-key (kbd "<f19>") nil) ;mac karabiner用来控制输入法 ,rime f19 send escape
(define-key isearch-mode-map (kbd "<f19>") nil) ;详见isearch-pre-command-hook
(defun vmacs-toggle-input-method ()
  "when toggle on input method, switch to evil-insert-state if possible.
when toggle off input method, switch to evil-normal-state if current state is evil-insert-state"
  (interactive)
  (if (not current-input-method)
      (if (not (string= evil-state "insert"))
          (evil-insert-state)
        (call-interactively #'toggle-input-method)
        )
    (call-interactively #'toggle-input-method)
    )
  )

(provide 'conf-evil)

;; Local Variables:
;; coding: utf-8
;; End:
