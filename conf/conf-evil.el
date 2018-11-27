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
 evil-motion-state-cursor '("red" box))



(require 'evil)
;; minor-mode
;; 设置一些mode的初始state
(add-hook 'org-capture-mode-hook 'evil-insert-state)
(setq-default evil-buffer-regexps
              '(("**testing snippet:" . insert)
                ("*compile*" . normal)
                ;; ("*Org Src" . insert)
                ("*Org Export Dispatcher*" . insert)
                ("*Async Shell Command*" . normal)
                ("^ \\*load\\*")))
;; 设置一些mode的初始state
(evil-set-initial-state 'bm-show-mode 'insert)
(evil-set-initial-state 'diff-mode 'normal)
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
(evil-set-initial-state 'ivy-occur-mode 'normal)
(evil-set-initial-state 'ivy-occur-grep-mode 'normal)
(evil-set-initial-state 'grep-mode 'normal)
(evil-set-initial-state 'Info-mode 'motion)
(evil-set-initial-state 'calc-mode 'normal)
;; (evil-set-initial-state 'term-mode 'normal)
;; (evil-set-initial-state 'eshell-mode 'normal)
(defun vmacs-calc-hook()
  (require 'calc-bin)
  ;; 默认calc 的移位移位操作是接32位的， 可以bw(calc-word-size) 来改成64位
  (calc-word-size 64)
  (define-key calc-mode-map (kbd "j") 'evil-next-line)
  (define-key calc-mode-map (kbd "k") 'evil-previous-line)
  (define-key calc-mode-map (kbd "SPC") nil)
  (define-key calc-mode-map (kbd "y") 'evil-yank))

(add-hook 'calc-mode-hook 'vmacs-calc-hook)


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
(add-to-list 'evil-overriding-maps '(vmacs-leader-map . nil))
(add-to-list 'evil-overriding-maps '(custom-mode-map . nil))
(add-to-list 'evil-overriding-maps '(ediff-mode-map . nil))
(add-to-list 'evil-overriding-maps '(package-menu-mode-map . nil))
(add-to-list 'evil-overriding-maps '(minibuffer-local-map . nil))
(add-to-list 'evil-overriding-maps '(minibuffer-local-completion-map . nil))
(add-to-list 'evil-overriding-maps '(minibuffer-local-must-match-map . nil))
(add-to-list 'evil-overriding-maps '(minibuffer-local-isearch-map . nil))
(add-to-list 'evil-overriding-maps '(minibuffer-local-ns-map . nil))
(add-to-list 'evil-overriding-maps '(epa-key-list-mode-map . nil))
(add-to-list 'evil-overriding-maps '(term-mode-map . nil))
(add-to-list 'evil-overriding-maps '(term-raw-map . nil))
(add-to-list 'evil-overriding-maps '(calc-mode-map . nil))
(add-to-list 'evil-overriding-maps '(git-timemachine-mode-map . nil))
(add-to-list 'evil-overriding-maps '(magit-popup-mode-map . nil))
(add-to-list 'evil-overriding-maps '(xref--xref-buffer-mode-map . nil))




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

;; (evil-update-pending-maps)

;; (require 'evil-leader)
;; (global-evil-leader-mode)
;; (evil-leader/set-leader "<SPC>")
;; (setq-default evil-magit-state 'normal)

;; (setq display-line-numbers-current-absolute t)
(defun vmacs-change-line-number-abs()
  (if (member major-mode '(vterm-mode term-mode eshell-mode ansi-term-mode vterm-mode))
      (setq display-line-numbers nil)
    (setq display-line-numbers 'absolute)))

(defun vmacs-change-line-number-relative()
  (if (member major-mode '(vterm-mode term-mode eshell-mode ansi-term-mode vterm-mode))
      (setq display-line-numbers nil)
    (setq display-line-numbers 'visual)))


(add-hook 'evil-insert-state-entry-hook 'vmacs-change-line-number-abs)
(add-hook 'evil-normal-state-entry-hook 'vmacs-change-line-number-relative)
(add-hook 'evil-motion-state-entry-hook 'vmacs-change-line-number-relative)

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

(define-key evil-ex-completion-map (kbd "M-p") 'previous-history-element) ;
(define-key evil-ex-completion-map (kbd "M-n")  'next-history-element)

(define-key evil-insert-state-map [escape] 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)


;; (define-key evil-normal-state-map "/" 'helm-swoop)

;; (define-key evil-motion-state-map "/" 'evil-search-forward)
;; (define-key evil-normal-state-map "/" 'evil-search-forward)



;; (define-key evil-window-map "1" 'delete-other-windows)
;; (define-key evil-window-map "0" 'delete-window)
;; (define-key evil-window-map "2" 'vmacs-split-window-vertically)
;; (define-key evil-window-map "3" 'vmacs-split-window-horizontally)

;; (define-key evil-normal-state-map (kbd "f") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "C-z") nil)
;; (define-key evil-normal-state-map (kbd "C-w") 'ctl-w-map)
;; (define-key evil-normal-state-map "\C-n" nil)
;; (define-key evil-normal-state-map "\C-p" nil)
(define-key evil-normal-state-map "\C-v" nil)
(define-key evil-motion-state-map "\C-v" nil)
(define-key evil-normal-state-map "\C-e" nil)

(evil-define-motion vmacs-evil-next-line (count)
  "Move the cursor COUNT lines down."
  :type line
  (let ((line-move-visual (not truncate-lines)))
    (evil-line-move (or count 1))))

(evil-define-motion vmacs-evil-previous-line (count)
  "Move the cursor COUNT lines up."
  :type line
  (let ((line-move-visual (not truncate-lines)))
    (evil-line-move (- (or count 1)))))


(fset 'evil-next-line 'vmacs-evil-next-line)
(fset 'evil-previous-line 'vmacs-evil-previous-line)
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
(define-key evil-normal-state-map "q" nil)
(define-key evil-normal-state-map (kbd "DEL") nil) ;backupspace
(define-key evil-motion-state-map  (kbd "RET") nil) ;
(define-key evil-normal-state-map  (kbd "RET") nil) ;
;; (define-key evil-motion-state-map "n" nil)
;; (define-key evil-motion-state-map "N" nil)
;; (define-key evil-normal-state-map "\C-r" nil)
;; (global-unset-key (kbd "C-s"))
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
;; (define-key evil-normal-state-map (kbd "C-[ [ 1 i") 'evil-record-macro) ;C-3 default q
;; (define-key evil-normal-state-map (kbd "C-3") 'evil-record-macro) ;C-3 default q
(define-key evil-normal-state-map (kbd "C-3") #'evil-search-word-backward) ;C-3
(define-key evil-normal-state-map (kbd "C-4") #'evil-search-word-forward) ;C-8
(define-key evil-normal-state-map (kbd "C-8") #'evil-search-word-forward) ;C-8

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
(define-key evil-normal-state-map "sw" 'evil-begin-of-defun)

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
;; (vmacs-leader "h" 'evil-mark-whole-buffer)

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



;; (define-key evil-normal-state-map "s;" 'vmacs-comment-dwim-line)




(vmacs-leader "o" 'other-window)
;; (vmacs-leader "G" 'helm-do-zgrep)
;; magit



(autoload 'dired-jump "dired-x" "dired-jump" t)
(vmacs-leader "j" 'dired-jump)
(global-set-key  (kbd "s-j") 'dired-jump)


(vmacs-leader "l" 'ibuffer)

(vmacs-leader (kbd "C-g") 'keyboard-quit)
(vmacs-leader "zd" 'sdcv-to-buffer)

(vmacs-leader "s" 'evil-write-all)
(global-set-key (kbd "C-x C-s") 'evil-write-all)
(global-set-key (kbd "C-x s") 'evil-write-all)

;; (vmacs-leader "S" 'save-buffer)
;; (vmacs-leader "j" 'open-line-or-new-line-dep-pos)
(vmacs-leader "rt" 'string-rectangle)
(vmacs-leader "rk" 'kill-rectangle)
(vmacs-leader "ry" 'yank-rectangle)

(vmacs-leader "nw" 'widen)
(vmacs-leader "nn" 'narrow-to-region)

(vmacs-leader "xu" 'undo-tree-visualize)
(vmacs-leader "xv" 'switch-to-scratch-buffer)
(vmacs-leader "<RET>r" 'revert-buffer-with-coding-system) ;C-x<RET>r
(vmacs-leader "(" 'kmacro-start-macro) ;C-x(
(vmacs-leader ")" 'kmacro-end-macro) ;C-x
(vmacs-leader "ca" 'org-agenda)
(vmacs-leader "cc" 'toggle-case-fold)
(vmacs-leader "u" 'backward-up-list)
(vmacs-leader "t" 'org-agenda)
(vmacs-leader "/" 'undo)
(vmacs-leader "$" 'toggle-truncate-lines)
(vmacs-leader  "f;" 'ff-find-other-file) ;头文件与源文件间快速切换

;; (vmacs-leader "pr" 'publish-my-note-recent)
;; (vmacs-leader "pa" 'publish-my-note-all)
;; (vmacs-leader "pp" 'publish-my-note-local-preview)

;; (vmacs-leader "\\" 'just-one-space-or-delete-horizontal-space)
(define-key evil-normal-state-map "\\" 'just-one-space-or-delete-horizontal-space)


(global-set-key (kbd "C-s") 'evil-search-forward)
(global-set-key (kbd "C-r") 'evil-search-backward)
(vmacs-leader "y" 'evil-paste-before) ;default P

;; 默认visual选中即复制到剪切版，去掉这个功能
(fset 'evil-visual-update-x-selection 'ignore)


;; (fset 'yank 'evil-paste-before)

(defadvice evil-ex-search-next (after dotemacs activate)
  (recenter))

(defadvice evil-ex-search-previous (after dotemacs activate)
  (recenter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (fset 'evil-visual-update-x-selection 'ignore)

(defadvice evil-write-all (after save-smart activate)
  (when (string-match-p "*Org Src" (buffer-name))
    (require 'org-src)
    (call-interactively 'org-edit-src-exit)))


(global-set-key (kbd "C-c C-c") 'vmacs-smart-double-ctrl-c)
(define-key evil-normal-state-map (kbd "C-o") 'toggle-camelize)
(define-key evil-motion-state-map (kbd "C-o") 'toggle-camelize)

(evil-declare-motion 'golden-ratio-scroll-screen-down)
(evil-declare-motion 'golden-ratio-scroll-screen-up)

(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up) ;C-v
;; (global-set-key "\C-u" 'gold-ratio-scroll-screen-up)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down) ;M-v


(provide 'conf-evil)

;; Local Variables:
;; coding: utf-8
;; End:
