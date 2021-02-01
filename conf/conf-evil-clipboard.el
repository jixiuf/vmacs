;; 实现以vim操作clipboard的方式来操作clipboard

;; vim register 的用法
;; :reg 可以查看所有寄存器中的内容
;; 默认的寄存器为"即yy dd 的内容会被放到"寄存器中,即 ""yy 与yy等同，""p 与p 等同

;; 要操作指定的寄存器可以
;; "ayy 即把当前行yy到a的寄存器中
;; "ap 把寄存器a的内容paste 下当下位置
;;


;; 利用 “ 与0的区别的一个小技巧
;; yank 操作默认会把内容放到"与0中，
;; 而delete操作则只会放到"而不会放到0中
;; 即都会影响",区别是 是否影响0
;;
;; 而"1 "2 "3 .... "9 则分别存最近yank or delete 的内容，即类似于emacs 中的kill-ring
;; 而利用1~9 这几个寄存器与.重复命令，可以依次将其取出
;; "1p... 则会依次将 1 2 3 4 寄存器中的内容paste 到当前位置
;;
;;
;; 所以 先yank 一段内容A，后delete 一段内容B, p操作 会paste B 而"0p 而会paster A

;; 在windows 下 * 寄存器表示system clipboard,"*p 将把clipboard 的内容paste
;; 在X(Linux, possibly also OS-X),+表示 Ctrl-XCV clipboard
;; 在linux下 * 寄存器表示primary selection ，即linux下选中即复制到此的clipboard
;; "/  last search command
;; ":  last command.
;; ". 里面存最近insert 的内容
;; "_ 类似于/dev/null 是个无底洞
;; evil-use-register 默认绑定在" 双引号上,让;也具有evil-use-register的功能
;; (define-key evil-motion-state-map ";" 'evil-repeat-find-char-or-evil-use-register)
;; (define-key evil-normal-state-map ";" 'evil-repeat-find-char-or-evil-use-register)
;; (define-key evil-visual-state-map ";" 'evil-repeat-find-char-or-evil-use-register)
;; [;;yy]==["+yy] 将此行copy到clipboard
;; lazy-evil.el中对evil-use-register做了改变如果将;寄存器转化为+寄存器，
;; 即"+p == ";p  "+d == ";d (其中+寄存是操纵的是clipboard)
;;而evil-repeat-find-char-or-evil-use-register 绑定在;上,所以
;; 所以针对clipboard的操作，变成 ;;d  ;;p ;;y
;; 所以 如果想把内容copy到clipboard 则选中区域后按 ;;y

;; (defadvice evil-use-register(around easy-clipboard activate)

;; ;; http://wayback.archive.org/web/20150313145313/http://www.codejury.com/bypassing-the-clipboard-in-emacs-evil-mode/
;; (setq interprogram-paste-function nil)  ;当paste时是否从clipbloard等系统剪切板 等获取内容，设置成nil表示不获取，只使用kill-ring
;; ;; (setq interprogram-cut-function #'gui-select-text)    ;默认当emacs 更新kill-ring时，会同时更新clipboard的值，这里改成nil 即不更新clipboard
;; ;; gui-select-text 会同时考虑select-enable-clipboard 及select-enable-primary的值，故一般不需要将interprogram-cut-function设置成nil
;; (setq select-enable-clipboard   nil)    ;每一次往kill-ring 里加入东西时,是否同时往clipboard中放一份,
;; (setq select-enable-primary  nil) ;每一次往kill-ring 里加入东西时,是否也往primary 中放入
(setq
 kill-do-not-save-duplicates t       ;不向kill-ring中加入重复内容
 save-interprogram-paste-before-kill t  ;将系统剪切板的内容放一份到kill-ring中，
 mouse-yank-at-point t
 kill-whole-line t                     ;在行首 C-k 时，同时删除末尾换行符
 kill-read-only-ok t                  ;kill read-only buffer内容时,copy之而不用警告
 kill-ring-max 200                       ;emacs内置剪切板默认保留60份，default 60
 )

;;; 关于没有选中区域,则默认为选中整行的advice
;;;;默认情况下M-w复制一个区域，但是如果没有区域被选中，则复制当前行
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "已选中当前行!")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(unless (window-system)
  ;; "+p 从系统剪切板paste时会调到此处
  ;; 如果在mac 终端下使用emacs ,则使用pbpaste从clipboard 获取内容
  (defadvice gui-backend-get-selection (around get-clip-from-terminal-on-osx activate)
    ad-do-it
    (when (and (equal system-type 'darwin)
               (not (display-graphic-p))
               (not (window-system))
               (equal (ad-get-arg 0) 'CLIPBOARD))
      (let ((default-directory "~/"))
        (setq ad-return-value (shell-command-to-string "pbpaste")))))

  ;; "+yy 设置内容到系统clipboard
  ;; 如果在mac 终端下使用emacs ,则使用pbpaste从clipboard 获取内容
  (defadvice gui-backend-set-selection (around set-clip-from-terminal-on-osx activate)
    ad-do-it
    ;; (message "%s %s"  (ad-get-arg 0)  (ad-get-arg 1))
    (when (and (equal system-type 'darwin)
               (not (display-graphic-p))
               (not (window-system))
               (equal (ad-get-arg 0) 'CLIPBOARD))
      (let ((process-connection-type nil)   ; ; use pipe
            (default-directory "~/"))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc (ad-get-arg 1))
          (process-send-eof proc))))))


;; (defun evil-paste-from-clipboard ()
;;   "Paste text from system clipboard."
;;   (interactive)
;;   (if (not  (string= major-mode "term-mode"))
;;       (let ((text (evil-paste-from-register ?+)))
;;         (when (> (length text) 0))
;;         (backward-char)
;;         )
;;     (require 'term)
;;     (term-send-raw-string (evil-get-register ?+))))

;; ;; 这里参数跟evil-yank 完全相同， 只是函数里忽略了register 参数，而写死成?+,即使用系统clipboard
;; (evil-define-operator evil-yank-to-clipboard (beg end type register yank-handler)
;;   "Yank text to system clipboard."
;;   :move-point nil
;;   :repeat nil
;;   (interactive "<R><x><y>")
;;   (evil-yank beg end type ?+ yank-handler))

;; (evil-define-operator evil-delete-to-clipboard (beg end type register yank-handler)
;;   "Delete text from BEG to END with TYPE.
;; Save in REGISTER or in the kill-ring with YANK-HANDLER."
;;   (interactive "<R><x><y>")
;;   (evil-delete beg end type ?+ yank-handler))

;; ;; Y不常用故
;; (define-key evil-normal-state-map "Y" 'evil-yank-to-clipboard)
;; (define-key evil-motion-state-map "Y" 'evil-yank-to-clipboard)
;; ;; (vmacs-leader (kbd "y") 'evil-yank-to-clipboard)

;; (when (equal system-type 'darwin)
;;   (global-set-key (kbd "s-c") 'evil-yank-to-clipboard) ;等同于 "+y
;;   (global-set-key (kbd "s-x") 'evil-delete-to-clipboard) ;等同于 "+d
;;   (global-set-key (kbd "s-v") 'evil-paste-from-clipboard) ;等同于 "+p
;;   ;; (global-set-key  (kbd "s-c") 'kill-ring-save)
;;   ;; (global-set-key  (kbd "s-v") 'yank)
;;   ;; (global-set-key  (kbd "s-x") 'vmacs-kill-region-or-line)
;;   )

(provide 'conf-evil-clipboard)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-clipboard.el ends here.
