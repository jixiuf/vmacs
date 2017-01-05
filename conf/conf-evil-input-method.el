;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这段代码实现 squirrel中文输入法的切换与evil-mode 的协调
;; 主要实现两个功能
;; 1 shift切换到中文输入法时，自动进入evil-insert-state
;; 2当emacs进入evil-normal-state ,使squirrel输入法进入输入英文状态
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://rime.im 官方版
;; 官方版的不支持从命令行修改输入法的中英文状态
;; 下面这个改版支持
;; https://github.com/jixiuf/squirrel/releases
;; 但是安装的过程需要选安装官方版，然后下载改版，将下载的解压后替换掉/Library/Input Methods/Squirrel.app
;; 下载这个文件，放到 ~/Libary/
;; 下面这个链接 如何实现只保留一个squirrel输入法
;; https://github.com/rime/squirrel/wiki/如何实现mac下删除默认的英文输入法（只保留一个Squirrel)
;; https://raw.githubusercontent.com/jixiuf/dotfiles/master/mac/com.apple.HIToolbox.plist

(defun disable-input-method-hook()
  ;; (start-process "squirrel-input-method-disable-chinese" nil "/Library/Input Methods/Squirrel.app/Contents/MacOS/squirrel_client" "-s" "ascii_mode" "--clear")
  (when (and (equal system-type 'darwin)(file-exists-p "/Library/Input Methods/Squirrel.app/Contents/MacOS/squirrel_client" )) ;mac 上squirrel与输入法相关
    (call-process  "/Library/Input Methods/Squirrel.app/Contents/MacOS/squirrel_client" nil 0 nil "-s" "ascii_mode" )))

;; 输入法进入normal state时，关闭输入法的中文模式
(add-hook 'evil-normal-state-entry-hook 'disable-input-method-hook)
(global-set-key (kbd "<f17>") 'my-evil-normal) ;mac karabiner用来控制输入法
(define-key isearch-mode-map (kbd "<f17>") 'my-evil-normal) ;详见isearch-pre-command-hook

;; 当切换输入法到中文状态时的时候会回调到这里
;;回调的时候有可能是上面disable-input-method-hook 里调，此时不应该切到insert模式
(defun my-evil-insert()
  (interactive)
  (unless (equal 'evil-normal-state last-command)
    (evil-insert-state)))
(global-set-key (kbd "<f18>") 'my-evil-insert) ;mac karabiner用来控制输入法
(define-key isearch-mode-map (kbd "<f18>") 'my-evil-insert) ;详见isearch-pre-command-hook
;; "Non-nil means random control characters terminate incremental search."
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'conf-evil-input-method)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-evil-input-method.el ends here.
