;;; Code:
;; https://gitlab.com/liberime/liberime
;; https://emacs-china.org/t/pyim-rime/8277
;; https://github.com/tumashu/pyim/issues/190

(setq-default pyim-dcache-directory "~/.emacs.d/cache/pyim")

(require 'pyim)

(setq default-input-method "pyim")
(setq pyim-page-tooltip 'posframe)
;; 选词框显示5个候选词
(setq pyim-page-length 5)
;; (setq pyim-page-style 'one-line)

;; (liberime-get-schema-list)
;; (liberime-search "wq")
;; (liberime-search "nihao")

;; https://github.com/tumashu/pyim
;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;; 我自己使用的中英文动态切换规则是：
;; 1. 光标只有在注释里面时，才可以输入中文。
;; 2. 光标前是汉字字符时，才能输入中文。
;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
;; (setq-default pyim-english-input-switch-functions;
;;               '(pyim-probe-dynamic-english
;;                 ;; pyim-probe-isearch-mode ;使用 isearch 搜索时，强制开启英文输入模式
                ;; pyim-probe-program-mode
;;                 pyim-probe-org-structure-template))
;;
;; (setq-default pyim-punctuation-half-width-functions
;;               '(pyim-probe-punctuation-line-beginning
;;                 pyim-probe-punctuation-after-punctuation))

(when (require 'liberime nil nil)
  (setq pyim-default-scheme 'rime)
  (setq liberime-shared-data-dir "/Library/Input Methods/Squirrel.app/Contents/SharedSupport")
  (unless (file-exists-p "~/.emacs.d/cache/rime")
    (shell-command "make rime -C ~/.emacs.d"))
  (setq liberime-user-data-dir (expand-file-name "~/.emacs.d/cache/rime"))
  (liberime-start liberime-shared-data-dir liberime-user-data-dir)
  (liberime-select-schema "wubi_pinyin_jixiuf")
  )


(defun evil-toggle-input-method ()
  "when toggle on input method, switch to evil-insert-state if possible.
when toggle off input method, switch to evil-normal-state if current state is evil-insert-state"
  (interactive)
  (if (not current-input-method)
      (if (not (string= evil-state "insert"))
          (evil-insert-state))
    ;; (if (string= evil-state "insert")
    ;;     (evil-normal-state))
    )
  (toggle-input-method))

(add-hook 'input-method-activate-hook 'vmacs-evil-input-method-activate t)
(add-hook 'input-method-deactivate-hook 'vmacs-evil-input-method-deactive t)

(defun vmacs-evil-input-method-activate()
  (call-process  "open" nil nil nil "-g" "hammerspoon://input_method_switch?id=Squirrel" ))
(defun vmacs-evil-input-method-deactive()
  (call-process  "open" nil nil nil "-g" "hammerspoon://input_method_switch?id=U.S." ))

;; (global-set-key (kbd "C-\\") 'toggle-input-method)
(global-set-key (kbd "C-\\") 'evil-toggle-input-method)
(global-set-key (kbd "<f16>") 'evil-toggle-input-method)

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

;; (defun disable-input-method-hook()
;;   ;; (start-process "squirrel-input-method-disable-chinese" nil "/Library/Input Methods/Squirrel.app/Contents/MacOS/squirrel_client" "-s" "ascii_mode" "--clear")
;;   (when (and (equal system-type 'darwin)
;;              (member evil-previous-state '(insert emacs))
;;              (file-exists-p "/Library/Input Methods/Squirrel.app/Contents/MacOS/squirrel_client" )) ;mac 上squirrel与输入法相关
;;     (call-process  "/Library/Input Methods/Squirrel.app/Contents/MacOS/squirrel_client" nil nil nil "-s" "ascii_mode" )))

;; ;; (defadvice keyboard-quit (before disable-input-method activate)
;; ;;   "disable-input-method-hook."
;; ;;   (disable-input-method-hook))

;; ;; 输入法进入normal state时，关闭输入法的中文模式
;; (add-hook 'evil-normal-state-entry-hook 'disable-input-method-hook)
;; (add-hook 'evil-motion-state-entry-hook 'disable-input-method-hook)
(global-set-key (kbd "<f17>") 'evil-normal-state) ;mac karabiner用来控制输入法
(define-key isearch-mode-map (kbd "<f17>") 'evil-normal-state) ;详见isearch-pre-command-hook

;; 结合 hammerspoon 实现按下shift 切换输入法时，当切换到中文中自动进入insert state
;; https://github.com/jixiuf/dotfiles/blob/master/mac/hammerspoon/init.lua
;; 当切换输入法到中文状态时的时候会回调到这里
;;回调的时候有可能是上面disable-input-method-hook 里调，此时不应该切到insert模式
;; (defun my-evil-insert()
;;   (interactive)
;;   (message "hello")
;;   (unless (equal 'evil-normal-state last-command)
;;     (evil-insert-state)))
(global-set-key (kbd "<f18>") 'evil-insert-state) ;mac karabiner用来控制输入法
(define-key isearch-mode-map (kbd "<f18>") 'evil-insert-state) ;详见isearch-pre-command-hook
;; "Non-nil means random control characters terminate incremental search."
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<f19>") nil) ;mac karabiner用来控制输入法
(define-key isearch-mode-map (kbd "<f19>") nil) ;详见isearch-pre-command-hook
(provide 'conf-evil-input-method)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-evil-input-method.el ends here.
