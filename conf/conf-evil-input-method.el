;;; Code:
;; https://gitlab.com/liberime/liberime
;; https://emacs-china.org/t/pyim-rime/8277
;; https://github.com/tumashu/pyim/issues/190
;; RIME_PATH=~/repos/squirrel/librime/ make liberime

;; (setq-default pyim-dcache-directory "~/.emacs.d/cache/pyim")

;; (require 'pyim)
;; (define-key pyim-mode-map "\C-v" 'pyim-page-next-page)
;; (define-key pyim-mode-map "\M-v" 'pyim-page-previous-page)

;; (setq default-input-method "pyim")
;; (setq pyim-page-tooltip 'posframe)
;; ;; 选词框显示5个候选词
;; (setq pyim-page-length 5)
;; ;; (setq pyim-punctuation-translate-p 'auto)
;; (add-to-list 'pyim-punctuation-dict '("\\" "、"))


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

;; (when (require 'liberime nil nil)
;;   (setq pyim-default-scheme 'rime)
;;   (setq liberime-shared-data-dir "/Library/Input Methods/Squirrel.app/Contents/SharedSupport")
;;   (unless (file-exists-p "~/.emacs.d/cache/rime")
;;     (shell-command "make rime -C ~/.emacs.d"))
;;   (setq liberime-user-data-dir (expand-file-name "~/.emacs.d/cache/rime"))
;;   (liberime-start liberime-shared-data-dir liberime-user-data-dir)
;;   (liberime-select-schema "wubi_pinyin_jixiuf")
;;   )


;; (defun evil-pyim ()
;;   "when toggle on input method, switch to evil-insert-state if possible.
;; when toggle off input method, switch to evil-normal-state if current state is evil-insert-state"
;;   (interactive)
;;   (if (not current-input-method)
;;       (if (not (string= evil-state "insert"))
;;           (evil-insert-state))
;;     ;; (if (string= evil-state "insert")
;;     ;;     (evil-normal-state))
;;     )
;;   (if(derived-mode-p 'vterm-mode)
;;      (vmacs-evil-set-input-method)
;;     (set-input-method "pyim")
;;     (vmacs-evil-input-method-activate)
;;      )

;;   )

;; (add-hook 'input-method-activate-hook 'vmacs-evil-input-method-activate t)
;; (add-hook 'input-method-deactivate-hook 'vmacs-evil-input-method-deactive t)

;; (defun vmacs-evil-input-method-activate()
;;   (call-process  "open" nil nil nil "-g" "hammerspoon://input_method_switch?id=Squirrel" ))
;; (defun vmacs-evil-input-method-deactive()
;;   (call-process  "open" nil nil nil "-g" "hammerspoon://input_method_switch?id=U.S." ))

;; (global-set-key (kbd "C-\\") 'toggle-input-method)
;; (global-set-key (kbd "C-\\") 'toggle-input-method)
;; (global-set-key (kbd "<f16>") 'evil-pyim)


;; ;; 输入法进入normal state时，关闭输入法的中文模式
;; (add-hook 'evil-motion-state-entry-hook 'disable-input-method-hook)
(global-set-key (kbd "<f17>") 'evil-normal-state) ;mac karabiner用来控制输入法
(define-key isearch-mode-map (kbd "<f17>") 'evil-normal-state) ;详见isearch-pre-command-hook
(global-set-key (kbd "<f18>") 'evil-insert-state) ;mac karabiner用来控制输入法
(define-key isearch-mode-map (kbd "<f18>") 'evil-insert-state) ;详见isearch-pre-command-hook

;; 结合 hammerspoon 实现按下shift 切换输入法时，当切换到中文中自动进入insert state
;; https://github.com/jixiuf/dotfiles/blob/master/mac/hammerspoon/init.lua
;; 当切换输入法到中文状态时的时候会回调到这里
;;回调的时候有可能是上面disable-input-method-hook 里调，此时不应该切到insert模式
;; (defun vmacs-evil-normal-state()
;;   (interactive)
;;   (vmacs-evil-input-method-deactive)
;;   (evil-normal-state))

;; "Non-nil means random control characters terminate incremental search."
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (global-set-key (kbd "<f19>") nil) ;mac karabiner用来控制输入法
;; (define-key isearch-mode-map (kbd "<f19>") nil) ;详见isearch-pre-command-hook
(provide 'conf-evil-input-method)
;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-evil-input-method.el ends here.
