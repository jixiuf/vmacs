;;; Code:
;; https://github.com/DogLooksGood/emacs-rime/blob/master/README_CN.org
;; https://gitlab.com/liberime/liberime
;; RIME_PATH=~/repos/squirrel/librime/ make liberime
;; (add-to-list 'load-path "~/.emacs.d/submodule/emacs-rime/")
;; (setq rime-user-data-dir "~/.emacs.d/cache/rime")
;; (setq default-input-method "rime")
;; (setq rime-show-candidate 'posframe)
;; (setq rime-disable-predicates           ;临时英文模式
;;       '(rime-predicate-evil-mode-p
;;         rime-predicate-prog-in-code-p   ;在 prog-mode 和 conf-mode 中除了注释和引号内字符串之外的区域
;;         ;; rime-predicate-in-code-string-p ;在代码的字符串中，不含注释的字符串。
;;         rime-predicate-space-after-cc-p ;在中文字符且有空格之后
;;         ;; rime-predicate-space-after-ascii-p ;在任意英文字符且有空格之后
;;         rime-predicate-after-alphabet-char-p
;;         rime-predicate-after-ascii-char-p
;;         ;; rime-predicate-auto-english-p
;;         ;; rime-predicate-current-uppercase-letter-p
;;         ;; rime-predicate-punctuation-after-space-cc-p
;;         ))
;; (setq rime-inline-predicates '(rime-predicate-space-after-cc-p))
;; (require 'rime)
;; (add-to-list 'rime-translate-keybindings "C-v")
;; (add-to-list 'rime-translate-keybindings  "M-v")

;; (add-hook 'input-method-activate-hook 'vmacs-evil-input-method-activate t)
;; (add-hook 'input-method-deactivate-hook 'vmacs-evil-input-method-deactive t)
;; (defun vmacs-evil-input-method-activate()
;;   (call-process  "open" nil nil nil "-g" "hammerspoon://input_method_switch?id=Squirrel" ))
;; (defun vmacs-evil-input-method-deactive()
;;   (call-process  "open" nil nil nil "-g" "hammerspoon://input_method_switch?id=U.S." ))

;; (global-set-key (kbd "C-\\") 'toggle-input-method)
;; (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)

;; ;; 输入法进入normal state时，关闭输入法的中文模式
;; (add-hook 'evil-motion-state-entry-hook 'disable-input-method-hook)
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
(provide 'conf-evil-input-method)
;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-evil-input-method.el ends here.
