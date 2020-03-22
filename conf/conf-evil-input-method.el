;;; Code:
;; https://github.com/DogLooksGood/emacs-rime/blob/master/README_CN.org
;; https://gitlab.com/liberime/liberime
;; RIME_PATH=~/repos/squirrel/librime/ make liberime
(add-to-list 'load-path "~/.emacs.d/submodule/emacs-rime/")
(setq rime-user-data-dir "~/.emacs.d/cache/rime")
(setq default-input-method "rime")
(setq rime-show-candidate 'posframe)
(setq rime-disable-predicates
      '(rime-predicate-evil-mode-p
        rime-predicate-after-alphabet-char-p
        rime-predicate-auto-english-p
        rime-predicate-prog-in-code-p))

(require 'rime)
(add-to-list 'rime-translate-keybindings "C-v")
(add-to-list 'rime-translate-keybindings  "M-v")

;; (add-hook 'input-method-activate-hook 'vmacs-evil-input-method-activate t)
;; (add-hook 'input-method-deactivate-hook 'vmacs-evil-input-method-deactive t)
;; (defun vmacs-evil-input-method-activate()
;;   (call-process  "open" nil nil nil "-g" "hammerspoon://input_method_switch?id=Squirrel" ))
;; (defun vmacs-evil-input-method-deactive()
;;   (call-process  "open" nil nil nil "-g" "hammerspoon://input_method_switch?id=U.S." ))

;; (global-set-key (kbd "C-\\") 'toggle-input-method)
(global-set-key (kbd "<f16>") 'toggle-input-method)

;; ;; 输入法进入normal state时，关闭输入法的中文模式
;; (add-hook 'evil-motion-state-entry-hook 'disable-input-method-hook)
(global-set-key (kbd "<f17>") 'evil-normal-state) ;mac karabiner用来控制输入法
(define-key isearch-mode-map (kbd "<f17>") 'evil-normal-state) ;详见isearch-pre-command-hook
(global-set-key (kbd "<f18>") 'evil-insert-state) ;mac karabiner用来控制输入法
(define-key isearch-mode-map (kbd "<f18>") 'evil-insert-state) ;详见isearch-pre-command-hook
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
