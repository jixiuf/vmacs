;;; Code:
;; 可视化书签功能与跳转功能
;; (eval-when-compile
;;   (require 'evil)
;;   (require 'evil-leader))

(require 'bm)
(setq-default
 bm-recenter nil                        ;跳转到书签处时，是否将其调整到屏幕中心
 bm-highlight-style 'bm-highlight-line-and-fringe ;书签以何种形式展示
 bm-cycle-all-buffers t                           ;书签跳转时，是在当前buffer间跳转还是在所有buffer间跳转
 bm-in-lifo-order t)                              ;是否按照书签的添加顺序进行跳转（默认是从上到下按位置顺序跳转）
;; (setq-default bm-buffer-persistence t)
;; (setq-default bm-restore-repository-on-load t)
;; (require 'bm)
;; (add-hook' after-init-hook 'bm-repository-load)
;; (add-hook 'find-file-hooks 'bm-buffer-restore)
;; (add-hook 'kill-buffer-hook 'bm-buffer-save)
;; (add-hook 'kill-emacs-hook '(lambda nil
;;                               (bm-buffer-save-all)
;;                               (bm-repository-save)))

;; (add-hook 'after-revert-hook 'bm-buffer-restore)

;; (global-set-key (kbd "C-.") 'bm-toggle)
;; (global-set-key (kbd "C-,")   'bm-next)

;; (global-set-key (kbd "M-.") 'bm-toggle)
;; (global-set-key (kbd "M-/")   'bm-next)
;; (global-set-key (kbd "M-,") 'bm-previous)


;; 按下ctrl-g时，如果当前光标处有书签，则删除此书签
(with-eval-after-load 'bm
  (defadvice keyboard-quit (before rm-bm-bookmark activate)
    "rm bm bookmark "
    (bm-bookmark-remove)))


;; evil-mode 存在jump机制，即光标在大范围移动时
;; (比如gd evil-goto-definition用于跳转到函数定义的地方)，会在原来的地方
;; 设置一个标记以便可以跳回到原处,默认ctrl-o ctrl-i 进行跳前跳后
;; (define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward)
;; (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)
;;
;;我给绑定到gf 与gb
(define-key evil-normal-state-map "gf" 'evil-jump-forward)
(define-key evil-normal-state-map "gb" 'evil-jump-backward)
;; 其实这个绑定不需要了

;; 有了下面的功能当evil-mode在某个地方设置标记时，同时在那里加一个可视化书签
;; 则利用可视化书签的跳转功能就可以进行前后跳转
(defadvice evil-set-jump (around evil-jump activate)
  (unless (string-match "bm-.*" (symbol-name this-command))
    (bm-bookmark-add nil nil t))
  ad-do-it)
;; isearch 搜索的时候也会进行大范围的光标移动，会在原处加书签，
;; 当取消搜索时,光标后回到原处，则将此处的书签去掉
(defadvice isearch-cancel(around evil-jump-remomve activate)
  (goto-char isearch-opoint)
  (bm-bookmark-remove)
  ad-do-it)

;; 在光标处手动添加一个书签
(define-key evil-normal-state-map "mm" 'bm-toggle) ;evil-set-marker

(evil-leader/set-key "," 'bm-previous)  ;space, 回到上一个书签
(evil-leader/set-key "." 'bm-next)      ;space. 下一个书签


(provide 'conf-bm)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-bm.el ends here.
