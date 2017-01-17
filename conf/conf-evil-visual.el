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
;;
(define-key evil-normal-state-map "mf" 'evil-mark-defun) ;mark-defun 相当于C-M-h
(define-key evil-normal-state-map "mh" 'evil-M-h)        ;相当于M-h
(define-key evil-normal-state-map "mxh" 'evil-mark-whole-buffer) ;相当于C-xh
(define-key evil-normal-state-map "mb" 'evil-mark-whole-buffer);相当于C-xh

;; (define-key evil-normal-state-map "mo" 'er/expand-region);
;; (define-key evil-visual-state-map "mo" 'er/expand-region);
;; (define-key evil-normal-state-map "mO" 'er/contract-region);


(provide 'conf-evil-visual)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-evil-mark.el ends here.
