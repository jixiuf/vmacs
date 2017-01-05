(eval-when-compile (require 'evil))

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
(define-key evil-normal-state-map "R" 'evil-backward-symbol-end)

(define-key evil-visual-state-map "e" 'evil-forward-symbol-begin)
(define-key evil-visual-state-map "r" 'evil-forward-symbol-end)
;; (define-key evil-visual-state-map "E" 'evil-forward-symbol-end)
(define-key evil-visual-state-map "v" 'evil-backward-symbol-begin)
(define-key evil-visual-state-map "R" 'evil-backward-symbol-end)


;; de dr
(define-key evil-motion-state-map "e" 'evil-forward-symbol-end)
(define-key evil-motion-state-map "r" 'evil-backward-symbol-begin)
;; dae die
(define-key evil-outer-text-objects-map "e" 'evil-a-symbol)
(define-key evil-inner-text-objects-map "e" 'evil-inner-symbol)


;;;###autoload
(evil-define-motion evil-forward-symbol-begin(count)
  "Move to the end of the COUNT-th next symbol."
  ;; :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob count)
  (evil-forward-beginning 'evil-symbol count)
  (let ((sym (thing-at-point 'evil-symbol)))
    (while (and sym (not (string-match "\\<" sym)))
      (evil-forward-beginning 'evil-symbol 1)
      (setq sym (thing-at-point 'evil-symbol))
      )
    )
  )

;;;###autoload
(evil-define-motion evil-backward-symbol-begin(count)
  "Move to the end of the COUNT-th next symbol."
  ;; :jump t
  :type exclusive
  ;; (evil-signal-at-bob-or-eob count)
  ;; (forward-evil-symbol count)
  (evil-backward-beginning 'evil-symbol count)
  (let ((sym (thing-at-point 'evil-symbol)))
    (while (and sym (not (string-match "\\<" sym)))
      (evil-backward-beginning 'evil-symbol 1)
      (setq sym (thing-at-point 'evil-symbol)))))


;;;###autoload
(evil-define-motion evil-forward-symbol-end(count)
  "Move to the end of the COUNT-th next symbol."
  ;; :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob count)
  (forward-evil-symbol count)

  ;; (let ((sym (thing-at-point 'evil-symbol)))
  ;;   (while (and sym (not (string-match "^\\<" sym)))
  ;;     (evil-forward-end 'evil-symbol 1)
  ;;     (setq sym (thing-at-point 'evil-symbol))
  ;;     )
  ;;   )
  )

;;;###autoload
(evil-define-motion evil-backward-symbol-end(count)
  "Move to the end of the COUNT-th next symbol."
  ;; :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob count)
  (evil-backward-end 'symbol count)
  (let ((sym (thing-at-point 'evil-symbol)))
    (while (and sym (not (string-match "\\<" sym)))
      (evil-backward-end 'evil-symbol 1)
      (setq sym (thing-at-point 'evil-symbol))
      )
    )
)


(provide 'conf-evil-symbol)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-evil-symbol.el ends here

