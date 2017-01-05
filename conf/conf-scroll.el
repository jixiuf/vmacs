;; 滚屏相关
(evil-declare-motion 'golden-ratio-scroll-screen-down)
(evil-declare-motion 'golden-ratio-scroll-screen-up)

(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up) ;C-v
;; (global-set-key "\C-u" 'gold-ratio-scroll-screen-up)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down) ;M-v


(provide 'conf-scroll)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-scroll.el ends here.
