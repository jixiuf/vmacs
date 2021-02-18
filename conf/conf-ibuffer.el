;;; -*- coding:utf-8 -*-
;;ibuffer 的管理(列出所有打开的文件/buffer)
;;n p 上下，
;; m进行标记,比如对两个buffer进行diff比较，可以m标记这两个认的，然后按M-=来进行对比
;;
;; d标记为删除（关闭buffer） x执行删除，
;; D直接删除
;; 在一个分组上按d/D/m 则对这个分组进行操作（标记\删除等）

;; = 进行diff操作（已保存的文件与buffer中的）
;; M-= 用ediff比较两个buffer

;;O 过滤标记的buffer ，在搜索进行搜索，显示搜索结果

;;M-n M-p 组间跳转


(require 'ibuf-ext)
(setq ibuffer-modified-char ?m);; 用m 字符表示modified的buffer
(setq ibuffer-read-only-char ?r);;用r 表示只读buffer
(setq ibuffer-show-empty-filter-groups nil);;不显示没有任何buffer的空分组
(setq ibuffer-default-sorting-mode (quote major-mode)) ;;排序
;;设置默认不显示maybe-show-predicates的buffer (即隐藏上面Hidden分组里的内容)
(setq ibuffer-default-display-maybe-show-predicates nil)

(with-eval-after-load 'evil-collection-ibuffer
  (run-with-timer 0.01 nil '(lambda()
                              (evil-collection-define-key 'normal 'ibuffer-mode-map
                                "gr" 'ibuffer-filter-disable
                                (kbd "M-=") 'ibuffer-ediff-merge
                                "gt" 'ibuffer-toggle-maybe-show
                                "r" 'ibuffer-update
                                "/" 'ibuffer-filter-by-name))))

(setq ibuffer-saved-filter-groups
      '(("Default"
         ("Files"  (or (mode . dired-mode) (visiting-file . "^.*$")))
         ("Shell"  (or (mode . shell-mode) (mode . vterm-mode)))
         ("Emacs"  (or (name . "^\\*.*$") (name . "magit")))
         ("Hidden(g则不显示此分组)"  (name . "^ ")))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            ;; (ibuffer-auto-mode t)       ;自动更新*Ibuffer* buffer
            (ibuffer-switch-to-saved-filter-groups "Default")))

;;toggle 显示上面的 Hidden分组里的内容
(defun ibuffer-toggle-maybe-show()
  (interactive)
  (setq ibuffer-default-display-maybe-show-predicates
        (not ibuffer-default-display-maybe-show-predicates))
  (kill-buffer "*Ibuffer*")
  (ibuffer))


;; 可读性好的size
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))
;;设置buffer中每一行的显示格式
(setq ibuffer-formats
      '((mark modified read-only  " "
              (name 50 50 :left :elide) " " ;buffer-name 宽度30 靠左
              (size-h 9 -1 :right) " " ;应用可读性好的 file size
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " " (name 24 -1) " " filename)))

(provide 'conf-ibuffer)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-ibuffer.el ends here.
