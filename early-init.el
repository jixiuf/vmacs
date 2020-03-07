;; __   ___ __ ___   __ _  ___ ___
;; \ \ / / '_ ` _ \ / _` |/ __/ __|
;;  \ V /| | | | | | (_| | (__\__ \
;;   \_/ |_| |_| |_|\__,_|\___|___/

(setq-default
 ;; user-full-name ""                ;记得改成你的名字
 ;; user-login-name "jixiuf"
 user-mail-address (concat "jixiuf" "@" "qq.com"))
(setq load-prefer-newer t)              ;当el文件比elc文件新的时候,则加载el,即尽量Load最新文件文件
;; By default Emacs will initiate GC every 0.76 MB allocated (gc-cons-threshold == 800000).
;; If we increase this to 20 MB (gc-cons-threshold == 20000000) we get:
(setq gc-cons-threshold  (* 10 1024 1024)) ;50 MB before garbage collection
;; 禁用工具栏，滚运条 菜单栏
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq-default initial-frame-alist
              '((alpha . 85)
                (font . "Sarasa Mono CL-18")
                (foreground-color . "#eeeeec")
                (background-color . "#202020") ;;
                (background-mode . dark)
                (ns-appearance . dark)
                (height . 43)
                (width . 120)
                ;; (left . 20)
                ;; (top . 80)
                ))
(setq-default default-frame-alist initial-frame-alist)

(setq-default mode-line-format nil)
(setq mode-line-format nil)
