;; __   ___ __ ___   __ _  ___ ___
;; \ \ / / '_ ` _ \ / _` |/ __/ __|
;;  \ V /| | | | | | (_| | (__\__ \
;;   \_/ |_| |_| |_|\__,_|\___|___/
;; https://emacs-china.org/t/macos-emacs-28-native-comp/12201/70?u=jixiuf
;; (setq comp-deferred-compilation-black-list '("yasnippet" "org-re-reveal" "tramp"))
(defconst my/start-time (current-time))




(defmacro mt (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold most-positive-fixnum   ;; Defer Garbage collection
      gc-cons-percentage 1.0)

(add-hook 'window-setup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old
                  gc-cons-threshold 800000
                  gc-cons-percentage 0.1)
            (garbage-collect)
            (message "Load time %.06f" (float-time (time-since my/start-time))))
          t)

(setq-default
 ;; user-full-name ""                ;记得改成你的名字
 ;; user-login-name "jixiuf"
 user-mail-address (concat "jixiuf" "@" "qq.com"))
(setq load-prefer-newer t)              ;当el文件比elc文件新的时候,则加载el,即尽量Load最新文件文件
;; By default Emacs will initiate GC every 0.76 MB allocated (gc-cons-threshold == 800000).
;; If we increase this to 20 MB (gc-cons-threshold == 20000000) we get:
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb default 4k

;; 禁用工具栏，滚运条 菜单栏
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(create-fontset-from-fontset-spec
    (concat "-*-*-*-*-*--*-*-*-*-*-*-fontset-mac"
            ",han:Sarasa Mono CL:size=18"
            ",symbol:Apple Color Emoji:size=18" ;😀
            ",cjk-misc:Sarasa Mono CL:size=18"
            ",bopomofo:Sarasa Mono CL:size=18"
            ",kana:Sarasa Mono CL:size=18"
            ",hangul:Sarasa Mono CL:size=18"
            ",latin:Sarasa Mono CL:size=18"))

(add-hook 'after-init-hook #'(lambda() (dolist (face '(fixed-pitch)) (set-face-attribute `,face nil :font "Sarasa Mono CL-18"))))

(setq-default initial-frame-alist
              '((alpha . 85)
                (height . 43)
                (width . 120)
                ;; (font . "Sarasa Mono CL-18")
                (font . "fontset-mac")
                (ns-appearance . dark)
                (foreground-color . "#ffffff")
                (background-color . "#000000") ;;
                ;; (foreground-color . "#eeeeec")
                ;; (background-color . "#202020") ;;
                ;; (background-mode . dark)
                ;; (left . 20)
                ;; (top . 80)
                ))

(setq-default default-frame-alist initial-frame-alist)

(setq-default mode-line-format nil)
(setq mode-line-format nil)

(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin19/11.1.0")