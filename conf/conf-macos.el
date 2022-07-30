(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(defun vmacs-set-font()
  ;; 当font 设置为单一字体的时候，遇到当前字体处理不了的，则使用 fontset-default 来解析
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fontsets.html
  ;; (set-fontset-font "fontset-default" 'emoji "Apple Color Emoji")
  ;; (set-fontset-font "fontset-default" 'symbol "Apple Color Emoji")
  (set-fontset-font t 'emoji "Apple Color Emoji-17")
  (set-fontset-font t 'symbol "Apple Symbols")
  (set-face-attribute 'default nil :font "Sarasa Mono SC Nerd" :height 220)
  (set-face-attribute 'fixed-pitch nil :font "Sarasa Mono SC Nerd" :height 1.0)
  )

(vmacs-set-font)
(add-hook 'after-init-hook #'vmacs-set-font)

;; (global-set-key (kbd "s-=") 'create-frame-font-big-mac)
;; (global-set-key [C-M-s-backspace] 'create-frame-font-large-mac)
;; (global-set-key (kbd "s--") 'create-frame-font-small-mac)
;; (global-set-key (kbd "C-M-s-9") 'create-frame-font-middle-mac) ;s-9
;; (global-set-key (kbd "s-0") 'create-frame-font-mac)

;; ;; pip install jieba
;; (setq osx-dictionary-use-chinese-text-segmentation t)
(vmacs-leader (kbd "wd") 'osx-dictionary-search-pointer)

;; (setq exec-path (delete-dups  (cons "/usr/local/bin" exec-path)))
;; (setenv "PATH" (concat  "/usr/local/bin:" (getenv "PATH") ))
;; 从shell 中获取环境变量
(setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOROOT" "GOPATH" "EDITOR" "PYTHONPATH" "LC_ALL" "LANG"))

;; 设成nil 则不从 .zshrc 读 只从 .zshenv读（可以加快速度，但是需要你将环境变量相关的都放到 .zshenv 中，而非 .zshrc 中）
(setq exec-path-from-shell-shell-name "zsh")
(setq exec-path-from-shell-check-startup-files nil) ;
(setq exec-path-from-shell-arguments '("-l" )) ;remove -i read form .zshenv

;; (setq-default server-auth-dir (expand-file-name "~/.emacs.d/cache/"))
;; (setq-default server-socket-dir (expand-file-name "~/.emacs.d/cache/"))
;; (setq-default server-name "emacs-server-file")
;; (require 'server)
;; (when (not (server-running-p)) (server-start))


;; (setq-default server-auth-dir (expand-file-name "~/.emacs.d/cache/"))
;; (setq-default server-socket-dir  (expand-file-name "~/.emacs.d/cache/"))
;; (setq-default server-name "emacs-server-file")
;; (require 'server)
;; (when (not (server-running-p)) (server-start))

;; 允许emacs 直接编辑 OSX下的 .plist文件
;; Allow editing of binary .plist files.
(require 'jka-cmpr-hook)
(add-to-list 'jka-compr-compression-info-list
             ["\\.plist$"
              "converting text XML to binary plist"
              "plutil"
              ("-convert" "binary1" "-o" "-" "-")
              "converting binary plist to text XML"
              "plutil"
              ("-convert" "xml1" "-o" "-" "-")
              nil nil "bplist"])

;; # mac 上 emacs 直接编辑二进制applescript
(add-to-list 'jka-compr-compression-info-list
             `["\\.scpt\\'"
               "converting text applescript to binary applescprit " ,(expand-file-name "applescript-helper.sh" "~/.emacs.d/bin/") nil
               "converting binary applescript to text applescprit " ,(expand-file-name "applescript-helper.sh" "~/.emacs.d/bin/") ("-d")
               nil t "FasdUAS"])
;;It is necessary to perform an update!
;; (add-hook 'after-init-hook #'jka-compr-update)
(run-with-timer 0.01 nil #'jka-compr-update)


;; f11 (toggle-frame-fullscreen) default
(setq ns-pop-up-frames nil)
(setq frame-resize-pixelwise t)         ;;设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙
;; 在Mac平台, Emacs不能进入Mac原生的全屏模式,否则会导致 `make-frame' 创建时也集成原生全屏属性后造成白屏和左右滑动现象.
;; 所以先设置 `ns-use-native-fullscreen' 和 `ns-use-fullscreen-animation' 禁止Emacs使用Mac原生的全屏模式.
;; 而是采用传统的全屏模式, 传统的全屏模式, 只会在当前工作区全屏,而不是切换到Mac那种单独的全屏工作区,
;; 这样执行 `make-frame' 先关代码或插件时,就不会因为Mac单独工作区左右滑动产生的bug.
;;
;; Mac平台下,不能直接使用 `set-frame-parameter' 和 `fullboth' 来设置全屏,
;; 那样也会导致Mac窗口管理器直接把Emacs窗口扔到单独的工作区, 从而对 `make-frame' 产生同样的Bug.
;; 所以, 启动的时候通过 `set-frame-parameter' 和 `maximized' 先设置Emacs为最大化窗口状态, 启动5秒以后再设置成全屏状态,
;; Mac就不会移动Emacs窗口到单独的工作区, 最终解决Mac平台下原生全屏窗口导致 `make-frame' 左右滑动闪烁的问题.
(setq ns-use-native-fullscreen nil)
(setq ns-use-fullscreen-animation nil)

(provide 'conf-macos)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-macos.el ends here.
