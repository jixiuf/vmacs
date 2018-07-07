;; __   ___ __ ___   __ _  ___ ___
;; \ \ / / '_ ` _ \ / _` |/ __/ __|
;;  \ V /| | | | | | (_| | (__\__ \
;;   \_/ |_| |_| |_|\__,_|\___|___/

(defvar vmacs-dumping-state nil)

;; ~/.emacs.d/conf/目录加到load-path中
(add-to-list 'load-path (concat user-emacs-directory "conf/"))
(require 'conf-dump)

(when (vmacs-not-dumping-p)
  ;; 如果当前不是在dumping的过程中，则尝试从load-path-back中恢复load-path
  ;; 因为在dumping 之后会将dumping结束时的load-path 存到load-path-backup中
  ;; 而从dump文件启动后的load-path 并不与dump结束的镜像里的load-path一致
  ;; 需要借助load-path-backup 来恢复
  (when (boundp 'load-path-backup)
    (setq load-path load-path-backup)))


;; custom-set-variables custom-set-faces 相关配置存放在custom-file指定的文件内
(setq custom-file (concat user-emacs-directory "conf/custom-file.el"))
(require 'custom-file)


;; 下面(package-initialize) 这行注释不要删
(package-initialize)
(require 'conf-keybind)
(require 'conf-lazy-load)               ;autoload相关，加快emacs启动速度
(when (member system-type '(gnu/linux darwin)) (require 'conf-sudo))
(require 'conf-space-tab)
(require 'conf-compile)
(require 'conf-version-control)         ;版本管理
(require 'conf-package)                 ;make sure package are installed
(require 'conf-auto-compile)          ;自动编译elisp文件,以加快elisp的加载速度

;; 如果你想重新编译，去掉这行前面的注释重新启动
;; (async-byte-recompile-directory user-emacs-directory)
;; 或者如果在linux 或mac 或 windows上有make rm等一系列命令
;; 可以在本目录下运行以下命令
;; make compile


(require 'conf-evil)
(require 'conf-evil-clipboard)
;; mac 上处理evil-mode 与中文输入法
(require 'conf-evil-visual)       ;跟选中区域相关的配置
(require 'conf-evil-symbol)       ;对symbol 的操作
(require 'conf-evil-window)       ;窗口
(with-eval-after-load 'dired (require 'conf-dired)) ;emacs文件浏览器，directory 管理理
(require 'conf-buffer)

(require 'conf-scroll)                  ;scroll screen C-v M-v
(when (eq system-type 'darwin) (require 'conf-evil-input-method))


(eval-after-load 'ibuffer '(require 'conf-ibuffer)) ;绑定在space l 上，用于列出当前打开的哪些文件
(require 'conf-bm)              ; 可视化书签功能与跳转功能
;; (require 'conf-helm)            ;
(require 'conf-ivy)
;; (with-eval-after-load 'ido (require 'conf-ido)) ;暂时决定不用ido的配置
;; mac 或linux上启用sudo ，用于切换成root或别的用户来编辑当前文件或目录

(when (eq system-type 'darwin) (require 'conf-macos))
(when (eq system-type 'windows-nt) (require 'conf-w32))


;; 一般性的配置在conf/conf-common.el中
(require 'conf-common)
(require 'conf-org)
(with-eval-after-load 'eshell (require 'conf-eshell)) ;
(with-eval-after-load 'term (require 'conf-term)) ;

(require 'conf-yasnippet)               ;模版系统
(require 'conf-yas-auto-insert)         ;利用yasnipet模版,在新建文件时,自动在文件中插入相应的模版
;; gpg 自动加密解密文件相关
(when (executable-find "gpg") (require 'conf-gpg))
(require 'conf-tags)                    ;ctags gtags 相关，代码跳转
(with-eval-after-load 'magit (require 'conf-magit))
(with-eval-after-load 'protobuf-mode (require 'conf-program-protobuf))
(with-eval-after-load 'css-mode (require 'conf-css))
(with-eval-after-load 'js (require 'conf-program-js))
(with-eval-after-load 'lua (require 'conf-program-lua))
(require 'conf-face)
(require 'conf-company-mode)            ;补全
(with-eval-after-load 'go-mode (require 'conf-program-golang))
(with-eval-after-load 'python (require 'conf-program-python))
(with-eval-after-load 'cc-mode (require 'conf-program-objc))

(when (vmacs-not-dumping-p)
  ;; (require 'conf-vterm)
  (global-undo-tree-mode t)
  (global-font-lock-mode)
  (transient-mark-mode 1))


;; Local Variables:
;; coding: utf-8
;; no-byte-compile:t
;; End:

;;; init.el ends here.
