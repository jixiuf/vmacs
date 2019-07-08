;; __   ___ __ ___   __ _  ___ ___
;; \ \ / / '_ ` _ \ / _` |/ __/ __|
;;  \ V /| | | | | | (_| | (__\__ \
;;   \_/ |_| |_| |_|\__,_|\___|___/

(defvar vmacs-dumping-state nil)

;; ~/.emacs.d/conf/目录加到load-path中
(add-to-list 'load-path (concat user-emacs-directory "conf/"))
(defvar lazy-load-dir (concat user-emacs-directory "lazy"))
(add-to-list 'load-path lazy-load-dir)

(require 'conf-dump)
(require 'conf-tmp-before nil t)

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
(require 'vmacs-theme)



;; 下面(package-initialize) 这行注释不要删
(when (< emacs-major-version 27) (package-initialize))
(require 'conf-package)
(require 'conf-dired-dump)
(require 'conf-evil-dump)

(require 'conf-lazy-load)               ;autoload相关，加快emacs启动速度
(when (member system-type '(gnu/linux darwin)) (require 'conf-sudo))
(require 'conf-space-tab)
(require 'conf-auto-compile)          ;自动编译elisp文件,以加快elisp的加载速度
(eval-after-load 'ibuffer '(require 'conf-ibuffer)) ;绑定在space l 上，用于列出当前打开的哪些文件
(with-eval-after-load 'protobuf-mode (require 'conf-program-protobuf))
(with-eval-after-load 'css-mode (require 'conf-css))
(with-eval-after-load 'lua (require 'conf-program-lua))
(require 'conf-face)
(when (executable-find "gpg") (require 'conf-gpg))
(require 'conf-sql)

;; 如果你想重新编译，去掉这行前面的注释重新启动
;; (async-byte-recompile-directory user-emacs-directory)
;; 或者如果在linux 或mac 或 windows上有make rm等一系列命令
;; 可以在本目录下运行以下命令
;; make compile



(require 'conf-keybind)
(require 'conf-evil)
(require 'conf-evil-visual)       ;跟选中区域相关的配置
;; mac 上处理evil-mode 与中文输入法
(require 'conf-evil-symbol)       ;对symbol 的操作
(require 'conf-evil-window)       ;窗口
(require 'conf-evil-clipboard)
(require 'conf-bm)              ; 可视化书签功能与跳转功能

(when (eq system-type 'darwin) (require 'conf-macos))
(when (eq system-type 'windows-nt) (require 'conf-w32))
(require 'conf-iedit)
(require 'conf-common)
(require 'conf-org)

(require 'conf-yasnippet)               ;模版系统
(require 'conf-yas-auto-insert)         ;利用yasnipet模版,在新建文件时,自动在文件中插入相应的模版
(require 'conf-buffer)
(require 'conf-compile)
(require 'conf-version-control)         ;版本管理
(with-eval-after-load 'cc-mode (require 'conf-program-objc))
(with-eval-after-load 'js (require 'conf-program-js))

(when (vmacs-not-dumping-p)
  (with-eval-after-load 'dired (require 'conf-dired)) ;emacs文件浏览器，directory 管理理



  ;; (require 'conf-helm)            ;
  (require 'conf-ivy)
  ;; (with-eval-after-load 'ido (require 'conf-ido)) ;暂时决定不用ido的配置
  ;; mac 或linux上启用sudo ，用于切换成root或别的用户来编辑当前文件或目录



  ;; 一般性的配置在conf/conf-common.el中
  (require 'conf-awesome-tab)
  ;; (with-eval-after-load 'eshell (require 'conf-eshell)) ;
  ;; (with-eval-after-load 'term (require 'conf-term)) ;

  ;; gpg 自动加密解密文件相关
  (require 'conf-tags)                    ;ctags gtags 相关，代码跳转
  ;; (with-eval-after-load 'eglot (define-key eglot-mode-map (kbd "C-h .") 'eglot-help-at-point))
  (require 'conf-company-mode)            ;补全
  (with-eval-after-load 'go-mode (require 'conf-program-golang))
  (with-eval-after-load 'python (require 'conf-program-python))

  (with-eval-after-load 'magit (require 'conf-magit))
  (global-undo-tree-mode t)
  (global-font-lock-mode)
  (transient-mark-mode 1)
  (save-place-mode t)
  (savehist-mode 1)
  (recentf-mode 1)
  (run-with-idle-timer 300 t 'vmacs-idle-timer) ;idle 300=5*60s
  (require 'conf-tmp nil t)
  (require 'conf-vterm)
  (when (eq system-type 'darwin) (require 'conf-evil-input-method))
  (when (eq system-type 'darwin) (exec-path-from-shell-initialize))
  (load-theme 'vmacs))


;; Local Variables:
;; coding: utf-8
;; no-byte-compile:t
;; End:

;;; init.el ends here.
