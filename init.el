;; __   ___ __ ___   __ _  ___ ___
;; \ \ / / '_ ` _ \ / _` |/ __/ __|
;;  \ V /| | | | | | (_| | (__\__ \
;;   \_/ |_| |_| |_|\__,_|\___|___/

(load "~/.emacs.d/init-base.el")

(when (< emacs-major-version 27) (package-initialize))


(when (member system-type '(gnu/linux darwin)) (require 'conf-sudo))
(require 'conf-space-tab)
(eval-after-load 'ibuffer '(require 'conf-ibuffer)) ;绑定在space l 上，用于列出当前打开的哪些文件
(with-eval-after-load 'protobuf-mode (require 'conf-program-protobuf))
(with-eval-after-load 'css-mode (require 'conf-css))
(with-eval-after-load 'lua (require 'conf-program-lua))
(require 'conf-face)
;; (when (executable-find "gpg") (require 'conf-gpg))
(with-eval-after-load 'sql (require 'conf-sql))

(require 'conf-evil)
;; mac 上处理evil-mode 与中文输入法
(require 'conf-evil-window)       ;窗口
(require 'conf-evil-clipboard)
;; (require 'conf-bm)              ; 可视化书签功能与跳转功能



(when (eq system-type 'darwin) (require 'conf-macos))
(when (eq system-type 'windows-nt) (require 'conf-w32))
(with-eval-after-load 'iedit (require 'conf-iedit))
(require 'conf-common)
(with-eval-after-load 'org (require 'conf-org))

(require 'conf-yasnippet)               ;模版系统
(with-eval-after-load 'compile (require 'conf-compile))
(with-eval-after-load 'cc-mode (require 'conf-program-objc))
(with-eval-after-load 'js (require 'conf-program-js))

(when (vmacs-not-dumping-p)
  (when (eq system-type 'darwin)
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize))
  (with-eval-after-load 'dired (require 'conf-dired)) ;emacs文件浏览器，directory 管理理



  ;; (require 'conf-helm)            ;
  ;; (require 'conf-ivy)
;; (require 'conf-rg)
(require 'conf-wgrep)
  ;; (with-eval-after-load 'ido (require 'conf-ido)) ;暂时决定不用ido的配置
  ;; mac 或linux上启用sudo ，用于切换成root或别的用户来编辑当前文件或目录
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(when (require 'mu4e nil t) (require 'conf-mail))



  ;; 一般性的配置在conf/conf-common.el中
  (require 'conf-tabs)
  ;; (with-eval-after-load 'eshell (require 'conf-eshell)) ;
  ;; (with-eval-after-load 'term (require 'conf-term)) ;

  ;; gpg 自动加密解密文件相关
  (require 'conf-tags)                    ;ctags gtags 相关，代码跳转
  ;; (with-eval-after-load 'eglot (define-key eglot-mode-map (kbd "C-h .") 'eglot-help-at-point))
  (require 'conf-company-mode)            ;补全
  (with-eval-after-load 'go-mode (require 'conf-program-golang))
  (with-eval-after-load 'python (require 'conf-program-python))
  ;; (with-eval-after-load 'dap-mode (require 'conf-dap-mode))

  (require 'conf-version-control)
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
  (require 'server)
  (unless (server-running-p) (server-start))
  (when (> emacs-major-version 27) (load-theme 'modus-vivendi)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile:t
;; End:

;;; init.el ends here.
