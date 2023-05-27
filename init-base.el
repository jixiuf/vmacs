;;; Code:

;; ~/.emacs.d/conf/目录加到 load-path 中
(add-to-list 'load-path (concat user-emacs-directory "conf/"))
(defvar lazy-load-dir (concat user-emacs-directory "lazy"))
(add-to-list 'load-path lazy-load-dir)
(setq submodules-dir (concat user-emacs-directory "submodule"))
(when submodules-dir
  (dolist (dir (directory-files submodules-dir  nil "[a-zA-Z0-9_-]"))
    (add-to-list 'load-path dir)))

(require 'conf-macro)
;; (require 'conf-dump)
(require 'conf-tmp-before nil t)

;; (when (vmacs-not-dumping-p)
;;   ;; 如果当前不是在 dumping 的过程中，则尝试从 load-path-back 中恢复 load-path
;;   ;; 因为在 dumping 之后会将 dumping 结束时的 load-path 存到 load-path-backup 中
;;   ;; 而从 dump 文件启动后的 load-path 并不与 dump 结束的镜像里的 load-path 一致
;;   ;; 需要借助 load-path-backup 来恢复
;;   (when (boundp 'load-path-backup)
;;     (setq load-path load-path-backup)))

(setq custom-file (concat user-emacs-directory "conf/custom-file.el"))
(require 'custom-file)

(require 'conf-package)
(require 'conf-lazy-load)               ;autoload 相关，加快 emacs 启动速度
(require 'conf-minibuffer)
;; custom-set-variables custom-set-faces 相关配置存放在 custom-file 指定的文件内
;; (require 'vmacs-theme)



(provide 'init-base)

;; Local Variables:
;; coding: utf-8
;; End:

;;; init-base.el ends here.
