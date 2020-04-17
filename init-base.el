;;; Code:
(defvar vmacs-dumping-state nil)

;; ~/.emacs.d/conf/目录加到load-path中
(add-to-list 'load-path (concat user-emacs-directory "conf/"))
(defvar lazy-load-dir (concat user-emacs-directory "lazy"))
(add-to-list 'load-path lazy-load-dir)
(setq submodules-dir (concat user-emacs-directory "submodule"))
(when submodules-dir
  (dolist (dir (directory-files submodules-dir  nil "[a-zA-Z0-9_-]"))
    (add-to-list 'load-path dir)))


(require 'conf-dump)
(require 'conf-tmp-before nil t)

(when (vmacs-not-dumping-p)
  ;; 如果当前不是在dumping的过程中，则尝试从load-path-back中恢复load-path
  ;; 因为在dumping 之后会将dumping结束时的load-path 存到load-path-backup中
  ;; 而从dump文件启动后的load-path 并不与dump结束的镜像里的load-path一致
  ;; 需要借助load-path-backup 来恢复
  (when (boundp 'load-path-backup)
    (setq load-path load-path-backup)))

;; 下面(package-initialize) 这行注释不要删
(when (< emacs-major-version 27) (package-initialize))
(require 'conf-package)
(require 'conf-lazy-load)               ;autoload相关，加快emacs启动速度
(require 'conf-minibuffer)
(require 'conf-icomplete)
(require 'conf-keybind)
;; custom-set-variables custom-set-faces 相关配置存放在custom-file指定的文件内
(setq custom-file (concat user-emacs-directory "conf/custom-file.el"))
(require 'custom-file)
(require 'vmacs-theme)



(provide 'init-base)

;; Local Variables:
;; coding: utf-8
;; End:

;;; init-base.el ends here.
