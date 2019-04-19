;; 懒加载autoload
;; 你会发现lazy/目录下的很多函数上面都会有;;;###autoload的标识
;; 此文件为 .emacs.d/lazy/内的文件生成autoload cookie
;; 生成的autoload 放在.emacs.d/lisp/lazy-loaddefs.el中,并load之
;; 如此 就可以做到在未真正用到这些函数之前，相应的文件不会被加载，以加快启动速度
;; 如果想重新生成.emacs.d/lisp/lazy-loaddefs.el,则只需要删除这个文件，重启emacs即可
;; 只需要加载.emacs.d/lisp/lazy-loaddefs.el,就可以于加载了放在lazy/目录下的所有el文件
(setq source-directory user-emacs-directory)
(setq generated-autoload-file "lazy-loaddefs.el")
(setq generated-autoload-file-path (expand-file-name (concat "lisp/" generated-autoload-file) source-directory))

(when (or (not (file-exists-p  generated-autoload-file-path))
          noninteractive)
  (dolist (file (directory-files lazy-load-dir t "\.el$"))
    (update-file-autoloads file t)))

(when (file-exists-p  generated-autoload-file-path)
  (load generated-autoload-file-path t t)
  (kill-buffer (get-file-buffer generated-autoload-file-path)))



(provide 'conf-lazy-load)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-lazy-load.el ends here.
