
;; brew install pyenv-virtualenv
;; pyenv install 2.7.13
;; pip install 'python-language-server[all]'

;; create
;; pyenv virtualenv 2.7.10 env-2.7.10
;; list
;; pyenv virtualenvs
;; pyenv activate env-name  # 激活虚拟环境
;; pyenv deactivate #退出虚拟环境，回到系统环境

;; pyenv commands

;; pip install jedi epc argparse
;; argparse (for Python 2.6)
;; doc http://tkf.github.io/emacs-jedi/latest/
;; 第一次需要启动emacs后 运行 jedi:install-server  会把jedi 目录下的一些py 安装到jedi:environment-root 目录下
;; 默认jedi:environment-root  是~/.emacs.d/.python-environments
;; C-c? 看文档
;; C-c. goto def
;; C-tab complete
;;  C-c , jedi:goto-definition-pop-marker s

(message "conf-program-python is loaded")
(defun vmacs-python-mode-hook ()
  (unless (executable-find "pyls")
    (find-file "~/.emacs.d/conf/conf-program-python.el")
    (message "pyls not found,try setup python now"))
  (lsp))
(add-hook 'python-mode-hook 'vmacs-python-mode-hook)


;; ; Set PYTHONPATH, because we don't load .bashrc
;; (defun set-python-path-from-shell-PYTHONPATH ()
;; (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PYTHONPATH'")))
;; (setenv "PYTHONPATH" path-from-shell)))





(provide 'conf-program-python)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-program-python.el ends here.
