;; pip install ‘python-language-server[all]’

;; brew install pyenv-virtualenv
;; pyenv install 2.7.13

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
;; C-c/ helm-jedi-related-names  Find related names of the object at point using helm interface.

(message "conf-program-python is loaded")
(defun vmacs-python-mode-hook ()
  ;; (when (fboundp 'jedi:setup) (jedi:setup))
  (make-local-variable 'company-backends)
  ;; (add-to-list 'company-backends 'company-jedi)

  ;; (push '("lambda" . #x1d77a) prettify-symbols-alist) ;lambda 入
  ;; (prettify-symbols-mode)
  (lsp)

  (flycheck-mode)

  ;; pip install isort
  ;; auto import module at point
  ;; (define-key python-mode-map (kbd "C-c i") 'python-auto-import)

  ;; gd 到函数定义处 space, 回到原处
  ;; evil-mode gd goto-definition 会调用jedi:goto-definition
  )
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
