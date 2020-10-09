
;; brew install pyenv-virtualenv

;; brew install zlib
;; brew install sqlite
;; export LDFLAGS="${LDFLAGS} -L/usr/local/opt/zlib/lib"
;; export CPPFLAGS="${CPPFLAGS} -I/usr/local/opt/zlib/include"
;; export LDFLAGS="${LDFLAGS} -L/usr/local/opt/sqlite/lib"
;; export CPPFLAGS="${CPPFLAGS} -I/usr/local/opt/sqlite/include"
;; export PKG_CONFIG_PATH="${PKG_CONFIG_PATH} /usr/local/opt/zlib/lib/pkgconfig"
;; export PKG_CONFIG_PATH="${PKG_CONFIG_PATH} /usr/local/opt/sqlite/lib/pkgconfig"
;; pyenv install 3.6.8
;; pyenv virtualenv 3.6.8 env-3.6.8 #create
;; ;;pyenv virtualenvs # list

;; .zshrc
;; if which pyenv-virtualenv-init > /dev/null; then
;;     export PYENV_VIRTUALENV_DISABLE_PROMPT=1;
;;     eval "$(pyenv init -)";
;;     eval "$(pyenv virtualenv-init -)";
;;     pyenv activate env-3.6.8    2>/dev/null;
;; fi



;; https://github.com/microsoft/python-language-server/blob/master/CONTRIBUTING.md
;; download dotnet https://dotnet.microsoft.com/learn/dotnet/hello-world-tutorial/intro

;; cd ~/repos
;; git clone https://github.com/Microsoft/python-language-server.git
;; cd ~/repos/python-language-server/src/LanguageServer/Impl
;; dotnet publish -c Release -r osx-x64




;; pyenv deactivate #退出虚拟环境，回到系统环境

;; pyenv commands

;; pip install 'python-language-server[all]'

;; pip install jedi epc argparse
;; argparse (for Python 2.6)
;; doc http://tkf.github.io/emacs-jedi/latest/
;; 第一次需要启动emacs后 运行 jedi:install-server  会把jedi 目录下的一些py 安装到jedi:environment-root 目录下
;; 默认jedi:environment-root  是~/.emacs.d/.python-environments
;; C-c? 看文档
;; C-c. goto def
;; C-tab complete
;;  C-c , jedi:goto-definition-pop-marker s
;; (setq lsp-python-ms-dir
;;       (expand-file-name "~/repos/python-language-server/output/bin/Release"))
;; (setq lsp-python-ms-executable
;;       (expand-file-name "osx-x64/publish/Microsoft.Python.LanguageServer" lsp-python-ms-dir))

;; (defun vmacs-python-mode-hook ()
;;   ;; (unless (executable-find "pyls")
;;   ;;   (find-file "~/.emacs.d/conf/conf-program-python.el")
;;   ;;   (message "pyls not found,try setup python now"))
;;   (require 'lsp-python-ms)
;;   (lsp-deferred))
;; (add-hook 'python-mode-hook 'vmacs-python-mode-hook)


;; ; Set PYTHONPATH, because we don't load .bashrc
;; (defun set-python-path-from-shell-PYTHONPATH ()
;; (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PYTHONPATH'")))
;; (setenv "PYTHONPATH" path-from-shell)))





(provide 'conf-program-python)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-program-python.el ends here.
