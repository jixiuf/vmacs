;;
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
  (add-to-list 'company-backends 'company-jedi)

  (push '("lambda" . #x1d77a) prettify-symbols-alist) ;lambda 入
  (prettify-symbols-mode)

  (flycheck-mode)

  ;; pip install isort
  ;; auto import module at point
  ;; (define-key python-mode-map (kbd "C-c i") 'python-auto-import)

  ;; gd 到函数定义处 space, 回到原处
  ;; evil-mode gd goto-definition 会调用jedi:goto-definition
  )
(add-hook 'python-mode-hook 'vmacs-python-mode-hook)

;; pip install ipython
(setq-default
 ;; jedi:environment-root "~/python/"
 jedi:complete-on-dot t
 ;; jedi:key-complete (kbd "C-h")
 ;; jedi:key-show-doc C-c d
 ;; jedi:key-related-names C-c r
 jedi:key-goto-definition-pop-marker C-, default
 ;; jedi:key-goto-definition-pop-marker  (kbd "M-,")
 ;; jedi:key-goto-definition (kbd "M-.")
 jedi:use-shortcuts nil
 jedi:setup-keys t
 python-shell-interpreter "ipython"
 python-shell-interpreter-args (if (equal system-type 'darwin)
                                   "--colors=Linux" ;--matplotlib=osx
                                 (if (equal system-type 'gnu/linux)
                                     "--gui=wx --matplotlib=wx --colors=Linux"))
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; ------------------
;; misc python config
;; ------------------
;; pydoc info
;; wget https://bitbucket.org/jonwaltman/pydoc-info/downloads/python.info.gz
;; gunzip python.info
;; sudo cp python.info /usr/share/info
;; sudo install-info --info-dir=/usr/share/info python.info
;; (require 'pydoc-info nil t)             ;C-hS ???

;; ; jedi python completion
;; (include-elget-plugin "ctable") ; required for epc
;; (include-elget-plugin "deferred") ; required for epc
;; (include-elget-plugin "epc") ; required for jedi
;; (include-elget-plugin "jedi")
;; (require 'jedi)
;; (setq jedi:setup-keys t)
;; (autoload 'jedi:setup "jedi" nil t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; pyflakes flymake integration
;; pep8 语法检查
;; http://stackoverflow.com/a/1257306/347942
;; pip install --upgrade pyflakes pep8


;; ;; Make sure it's not a remote buffer or flymake would not work
;; (defun flymake-pyflakes-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      'flymake-create-temp-in-system-tempdir))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;     (list "pychecker" (list temp-file))))

;; (with-eval-after-load 'flymake
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))



;; ; Set PYTHONPATH, because we don't load .bashrc
;; (defun set-python-path-from-shell-PYTHONPATH ()
;; (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PYTHONPATH'")))
;; (setenv "PYTHONPATH" path-from-shell)))





(provide 'conf-program-python)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-program-python.el ends here.
