;;; -*- coding:utf-8 -*-

(setq eglot-confirm-server-initiated-edits nil)
;; :documentHighlightProvider 禁用高亮光标下的单词
(setq eglot-ignored-server-capabilites '(:documentHighlightProvider))
(defun vmacs-eglot-organize-imports() (call-interactively 'eglot-code-action-organize-imports))
(defun vmacs-lsp-hook()
  ;; The depth of -10 places this before eglot's willSave notification,
  ;; so that that notification reports the actual contents that will be saved.
  (hs-minor-mode 1)
  (add-hook 'before-save-hook #'vmacs-eglot-organize-imports -9 t);before hook有时无效，只好After
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

(dolist (mod '(python-mode-hook c++-mode-hook go-mode-hook c-mode-hook ))
  (add-hook mod #'eglot-ensure)
  (add-hook mod #'vmacs-lsp-hook))

(dolist (mod '(go-mode-hook)) (add-hook mod 'vmacs-lsp-hook))
(with-eval-after-load 'eglot
  ;; brew install llvm
  ;;clangd https://clangd.llvm.org/installation.html
  ;; ln -s ~/myproject/compile_commands.json ~/myproject-build/
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "/usr/local/opt/llvm/bin/clangd")))

(define-key evil-normal-state-map "gf" 'evil-jump-forward)
(define-key evil-normal-state-map "gb" 'evil-jump-backward)
(define-key evil-normal-state-map "gn" 'next-error)
(define-key evil-normal-state-map "gp" 'previous-error)
(vmacs-leader (kbd ",") 'evil-jump-backward)  ;space, 回到上一个书签
(vmacs-leader (kbd ".") 'evil-jump-forward)      ;space. 下一个书签

(define-key evil-motion-state-map "g." 'evil-jump-to-tag) ;对 xref-find-definitions 进行了包装
;; (define-key evil-motion-state-map "gr" 'lsp-find-references)
(define-key evil-motion-state-map "gR" 'eglot-rename)
(define-key evil-motion-state-map "gr" 'xref-find-references)
(define-key evil-motion-state-map "gc" 'eglot-find-declaration)
(define-key evil-normal-state-map "gi" 'eglot-find-implementation)
(define-key evil-motion-state-map "gt" 'eglot-find-typeDefinition)
(define-key evil-motion-state-map "gs" 'eglot-reconnect)
(define-key evil-normal-state-map "gh" 'eglot-code-actions)
(define-key evil-normal-state-map "gp" 'evil-project-find-regexp)
(define-key evil-normal-state-map "gP" 'project-or-external-find-file)
;;
;; ;; (define-key evil-motion-state-map "gd" 'evil-goto-definition);evil default,see evil-goto-definition-functions
;; (define-key evil-motion-state-map "gi" 'lsp-find-implementation)
;; (define-key evil-motion-state-map "gR" 'lsp-rename)
(defun evil-project-find-regexp( &optional string _pos)
  (interactive)
  (when current-prefix-arg (setq string (project--read-regexp)))
  (project-find-regexp (or string (regexp-quote (thing-at-point 'symbol)))))

(setq evil-goto-definition-functions
      '(evil-goto-definition-xref  evil-project-find-regexp evil-goto-definition-imenu evil-goto-definition-semantic evil-goto-definition-search))

(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep)     ;project-find-regexp
  (when (functionp 'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function 'xref-show-definitions-completing-read)
    (setq xref-show-xrefs-function 'xref-show-definitions-completing-read)))




(provide 'conf-tags)
