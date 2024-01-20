;;; -*- coding:utf-8 -*-

(setq eglot-confirm-server-initiated-edits nil)
(setq eglot-autoshutdown nil)
(setq eglot-sync-connect 0)
(setq eglot-events-buffer-size 0)
(setq eglot-extend-to-xref t)
;; :documentHighlightProvider 禁用高亮光标下的单词
(setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
(defun vmacs-eglot-organize-imports() (eglot-code-actions nil nil "source.organizeImports" t))
(setq codeium/metadata/api_key (auth-source-pick-first-password :host "codeium.com"))
(defun vmacs-lsp-hook()
  (eglot-ensure)
  (hs-minor-mode 1)
  (evil-collection-define-key 'normal 'eglot-mode-map "\C-t" #'embark-act)
  (evil-define-key 'normal 'local  "=" #'eglot-format-buffer)
  (evil-define-key 'normal 'local "gd" #'vmacs-find-def)
  (evil-define-key 'normal 'local "gR" #'eglot-rename)
  (evil-define-key 'normal 'local "gc" #'eglot-find-declaration)
  (evil-define-key 'normal 'local "gi" #'eglot-find-implementation)
  (evil-define-key 'normal 'local "gt" #'eglot-find-typeDefinition)
  (evil-define-key 'normal 'local "gs" #'eglot-reconnect)
  (evil-define-key 'normal 'local "gS" #'(lambda()(interactive)(call-interactively #'eglot-shutdown-all)(call-interactively #'eglot)))
  (evil-define-key 'normal 'local "gh" #'eglot-code-actions)
  ;; (unless (eq major-mode 'go-mode)      ;go 暂时用 goimports,no block ui
  ;; The depth of -10 places this before eglot's willSave notification,
  ;; so that that notification reports the actual contents that will be saved.
  (add-hook 'before-save-hook #'vmacs-eglot-organize-imports -9 t)
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  (require 'cape)
  ;; (remove-hook 'completion-at-point-functions 'eglot-completion-at-point)
  (setq-local completion-at-point-functions
              (list (cape-capf-super #'codeium-completion-at-point #'eglot-completion-at-point)))
  )

(dolist (mod '(python-mode-hook c++-mode-hook go-ts-mode-hook rust-ts-mode-hook c-mode-hook ))
  (add-hook mod #'vmacs-lsp-hook))

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

(define-key evil-normal-state-map "gp" #'evil-project-find-regexp)
(define-key evil-normal-state-map "gP" #'project-or-external-find-file)
(define-key evil-motion-state-map "g." #'evil-jump-to-tag) ;对 xref-find-definitions 进行了包装
;; (define-key evil-motion-state-map "gr" 'lsp-find-references)
(define-key evil-motion-state-map "gd" #'vmacs-find-def)
(define-key evil-motion-state-map "gr" #'xref-find-references)
;;
;; ;; (define-key evil-motion-state-map "gd" 'evil-goto-definition);evil default,see evil-goto-definition-functions
;; (define-key evil-motion-state-map "gi" 'lsp-find-implementation)
;; (define-key evil-motion-state-map "gR" 'lsp-rename)
(defun evil-project-find-regexp( &optional string _pos)
  (interactive)
  (when current-prefix-arg (setq string (project--read-regexp)))
  (project-find-regexp (or string (regexp-quote (thing-at-point 'symbol)))))

(defun vmacs-find-def()
  (interactive)
  (require 'eglot)
  (when (and eglot--managed-mode
             eglot--change-idle-timer)
    (cancel-timer eglot--change-idle-timer)
    (eglot--signal-textDocument/didChange)
    (setq eglot--change-idle-timer nil))
  (call-interactively #'evil-goto-definition))

(evil-add-command-properties #'vmacs-find-def :jump t)

(setq evil-goto-definition-functions
      '(evil-goto-definition-xref  evil-project-find-regexp evil-goto-definition-imenu  evil-goto-definition-search))

(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep)     ;project-find-regexp
  (when (functionp 'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)))




(provide 'conf-lsp)
