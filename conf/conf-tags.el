;;; -*- coding:utf-8 -*-
(setq eglot-confirm-server-initiated-edits nil)
(setq eglot-autoshutdown nil)
(setq eglot-sync-connect 0)
(defun vmacs-lsp-hook()
  ;; (lsp-deferred)
  ;; (add-hook 'before-save-hook #'lsp-organize-imports 10 t)
  ;; (add-hook 'before-save-hook #'lsp-format-buffer 20 t)
  (add-hook 'before-save-hook #'eglot-organize-imports 30 t)
  (add-hook 'before-save-hook #'eglot-format-buffer 20 t))

;; :documentHighlightProvider 禁用高亮光标下的单词
(setq eglot-ignored-server-capabilites '(:documentHighlightProvider))
(dolist (mod '(python-mode-hook c++-mode-hook go-mode-hook c-mode-hook ))
  (add-hook mod 'eglot-ensure))
(dolist (mod '(go-mode-hook)) (add-hook mod 'vmacs-lsp-hook))

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
(define-key evil-motion-state-map "gi" 'eglot-find-implementation)
(define-key evil-motion-state-map "gt" 'eglot-find-typeDefinition)
(define-key evil-motion-state-map "gs" 'eglot-reconnect)
(define-key evil-normal-state-map "gh" 'eglot-code-actions)
;; ;; (define-key evil-motion-state-map "gd" 'evil-goto-definition);evil default,see evil-goto-definition-functions
;; (define-key evil-motion-state-map "gi" 'lsp-find-implementation)
;; (define-key evil-motion-state-map "gR" 'lsp-rename)
(setq evil-goto-definition-functions
      '(evil-goto-definition-xref evil-goto-definition-imenu evil-goto-definition-semantic evil-goto-definition-search))


(setq xref-show-xrefs-function 'xref--show-defs-minibuffer)
(setq xref-show-definitions-function 'xref--show-defs-minibuffer)


(defun eglot-organize-imports ()
  "Offer to execute code actions `source.organizeImports'."
  (interactive)
  (unless (eglot--server-capable :codeActionProvider)
    (eglot--error "Server can't execute code actions!"))
  (let* ((server (eglot--current-server-or-lose))
         (actions (jsonrpc-request
                   server
                   :textDocument/codeAction
                   (list :textDocument (eglot--TextDocumentIdentifier))))
         (action (cl-find-if
                  (jsonrpc-lambda (&key kind &allow-other-keys)
                    (string-equal kind "source.organizeImports" ))
                  actions)))
    (when action
      (eglot--dcase action
        (((Command) command arguments)
         (eglot-execute-command server (intern command) arguments))
        (((CodeAction) edit command)
         (when edit (eglot--apply-workspace-edit edit))
         (when command
           (eglot--dbind ((Command) command arguments) command
             (eglot-execute-command server (intern command) arguments))))))))



(provide 'conf-tags)
