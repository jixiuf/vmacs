;;; -*- coding:utf-8 -*-

;; (setq eglot-confirm-server-edits nil)
(setq eglot-autoshutdown nil)
(setq eglot-sync-connect 0)
(setq eglot-extend-to-xref t)
(setq eglot-events-buffer-config (list :size 0 :format 'full))
;; :documentHighlightProvider 禁用高亮光标下的单词
(setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
(defun vmacs-eglot-organize-imports() (eglot-code-actions nil nil "source.organizeImports" t))
;; (setq codeium/metadata/api_key (auth-source-pick-first-password :host "codeium.com"))
(autoload 'dape-breakpoint-toggle "dape" "" t)
(defun vmacs-lsp-hook()
  ;; (eglot-ensure)
  (when (eglot-managed-p)
    (eldoc-mode)
    (hs-minor-mode 1)
    ;; The depth of -10 places this before eglot's willSave notification,
    ;; so that that notification reports the actual contents that will be saved.
    (add-hook 'before-save-hook #'vmacs-eglot-organize-imports -9 t)
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
    (local-set-key (kbd "C-c C-c") 'dape-breakpoint-toggle)
    (local-set-key (kbd "C-c C-e") 'dape-eval)
    (local-set-key (kbd "C-c C-f") 'dape-continue)

    ;; (add-hook 'completion-at-point-functions 'codeium-completion-at-point -10 t)
    ;; (add-hook 'completion-at-point-functions
    ;;            (cape-capf-super #'eglot-completion-at-point #'codeium-completion-at-point   ) -100 t )
    ))
(add-hook 'eglot-managed-mode-hook #'vmacs-lsp-hook)

(dolist (mod '(python-mode-hook c++-mode-hook go-ts-mode-hook rust-ts-mode-hook c-mode-hook ))
  (add-hook mod #'eglot-ensure))

(with-eval-after-load 'eglot
  (defvar-keymap  lsp-g-map :parent g-mode-map
                  "=" #'eglot-format
                  "R" #'eglot-rename
                  "x" #'xref-find-references
                  "a" #'xref-find-apropos
                  "c" #'eglot-find-declaration
                  "i" #'eglot-find-implementation
                  "t" #'eglot-find-typeDefinition
                  "n" #'flymake-goto-next-error
                  "p" #'flymake-goto-prev-error
                  "l" #'flymake-show-project-diagnostics
                  "s" #'eglot-reconnect
                  "h" #'eglot-code-actions)
  ;; brew install llvm
  ;;clangd https://clangd.llvm.org/installation.html
  (define-key eglot-mode-map (kbd "C-c G") lsp-g-map)
  (define-key eglot-mode-map (kbd "C-M-\\") #'eglot-format)
  ;; ln -s ~/myproject/compile_commands.json ~/myproject-build/
  ;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "/usr/local/opt/llvm/bin/clangd"))
  )

(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep)     ;project-find-regexp
  (when (functionp 'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)))




(provide 'conf-lsp)
