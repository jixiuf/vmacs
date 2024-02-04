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
  ;; The depth of -10 places this before eglot's willSave notification,
  ;; so that that notification reports the actual contents that will be saved.
  (add-hook 'before-save-hook #'vmacs-eglot-organize-imports -9 t)
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  ;; (add-hook 'completion-at-point-functions 'codeium-completion-at-point -10 t)
  ;; (add-hook 'completion-at-point-functions
  ;;            (cape-capf-super #'eglot-completion-at-point #'codeium-completion-at-point   ) -100 t )
  )

(dolist (mod '(python-mode-hook c++-mode-hook go-ts-mode-hook rust-ts-mode-hook c-mode-hook ))
  (add-hook mod #'vmacs-lsp-hook))

(with-eval-after-load 'eglot
  (defvar-keymap  lsp-g-map :parent vmacs-g-mode-map
                  "=" #'eglot-format
                  "R" #'eglot-rename
                  "x" #'xref-find-references
                  "c" #'eglot-find-declaration
                  "i" #'eglot-find-implementation
                  "t" #'eglot-find-typeDefinition
                  "s" #'eglot-reconnect
                  "h" #'eglot-code-actions)
  ;; brew install llvm
  ;;clangd https://clangd.llvm.org/installation.html
  (define-key eglot-mode-map (kbd "C-c g") lsp-g-map)
  (define-key eglot-mode-map (kbd "C-M-\\") #'eglot-format)
  ;; ln -s ~/myproject/compile_commands.json ~/myproject-build/
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "/usr/local/opt/llvm/bin/clangd")))


;; (evil-add-command-properties #'vmacs-find-def :jump t)

;; (setq evil-goto-definition-functions
;;       '(evil-goto-definition-xref  evil-project-find-regexp evil-goto-definition-imenu  evil-goto-definition-search))

(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep)     ;project-find-regexp
  (when (functionp 'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)))


;; (with-eval-after-load 'dape
;;   (define-key dape-active-mode (kbd "M-n") 'dape-next)
;;   (define-key dape-active-mode (kbd "M-j") 'dape-next)
;;   (define-key dape-active-mode (kbd "M-i") 'dape-step-in)
;;   (define-key dape-active-mode (kbd "M-h") 'dape-step-out)
;;   (define-key dape-active-mode (kbd "C-c o") 'dape-step-out)
;;   (define-key dape-active-mode (kbd "M-c") 'dape-continue)
;;   (define-key dape-active-mode (kbd "C-c c") 'dape-continue)
;;   (define-key dape-active-mode (kbd "M-r") 'dape-restart)
;;   (define-key dape-active-mode (kbd "C-c v") 'dape-repl)
;;   (define-key dape-active-mode (kbd "C-c q") 'dape-quit)
;;   (define-key dape-active-mode (kbd "C-c p") 'dape-pause)
;;   (define-key dape-active-mode (kbd "C-c w") 'dape-watch-dwim)
;;   (define-key dape-active-mode (kbd "C-c e") 'dape-breakpoint-expression)
;;   (define-key dape-active-mode (kbd "C-c C-c") 'dape-breakpoint-toggle)
;;   (define-key dape-active-mode (kbd "C-c b") 'dape-breakpoint-remove-all)

;;   )


(provide 'conf-lsp)
