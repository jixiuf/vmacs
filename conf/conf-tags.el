;;; -*- coding:utf-8 -*-
(setq eglot-confirm-server-initiated-edits nil)
(setq eglot-autoshutdown nil)
(setq eglot-sync-connect 0)
(defun vmacs-lsp-hook()
  ;; (lsp-deferred)
  (eglot-ensure)
  ;; (add-hook 'before-save-hook #'lsp-organize-imports 10 t)
  ;; (add-hook 'before-save-hook #'lsp-format-buffer 20 t)
  (add-hook 'before-save-hook #'eglot-organize-imports 30 t)
  (add-hook 'before-save-hook #'eglot-format-buffer 20 t))

;; :documentHighlightProvider 禁用高亮光标下的单词
(setq eglot-ignored-server-capabilites '(:documentHighlightProvider))
(dolist (mod '(python-mode-hook c++-mode-hook go-mode-hook c-mode-hook ))
  (add-hook mod 'vmacs-lsp-hook))

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


(with-eval-after-load 'xref
  ;; ;; (define-key xref--xref-buffer-mode-map (kbd "j") #'xref-next-line)
  ;; ;; (define-key xref--xref-buffer-mode-map (kbd "k") #'xref-prev-line)
  ;; (define-key xref--xref-buffer-mode-map (kbd "r") #'xref-query-replace-in-results)
  ;; (define-key xref--xref-buffer-mode-map (kbd "TAB") #'xref-goto-xref)
  ;; (define-key xref--xref-buffer-mode-map (kbd "<return>")  #'xref-quit-and-goto-xref)
  ;; (define-key xref--xref-buffer-mode-map (kbd "RET")  #'xref-quit-and-goto-xref)

  (setq xref-show-xrefs-function 'completing-read-xref-show-defs)
  (setq xref-show-definitions-function 'completing-read-xref-show-defs)

  (defun completing-read-xref-make-collection (xrefs)
    "Transform XREFS into a collection for display via `completing-read'."
    (let ((collection nil))
      (dolist (xref xrefs)
        (with-slots (summary location) xref
          (let* ((line (xref-location-line location))
                 (file (xref-location-group location))
                 (candidate
                  (concat
                   (propertize
                    (concat
                     (file-name-nondirectory file)
                     (if (integerp line)
                         (format ":%d: " line)
                       ": "))
                    'face 'compilation-info)
                   summary)))
            (push `(,candidate . ,location) collection))))
      (nreverse collection)))

  (defun completing-xref-show-xrefs (fetcher alist)
    "Show the list of xrefs returned by FETCHER and ALIST via completing-read."
    ;; call the original xref--show-xref-buffer so we can be used with
    ;; dired-do-find-regexp-and-replace etc which expects to use the normal xref
    ;; results buffer but then bury it and delete the window containing it
    ;; immediately since we don't want to see it - see #2
    (let* ((xrefs (if (functionp fetcher)
                      ;; Emacs 27
                      (or (assoc-default 'fetched-xrefs alist)
                          (funcall fetcher))
                    fetcher))
           (buffer (xref--show-xref-buffer fetcher alist)))
      (quit-window)
      (let* ((orig-buf (current-buffer))
             (orig-pos (point))
             (cands (completing-read-xref-make-collection xrefs))
             (candidate (completing-read "xref: "  cands nil t ))
             done)
        (setq candidate (assoc candidate cands))
        (condition-case err
            (let* ((marker (xref-location-marker (cdr candidate)))
                   (buf (marker-buffer marker)))
              (with-current-buffer buffer
                (select-window
                 ;; function signature changed in
                 ;; 2a973edeacefcabb9fd8024188b7e167f0f9a9b6
                 (if (version< emacs-version "26.0.90")
                     (xref--show-pos-in-buf marker buf t)
                   (xref--show-pos-in-buf marker buf)))))
          (user-error (message (error-message-string err)))))
      ;; honor the contact of xref--show-xref-buffer by returning its original
      ;; return value
      buffer))

  (defun completing-read-xref-show-defs (fetcher alist)
    "Show the list of definitions returned by FETCHER and ALIST via completing-read.
Will jump to the definition if only one is found."
    (let ((xrefs (funcall fetcher)))
      (cond
       ((not (cdr xrefs))
        (xref-pop-to-location (car xrefs)
                              (assoc-default 'display-action alist)))
       (t
        (completing-xref-show-xrefs fetcher
                                    (cons (cons 'fetched-xrefs xrefs)
                                          alist))))))

  )


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
