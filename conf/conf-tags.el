;;; -*- coding:utf-8 -*-

(setq lsp-print-performance t)
(setq-default lsp-keymap-prefix "C-M-s-l")
;; (setq lsp-auto-configure t)
;; (setq lsp-enable-indentation nil)

(with-eval-after-load 'cc-mode
  (require 'ccls)
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'objc-mode-hook 'lsp))

(define-key evil-normal-state-map "gf" 'evil-jump-forward)
(define-key evil-normal-state-map "gb" 'evil-jump-backward)
(define-key evil-normal-state-map "gn" 'next-error)
(define-key evil-normal-state-map "gp" 'previous-error)
(evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>,") 'evil-jump-backward)  ;space, 回到上一个书签
(evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>.") 'evil-jump-forward)      ;space. 下一个书签

(define-key evil-motion-state-map "g." 'evil-jump-to-tag) ;对 xref-find-definitions 进行了包装
(define-key evil-motion-state-map "gr" 'lsp-find-references)
;; (define-key evil-motion-state-map "gd" 'evil-goto-definition);evil default,see evil-goto-definition-functions
;; (define-key evil-motion-state-map "gd" 'xref-find-references)
(define-key evil-motion-state-map "gi" 'lsp-find-implementation)
(define-key evil-normal-state-map "gi" 'lsp-find-implementation)
(define-key evil-motion-state-map "gc" 'lsp-find-implementation)
(define-key evil-motion-state-map "gR" 'lsp-rename)


(with-eval-after-load 'xref
  ;; (define-key xref--xref-buffer-mode-map (kbd "j") #'xref-next-line)
  ;; (define-key xref--xref-buffer-mode-map (kbd "k") #'xref-prev-line)
  (define-key xref--xref-buffer-mode-map (kbd "r") #'xref-query-replace-in-results)
  (define-key xref--xref-buffer-mode-map (kbd "TAB") #'xref-goto-xref)
  (define-key xref--xref-buffer-mode-map (kbd "<return>")  #'xref-quit-and-goto-xref)
  (define-key xref--xref-buffer-mode-map (kbd "RET")  #'xref-quit-and-goto-xref))

(provide 'conf-tags)
