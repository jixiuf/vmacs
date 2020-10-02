;;; -*- coding:utf-8 -*-

(setq lsp-print-performance t)
(setq-default lsp-keymap-prefix "C-M-s-l")
;; (setq lsp-auto-configure t)
;; (setq lsp-enable-indentation nil)

(require 'ccls)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'objc-mode-hook 'lsp)

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
  (define-key xref--xref-buffer-mode-map (kbd "RET")  #'xref-quit-and-goto-xref)
  )



;; ;;; Enable helm-gtags-mode
;; (add-hook 'asm-mode-hook 'helm-gtags-mode)
;; (add-hook 'java-mode-hook 'helm-gtags-mode)

;; (add-hook 'python-mode-hook 'helm-gtags-mode)
;; ;; brew install global --with-exuberant-ctags --with-pygments
;; ;;  pip install pygments
;; ;; cp /usr/local/Cellar/global/6.3.2/share/gtags/gtags.conf /usr/local/etc/
;; ;; sed -i -e "s/pygments-parser\.la/pygments-parser.so/g" gtags.conf
;; ;; sed -i -e "s?#\!/usr/bin/python?#\!/usr/bin/env python?g" /usr/local/Cellar/global/6.3.2/share/gtags/script/pygments_parser.py
;; ;; test
;; ;; gtags --gtagsconf=/usr/local/etc/gtags.conf --gtagslabel=pygments --debug
;; ;; 如何让gtags 支持 python  golang ruby 等语言，
;; ;; (add-hook 'go-mode-hook 'helm-gtags-mode)

;; ;; customize
;; ;; (setq-default helm-gtags-path-style 'absolute)
;; ;; (setq-default helm-gtags-debug t)
;; ;; (setq debug-on-error t)
;; ;; (setq-default helm-gtags-ignore-case t)
;; ;; (setq helm-gtags-read-only t)
;; (setq-default helm-gtags-auto-update t)

;; ;; (setq helm-gtags-tag-location-alist
;; ;;       '(
;; ;;         ;; (c-mode  "/usr/include/" "/usr/kernel/")
;; ;;         (c++-mode  "/Volumes/data/repos/opencd/opencv-2.4.6.1/")))

;; ;; (global-set-key "\C-wE" 'helm-gtags-update-tags)
;; ;; (global-set-key "\M-*" 'helm-gtags-show-stack)

;; (defun vmacs-helm-gtags-mode-hook()
;;   ;; (local-set-key (kbd "M-.") 'helm-gtags-find-tag-and-symbol)
;;   ;; (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
;;   ;; (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
;;   (local-set-key [(control return)] 'helm-gtags-complete)
;;   (local-set-key (kbd "C-[ [ 1 c") (key-binding [(control return)])))   ; iterm map to ctrl-return

;; ;; key bindings
;; (add-hook 'helm-gtags-mode-hook 'vmacs-helm-gtags-mode-hook)
;; ;; ;;you can use  C-uM-. input symbol (default thing-at-point 'symbol)
;; (define-key global-map "\M-." 'goto-definition)

;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>wge") 'helm-gtags-update-tags)
;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>wgr") 'helm-gtags-find-rtag)
;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>wgp") 'helm-gtags-parse-file)
;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>wgi") 'helm-gtags-parse-file)
;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>we") 'ctags-update)
(provide 'conf-tags)
