;;; -*- coding:utf-8 -*-
;;; ETAG
;;如果要生成emacs 支持的tags 可以使用etags 和ExuberantCtags(ctags)
;;使用ctags 需要加-e选择表示生成emacs支持的tag文件
;;-R 应该是递归整个目录,它会自动根据文件类型判断哪些文件需要索引
;;  cd /path/to/my/project
;;  ctags -e myfile.cpp
;;一般使用"ctags -e -R /usr/src/linux" 就可以了(建议使用绝对路径)
;;可以加上-f 或者-o 指定输出文件(应该是-e 选项前), 如:
;;ctags -f /tmp/tags-file -e -R /usr/src/linux
;;
;;etags 的使用方法
;;find . -regex ".*\.\(h\|c\|cpp\)" -exec etags -a -o "/tmp/TAG" {} \;

;; * `M-.’ (‘find-tag’) – find a tag, that is, use the Tags file to look up a definition. If there are multiple tags in the project with the same name, use `C-u M-.’ to go to the next match.
;; * `M-*’ (‘pop-tag-mark’) – jump back

;; * ‘M-x tags-search’ – regexp-search through the source files indexed by a tags file (a bit like ‘grep’)
;; * ‘M-x tags-query-replace’ – query-replace through the source files indexed by a tags file
;; * `M-,’ (‘tags-loop-continue’) – resume ‘tags-search’ or ‘tags-query-replace’ starting at point in a source file
;; * ‘M-x tags-apropos’ – list all tags in a tags file that match a regexp
;; * ‘M-x list-tags’ – list all tags defined in a source file

;; ;; ;;; defined in ctags-update.el
;; (when (equal system-type 'windows-nt)
;;   (setq-default ctags-update-command (expand-file-name  "binw32/ctags.exe" user-emacs-directory)))
;; (add-hook 'csharp-mode-hook  'turn-on-ctags-auto-update-mode)


(setq lsp-print-performance t)
(setq-default lsp-keymap-prefix "C-M-s-l")
;; (setq lsp-auto-configure t)
;; (setq lsp-enable-indentation nil)
(autoload 'bm-bookmark-add "bm" "add bookmark")
(autoload 'bm-bookmark-remove "bm" "remove bookmark")

(require 'ccls)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'objc-mode-hook 'lsp)

(define-key evil-motion-state-map "g." 'evil-jump-to-tag) ;对 xref-find-definitions 进行了包装
(define-key evil-motion-state-map "gr" 'vmacs-xref-find-references)
(define-key evil-motion-state-map "gd" 'goto-definition)
(define-key evil-motion-state-map "gD" 'lsp-find-definition)
;; (define-key evil-motion-state-map "gt" 'helm-gtags-find-tag-and-symbol)
;; (define-key evil-motion-state-map "g/" 'helm-gtags-find-rtag)
;; (define-key evil-motion-state-map "gc" 'helm-gtags-find-tag-from-here)
(define-key evil-motion-state-map "gi" 'lsp-find-implementation)
(define-key evil-normal-state-map "gi" 'lsp-find-implementation)
(define-key evil-motion-state-map "gc" 'lsp-find-implementation)
(define-key evil-motion-state-map "gR" 'lsp-rename)

(defun vmacs-xref-find-references()
  (interactive)
  (if current-prefix-arg
      (call-interactively 'xref-find-references)
    (lsp-find-references)
    ))
;; (condition-case nil
;;     (lsp-find-references)
;;   (error
;;    ))

(with-eval-after-load 'xref
  ;; (define-key xref--xref-buffer-mode-map (kbd "j") #'xref-next-line)
  ;; (define-key xref--xref-buffer-mode-map (kbd "k") #'xref-prev-line)
  (define-key xref--xref-buffer-mode-map (kbd "r") #'xref-query-replace-in-results)
  (define-key xref--xref-buffer-mode-map (kbd "TAB") #'xref-goto-xref)
  (define-key xref--xref-buffer-mode-map (kbd "<return>")  #'xref-quit-and-goto-xref)
  (define-key xref--xref-buffer-mode-map (kbd "RET")  #'xref-quit-and-goto-xref)


  )


;; (with-eval-after-load 'helm-etags-plus
;;   (add-hook 'helm-etags-plus-before-jump-hook '(lambda()(bm-bookmark-add nil nil t)))
;;   (add-hook 'helm-etags-plus-after-jump-hook '(lambda()(bm-bookmark-remove)))
;;   )

;; (with-eval-after-load 'helm-gtags
;;   ;; (add-hook 'helm-for-files-preferred-list 'helm-source-gtags-files t)
;;   (add-hook 'helm-gtags-goto-line-before-hook '(lambda()(bm-bookmark-add nil nil t)))
;;   (add-hook 'helm-gtags-quit-or-no-candidates-hook 'bm-bookmark-remove) ;如果根本没跳转， 则没有必要在当行行加bookmark
;;   (add-hook 'helm-gtags-goto-line-after-hook 'bm-bookmark-remove) ;after jump 如果本行有bookmark ,remove it

;;   )

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
