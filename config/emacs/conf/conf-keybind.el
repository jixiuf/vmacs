;;; -*- lexical-binding: t; -*-
(setq widen-automatically nil)         ;for goto-line

(defvar-keymap  g-mode-map             ;meow: g
  "4" #'query-replace ;space g4
  "5" #'re-builder                                 ;;query-replace-regexp
  "g" #'vmacs-goto-line
  "T" #'consult-grep
  "t" #'consult-ripgrep
  "e" #'grep
  "w" (vmacs-defun consult-ripgrep-default (consult-ripgrep default-directory))
  "x" (vmacs-defun consult-ripgrep-root-symbol (consult-ripgrep(vc-root-dir)  (concat "\\b" (thing-at-point 'symbol) "\\b")))
  "X" #'consult-ripgrep-root-symbol
  "s" (vmacs-defun consult-ripgrep-default-symbol (consult-ripgrep default-directory (concat "\\b" (thing-at-point 'symbol) "\\b")))
  "/" #'consult-focus-lines
  "z" #'consult-hide-lines
  "r" #'revert-buffer
  "i" #'meow-insert
  "n" #'next-error
  "p" #'previous-error
  "b" #'pop-global-mark
  "u" #'upcase-dwim
  "U" #'downcase-dwim
  "m" #'push-mark-command
  "P" #'project-or-external-find-file
  "d" #'xref-find-definitions
  "," #'goto-last-change
  "." #'goto-last-change-reverse
  "f" #'gptel-rewrite
  )
(global-set-key (kbd "C-c G") g-mode-map)
(global-set-key (kbd "C-c g") g-mode-map)
(defvar-keymap  m-mode-map              ;meow: m
  "4" #'toggle-truncate-lines
  "f" #'narrow-to-defun
  "h" #'mark-defun
  "v" #'rectangle-mark-mode
  "q" #'fill-paragraph
  "s" #'gt-do-translate
  "," #'pop-to-mark-command
  "t" #'org-capture
  "z" #'hs-toggle-hiding		;
  "m" #'vmacs-meow-grab-set-mark                ;C-@
  "r" #'meow-swap-grab
  "y" #'meow-sync-grab
  "g" #'vmacs-meow-grab
  "b" #'meow-grab
  "n" #'narrow-to-region
  "w" #'vmacs-widen)
(global-set-key (kbd "C-c m") m-mode-map)


(with-eval-after-load 'smerge-mode
  (define-key g-mode-map "v" smerge-basic-map))

(defvar-keymap  vmacs-normal-mode-map
  "="  #'meow-indent
  "i" #'meow-insert
  "G"  #'vmacs-goto-line
  "g" g-mode-map
  "m" m-mode-map
  "n"  #'meow-search
  "N"  #'meow-search-reverse
  "/"  #'isearch-forward-regexp
  "z"   #'meow-pop-selection)

(global-set-key (kbd "C-c N") vmacs-normal-mode-map)
(defvar-keymap  vmacs-motion-mode-map
  "j"  #'meow-next
  "k"  #'meow-prev
  "h"  #'meow-left
  "l"  #'meow-right
  "G"  #'vmacs-goto-line
  "g"  g-mode-map
  "m"  m-mode-map
  "/"  #'isearch-forward-regexp
  "n"  #'meow-search
  "N"  #'meow-search-reverse
  ":" #'viper-ex
  "z"   #'meow-pop-selection)
(global-set-key (kbd "C-c M") vmacs-motion-mode-map)


(global-set-key (kbd "C--")   #'(lambda() (interactive)(insert "_")))
(defun vmacs-isearch-insert_()
  (interactive)
  (isearch-printing-char ?_))
(define-key isearch-mode-map  (kbd "C--")   'vmacs-isearch-insert_)

(define-key isearch-mode-map  (kbd "M-n")   'isearch-forward-thing-at-point)
(define-key isearch-mode-map  (kbd "M-p")   'consult-isearch-history)
(define-key isearch-mode-map  (kbd "C-f")   'isearch-yank-word-or-char)
(define-key isearch-mode-map  (kbd "C-,")   'isearch-beginning-of-buffer)
(define-key isearch-mode-map  (kbd "C-.")   'isearch-end-of-buffer)
(define-key isearch-mode-map  (kbd "C-t")   'isearch-toggle-regexp)
(define-key isearch-mode-map  (kbd "C-e")   'isearch-edit-string)
(setq isearch-message-prefix-add "(C-t:rx C-e:edit M-c:case)")

(add-hook 'isearch-mode-end-hook
          (lambda()
            (when isearch-success ; 只有在搜索成功时
              (set-mark isearch-other-end)
              (activate-mark))))
(add-hook 'isearch-mode-hook (lambda()(require 'xref)
                               (if (equal emacs-major-version 30)
                                   (xref--push-markers (current-buffer) (point) )
                                 (xref--push-markers (current-buffer) (point) (selected-window))
                                 )))
(setq isearch-lazy-count t)
(setq lazy-highlight-cleanup nil)
(setq isearch-wrap-pause 'no)
(vmacs-leader "n" #'xref-go-back)
(with-eval-after-load 'xref
  ;; for clean up second-selection set by vmacs-meow-grab-set-mark
  (add-hook 'xref-after-jump-hook #'meow--cancel-second-selection))
;; (vmacs-leader "," #'pop-global-mark)
;; (advice-add 'pop-global-mark :before #'deactivate-mark)

(setq shell-command-buffer-name "*Messages*")
(setq shell-command-default-error-buffer "*Messages*")
(setq shell-command-dont-erase-buffer nil)
(defun vmacs-isearch-insert-shift1()
  (interactive)
  (isearch-printing-char ?\!))
(global-set-key (kbd "C-1")   (lambda()
                                (interactive)
                                (let ((shell-file-name "zsh")
                                      (shell-command-switch "-ic"))
                                  (cond
                                   ((eq major-mode 'dired-mode)
                                    (call-interactively 'dired-do-shell-command))
                                   (t
                                    (if current-prefix-arg
                                        (if (equal current-prefix-arg '(16))
                                            (emamux:send-command)
                                          (setq current-prefix-arg nil)
                                          (emamux:send-command))
                                      (if (region-active-p)
                                          (shell-command-on-region (region-beginning) (region-end)
                                                                   (read-shell-command "Shell command on region: ")
                                                                   t t)
                                        (call-interactively 'shell-command))))))))

(define-key isearch-mode-map  (kbd "C-1")   'vmacs-isearch-insert-shift1)

(with-eval-after-load 'isearch (define-key isearch-mode-map [escape] 'isearch-abort))


;; (global-set-key  (kbd "s-a") 'evil-mark-whole-buffer) ;mac Cmd+a
;; (global-set-key  (kbd "s-t") 'shell-toggle-cd) ;mac Cmd+a
;; (global-set-key  (kbd "s-s") 'evil-write-all)
(global-set-key  (kbd "s-z") 'undo)
(global-set-key  (kbd "s-r") 'compile-dwim-compile)
(global-set-key  (kbd "C-\\") 'hippie-expand)
(global-set-key  (kbd "s-1") 'delete-other-windows)
(global-set-key  (kbd "s-M-1") 'delete-other-windows) ;hyper-1
(global-set-key  (kbd "s-M-2") 'vmacs-split-window-vertically) ;hyper-2
(global-set-key  (kbd "s-2") 'vmacs-split-window-vertically)
(global-set-key  (kbd "s-M-3") 'vmacs-split-window-horizontally) ;hyper-2
(global-set-key  (kbd "s-3") 'vmacs-split-window-horizontally)
(with-eval-after-load 'cus-edit (define-key custom-mode-map "n" nil))
(global-set-key (kbd "s-M-i")  'project-find-file)
(global-set-key (kbd "s-i")  'project-find-file)
(global-set-key (kbd "C-c i")  'project-or-external-find-file)
(global-set-key (kbd "s-I")  'project-or-external-find-file)
(global-set-key [S-insert] 'mouse-yank-primary)

;; (global-set-key  (kbd "s-M-u") 'vmacs-prev-buffer)
;; (global-set-key  (kbd "s-u") 'vmacs-prev-buffer)
(global-set-key  (kbd "s-M-u") 'vmacs-undo-kill-buffer)
(global-set-key  (kbd "C-c u") 'vmacs-undo-kill-buffer)
(global-set-key  (kbd "C-c wm") 'gnus)

(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "s-w") 'vmacs-kill-buffer-dwim)
(global-set-key (kbd "C-x K") 'vmacs-kill-buffer-dwim)
(global-set-key (kbd "C-c C-v") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-M-w") 'vmacs-kill-buffer-dwim)
(global-set-key (kbd "M-o") 'toggle-camelize)
(setq golden-ratio-scroll-highlight-flag nil)
(autoload 'golden-ratio-scroll-screen-up "golden-ratio-scroll-screen" "" t)
(autoload 'golden-ratio-scroll-screen-down "golden-ratio-scroll-screen" "" t)
(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up) ;C-v
;; (global-set-key "\C-u" 'gold-ratio-scroll-screen-up)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down) ;M-v
(global-set-key  (kbd "C-2") 'meow-set-mark)
;; (global-set-key  (kbd "C-3") 'rectangle-mark-mode)
(global-set-key  (kbd "C-4") 'vmacs-meow-grab)
(global-set-key  (kbd "C-M-d") 'backward-kill-sexp)
(global-set-key (kbd "C-c C-k") 'compile-dwim-compile)
(global-set-key (kbd "s-M-r") 'compile-dwim-run)
(global-set-key (kbd "C-c C-r") 'compile-dwim-run)
(global-set-key (kbd "C-c r") 'compile-dwim-run) ;space r
(global-set-key  (kbd "M-,") 'scroll-other-window-down)
(global-set-key  (kbd "M-.") 'scroll-other-window)
(global-set-key  (kbd "M-w") 'vmacs-kill-ring-save)
(global-set-key  (kbd "C-d") 'vmacs-delete-char)


(vmacs-leader (kbd "t") 'org-agenda)   ;列出 todo list 等
(vmacs-leader "b" 'meow-last-buffer)
(vmacs-leader  "fg" #'vmacs-ai)


(vmacs-leader  "fm" (vmacs-defun switch-to-message
                      (if (equal (buffer-name) "*Messages*")
                          (switch-to-buffer (other-buffer))
                        (switch-to-buffer "*Messages*")
                        (display-line-numbers-mode -1)
                        (setq truncate-lines nil)
                        (goto-char (point-max)))))


(global-set-key (kbd "C-x C-u") #'vundo)
(vmacs-leader (kbd "l") 'ibuffer)
(vmacs-leader (kbd "j") 'dired-jump)
(vmacs-leader (kbd "k") 'dired-jump)
;; (vmacs-leader "k" ctl-x-r-map)
;; (vmacs-leader (kbd "(") 'kmacro-start-macro) ;C-x(
;; (vmacs-leader (kbd ")") 'kmacro-end-macro) ;C-x
(vmacs-leader (kbd "$") 'toggle-truncate-lines)
(vmacs-leader (kbd "wc") 'toggle-case-fold)
(vmacs-leader (kbd "wl") #'git-link)
;; (global-set-key  (kbd "s-h") 'vmacs-undo-kill-buffer)
(vmacs-leader "p" 'list-packages)

(defun vmacs-bury-boring-windows ()
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))
    (minibuffer-keyboard-quit))
    (bury-boring-windows))

(advice-add 'keyboard-quit :before #'vmacs-bury-boring-windows)

;; (global-set-key (kbd "C-;") #'vmacs-meow-iedit)
(global-set-key (kbd "C-;") #'query-replace-iedit-mode)

(global-set-key (kbd "C-c C-c") #'exit-recursive-edit) ;query-replace C-r临时退出replace 后，可C-cC-c 继续replace

;; https://emacs.stackexchange.com/questions/80484/query-replace-ignore-events-not-binded-in-query-replace-map
(defvar vmacs-do-nothing-map
  (let ((map (make-keymap)))
    (set-char-table-range (nth 1 map) t 'ignore)
    map))
(set-keymap-parent query-replace-map vmacs-do-nothing-map)
(define-key query-replace-map "g" 'automatic) ;old ! replace all automatic
(define-key query-replace-map "p" 'backup)
(define-key query-replace-map "c" 'act)     ;old y
(define-key query-replace-map "\C-e" 'edit) ;临时退出
(setq query-replace-read-from-default #'vmacs-query-replace-read-from-default)
(defvar vmacs-query-replace-read-from-def nil)
(defun vmacs-query-replace-read-from-default()
  (if (eq this-command 'vmacs-replace-all)
      vmacs-query-replace-read-from-def
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (thing-at-point 'symbol))))

(with-eval-after-load 'man
  (set-keymap-parent Man-mode-map meow-normal-state-keymap)
  (define-key Man-mode-map "q" #'save-buffers-kill-terminal)
  (define-key Man-mode-map (kbd "C-c Mq") 'save-buffers-kill-terminal)
  (define-key Man-mode-map (kbd "n") 'meow-search)
  (define-key Man-mode-map (kbd "N") 'meow-search-reverse)
  (define-key Man-mode-map (kbd "s-M-w") 'save-buffers-kill-terminal)
  (define-key Man-mode-map (kbd "C-x K") 'save-buffers-kill-terminal))

(defun vmacs-tui(&optional f)
  ;; https://github.com/benjaminor/kkp for kitty keyboard protocol
  ;; 支持terminal下使用ctrl-;的协议， alacritty/kitty等支持
  (xterm-mouse-mode 1)
  (setq mouse-wheel-scroll-amount
        '(10 ((shift) . hscroll)
             ((meta) . nil)
             ((control meta) . global-text-scale)
             ((control) . text-scale)))
  
  (global-kkp-mode)
  (require 'clipetty)
  (global-clipetty-mode))
(add-hook 'tty-setup-hook #'vmacs-tui)

(provide 'conf-keybind)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-iterm2.el ends here.
