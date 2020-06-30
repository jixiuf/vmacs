;; ;;; -*- coding:utf-8 -*-
;; ;; (setq helm-idle-delay 0.3)
;; ;; (setq helm-input-idle-delay 0)
;; (setq-default
;;  helm-locate-command (cl-case system-type
;;                        ('gnu/linux (concat user-emacs-directory "bin/locate.sh %s %s"))
;;                        ;; "locate  %s -e -a %s"
;;                        ('berkeley-unix "locate %s %s")
;;                        ('windows-nt "es %s %s")
;;                        ('darwin (concat user-emacs-directory "bin/locate.sh %s %s")) ;mdfind -name %s %s
;;                        (t "locate %s %s"))
;;  helm-mode-reverse-history t
;;  helm-echo-input-in-header-line t
;;  helm-case-fold-search t;nil=case sensitive
;;  case-fold-search t ;nil=case sensitive //space cc toggle it
;;  helm-buffer-max-length 60
;;  helm-buffers-fuzzy-matching t
;;  helm-recentf-fuzzy-match t
;;  helm-mode-fuzzy-match t
;;  ;; helm-locate-fuzzy-match t  ;mdfind doesnot support fuzzy match
;;  helm-m-x-fuzzy-match t
;;  helm-semantic-fuzzy-match t
;;  helm-imenu-fuzzy-match t
;;  helm-ls-git-status-command  'magit-status-internal
;;  helm-grep-save-buffer-name-no-confirm t
;;  helm-ls-git-fuzzy-match t
;;  helm-apropos-fuzzy-match t
;;  helm-lisp-fuzzy-completion t
;;  eshell-hist-ignoredups t
;;  helm-eshell-hist-ignoredups t
;;  helm-ff-transformer-show-only-basename nil
;;  helm-adaptive-history-file (concat user-emacs-directory "cache/helm-adaptive-history")
;;  helm-ff-no-preselect t
;;  helm-for-files-preferred-list '(
;;                                  helm-source-buffers-list
;;                                  ;; helm-source-ido-virtual-buffers
;;                                  helm-source-recentf
;;                                  helm-source-files-in-current-dir
;;                                  ;; helm-source-bookmarks
;;                                  helm-source-file-cache
;;                                  ;; helm-source-files-in-all-dired
;;                                  ;; helm-source-joseph-filelist
;;                                  ;; helm-source-locate
;;                                  ;; helm-source-ls-git
;;                                  ;; helm-source-mac-spotlight
;;                                  helm-source-buffer-not-found
;;                                  )
;;  helm-ff-skip-boring-files t
;;  helm-ff-newfile-prompt-p nil
;;  helm-ff-auto-update-initial-value t
;;  helm-ff-file-name-history-use-recentf t
;;  ;; helm-split-window-default-side 'above
;;  ;; helm-display-buffer-default-size 1
;;  ;; helm-autoresize-min-height 6
;;  helm-always-two-windows t)
;; ;; add --nocolor ,the default color is bad for helm selection


;; ;; (when (executable-find "ack")
;; ;;   (setq-default helm-grep-default-command "ack -hn --no-group --no-color %e %p %f"
;; ;;                 helm-grep-default-recurse-command "ack -h --no-group --no-color %e %p %f"))

;; (fset 'describe-bindings 'helm-descbinds)
;; (autoload 'helm-occur "helm-regexp" "" t nil)
;; (autoload 'helm-semantic-or-imenu "helm-semantic" "" t nil)
;; (autoload 'helm-m-x "helm-command" "" t nil)
;; (autoload 'helm-all-mark-rings "helm-ring" "" t nil)
;; (autoload 'helm-show-kill-ring "helm-ring" "" t nil)
;; (autoload 'helm-find-files "helm-files" "" t nil)
;; (autoload 'helm-multi-files "helm-files" "" t nil)
;; (autoload 'helm-locate "helm-locate" "" t nil)
;; (autoload 'helm-list-emacs-process "helm-sys" "" t nil)
;; (autoload 'helm-source-ls-git "helm-ls-git" "" nil nil)
;; (autoload 'helm-lisp-completion-at-point "helm-elisp" "" t nil)

;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>spc") 'helm-multi-files)
;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>ff") 'helm-find-files)
;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>fh") 'helm-find-files)
;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>fl") 'helm-locate)
;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>fg") 'helm-ls-git-ls)
;; (define-key evil-normal-state-map "sl" 'helm-locate)

;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>?") 'helm-descbinds)
;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>wi") 'helm-semantic-or-imenu)
;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>b") 'helm-resume)
;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>wy") 'helm-all-mark-rings)
;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>wp") 'helm-list-emacs-process)

;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>;") 'helm-m-x)
;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>；") 'helm-m-x)
;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>g") 'helm-search)
;; ;; (autoload 'helm-apropos "helm-elisp" "" t nil)
;; ;; (define-key-lazy help-mode-map (kbd "a")         'helm-apropos "help-mode")
;; ;; (global-set-key  [remap apropos-command] 'helm-apropos) ;c-ha f1-a


;; (global-set-key "\m-x" 'helm-m-x)

;; (setq-default helm-multi-files-toggle-locate-binding "m-l") ;helm-multi-files 时按此键切到helm-locate
;; ;;
;; ;; lisp complete.
;; (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
;; (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point)



;; ;; from browse-kill-ring.el
;; (defadvice yank-pop (around kill-ring-browse-maybe (arg) activate)
;;   "if last action was not a yank, run `browse-kill-ring' instead."
;;   ;; yank-pop has an (interactive "*p") form which does not allow
;;   ;; it to run in a read-only buffer. we want browse-kill-ring to
;;   ;; be allowed to run in a read only buffer, so we change the
;;   ;; interactive form here. in that case, we need to
;;   ;; barf-if-buffer-read-only if we're going to call yank-pop with
;;   ;; ad-do-it
;;   (interactive "p")
;;   (if (not (eq last-command 'yank))
;;       (helm-show-kill-ring)
;;     (barf-if-buffer-read-only)
;;     ad-do-it))



;; (with-eval-after-load 'helm
;;   ;; (require 'helm-adaptive)
;;   ;; (helm-adaptive-mode 1)
;;   ;; (helm-autoresize-mode 1)
;;   ;; (set-face-attribute 'helm-selection nil
;;   ;;                     :inherit nil
;;   ;;                     ;; 1. box selected
;;   ;;                     ;; :background "#004a5d" :foreground "white"
;;   ;;                     ;; :box '(:color "cyan" :line-width -1)
;;   ;;                     ;; :underline nil
;;   ;;                     ;; 2. darker background
;;   ;;                     :inverse-video nil
;;   ;;                     :foreground nil
;;   ;;                     :background (color-darken-name (face-background 'default) 10)
;;   ;;                     )

;;   (define-key helm-map  (kbd "c-2") 'helm-toggle-visible-mark);;mark m-m
;;   ;; (define-key helm-map  [?\h-m] 'helm-maybe-exit-minibuffer);;return
;;   (define-key helm-map (kbd "<tab>")        'helm-select-action)


;;   ;; (define-key helm-map (kbd "c-r") 'helm-execute-persistent-action);;默认是c-z
;;   (define-key helm-map (kbd "c-s") 'helm-next-line) ;;
;;   (define-key helm-map (kbd "c-r") 'helm-previous-line) ;;
;;   ;; (define-key helm-map (kbd "c-e") 'helm-execute-persistent-action); default c-j

;;   ;; (define-key helm-map (kbd "c-k") 'helm-previous-source)
;;   (define-key helm-map (kbd "c-o") 'helm-next-source)
;;   (define-key helm-map (kbd "`")        'helm-select-action)
;;   (define-key helm-map (kbd "c-c c")        'toggle-case-fold)
;;   (define-key helm-map (kbd "c-,")        'helm-beginning-of-buffer)
;;   (define-key helm-map (kbd "c-.")        'helm-end-of-buffer)
;;   (define-key helm-map (kbd "c-e")        'vmacs-helm-magic-eol)



;;   ;; use default-as-input in grep
;;   (add-to-list 'helm-sources-using-default-as-input 'helm-source-grep)
;;   (add-to-list 'helm-sources-using-default-as-input 'helm-source-grep-ag)

;;   )

;; (with-eval-after-load 'helm-buffers

;;   (define-key helm-buffer-map (kbd "c-j")       'vmacs-helm-find-file-select-text) ;make it like c-j in ido
;;   (define-key helm-buffers-ido-virtual-map (kbd "c-j") 'vmacs-helm-find-file-select-text)

;;   (define-key helm-buffers-ido-virtual-map (kbd "c-[ [ a i") 'helm-ff-run-grep) ;c-3 for iterm2 keymap
;;   (define-key helm-buffer-map (kbd "m-s")       'helm-buffers-run-multi-occur)
;;   (define-key helm-buffer-map (kbd "c-3")       'helm-buffer-run-zgrep)
;;   (define-key helm-buffer-map (kbd "c-[ [ a i") 'helm-buffer-run-zgrep) ;c-3 for iterm2 keymap
;;   (define-key helm-buffer-map (kbd "c-s")       'helm-next-line)
;;   (define-key helm-buffer-map (kbd "c-r") 'helm-previous-line))

;; (with-eval-after-load 'helm-files
;;   ;; (require 'helm-ls-git)
;;   ;; (unless helm-source-ls-git
;;   ;;   (setq helm-source-ls-git (helm-make-source "git files" 'helm-ls-git-source)))
;;   ;; (require 'joseph-helm-filelist)


;;   (define-key helm-find-files-map (kbd "c-s")       'helm-next-line)

;;   (define-key helm-find-files-map (kbd "c-3")       'helm-ff-run-grep)
;;   (define-key helm-find-files-map (kbd "c-[ [ a i")       'helm-ff-run-grep) ;c-3 for iterm2 keymap
;;   (define-key helm-find-files-map (kbd "c-4")       'helm-ff-run-zgrep)
;;   (define-key helm-find-files-map (kbd "c-[ [ a j")       'helm-ff-run-zgrep) ;c-3 for iterm2 keymap

;;   (define-key helm-find-files-map (kbd "c-[ [ a a") 'helm-ff-run-toggle-auto-update) ;c-backspace

;;   (define-key helm-find-files-map (kbd "c-r") 'helm-previous-line) ;;

;;   (define-key helm-find-files-map (kbd "c-d") 'vmacs-helm-magic-delete-char) ;;
;;   (define-key helm-find-files-map (kbd "c-j") 'vmacs-helm-find-file-select-text) ;;
;;   (define-key helm-find-files-map (kbd "<return>") 'vmacs-helm-ido-exit-minibuffer)
;;   (define-key helm-find-files-map (kbd "<ret>")      'vmacs-helm-ido-exit-minibuffer)
;;   (define-key helm-generic-files-map (kbd "c-d") 'vmacs-helm-magic-delete-char) ;;
;;   (define-key helm-generic-files-map (kbd "c-j") 'vmacs-helm-find-file-select-text) ;;

;;   (define-key helm-generic-files-map (kbd "c-s")       'helm-next-line)
;;   (define-key helm-generic-files-map (kbd "c-r") 'helm-previous-line) ;;
;;   (define-key helm-generic-files-map (kbd "c-3")       'helm-ff-run-grep)
;;   (define-key helm-generic-files-map (kbd "c-[ [ a i")       'helm-ff-run-grep) ;c-3 for iterm2 keymap
;;   (define-key helm-generic-files-map (kbd "c-4")       'helm-ff-run-zgrep)
;;   (define-key helm-generic-files-map (kbd "c-[ [ a j")       'helm-ff-run-zgrep) ;c-3 for iterm2 keymap

;;   )


;; (with-eval-after-load 'helm-grep
;;   (define-key helm-grep-mode-map (kbd "c-o") nil)
;;   (define-key helm-grep-map (kbd "c-c c-c") 'helm-grep-run-save-buffer))
;; (with-eval-after-load 'grep (define-key grep-mode-map (kbd "c-o") nil))
;; (with-eval-after-load 'helm-mode
;;   (helm-mode 1)
;;   ;; (add-to-list 'helm-completing-read-handlers-alist '(ibuffer-find-file . ido))
;;   ;; (add-to-list 'helm-completing-read-handlers-alist '(switch-to-buffer . ido))
;;   ;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . ido))
;;   )

;; (run-with-idle-timer 3 nil '(lambda()
;;                               (require 'helm-config)
;;                               (require 'helm)
;;                               (require 'helm-mode)
;;                               (require 'helm-grep)
;;                               (require 'helm-ls-git)
;;                               (require 'helm-locate)))
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;; 隐藏helm buffer ，以避免干扰buffer list等
;; ;; (defun vmacs-helm-hide-buffer()
;; ;;   (unless (helm-alive-p)
;; ;;     (when (and  (string-match-p "^helm" (symbol-name last-command))
;; ;;                 (not (string-match-p "^helm" (symbol-name this-command))))
;; ;;       (let ((backup))
;; ;;         (dolist (helm-current-buffer (buffer-list))
;; ;;           (when (string-match-p "^\\*[hh]elm" (buffer-name helm-current-buffer))
;; ;;             (with-current-buffer helm-current-buffer
;; ;;               (setq backup (concat "  " (buffer-name helm-current-buffer)))
;; ;;               (when (get-buffer backup) (kill-buffer backup))
;; ;;               (rename-buffer backup))))))))

;; ;; (add-hook 'post-command-hook  'vmacs-helm-hide-buffer)
;; ;; (add-hook 'helm-after-action-hook 'vmacs-helm-hide-buffer)
;; ;; (add-hook 'helm-after-action-hook #'(lambda()(add-hook 'post-command-hook  'vmacs-helm-hide-buffer)))

;; ;; (defun vmacs-helm-resume(&optional arg)
;; ;;   (let ((backup))
;; ;;     (dolist (buf (buffer-list))
;; ;;       (when (string-match-p "^  \\*[hh]elm" (buffer-name buf))
;; ;;         (with-current-buffer buf
;; ;;           (setq backup (substring (buffer-name buf) 2 ))
;; ;;           (when (get-buffer backup) (kill-buffer backup))
;; ;;           (rename-buffer backup))))))

;; ;; (advice-add #'helm-resume :before #'vmacs-helm-resume)
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (defun vmacs-helm-hide-buffer()
;; ;;   (let ((backup))
;; ;;     (with-current-buffer (helm-buffer-get)
;; ;;       (setq backup (concat "  " (helm-buffer-get)))
;; ;;       (when (get-buffer backup) (kill-buffer backup))
;; ;;       (rename-buffer backup)
;; ;;       )
;; ;;     )
;; ;;   )
;; ;; (defun vmacs-helm-keyboard-quit()
;; ;;     "quit minibuffer in helm.
;; ;; if action buffer is displayed, kill it."
;; ;;   (interactive)
;; ;;   (with-helm-alive-p
;; ;;     (when (get-buffer-window helm-action-buffer 'visible)
;; ;;       (kill-buffer helm-action-buffer))
;; ;;     (helm-cleanup)
;; ;;     (vmacs-helm-hide-buffer)
;; ;;     (setq helm-exit-status 1)
;; ;;     (abort-recursive-edit)))
;; ;; (with-eval-after-load 'helm (define-key helm-map  (kbd "c-g") 'vmacs-helm-keyboard-quit))

;; ;; (advice-add #'helm-keyboard-quit :after #'vmacs-helm-hide-buffer)
(provide 'conf-helm)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-helm.el ends here.
