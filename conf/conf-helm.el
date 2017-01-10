;;; -*- coding:utf-8 -*-
;; (setq helm-idle-delay 0.3)
;; (setq helm-input-idle-delay 0)

(eval-when-compile
  (require 'cl-lib)                         ;func case is defined in cl.el
  (require 'helm)                         ;func case is defined in cl.el
  (require 'helm-mode)                         ;func case is defined in cl.el
  (require 'helm-help)                         ;func case is defined in cl.el
  (require 'helm-buffers)                         ;func case is defined in cl.el
  (require 'helm-files)                         ;func case is defined in cl.el
  (require 'helm-locate)                         ;func case is defined in cl.el
  (require 'help)
  (require 'helm-ls-git)                         ;func case is defined in cl.el
  )

(setq-default
 helm-locate-command (cl-case system-type
                                    ('gnu/linux (concat user-emacs-directory "bin/locate.sh %s %s"))
                                    ;; "locate  %s -e -A %s"
                                    ('berkeley-unix "locate %s %s")
                                    ('windows-nt "es %s %s")
                                    ('darwin (concat user-emacs-directory "bin/locate.sh %s %s")) ;mdfind -name %s %s
                                    (t "locate %s %s"))
              helm-echo-input-in-header-line t
              helm-case-fold-search t;nil=case sensitive
              case-fold-search t ;nil=case sensitive //space cc toggle it
              helm-buffer-max-length 60
              helm-buffers-fuzzy-matching t
              helm-recentf-fuzzy-match t
              ;; helm-locate-fuzzy-match t  ;mdfind doesnot support fuzzy match
              helm-M-x-fuzzy-match t
              helm-semantic-fuzzy-match t
              helm-imenu-fuzzy-match t
              helm-ls-git-status-command  'magit-status-internal
              helm-grep-save-buffer-name-no-confirm t
              helm-ls-git-fuzzy-match t
              helm-apropos-fuzzy-match t
              helm-lisp-fuzzy-completion t
              eshell-hist-ignoredups t
              helm-eshell-hist-ignoredups t
              helm-ff-transformer-show-only-basename nil
              helm-adaptive-history-file (concat user-emacs-directory "cache/helm-adaptive-history")
              helm-ff-no-preselect t
              helm-for-files-preferred-list '(
                                              helm-source-buffers-list
                                              ;; helm-source-ido-virtual-buffers
                                              helm-source-recentf
                                              helm-source-files-in-current-dir
                                              ;; helm-source-bookmarks
                                              helm-source-file-cache
                                              ;; helm-source-files-in-all-dired
                                              ;; helm-source-joseph-filelist
                                              ;; helm-source-locate
                                              ;; helm-source-ls-git
                                              ;; helm-source-mac-spotlight
                                              helm-source-buffer-not-found
                                              )
              helm-ff-skip-boring-files t
              helm-ff-newfile-prompt-p nil
              helm-ff-auto-update-initial-value t
              helm-ff-file-name-history-use-recentf t
              ;; helm-split-window-default-side 'above
              ;; helm-display-buffer-default-size 1
              ;; helm-autoresize-min-height 6
              helm-always-two-windows t)
;; add --nocolor ,the default color is bad for helm selection
(defun helm-search(&optional arg)
  (interactive "P")
  (if (vc-find-root default-directory ".git")
      (call-interactively 'helm-grep-do-git-grep)
    (helm-do-grep-1 (list default-directory) (not arg))))


;; (defun helm-do-grep (&optional arg)
;;   (interactive "P")
;;   (helm-do-grep-1 (list default-directory) (not arg)))

;; (defun helm-do-my-grep-ag (arg)
;;   "Preconfigured helm for grepping with AG in `default-directory'.
;; With prefix-arg prompt for type if available with your AG version."
;;   (interactive "P")
;;   (require 'helm-files)
;;   (let ((helm-grep-ag-command))
;;     (if  (> (prefix-numeric-value current-prefix-arg) 1)
;;         (setq helm-grep-ag-command   "ag --line-numbers -S --hidden --color-match=\"10;35\" --depth 25 --nogroup %s %s %s")
;;       (setq helm-grep-ag-command   "ag --line-numbers -S --hidden --color-match=\"10;35\"  --nogroup %s %s %s")))
;;   (helm-grep-ag default-directory nil))

;; (when (executable-find "ack")
;;   (setq-default helm-grep-default-command "ack -Hn --no-group --no-color %e %p %f"
;;                 helm-grep-default-recurse-command "ack -H --no-group --no-color %e %p %f"))

(fset 'describe-bindings 'helm-descbinds)
(autoload 'helm-occur "helm-regexp" "" t nil)
(autoload 'helm-semantic-or-imenu "helm-semantic" "" t nil)
(autoload 'helm-M-x "helm-command" "" t nil)
(autoload 'helm-all-mark-rings "helm-ring" "" t nil)
(autoload 'helm-show-kill-ring "helm-ring" "" t nil)
(autoload 'helm-find-files "helm-files" "" t nil)
(autoload 'helm-multi-files "helm-files" "" t nil)
(autoload 'helm-locate "helm-locate" "" t nil)
(autoload 'helm-list-emacs-process "helm-sys" "" t nil)
(autoload 'helm-source-ls-git "helm-ls-git" "" nil nil)
(autoload 'helm-lisp-completion-at-point "helm-elisp" "" t nil)

(evil-leader/set-key "?" 'helm-descbinds)
(evil-leader/set-key "SPC" 'helm-multi-files)
(evil-leader/set-key "wl" 'helm-locate)
(evil-leader/set-key "wi" 'helm-semantic-or-imenu)
(evil-leader/set-key "wf" 'helm-locate)
(evil-leader/set-key "wo" 'helm-ls-git-ls)
(evil-leader/set-key "b" 'helm-resume)
(evil-leader/set-key "wy" 'helm-all-mark-rings)
(evil-leader/set-key "wp" 'helm-list-emacs-process)

;; (autoload 'helm-apropos "helm-elisp" "" t nil)
;; (define-key-lazy help-mode-map (kbd "a")         'helm-apropos "help-mode")
;; (global-set-key  [remap apropos-command] 'helm-apropos) ;C-ha f1-a


(global-set-key "\M-x" 'helm-M-x)

(setq-default helm-multi-files-toggle-locate-binding "M-l") ;helm-multi-files 时按此键切到helm-locate
;;
;; lisp complete.
(define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
(define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point)



;; From browse-kill-ring.el
(defadvice yank-pop (around kill-ring-browse-maybe (arg) activate)
  "If last action was not a yank, run `browse-kill-ring' instead."
  ;; yank-pop has an (interactive "*p") form which does not allow
  ;; it to run in a read-only buffer. We want browse-kill-ring to
  ;; be allowed to run in a read only buffer, so we change the
  ;; interactive form here. In that case, we need to
  ;; barf-if-buffer-read-only if we're going to call yank-pop with
  ;; ad-do-it
  (interactive "p")
  (if (not (eq last-command 'yank))
      (helm-show-kill-ring)
    (barf-if-buffer-read-only)
    ad-do-it))



(with-eval-after-load 'helm
  (require 'helm-adaptive)
  (helm-adaptive-mode 1)
  ;; (helm-autoresize-mode 1)
  ;; (set-face-attribute 'helm-selection nil
  ;;                     :inherit nil
  ;;                     ;; 1. box selected
  ;;                     ;; :background "#004A5D" :foreground "white"
  ;;                     ;; :box '(:color "cyan" :line-width -1)
  ;;                     ;; :underline nil
  ;;                     ;; 2. darker background
  ;;                     :inverse-video nil
  ;;                     :foreground nil
  ;;                     :background (color-darken-name (face-background 'default) 10)
  ;;                     )

  (define-key helm-map  (kbd "C-2") 'helm-toggle-visible-mark);;mark M-m
  ;; (define-key helm-map  [?\H-m] 'helm-maybe-exit-minibuffer);;return
  (define-key helm-map (kbd "<tab>")        'helm-select-action)


  ;; (define-key helm-map (kbd "C-r") 'helm-execute-persistent-action);;默认是C-z
  (define-key helm-map (kbd "C-s") 'helm-next-line) ;;
  (define-key helm-map (kbd "C-r") 'helm-previous-line) ;;
  ;; (define-key helm-map (kbd "C-e") 'helm-execute-persistent-action); default C-j

  ;; (define-key helm-map (kbd "C-k") 'helm-previous-source)
  (define-key helm-map (kbd "C-o") 'helm-next-source)
  (define-key helm-map (kbd "`")        'helm-select-action)
  (define-key helm-map (kbd "C-c c")        'toggle-case-fold)

  ;; Use default-as-input in grep
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-grep)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-grep-ag)

  )

(with-eval-after-load 'helm-buffers

  (define-key helm-buffers-ido-virtual-map (kbd "C-[ [ a i") 'helm-ff-run-grep) ;C-3 for iterm2 keymap
  (define-key helm-buffer-map (kbd "M-s")       'helm-buffers-run-multi-occur)
  (define-key helm-buffer-map (kbd "C-3")       'helm-buffer-run-zgrep)
  (define-key helm-buffer-map (kbd "C-[ [ a i") 'helm-buffer-run-zgrep) ;C-3 for iterm2 keymap
  (define-key helm-buffer-map (kbd "C-s")       'helm-next-line)
  (define-key helm-buffer-map (kbd "C-r") 'helm-previous-line))

(with-eval-after-load 'helm-files
  ;; (require 'helm-ls-git)
  ;; (unless helm-source-ls-git
  ;;   (setq helm-source-ls-git (helm-make-source "Git files" 'helm-ls-git-source)))
  ;; (require 'joseph-helm-filelist)


  (define-key helm-find-files-map (kbd "C-s")       'helm-next-line)

  (define-key helm-find-files-map (kbd "C-3")       'helm-ff-run-grep)
  (define-key helm-find-files-map (kbd "C-[ [ a i")       'helm-ff-run-grep) ;C-3 for iterm2 keymap
  (define-key helm-find-files-map (kbd "C-4")       'helm-ff-run-zgrep)
  (define-key helm-find-files-map (kbd "C-[ [ a j")       'helm-ff-run-zgrep) ;C-3 for iterm2 keymap

  (define-key helm-find-files-map (kbd "C-[ [ a a") 'helm-ff-run-toggle-auto-update) ;C-backspace

  (define-key helm-find-files-map (kbd "C-r") 'helm-previous-line) ;;
  (define-key helm-generic-files-map (kbd "C-s")       'helm-next-line)
  (define-key helm-generic-files-map (kbd "C-r") 'helm-previous-line) ;;
  (define-key helm-generic-files-map (kbd "C-3")       'helm-ff-run-grep)
  (define-key helm-generic-files-map (kbd "C-[ [ a i")       'helm-ff-run-grep) ;C-3 for iterm2 keymap
  (define-key helm-generic-files-map (kbd "C-4")       'helm-ff-run-zgrep)
  (define-key helm-generic-files-map (kbd "C-[ [ a j")       'helm-ff-run-zgrep) ;C-3 for iterm2 keymap

  )


;; (with-eval-after-load 'helm-swoop
;;   (setq-default helm-swoop-pre-input-function 'ignore) ;初始不以光标下的内容作搜索内容，若需要，C-w即可
;;   (defun push-mark-after-helm-swoop()
;;     (isearch-update-ring helm-pattern evil-regexp-search))
;;   (add-hook 'helm-swoop-goto-line-action-hook 'push-mark-after-helm-swoop)

;;   ;; (defun helm-swoop-split-window-keep-two-window($buf)
;;   ;;   (if helm-swoop-split-with-multiple-windows
;;   ;;       (funcall helm-swoop-split-direction)
;;   ;;     (unless (one-window-p) (delete-other-windows)) ;add this line
;;   ;;     (when (one-window-p)
;;   ;;       (funcall helm-swoop-split-direction)))
;;   ;;   (other-window 1)
;;   ;;   (switch-to-buffer $buf))

;;   (setq helm-swoop-split-window-function 'helm-default-display-buffer)
;;   ;; (setq helm-swoop-split-window-function 'helm-swoop-split-window-keep-two-window)
;;   (defun push-mark-before-helm-swoop()
;;     (bm-bookmark-add nil nil t)) ; push bookmark for bm.el

;;   (add-hook 'helm-swoop-before-goto-line-action-hook 'push-mark-before-helm-swoop)

;;   (define-key helm-swoop-map (kbd "C-x C-s") 'helm-swoop-edit)
;;   (define-key helm-swoop-map (kbd "C-c C-c") 'helm-swoop-edit)
;;   (define-key helm-multi-swoop-map (kbd "C-x C-s") 'helm-multi-swoop-edit)
;;   (define-key helm-swoop-edit-map (kbd "C-c C-c") 'helm-swoop--edit-complete)
;;   (define-key helm-swoop-edit-map (kbd "C-c C-k") 'helm-swoop--edit-cancel))


(with-eval-after-load 'helm-grep
  (define-key helm-grep-mode-map (kbd "C-o") nil)
  (define-key helm-grep-map (kbd "C-c C-c") 'helm-grep-run-save-buffer))
(with-eval-after-load 'grep (define-key grep-mode-map (kbd "C-o") nil))
(with-eval-after-load 'helm-mode
  (helm-mode 1)
  ;; (add-to-list 'helm-completing-read-handlers-alist '(ibuffer-find-file . ido))
  ;; (add-to-list 'helm-completing-read-handlers-alist '(switch-to-buffer . ido))
  ;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . ido))
  )

(run-with-idle-timer 3 nil '(lambda()
                              (require 'helm-config)
                              (require 'helm)
                              (require 'helm-mode)
                              (require 'helm-grep)
                              (require 'helm-ls-git)
                              (require 'helm-locate)))
;; it's slow
;; (with-eval-after-load 'helm-ls-git
;;   (setq helm-source-ls-git
;;         (helm-make-source "Git files" 'helm-ls-git-source
;;                      :fuzzy-match helm-ls-git-fuzzy-match))
;;   (add-to-list 'helm-for-files-preferred-list 'helm-source-ls-git t)
;;   )



(provide 'conf-helm)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-helm.el ends here.
