(eval-when-compile
  (require 'em-term)
  (require 'em-hist)
  (require 'eshell))

(require 'pcmpl-git)
;; zsh 的alias  转变成eshell
;; alias | awk '{print "alias "$0}' | sed -E "s/^alias ([^=]+)='(.*)'$/alias \1 \2 \$*/g; s/^alias ([^= ]+)=(.*)$/alias \1 \2 \$*/g;  s/'\\\''/'/g;" >~/.emacs.d/eshell/alias
(setq-default eshell-directory-name (concat user-emacs-directory "eshell"))

;; (setq-default eshell-destroy-buffer-when-process-dies t)

(autoload 'epe-theme-lambda "eshell-prompt-extras")
(setq-default eshell-highlight-prompt nil)
(setq-default epe-show-python-info nil)
(setq-default eshell-prompt-function 'epe-theme-lambda)

;; ;始终确何prompt 出现在行首
;; *echo -n "hello"
(add-hook 'eshell-before-prompt-hook 'eshell-begin-on-new-line)

(with-eval-after-load 'em-term
  (add-to-list 'eshell-visual-commands  "tmux")
  (add-to-list 'eshell-visual-commands  "redis-cli")
  (add-to-list 'eshell-visual-commands  "zsh")
  (add-to-list 'eshell-visual-commands  "bash")
  (add-to-list 'eshell-visual-commands  "ssh")
  (when (boundp 'eshell-visual-subcommands) ;from emacs24.4
    (add-to-list 'eshell-visual-subcommands  '("tmux"))
    (add-to-list 'eshell-visual-subcommands  '("bee" "run"))
    (add-to-list 'eshell-visual-subcommands  '("go" "run"))
    (add-to-list 'eshell-visual-subcommands  '("git" "l" "log" "diff" "show"))))

;; (autoload 'helm-eshell-history "helm-eshell" "" t nil)
;; (autoload 'helm-esh-pcomplete "helm-eshell" "" t nil)

;;using helm.el as the complete engine
(defun vmacs-eshell-hook()
  ;; (company-mode -1)

  (setenv "TERM" "xterm-256color")
  (define-key eshell-mode-map (kbd "C-a") nil)

  ;; (define-key eshell-mode-map [remap eshell-previous-matching-input] 'helm-eshell-history ) ;M-r
  (define-key eshell-mode-map [remap eshell-previous-matching-input] 'vmacs-esh-history ) ;M-r
  (define-key eshell-mode-map (kbd "C-r") 'vmacs-esh-history)
  ;; (define-key eshell-mode-map (kbd "M-r") 'vmacs-esh-history)

  (define-key eshell-mode-map (kbd "C-p") 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map (kbd "C-n") 'eshell-next-matching-input-from-input)
  (define-key eshell-mode-map (kbd "M-k") 'eshell-previous-prompt)
  (define-key eshell-mode-map (kbd "M-j") 'eshell-next-prompt)
  (define-key eshell-mode-map (kbd "C-t") 'vmacs-eshell-new)
  (define-key eshell-mode-map (kbd "s-t") 'vmacs-eshell-new)
  (define-key eshell-mode-map (kbd "C-M-s-t") 'vmacs-eshell-new)
  ;; (define-key eshell-mode-map (kbd "C-M-s-n") 'vmscs-eshell-next)
  ;; (define-key eshell-mode-map (kbd "C-M-s-p") 'vmscs-eshell-prev)

  (define-key eshell-mode-map (kbd "M-.") 'eshell-insert-last-cmd-argument)

  ;; (define-key eshell-mode-map [remap eshell-previous-matching-input] 'helm-eshell-history ) ;M-r
  ;; (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete) ;tab
  ;; (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete);Tab
  (define-key eshell-mode-map (kbd "<tab>") (lambda () (interactive) (pcomplete-std-complete)))
  (eshell-hist-use-global-history)
  )

(add-hook 'eshell-mode-hook 'vmacs-eshell-hook)


(defalias 'vi 'find-file)
(defalias 'o 'find-file-other-window)



(with-eval-after-load 'esh-mode
  (defadvice eshell/clear (around zsh-like-clear activate)
    "clear eshell buffer"
    (let ((eshell-buffer-maximum-lines 0)) (eshell-truncate-buffer))))



(defun eshell/zsh ()
  (interactive)
  (term  (getenv "SHELL"))
  nil)

(provide 'conf-eshell)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-eshell.el ends here.
