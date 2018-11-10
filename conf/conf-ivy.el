(eval-when-compile (require 'ivy-dired-history))
(require 'recentf)
(require 'counsel)
;; (eval-when-compile )
(ivy-mode 1)
(setq-default smex-save-file (expand-file-name "./cache/smex" user-emacs-directory))
(setq-default smex-history-length 15)
(setq ivy-use-virtual-buffers t)
(setq counsel-find-file-at-point t)
(setq counsel-preselect-current-file t)
(setq ivy-initial-inputs-alist nil)
(setq ivy-extra-directories '("./")) ; default value: ("../" "./")
(setq ivy-wrap t)
(setq ivy-count-format "")
;; (setq ivy-count-format "%d/%d ")
;; (setq ivy-virtual-abbreviate 'full) ; Show the full virtual file paths
;; (setq ivy-add-newline-after-prompt nil)
(setq ivy-height 25)
(setq ivy-fixed-height-minibuffer t)
(setq counsel-git-grep-skip-counting-lines t)
;; (defun ivy--regex-both (str) (if(string-match ".*? .*?" str) (ivy--regex-plus str) (ivy--regex-fuzzy str)))

(setq ivy-re-builders-alist '((swiper . ivy--regex-plus) ;
                              (ivy-switch-buffer . ivy--regex-plus)
                              (counsel-ag . ivy--regex-plus)
                              (counsel-rg . ivy--regex-plus)
                              (counsel-git . ivy--regex-plus)
                              (counsel-git-grep . ivy--regex-plus)
                              (counsel-grep-or-swiper . ivy--regex-plus)
                              (read-file-name-internal . ivy--regex-plus)
                              (counsel--M-x . ivy--regex-plus)
                              (t . ivy--regex-plus)))

(setq counsel-git-grep-cmd-default "git --no-pager grep --full-name -n --no-color -i -e '%s'|cut -c -300") ;trunc long line
;; (setq counsel-rg-base-command  "rg -S --no-heading --line-number --search-zip --color never %s .")
(setq counsel-rg-base-command  "~/.emacs.d/bin/rgwrapper -z %s ")


(setq magit-completing-read-function 'ivy-completing-read)
(setq counsel-find-file-at-point t)
(setq ivy-ignore-buffers
       (list
        "\*EGLOT"
           "\\` "
           "\*Helm"
           "\*helm"
           "\*vc-diff\*"
           "\*magit-"
           "\*vc-"
           "*Backtrace*"
           "*Package-Lint*"
           ;; "todo.txt"
           "\*vc*"
           "*Completions*"
           "\*vc-change-log\*"
           "\*VC-log\*"
           "\*Async Shell Command\*"
           "\*Shell Command Output\*"
           "\*sdcv\*"
           ;; "\*Messages\*"
           "\*Ido Completions\*"))

;; (setq enable-recursive-minibuffers t)
;; (global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f1> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f1> u") 'counsel-unicode-char)
(global-set-key (kbd "<f1> b") 'counsel-descbinds)

;; (define-key read-expression-map (kbd "C-r") 'counsel-expression-history) ;M-:
;; windows 上将space映射成windows键，而单按时仍然发送空格键，便是在空格键前会发射lwindow键，
;; (defun vmacs-space()
;;   (interactive)
;;   (unless (string= ivy-text "")
;;     (insert " ")))
(define-key ivy-switch-buffer-map (kbd "M-k") 'ivy-switch-buffer-kill)
(define-key ivy-switch-buffer-map (kbd "s-k") 'ivy-switch-buffer-kill)
(define-key ivy-switch-buffer-map (kbd "C-M-s-k") 'ivy-switch-buffer-kill)

;; (vmacs-leader "<lwindow>" 'ivy-switch-buffer) ;for windows

(vmacs-leader "SPC" 'ivy-switch-buffer)
(vmacs-leader "ff" 'counsel-find-file)
(vmacs-leader "ft" #'(lambda()(interactive)(let ((default-directory "/tmp/"))(call-interactively 'counsel-find-file))))
(vmacs-leader "fh" #'(lambda()(interactive)(let ((default-directory "~"))(call-interactively 'counsel-find-file))))
(vmacs-leader "fl" 'counsel-locate)
(vmacs-leader "fg" 'counsel-git)
(vmacs-leader "g" 'vmacs-counsel-rg)
(vmacs-leader "fp" 'vmacs-counsel-git-grep-region-or-symbol)

(vmacs-leader "?" 'counsel-descbinds)
(vmacs-leader "wi" 'counsel-imenu)
(vmacs-leader "b" 'ivy-resume)
(vmacs-leader "wy" 'counsel-mark-ring)
(vmacs-leader ";" 'counsel-M-x)
(vmacs-leader "；" 'counsel-M-x)
(global-set-key  (kbd "s-;") 'counsel-M-x)
;; gui 下让ctrl-i与tab 不同
(global-set-key (kbd "<C-i>") 'counsel-git) ;Ctrl-i not tab
(global-set-key (kbd "s-i") 'counsel-git)   ;cmd-i
(global-set-key (kbd "C-s-M-i") 'counsel-git)   ;hyper-i

(define-key ivy-minibuffer-map (kbd "<C-i>") 'vmacs-ivy-dropto-counsel-git)
(define-key ivy-minibuffer-map (kbd "C-c c") 'toggle-case-fold)
(define-key ivy-minibuffer-map (kbd "C-t") 'toggle-case-fold)

(define-key ivy-minibuffer-map (kbd "C-f") 'vmacs-ivy-magic-call)
(define-key ivy-minibuffer-map (kbd "C-c C-c") 'ivy-occur)
(define-key ivy-minibuffer-map (kbd "C-o") 'ivy-dispatching-done)
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)
(define-key ivy-minibuffer-map (kbd "C-e") 'vmacs-ivy-magic-eol)
(define-key ivy-minibuffer-map (kbd "<tab>") 'ivy-partial-or-done)
(define-key ivy-minibuffer-map (kbd "C-l") 'ivy-backward-kill-word)
(define-key ivy-minibuffer-map (kbd "C-h") 'ivy-backward-kill-word)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-kill-line)
(define-key ivy-minibuffer-map (kbd "C-.") 'ivy-next-history-element)
(define-key ivy-minibuffer-map (kbd "C-[ [ 1 e") 'ivy-next-history-element) ;iterm2 map to ctrl-.
(define-key ivy-minibuffer-map (kbd "<C-m>") 'ivy-done)
(define-key ivy-minibuffer-map (kbd "C-[ [ 1 m")  'ivy-done)
(define-key ivy-minibuffer-map (kbd "C-[ [ 1 m")  'ivy-done)
(define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
;; (define-key ivy-minibuffer-map (kbd "SPC") 'ignore) ;
;; (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-delete-char)

;; (define-key ivy-minibuffer-map (kbd "C-;") 'ivy-avy)
;; (define-key ivy-minibuffer-map (kbd "C-[ [ 1 f") 'ivy-avy) ; ;iterm map C-; to this
(define-key ivy-occur-grep-mode-map (kbd "n") 'evil-search-next)
(define-key ivy-occur-grep-mode-map (kbd "p") 'evil-search-previous)
(define-key ivy-occur-mode-map (kbd "g") nil)

(define-key ivy-occur-mode-map (kbd "r") 'ivy-occur-revert-buffer)
(define-key ivy-occur-grep-mode-map (kbd "r") 'ivy-occur-revert-buffer)
(define-key ivy-occur-grep-mode-map (kbd "g") nil)



(with-eval-after-load 'counsel
  (define-key counsel-find-file-map (kbd "C-l") 'counsel-up-directory)
  (define-key counsel-find-file-map (kbd "C-h") 'counsel-up-directory)
  (define-key counsel-ag-map (kbd "C-l") 'vmacs-counsel-ag-up-directory)
  (define-key counsel-ag-map (kbd "C-o") 'vmacs-counsel-rg-select-directory)
  (define-key counsel-ag-map (kbd "C-;") 'vmacs-counsel-ag-toggle-git-root)
  (define-key counsel-ag-map (kbd "C-h") 'vmacs-counsel-ag-up-directory)
  (define-key counsel-git-grep-map (kbd "C-h") 'counsel-up-directory)
  (define-key counsel-git-grep-map (kbd "C-l") 'counsel-up-directory)


  (define-key counsel-find-file-map (kbd "<return>") 'ivy-alt-done)
  (define-key counsel-find-file-map (kbd "<RET>")      'ivy-alt-done))

(ivy-add-actions 'counsel-find-file `(( ,(kbd "C-o") find-file-other-window "other window")))
(ivy-add-actions 'ivy-switch-buffer `(( ,(kbd "C-o") ivy--switch-buffer-other-window-action "other window")))
(ivy-add-actions 'counsel-find-file '(("d" vmacs-ivy-dired "dired")))
(ivy-add-actions 'ivy-switch-buffer '(("d" vmacs-ivy-swithc-buffer-open-dired "dired")))
(ivy-add-actions 'counsel-git '(("d" vmacs-ivy-dired "dired")))
(ivy-add-actions 'counsel-git `((,(kbd "C-o") find-file-other-window "other window")))

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
      (counsel-yank-pop)
    (barf-if-buffer-read-only)
    ad-do-it))
(defadvice ivy--virtual-buffers (around counsel-git activate)
  "Append git files as virtual buffer"
  (let ((recentf-list recentf-list )list
        (default-directory default-directory)
        counsel--git-dir)
    (setq counsel--git-dir (locate-dominating-file default-directory ".git"))
    (when counsel--git-dir
      (setq counsel--git-dir (expand-file-name counsel--git-dir))
      (setq default-directory counsel--git-dir)
      (setq list (split-string (shell-command-to-string (format "git ls-files --full-name --|grep -v /snippets/|sed \"s|^|%s/|g\"|head -n 3000" default-directory)) "\n" t))
      (setq recentf-list (append recentf-list list))
      ;; (when (< (length list) 5000)
      ;; (dolist (c list)
      ;;   (setq c (expand-file-name c default-directory))
      ;;   (add-to-list 'recentf-list c))
      ;; )
      )
    ad-do-it))


;; ;; 主要为了支持 foo bar !far 模式
;; ;; 主要为了支持 foo bar !far 模式
;; (defadvice counsel-ag-function (around support-multimatch (arg) activate)
;;   "Grep in the current directory for STRING using BASE-CMD.
;; If non-nil, append EXTRA-AG-ARGS to BASE-CMD."
;;   (if (< (length (ad-get-arg 0)) 3)
;;       (counsel-more-chars 3)
;;     (let ((default-directory (ivy-state-directory ivy-last))
;;           (regex (counsel-unquote-regex-parens
;;                   (setq ivy--old-re
;;                         (ivy--regex (ad-get-arg 0)))))
;;           (param (with-temp-buffer
;;                    (insert (shell-quote-argument (ad-get-arg 0)))
;;                    (goto-char (point-min))
;;                    (while (search-forward "\\ " nil t)
;;                      (replace-match " " nil t))
;;                    (buffer-string))))
;;       (counsel--async-command
;;        (format counsel-ag-command param)) nil)))


;; (defvar noct--original-ivy-regex-function nil)

;; (defun noct-ivy-space-switch-to-regex ()
;;   (interactive)
;;   (unless (eq ivy--regex-function 'ivy--regex-fuzzy)
;;     (setq ivy--old-re nil)
;;     (setq noct--original-ivy-regex-function ivy--regex-function)
;;     (setq ivy--regex-function 'ivy--regex-fuzzy))
;;   (self-insert-command 1))

;; (define-key ivy-minibuffer-map (kbd "SPC") #'noct-ivy-space-switch-to-regex)

;; (defun noct-ivy-maybe-reset-regex-function ()
;;   (interactive)
;;   (let ((input (replace-regexp-in-string "\n.*" "" (minibuffer-contents))))
;;     (when (and noct--original-ivy-regex-function
;;                (not (string-match " " input)))
;;       (setq ivy--old-re nil)
;;       (setq ivy--regex-function noct--original-ivy-regex-function)
;;       (setq noct--original-ivy-regex-function nil))))

;; (advice-add 'ivy-backward-delete-char :after #'noct-ivy-maybe-reset-regex-function)
;; (advice-add 'ivy-delete-char :after #'noct-ivy-maybe-reset-regex-function)

(provide 'conf-ivy)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-ivy.el ends here.
