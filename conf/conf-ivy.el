(eval-when-compile (require 'ivy-dired-history))
(eval-when-compile (require 'counsel))
(eval-when-compile (require 'recentf))
(ivy-mode 1)
(setq-default smex-save-file (expand-file-name "./cache/smex" user-emacs-directory))
(setq-default smex-history-length 15)
(setq ivy-use-virtual-buffers t)
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

(setq ivy-re-builders-alist '((swiper . ivy--regex-fuzzy) ;
                              (ivy-switch-buffer . ivy--regex-fuzzy)
                              (counsel-ag . ivy--regex-plus)
                              (counsel-rg . ivy--regex-plus)
                              (counsel-git . ivy--regex-plus)
                              (vmacs-counsel-git . ivy--regex-plus)
                              (counsel-git-grep . ivy--regex-plus)
                              (counsel-grep-or-swiper . ivy--regex-plus)
                              (read-file-name-internal . ivy--regex-fuzzy)
                              (counsel--M-x . ivy--regex-fuzzy)
                              (t . ivy--regex-fuzzy)))

(setq counsel-git-grep-cmd-default "git --no-pager grep --full-name -n --no-color -i -e '%s'|cut -c -300") ;trunc long line

(setq magit-completing-read-function 'ivy-completing-read)
(setq counsel-find-file-at-point t)
(setq ivy-ignore-buffers
       (list
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
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f1> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f1> u") 'counsel-unicode-char)
;; (define-key read-expression-map (kbd "C-r") 'counsel-expression-history) ;M-:

(evil-leader/set-key "SPC" 'ivy-switch-buffer)
(evil-leader/set-key "ff" 'counsel-find-file)
(evil-leader/set-key "fl" 'counsel-locate)
(evil-leader/set-key "fg" 'vmacs-counsel-git)
(evil-leader/set-key "g" 'vmacs-counsel-rg-region-or-symbol)
(evil-leader/set-key "fp" 'vmacs-counsel-git-grep-region-or-symbol)

(evil-leader/set-key "?" 'counsel-descbinds)
(evil-leader/set-key "wi" 'counsel-imenu)
(evil-leader/set-key "b" 'ivy-resume)
(evil-leader/set-key "wy" 'counsel-mark-ring)
(evil-leader/set-key ";" 'counsel-M-x)
(evil-leader/set-key "；" 'counsel-M-x)
(global-set-key  (kbd "s-;") 'counsel-M-x)
;; gui 下让ctrl-i与tab 不同
(global-set-key (kbd "<C-i>") 'vmacs-counsel-git) ;Ctrl-i not tab
(global-set-key (kbd "s-i") 'vmacs-counsel-git)   ;cmd-i

(define-key ivy-minibuffer-map (kbd "<C-i>") 'vmacs-ivy-dropto-counsel-git)

(define-key ivy-minibuffer-map (kbd "C-c C-c") 'ivy-occur)
(define-key ivy-minibuffer-map (kbd "C-o") 'ivy-dispatching-done)
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)
(define-key ivy-minibuffer-map (kbd "C-e") 'vmacs-ivy-magic-eol)
(define-key ivy-minibuffer-map (kbd "<tab>") 'ivy-partial-or-done)
(define-key ivy-minibuffer-map (kbd "C-l") 'ivy-backward-kill-word)
(define-key ivy-minibuffer-map (kbd "C-h") 'ivy-backward-kill-word)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-kill-line)
(define-key ivy-minibuffer-map (kbd "C-.") 'ivy-next-history-element)
(define-key ivy-minibuffer-map (kbd "C-[ [ a e") 'ivy-next-history-element) ;iterm2 map to ctrl-.
(define-key ivy-minibuffer-map (kbd "<C-m>") 'ivy-done)
(define-key ivy-minibuffer-map (kbd "C-[ [ a m")  'ivy-done)
;; (define-key ivy-minibuffer-map (kbd "SPC") 'ignore) ;
;; (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-delete-char)

;; (define-key ivy-minibuffer-map (kbd "C-;") 'ivy-avy)
;; (define-key ivy-minibuffer-map (kbd "C-[ [ a f") 'ivy-avy) ; ;iterm map C-; to this


(with-eval-after-load 'counsel
  (define-key counsel-find-file-map (kbd "C-l") 'counsel-up-directory)
  (define-key counsel-find-file-map (kbd "C-h") 'counsel-up-directory)
  ;; (define-key counsel-ag-map (kbd "C-l") 'counsel-up-directory)
  ;; (define-key counsel-ag-map (kbd "C-h") 'counsel-up-directory)
  ;; (define-key counsel-git-grep-map (kbd "C-h") 'counsel-up-directory)
  ;; (define-key counsel-git-grep-map (kbd "C-l") 'counsel-up-directory)


  (define-key counsel-find-file-map (kbd "<return>") 'ivy-alt-done)
  (define-key counsel-find-file-map (kbd "<RET>")      'ivy-alt-done))

(ivy-add-actions 'counsel-find-file '(("d" vmacs-ivy-dired "dired")))
(ivy-add-actions 'ivy-switch-buffer '(("d" vmacs-ivy-swithc-buffer-open-dired "dired")))

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

(defadvice ivy--virtual-buffers (around vmacs-counsel-git activate)
  "Append git files as virtual buffer"
  (require 'recentf)
  (let ((recentf-list recentf-list )list
        (default-directory default-directory))
    (setq counsel--git-dir (locate-dominating-file default-directory ".git"))
    (when counsel--git-dir
      (setq counsel--git-dir (expand-file-name counsel--git-dir))
      (setq default-directory counsel--git-dir)
      (setq list (split-string (shell-command-to-string (format "git ls-files --full-name --|sed \"s|^|%s/|g\"|head -n 1500" default-directory)) "\n" t))
      (setq recentf-list (append recentf-list list))
      ;; (when (< (length list) 5000)
      ;; (dolist (c list)
      ;;   (setq c (expand-file-name c default-directory))
      ;;   (add-to-list 'recentf-list c))
      ;; )
      )
    ad-do-it))


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
