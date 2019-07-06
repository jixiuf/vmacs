(eval-when-compile (require 'ivy-dired-history))
(require 'recentf)
(require 'counsel)
;; (eval-when-compile )
(ivy-mode 1)
(setq-default smex-save-file (expand-file-name "./cache/smex" user-emacs-directory))
(setq-default smex-history-length 15)
(setq ivy-use-virtual-buffers t)
;; (setq ffap-machine-p-local 'accept)
;; (setq ffap-machine-p-unknown'accept)
(setq counsel-find-file-at-point t)
(setq ffap-machine-p-known 'accept)     ;起用counsel-find-file-at-point时 ，有平会莫名其妙地ping，此处禁用ping
(setq counsel-find-file-at-point t)
(setq counsel-preselect-current-file t)
(setq ivy-initial-inputs-alist nil)
(setq ivy-extra-directories '("./")) ; default value: ("../" "./")
(setq ivy-wrap t)
(setq ivy-count-format "")
;; (setq ivy-count-format "%d/%d ")
(setq ivy-virtual-abbreviate 'abbreviate) ; not only show buffer name,but path
;; (setq ivy-add-newline-after-prompt nil)
(setq ivy-height 25)
(setq ivy-fixed-height-minibuffer t)
(setq counsel-git-grep-skip-counting-lines t)
;; (defun ivy--regex-both (str) (if(string-match ".*? .*?" str) (ivy--regex-plus str) (ivy--regex-fuzzy str)))


;; ivy--regex-ignore-order
;; ivy-re-builders-alist 如果想搜索空格 就连续两个空格代表一个空格即可
;; (setq ivy-re-builders-alist '((swiper . ivy--regex-plus) ;
;;                               (ivy-switch-buffer . ivy--regex-ignore-order)
;;                               ;; (counsel-ag . ivy--regex-ignore-order)
;;                               ;; (counsel-rg . ivy--regex-ignore-order)
;;                               ;; (counsel-git . ivy--regex-ignore-order)
;;                               (counsel-git-grep . ivy--regex-plus)
;;                               ;; (counsel-grep-or-swiper . ivy--regex-plus)
;;                               ;; (read-file-name-internal . ivy--regex-plus)
;;                               ;; (counsel--M-x . ivy--regex-plus)
;;                               (t . ivy--regex-plus)))

(setq counsel-git-grep-cmd-default "git --no-pager grep --full-name -n --no-color -i -e '%s'|cut -c -300") ;trunc long line
;; (setq counsel-rg-base-command  "rg -S --no-heading --line-number --search-zip --color never %s .")
;; \b 用来匹配单词边界
(setq counsel-rg-base-command  "rg -S --no-heading --line-number --color never  -z %s ." )


;; (setq magit-completing-read-function 'ivy-completing-read)
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
(define-key ivy-switch-buffer-map [C-M-s-268632075] 'ivy-switch-buffer-kill)


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
(vmacs-leader "wi" 'counsel-semantic-or-imenu)
(vmacs-leader "b" 'ivy-resume)
(vmacs-leader "wy" 'counsel-mark-ring)
(vmacs-leader ";" 'counsel-M-x)
(vmacs-leader "；" 'counsel-M-x)

;; window layout
(vmacs-leader "wl" 'ivy-switch-view)
(vmacs-leader "m" 'ivy-push-view)
(vmacs-leader "wc" 'ivy-push-view)
(vmacs-leader "wn" 'ivy-push-view)
(vmacs-leader "wk" 'ivy-pop-view)


(global-set-key  (kbd "s-;") 'counsel-M-x)
;; gui 下让ctrl-i与tab 不同
(global-set-key (kbd "<C-i>") 'counsel-git) ;Ctrl-i not tab
(global-set-key (kbd "s-i") 'counsel-git)   ;cmd-i
(global-set-key (kbd "C-s-M-i") 'counsel-git)   ;hyper-i
(global-set-key [C-M-s-268632073]  #'counsel-git) ;mac


(define-key ivy-minibuffer-map (kbd "<C-i>") 'vmacs-ivy-dropto-counsel-git)
(define-key ivy-minibuffer-map (kbd "C-c c") 'vmacs-counsel-toggle-case-senstive)
(define-key ivy-minibuffer-map (kbd "C-t") 'vmacs-counsel-toggle-case-senstive)

(define-key ivy-minibuffer-map (kbd "C-f") 'vmacs-ivy-magic-call)
(define-key ivy-minibuffer-map (kbd "C-c C-c") 'ivy-occur)
(define-key ivy-minibuffer-map (kbd "C-o") 'ivy-dispatching-done)
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)
(define-key ivy-minibuffer-map (kbd "C-e") 'vmacs-ivy-magic-eol)
(define-key ivy-minibuffer-map (kbd "<tab>") 'ivy-partial-or-done)
(define-key ivy-minibuffer-map (kbd "C-l") 'ivy-backward-kill-word)
(define-key ivy-minibuffer-map (kbd "C-h") 'ivy-backward-kill-word)
(define-key ivy-minibuffer-map (kbd "C-k") 'vmacs-ivy-magic-kill)
(define-key ivy-minibuffer-map (kbd "C-.") 'ivy-next-history-element)
(define-key ivy-minibuffer-map (kbd "C-[ [ 1 e") 'ivy-next-history-element) ;iterm2 map to ctrl-.
(define-key ivy-minibuffer-map (kbd "<C-m>") 'ivy-done)
(define-key ivy-minibuffer-map (kbd "C-[ [ 1 m")  'ivy-done)
(define-key ivy-minibuffer-map (kbd "C-[ [ 1 m")  'ivy-done)
(define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)

(define-key ivy-occur-grep-mode-map (kbd "n") 'evil-search-next)
(define-key ivy-occur-grep-mode-map (kbd "p") 'evil-search-previous)
(define-key ivy-occur-grep-mode-map (kbd "j") nil)
(define-key ivy-occur-grep-mode-map (kbd "k") nil)
(define-key ivy-occur-mode-map (kbd "g") nil)
(define-key ivy-occur-mode-map (kbd "SPC") nil)
(define-key ivy-occur-grep-mode-map (kbd "SPC") nil)

(define-key ivy-occur-mode-map (kbd "r") nil)
(define-key ivy-occur-grep-mode-map (kbd "r") nil)
(define-key ivy-occur-grep-mode-map (kbd "g") nil)
(define-key ivy-occur-grep-mode-map (kbd "gr") 'ivy-occur-revert-buffer)
(define-key ivy-occur-grep-mode-map (kbd "z") 'ivy-occur-hide-lines-matching)
(define-key ivy-occur-grep-mode-map (kbd "/") 'ivy-occur-hide-lines-not-matching)

;;;###autoload
(defun ivy-occur-hide-lines-not-matching (search-text)
  "Hide lines that don't match the specified regexp."
  (interactive "MHide lines not matched by regexp: ")
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (save-excursion
    (goto-char (point-min))
    (forward-line 4)
    (let ((inhibit-read-only t)
          (start-position (point))
          (pos (re-search-forward search-text nil t)))
      (while pos
        (beginning-of-line)
        (delete-region start-position (point))
        (forward-line 1)
        (setq start-position (point))
        (if (eq (point) (point-max))
            (setq pos nil)
          (setq pos (re-search-forward search-text nil t))))
              (delete-region start-position (point-max) ))))

;;;###autoload
(defun ivy-occur-hide-lines-matching  (search-text)
  "Hide lines matching the specified regexp."
  (interactive "MHide lines matching regexp: ")
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (save-excursion
    (goto-char (point-min))
    (forward-line 4)
    (let ((inhibit-read-only t)
          (pos (re-search-forward search-text nil t))
          start-position)
      (while pos
        (beginning-of-line)
        (setq start-position (point))
        (end-of-line)
        (delete-region start-position (+ 1 (point)))
        (if (eq (point) (point-max))
            (setq pos nil)
          (setq pos (re-search-forward search-text nil t)))))))



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
(defvar git-repos-files-cache (make-hash-table :test 'equal))
(defadvice ivy--virtual-buffers (around counsel-git activate)
  "Append git files as virtual buffer"
  (let ((recentf-list recentf-list)
        (default-directory default-directory)
        (magit-repos (mapcar 'car magit-repository-directories))
        list counsel--git-dir)
    (unless (file-remote-p default-directory)
      (setq counsel--git-dir (counsel--git-root))
      (when counsel--git-dir
        (setq counsel--git-dir (abbreviate-file-name (directory-file-name (file-truename counsel--git-dir))))
        (setq default-directory counsel--git-dir)
        (setq list (gethash counsel--git-dir  git-repos-files-cache))
        (when (or (not list) current-prefix-arg) ;prefix则会刷新缓存
          (setq list (split-string (shell-command-to-string (format "git ls-files --full-name --|grep -v /snippets/|sed \"s|^|%s/|g\"" default-directory)) "\n" t))
          (puthash counsel--git-dir list git-repos-files-cache))

        (setq recentf-list (append recentf-list list))))
    (dotimes (n 3 magit-repos)
      (let ((magit-repo (nth  n magit-repos)))
        (when magit-repo
          (setq magit-repo (abbreviate-file-name (directory-file-name (file-truename magit-repo))))
          (unless (string-equal magit-repo counsel--git-dir)
            (setq default-directory magit-repo)
            (setq list (gethash magit-repo  git-repos-files-cache))
            (when (or (not list) current-prefix-arg)
              (setq list (split-string (shell-command-to-string (format "git ls-files --full-name --|grep -v /snippets/|sed \"s|^|%s/|g\"" default-directory)) "\n" t)))
            (setq recentf-list (append recentf-list list))
            ))))
    ad-do-it))



(defun posframe-poshandler-frame-left-center (info)
  "Posframe's position handler.
Get a position which let posframe stay onto its
parent-frame's center.  The structure of INFO can
be found in docstring of `posframe-show'."
  (cons  0
         (/ (- (plist-get info :parent-frame-height)
              (plist-get info :posframe-height))
           2)))
;; 注要是为了避免窗口大小变更导致输入框乱动
(defun ivy-posframe-display-at-frame-left-center (str)
  (ivy-posframe--display str #'posframe-poshandler-frame-left-center))

(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-left-center)))
;; (setq ivy-posframe-display-functions-alist
;;       '((counsel-rg     . ivy-posframe-display-at-window-bottom-left) ;ivy-posframe-display-at-point
;;         (t               . ivy-posframe-display-at-frame-left-center)))
(setq ivy-posframe-parameters
      '((left-fringe . 40)
        (alpha . 85)))

(ivy-posframe-mode 1)

(provide 'conf-ivy)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-ivy.el ends here.
