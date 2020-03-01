;; -*- lexical-binding: t -*-
(setq enable-recursive-minibuffers t)        ;在minibuffer 中也可以再次使用minibuffer
(setq history-delete-duplicates t)          ;minibuffer 删除重复历史
(setq minibuffer-prompt-properties;minibuffer prompt 只读，且不允许光标进入其中
              '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
(setq resize-mini-windows 'grow-only)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq completion-cycle-threshold 4)
(setq completion-flex-nospace t)              ;default t
(setq completion-pcm-complete-word-inserts-delimiters t) ;partial-completion in completion-styles
(setq completion-pcm-word-delimiters "-_./:| ")
(setq completions-format 'vertical)   ; *Completions* buffer
(setq completion-show-help t) ;*Completions* show help
(setq completion-styles '(partial-completion substring initials flex))
(setq completion-category-overrides ;支持哪些stypes见completion-styles-alist
              '((file (styles initials basic))
                (buffer (styles initials basic))
                (info-menu (styles basic))))
;; (setq read-answer-short t)

(defun vmacs-minibuffer-hook()
  (local-set-key (kbd "<C-m>") 'exit-minibuffer)
  (local-set-key (kbd "C-l") 'backward-kill-word)
  (local-set-key [escape] 'abort-recursive-edit)
  (local-set-key (kbd "SPC") 'self-insert-command)
  (local-set-key (kbd "TAB") 'minibuffer-complete) ;tab
  (local-set-key (kbd "<tab>") 'minibuffer-complete) ;tab
  ;; (define-key minibuffer-local-must-match-map (kbd "<C-m>") 'exit-minibuffer)
  ;; (define-key minibuffer-local-map (kbd "<C-m>") 'exit-minibuffer)
  ;; (define-key minibuffer-local-completion-map (kbd "<C-m>") 'exit-minibuffer)
  (define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)
  (define-key minibuffer-local-map (kbd "M-p") 'previous-history-element)
  (define-key minibuffer-local-map (kbd "M-n") 'next-history-element)

  ;; (autoload 'minibuffer-keyboard-quit "delsel" "" t nil)
  ;; (define-key minibuffer-local-map [escape]  'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map (kbd "C-e") 'minibuffer-complete))

(add-hook 'minibuffer-setup-hook #'vmacs-minibuffer-hook)

(file-name-shadow-mode 1)
(minibuffer-depth-indicate-mode 1)                   ;显示minibuffer深度
;; (minibuffer-electric-default-mode 1)    ;当输入内容后，prompt的default值就会被隐藏
(require 'icomplete)
(vmacs-leader ";" 'execute-extended-command)
(vmacs-leader "；" 'execute-extended-command)
(vmacs-leader "wi" 'imenu)
(vmacs-leader "fh" #'(lambda()(interactive)(let ((default-directory "~/"))(call-interactively 'vmacs-find-file))))
(vmacs-leader "ft" #'(lambda()(interactive)(let ((default-directory "/tmp/"))(call-interactively 'vmacs-find-file))))
(vmacs-leader "ff" 'vmacs-find-file)
(vmacs-leader "f." 'ffap)
(vmacs-leader "SPC" 'vmacs-switch-buffer)

(defun vmacs-find-file ()
  "Open `recent-list' item in a new buffer.
The user's $HOME directory is abbreviated as a tilde."
  (interactive)
  (let* ((icomplete-separator " . "))
    (call-interactively #'find-file)))


(setq icomplete-prospects-height 20)
(setq max-mini-window-height 0.5)
(setq icomplete-delay-completions-threshold 0)
(setq icomplete-max-delay-chars 0)
(setq icomplete-delay-completions-threshold 2000)
(setq icomplete-compute-delay 0)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-hide-common-prefix nil)
(setq icomplete-separator "\n")
(setq icomplete-with-completion-tables t)
(setq icomplete-in-buffer t)
(setq icomplete-tidy-shadowed-file-names t)
;; (icomplete-mode 1)
(define-key icomplete-minibuffer-map (kbd "C-n") #'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-p") #'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-s") #'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-r") #'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-k") #'icomplete-fido-kill)
(define-key icomplete-minibuffer-map (kbd "C-m") #'icomplete-fido-ret)
(define-key icomplete-minibuffer-map (kbd "RET") #'icomplete-fido-ret)
(define-key icomplete-minibuffer-map (kbd "C-l") #'icomplete-fido-backward-updir)

;; (macroexpand '(with-mode-on icomplete-mode (message "ss")))
(defmacro with-mode-on (mode &rest body)
  (declare (indent defun)
           (doc-string 3))
  (macroexp-let2 nil mode-p mode
    `(progn
       (unless ,mode-p (,mode 1))
       ,@body
       (unless ,mode-p (,mode -1)))))

;; (macroexpand '(with-mode-off icomplete-mode (message "ss")))
(defmacro with-mode-off (mode &rest body)
  (declare (indent defun)
           (doc-string 3))
  (macroexp-let2 nil mode-p mode
    `(progn
       (when ,mode-p (,mode -1))
       ,@body
       (when ,mode-p (,mode 1)))))


 (when (file-directory-p "~/.emacs.d/submodule/prescient")
   (add-to-list 'load-path "~/.emacs.d/submodule/prescient"))
 (when (file-directory-p "~/.emacs.d/submodule/selectrum")
   (add-to-list 'load-path "~/.emacs.d/submodule/selectrum"))
 (when (file-directory-p "~/.emacs.d/submodule/mini-frame")
   (add-to-list 'load-path "~/.emacs.d/submodule/mini-frame"))

(require 'selectrum)
(require 'selectrum-prescient)
(setq selectrum-num-candidates-displayed 30)
(add-to-list 'selectrum-minibuffer-bindings '("C-e" . selectrum-insert-current-candidate) )

(setq prescient-filter-method  '(literal fuzzy initialism))
(selectrum-mode 1)
(selectrum-prescient-mode 1)       ; to make sorting and filtering more intelligent
;; to save your command history on disk, so the sorting gets more
;; intelligent over time
(prescient-persist-mode 1)

(require 'mini-frame)
(setq mini-frame-show-parameters '((top . 0.2) (width . 0.7) (left . 0.2) (height . 0.8)))
(add-to-list 'mini-frame-ignore-commands 'yank)
(mini-frame-mode 1)

;; yank-pop icomplete 支持， selectrum-mode 有问题，故临时关selectrum-mode雇用icomplete
(defun icomplete-mode-yank-pop ()
  (with-mode-off mini-frame-mode
    (with-mode-off selectrum-mode
      (with-mode-on icomplete-mode
        (let* ((icomplete-separator (concat "\n" (propertize "......" 'face 'shadow) "\n "))
               ;;disable sorting https://emacs.stackexchange.com/questions/41801/how-to-stop-completing-read-ivy-completing-read-from-sorting
               (completion-table
                (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata (display-sort-function . identity)
                                 (cycle-sort-function . identity))
                    (complete-with-action
                     action kill-ring string pred)))))
          (insert
           (completing-read "Yank from kill ring: " completion-table nil t)))))))

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
      (icomplete-mode-yank-pop)
    ad-do-it))

(provide 'conf-minibuffer)
