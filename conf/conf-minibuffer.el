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
(setq completions-format 'vertical)   ; *Completions* buffer M-v跳到*Completions* buffer
(setq completion-show-help nil) ;*Completions* show help
(setq max-mini-window-height 15)        ;selectrum-num-candidates-displayed 受影响
;; (setq read-answer-short t)
(define-key completion-list-mode-map (kbd "C-g") 'quit-window) ;*Completions*

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
  (define-key minibuffer-local-completion-map (kbd "C-e") 'minibuffer-complete)
  )

(add-hook 'minibuffer-setup-hook #'vmacs-minibuffer-hook)

(file-name-shadow-mode 1)
(minibuffer-depth-indicate-mode 1)                   ;显示minibuffer深度
;; (minibuffer-electric-default-mode 1)    ;当输入内容后，prompt的default值就会被隐藏
(vmacs-leader ";" 'execute-extended-command)
(vmacs-leader "；" 'execute-extended-command)
(vmacs-leader "wi" 'imenu)
(vmacs-leader "f." 'ffap)
(vmacs-leader "SPC" 'vmacs-switch-buffer)
(vmacs-leader "fh" #'(lambda()(interactive)(let ((default-directory "~/"))(call-interactively 'find-file))))
(vmacs-leader "ft" #'(lambda()(interactive)(let ((default-directory "/tmp/"))(call-interactively 'find-file))))
(vmacs-leader "ff" 'find-file)



 (when (file-directory-p "~/.emacs.d/submodule/mini-frame")
   (add-to-list 'load-path "~/.emacs.d/submodule/mini-frame"))

;; ;; 把minibuffer 搬到一个特定的frame上
(require 'mini-frame)
(setq mini-frame-resize-max-height max-mini-window-height)
;; (setq mini-frame-ignore-commands nil)
(add-to-list 'mini-frame-ignore-commands 'dired-narrow)
(setq mini-frame-show-parameters
      '((top . 0.2) (width . 0.7) (left . 0.3)
        (min-height .  2)
        (height . 10)
        (minibuffer-exit . t)
        (font . "Sarasa Mono CL-30")
        (alpha . 100)
        (left-fringe . 10 )
        (cursor-color . "Yellow")
        ;; (border-color . "green")
        ;; (background-mode 'light)
        (background-mode 'dark)
        (foreground-color . "#bbc2cf") ;;
        (background-color . "#242730")))


(mini-frame-mode 1)


(provide 'conf-minibuffer)
