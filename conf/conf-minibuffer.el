;; -*- lexical-binding: t -*-
(setq enable-recursive-minibuffers t)        ;在minibuffer 中也可以再次使用minibuffer
(setq history-delete-duplicates t)          ;minibuffer 删除重复历史
(setq minibuffer-prompt-properties;minibuffer prompt 只读，且不允许光标进入其中
              '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq completion-cycle-threshold 3)
;; (setq completion-flex-nospace t)              ;default t
(setq completion-pcm-complete-word-inserts-delimiters t) ;partial-completion in completion-styles
;; (setq completion-pcm-word-delimiters "-_/:| ")
(setq completion-auto-help nil)         ;不主动弹出 *Completions*
(setq completions-format 'vertical)   ; *Completions* buffer M-v跳到*Completions* buffer
(setq completion-show-help nil) ;*Completions* show help
(setq max-mini-window-height 20)        ;selectrum-num-candidates-displayed 受影响
(setq eldoc-echo-area-use-multiline-p 3)
(setq resize-mini-windows 'grow-only)
;; (setq read-answer-short t)
(setq minibuffer-eldef-shorten-default t)
(minibuffer-electric-default-mode 1)    ;当输入内容后，prompt的default值就会被隐藏
(file-name-shadow-mode 1)
(minibuffer-depth-indicate-mode 1)                   ;显示minibuffer深度

(define-key completion-list-mode-map (kbd "C-g") 'quit-window) ;*Completions*

(defface vmacs-minibuffer-font
  `((t :inherit default :height 1.2))
  "The default font for minibuffer buffer.
Monospaced font whihc is fixed idth and height is recommended."
  :group 'minibuffer)

(defun vmacs-minibuffer-hook()
  (set (make-local-variable 'buffer-face-mode-face) 'vmacs-minibuffer-font)
  (buffer-face-mode t)
  (local-set-key (kbd "<C-m>") 'exit-minibuffer)
  (local-set-key (kbd "C-l") 'backward-kill-word)
  (local-set-key [escape] 'abort-recursive-edit)
  (local-set-key (kbd "TAB") 'minibuffer-complete)
  (local-set-key (kbd "<tab>") 'minibuffer-complete)
  (define-key minibuffer-local-completion-map (kbd "C-e") 'minibuffer-complete)
  (define-key minibuffer-local-map (kbd "M-p") 'previous-history-element)
  (define-key minibuffer-local-map (kbd "M-n") 'next-history-element)
  ;; (local-set-key (kbd "SPC") 'self-insert-command)
  ;; (define-key minibuffer-local-must-match-map (kbd "<C-m>") 'exit-minibuffer)
  ;; (define-key minibuffer-local-map (kbd "<C-m>") 'exit-minibuffer)
  ;; (define-key minibuffer-local-completion-map (kbd "<C-m>") 'exit-minibuffer)
  ;; (define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)

  ;; (autoload 'minibuffer-keyboard-quit "delsel" "" t nil)
  ;; (define-key minibuffer-local-map [escape]  'minibuffer-keyboard-quit)
  )

(add-hook 'minibuffer-setup-hook #'vmacs-minibuffer-hook)




;; (when (file-directory-p "~/.emacs.d/submodule/mini-frame")
;;   (add-to-list 'load-path "~/.emacs.d/submodule/mini-frame"))
;; ;; ;; 把minibuffer 搬到一个特定的frame上
(setq mini-frame-resize-max-height max-mini-window-height)
(setq mini-frame-completions-focus 'minibuffer)
(setq mini-frame-internal-border-color "gray70")
(when (and (not noninteractive) (require 'mini-frame nil t)) ;batch 模式下miniframe 有问题
  ;; (setq mini-frame-ignore-commands nil)
  (add-to-list 'mini-frame-ignore-commands 'dired-narrow)
  (add-to-list 'mini-frame-ignore-commands 'vmacs-rg-rerun-change-regex)
  (add-to-list 'mini-frame-ignore-commands 'vmacs-rg-rerun-hide-matched)


  (setq mini-frame-show-parameters
        '((top . 0.4)
          (width . 1.0)
          (left . 0.5)
          (min-height .  5)
          (height . 15)
          (minibuffer-exit . t)
          (font . "Sarasa Mono CL-22")
          (alpha . 100)
          ;; (left-fringe . 10 )
          (cursor-color . "Yellow")
          (background-color . "#2a3a4a")
          (background-mode . 'dark)))
  (mini-frame-mode 1))



(provide 'conf-minibuffer)
