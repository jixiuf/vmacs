;; -*- lexical-binding: t -*-
;;; nn-fido-frame.el --- fido minibuffer in centered child frame -*- lexical-binding: t; -*-
(defvar nn-fido--frame nil)
(defvar nn-fido--saved-minibuffer-follow t
  "Saved value of `minibuffer-follows-selected-frame' to restore on exit.")
(defvar nn-fido--redirected-frame nil
  "Frame whose focus is temporarily redirected to the child frame.
Only used on PGTK; see `nn-fido-frame-setup'.")
(defvar nn-fido-frame-width 0.85)
(defvar nn-fido-frame-left 0.35)
(defvar nn-fido-frame-top 0.3)

;; consult-focus-lines consult-hide-lines dired-flag-files-regexp
(defcustom nn-fido-frame-ignore-commands '()
  "List of commands that should NOT use the child frame.
When `this-command' is in this list, the minibuffer appears in
the parent frame instead."
  :type '(repeat symbol)
  :group 'convenience)

(defun nn-fido-frame--init ()
  (setq nn-fido--frame
        (make-frame
         `((parent-frame . ,(selected-frame))
           (undecorated . t)
           (z-group . above)
           (minibuffer . only)
           ;; On PGTK, do not let clicks grab GTK widget focus to the
           ;; child edit widget: once grabbed, it cannot be moved back
           ;; and the toplevel keyboard input dies on the next hide.
           ;; Only set on PGTK: on the macOS NS port `no-accept-focus'
           ;; makes the child frame's window unable to become the key
           ;; window (nsterm.m `canBecomeKeyWindow' returns
           ;; !FRAME_NO_ACCEPT_FOCUS), so the mini frame can never
           ;; receive keyboard input.
           ,@(when (eq window-system 'pgtk)
               '((no-accept-focus . t)))
           (minibuffer . only)
           (left . ,nn-fido-frame-left)
           (top . ,nn-fido-frame-top)
           (width . ,nn-fido-frame-width)
           (height . 1)
           (left-fringe . 1)
           (right-fringe . 0)
           (internal-border-width . 2)
           (font . "Sarasa Term SC Nerd Light-24")
           (background-color . "gray26")
           (cursor-color . "green")
           )))
  ;; macOS NS port draws child-frame internal border using
  ;; the 'child-frame-border' face, not frame parameters.
  ;; Inherit cursor color so it adapts to theme changes.
  (set-face-attribute 'child-frame-border nn-fido--frame :inherit 'cursor))

(defun nn-fido-frame-setup ()
  "Setup minibuffer in centered child frame."
  (setq-local max-mini-window-height 1)
  ;; Skip if this command should not use the child frame.
  (if (memq this-command nn-fido-frame-ignore-commands)
      (progn
        (when (frame-live-p nn-fido--frame)
          (make-frame-invisible nn-fido--frame))
        (setq minibuffer-follows-selected-frame t))
    (unless (frame-live-p nn-fido--frame)
      (nn-fido-frame--init))
    ;; Re-apply float position/size: Emacs interprets them relative to
    ;; the current parent frame, so the child adapts on parent resize.
    (modify-frame-parameters
     nn-fido--frame
     `((left . ,nn-fido-frame-left)
       (top . ,nn-fido-frame-top)
       (width . ,nn-fido-frame-width)
       (height . 1)))
    (make-frame-visible nn-fido--frame)
    (if (eq window-system 'pgtk)
        (progn
          ;; PGTK child frames are plain widgets inside the parent
          ;; toplevel.  Never let the child take GTK widget focus:
          ;; `select-frame-set-input-focus' on the child runs
          ;; gtk_widget_grab_focus on its edit widget (see
          ;; pgtk_focus_frame in pgtkterm.c), and once grabbed,
          ;; nothing can move widget focus back to the parent edit
          ;; widget, so all keyboard input of the toplevel dies as
          ;; soon as the child is hidden.  Instead, select the child
          ;; frame without touching WM focus and temporarily redirect
          ;; the invoking frame's focus to it: keystrokes keep
          ;; arriving at the parent edit widget but are treated as
          ;; the child frame's input (see kbd_buffer_get_event in
          ;; keyboard.c).  The redirect is removed on exit.
          (setq nn-fido--redirected-frame (selected-frame))
          (redirect-frame-focus nn-fido--redirected-frame nn-fido--frame)
          (select-frame nn-fido--frame))
      (select-frame-set-input-focus nn-fido--frame))
    (setq nn-fido--saved-minibuffer-follow minibuffer-follows-selected-frame)
    (setq minibuffer-follows-selected-frame nil)))

(defun nn-fido-frame-resize (&optional frame)
  "Resize child frame to fit completions count."
  (when (and nn-fido--frame (frame-live-p nn-fido--frame)
             (or (null frame) (eq frame nn-fido--frame)))
    (let* ((ov (and (boundp 'icomplete-overlay)
                    (overlayp icomplete-overlay)
                    ;; Only use overlay if it belongs to the
                    ;; minibuffer of our frame.
                    (eq (overlay-buffer icomplete-overlay)
                        (with-selected-frame nn-fido--frame
                          (window-buffer (minibuffer-window))))
                    icomplete-overlay))
           (s (and ov (overlay-get ov 'after-string)))
           (lines
            (cond
             ((not s) 0)
             ((string-match-p "\n" s)
              (1+ (length (split-string s "\n" t))))
             (t 0)))
           (target (max 1 (min (or completions-max-height 10) lines))))
      (with-selected-frame nn-fido--frame
        (ignore-errors
          (modify-frame-parameters nil `((height . ,target))))))))

(defun nn-fido-frame--resize-mini-frame-advice (orig-fun frame)
  "Advice for `window--resize-mini-frame': delegate to `nn-fido-frame-resize'
if FRAME is ours, otherwise call ORIG-FUN."
  (if (eq frame nn-fido--frame)
      (nn-fido-frame-resize frame)
    (funcall orig-fun frame)))

(defun nn-fido-frame--max-mini-lines (orig-fun &optional _frame)
  "Return completions-max-height so icomplete positions correctly."
  (if nn-fido-frame-mode (or completions-max-height 10) (funcall orig-fun)))

(defun nn-fido-frame-exit ()
  "Handle minibuffer exit."
  ;; Skip for ignored commands: setup didn't prepare the child frame.
  (unless (memq this-command nn-fido-frame-ignore-commands)
    ;; Undo the temporary focus redirect made on PGTK (see
    ;; `nn-fido-frame-setup'), so further keystrokes are routed
    ;; normally again.
    (when (frame-live-p nn-fido--redirected-frame)
      (redirect-frame-focus nn-fido--redirected-frame nil))
    (setq nn-fido--redirected-frame nil)
    (when (frame-live-p nn-fido--frame)
      (make-frame-invisible nn-fido--frame))
    ;; Restore minibuffer-follows-selected-frame so it's not permanently
    ;; nil after a minibuffer session.
    (setq minibuffer-follows-selected-frame nn-fido--saved-minibuffer-follow)
    (when-let* ((parent (and (frame-live-p nn-fido--frame)
                             (frame-parameter nn-fido--frame 'parent-frame))))
      (when (frame-live-p parent)
        (select-frame-set-input-focus parent)))))

;;;###autoload
(define-minor-mode nn-fido-frame-mode
  "Show fido minibuffer completions in a centered child frame."
  :global t
  (if nn-fido-frame-mode
      (progn
        (setq resize-mini-frames #'nn-fido-frame-resize)
        (add-hook 'minibuffer-setup-hook #'nn-fido-frame-setup)
        (add-hook 'minibuffer-exit-hook #'nn-fido-frame-exit)
        (advice-add 'icomplete-exhibit :after #'nn-fido-frame-resize)
        (advice-add 'max-mini-window-lines :around #'nn-fido-frame--max-mini-lines)
        (advice-add 'window--resize-mini-frame :around #'nn-fido-frame--resize-mini-frame-advice))
    (setq resize-mini-frames nil)
    (remove-hook 'minibuffer-setup-hook #'nn-fido-frame-setup)
    (remove-hook 'minibuffer-exit-hook #'nn-fido-frame-exit)
    (advice-remove 'icomplete-exhibit #'nn-fido-frame-resize)
    (advice-remove 'max-mini-window-lines #'nn-fido-frame--max-mini-lines)
    (advice-remove 'window--resize-mini-frame #'nn-fido-frame--resize-mini-frame-advice)
    (when (frame-live-p nn-fido--redirected-frame)
      (redirect-frame-focus nn-fido--redirected-frame nil)
      (setq nn-fido--redirected-frame nil))
    (when (frame-live-p nn-fido--frame)
      (delete-frame nn-fido--frame)
      (setq nn-fido--frame nil))))
 ;; (nn-fido-frame-mode)


;; (setq enable-recursive-minibuffers t)        ;在 minibuffer 中也可以再次使用 minibuffer
;; (setq history-delete-duplicates t)          ;minibuffer 删除重复历史
;; (setq minibuffer-prompt-properties;minibuffer prompt 只读，且不允许光标进入其中
;;               '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
;; ;; (setq read-buffer-completion-ignore-case nil)
;; ;; (setq read-file-name-completion-ignore-case nil)
;; ;; (setq completion-ignore-case nil)      ;company-capf 匹配时不区分大小写

;; (setq completion-cycle-threshold 3)
;; ;; (setq completion-flex-nospace t)              ;default t
;; (setq completion-pcm-complete-word-inserts-delimiters t) ;partial-completion in completion-styles
;; ;; (setq completion-pcm-word-delimiters "-_/:| ")
;; (setq completion-auto-help nil)         ;不主动弹出 *Completions*
;; (setq completions-format 'one-column)   ; *Completions* buffer M-v 跳到*Completions* buffer
;; (setq completions-header-format nil)
;; (setq max-mini-window-height 4)        ;selectrum-num-candidates-displayed 受影响
;; (setq completions-max-height 10)
;; ;; (setq completion-auto-select nil)
;; (setq completions-detailed t)
;; (setq completion-show-help nil) ;*Completions* show help
;; (setq eldoc-echo-area-use-multiline-p 2)
;; (setq resize-mini-windows 'grow-only)
;; ;; (setq read-answer-short t)
;; (setq minibuffer-eldef-shorten-default t)
;; (minibuffer-electric-default-mode 1)    ;当输入内容后，prompt 的 default 值就会被隐藏
;; (file-name-shadow-mode 1)
;; (minibuffer-depth-indicate-mode 1)                   ;显示 minibuffer 深度

;; (define-key completion-list-mode-map (kbd "C-g") 'quit-window) ;*Completions*

;; (defface vmacs-minibuffer-font
;;   `((t :inherit default :height 1.0))
;;   "The default font for minibuffer buffer.
;; Monospaced font whihc is fixed idth and height is recommended."
;;   :group 'minibuffer)

;; (defun vmacs-minibuffer-hook()
;;   ;; (set (make-local-variable 'buffer-face-mode-face) 'vmacs-minibuffer-font)
;;   ;; (buffer-face-mode t)
;;   (local-set-key (kbd "C-.") 'completion-at-point)
;;   (local-set-key (kbd "<C-m>") 'exit-minibuffer)
;;   (local-set-key (kbd "<return>") 'exit-minibuffer)
;;   (local-set-key (kbd "<C-h>") 'backward-delete-char-untabify)
;;   (local-set-key (kbd "C-l") 'backward-kill-word)
;;   (local-set-key [escape] 'abort-recursive-edit)
;;   (local-set-key (kbd "TAB") 'minibuffer-complete)
;;   (local-set-key (kbd "<tab>") 'minibuffer-complete)
;;   (local-set-key  (kbd "<f19>") #'ignore) ;详见 isearch-pre-command-hook

;;   ;; (define-key minibuffer-local-completion-map (kbd "C-e") 'minibuffer-complete)
;;   (define-key minibuffer-local-map (kbd "M-p") 'previous-history-element)
;;   (define-key minibuffer-local-map (kbd "M-n") 'next-history-element)
;;   ;; (define-key minibuffer-local-must-match-map (kbd "<C-m>") 'exit-minibuffer)
;;   ;; (define-key minibuffer-local-map (kbd "<C-m>") 'exit-minibuffer)
;;   ;; (define-key minibuffer-local-completion-map (kbd "<C-m>") 'exit-minibuffer)
;;   ;; (define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)

;;   ;; (autoload 'minibuffer-keyboard-quit "delsel" "" t nil)
;;   ;; (define-key minibuffer-local-map [escape]  'minibuffer-keyboard-quit)
;;   )

;; (add-hook 'minibuffer-setup-hook #'vmacs-minibuffer-hook)




;; ;; (when (file-directory-p "~/.emacs.d/submodule/mini-frame")
;; ;;   (add-to-list 'load-path "~/.emacs.d/submodule/mini-frame"))
;; ;; ;; ;; 把 minibuffer 搬到一个特定的 frame 上
;; (setq mini-frame-resize-max-height max-mini-window-height)
;; (setq mini-frame-internal-border-color "gray80")
;; (setq mini-frame-standalone t)
;; (when (and (not noninteractive) (require 'mini-frame nil t)) ;batch 模式下 miniframe 有问题
;;   (add-to-list 'mini-frame-ignore-functions 'y-or-n-p)
;;   (add-to-list 'mini-frame-ignore-functions 'yes-or-no-p)
;;   (add-to-list 'mini-frame-ignore-commands 'consult-focus-lines)
;;   (add-to-list 'mini-frame-ignore-commands 'consult-hide-lines)
;;   (add-to-list 'mini-frame-ignore-commands 'evil-ex-search-forward)
;;   (add-to-list 'mini-frame-ignore-commands 'evil-ex-search-backward)
;;   (setq mini-frame-show-parameters
;;         '((top . 0.4)
;;           (width . 0.9)
;;           (left . 0.5)
;;           (min-height .  2)
;;           (height . 15)
;;           (minibuffer-exit . t)
;;           ;; (font . "Sarasa Mono SC Nerd-22")
;;           (alpha . 100)
;;           ;; (left-fringe . 10 )
;;           (cursor-color . "Yellow")
;;           (background-color . "black")
;;           (background-mode . 'dark)))
;;   (mini-frame-mode 1))



(provide 'conf-minibuffer)
