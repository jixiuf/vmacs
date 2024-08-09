(setq pop-up-frames nil)
(setq pop-up-windows nil)
(setq display-buffer-reuse-frames t)
;; (setq pop-up-frames 'graphic-only)
(setq org-agenda-window-setup 'current-window)
(setq org-src-window-setup 'current-window)
;; kill frames when a buffer is buried, makes most things play nice with
;; frames
(setq frame-auto-hide-function 'delete-frame)

;; 窗口相关操作，
;; 当分屏时，默认的 emacs 是 C-x 2 ,C-x 3 两个窗口中显示的内容都是同一个 buffer
;; 此处则在新开的窗口中显示不同的 buffer

(global-set-key (kbd "C-x 2")  'vmacs-split-window-vertically)
(global-set-key (kbd "C-x 3")  'vmacs-split-window-horizontally)
(global-set-key (kbd "C-M-1") 'move-border-up)
(global-set-key (kbd "C-M-2") 'move-border-down)
(global-set-key (kbd "C-M-3") 'move-border-left)
(global-set-key (kbd "C-M-4") 'move-border-right)
(global-set-key (kbd "s-C-M-1") 'move-border-up)
(global-set-key (kbd "s-C-M-2") 'move-border-down)
(global-set-key (kbd "s-C-M-3") 'move-border-left)
(global-set-key (kbd "s-C-M-4") 'move-border-right)

(global-set-key (kbd "C-s-\\") 'toggle-split-window)

(vmacs-leader (kbd "2") 'vmacs-split-window-vertically) ;横着分屏
(vmacs-leader (kbd "3") 'vmacs-split-window-horizontally) ;竖着分屏
(vmacs-leader (kbd "1") 'vmacs-delete-other-frame) ;只保留当前窗口
(vmacs-leader (kbd "0") 'vmacs-delete-frame)        ;删除当前窗口
(vmacs-leader (kbd "C-s-m") 'vmacs-fullscreen)
(global-set-key (kbd "C-s-m") 'vmacs-fullscreen)
(defvar vmacs--fullscreen-window-configuration nil)
(defun vmacs-fullscreen()
  (interactive)
  (if (and (eq this-command 'vmacs-fullscreen)
           (eq last-command 'vmacs-fullscreen)
           (not (string-equal "1" (string-trim (shell-command-to-string "ws_id=$(hyprctl activeworkspace -j|jq -r '.id');hyprctl clients -j | jq -cr '.[] | select(.workspace.id == '$ws_id')'|wc -l")))))
      (progn
        (call-process "hyprctl" nil nil nil "dispatch" "fullscreen" "1")
        (setq this-command 'toggle-fullscreen))
    (if (> (length (window-list)) 1)
        (progn
          (setq vmacs--fullscreen-window-configuration (current-window-configuration))
          (vmacs-delete-other-frame))
      (when vmacs--fullscreen-window-configuration
        (set-window-configuration vmacs--fullscreen-window-configuration)
        (setq vmacs--fullscreen-window-configuration nil)))))

(global-set-key (kbd "C-s-o") 'vmacs-other-window)
(defvar vmacs-window-status nil)
(defun vmacs-focus()
  (if (frame-focus-state)
      (unless (and vmacs-window-status
                   (window-live-p vmacs-window-status))
        (setq vmacs-window-status (selected-window)))
    ;; (setq vmacs-window-status (selected-window))
    (setq vmacs-window-status nil)))
(add-function :after after-focus-change-function #'vmacs-focus)

(defun vmacs-other-window()
  (interactive)
  (select-window (next-window))
  (when (eq (selected-window) vmacs-window-status)
    (call-process "other-window" nil nil nil "skip-emacs"))
  (unless (and vmacs-window-status
               (window-live-p vmacs-window-status))
    (setq vmacs-window-status (selected-window))))


(defun vmacs-delete-frame()
  (interactive)
  (if (= (gui-frame-cnt) 1)
        (call-interactively #'delete-window)
      (delete-frame)))

(defun vmacs-delete-other-frame()
  (interactive)
  (let ((curframe (selected-frame))
        (fs (frame-list)))
    (if (<= (gui-frame-cnt) 1)
        (vmacs-delete-other-windows)
      (dolist (f fs)
        (unless (equal f curframe)
          (delete-frame f))))))


(defun vmacs-kill-buffer-delete-window()
  (interactive)
  (cl-letf (((symbol-function #'delete-window)
             #'vmacs-delete-window))
    (kill-buffer-and-window)))

(defvar vmacs--delete-window (symbol-function #'delete-window))
(defun vmacs-delete-window(&optional win)
  (interactive)
  (let ((main-win (window-at-x-y (- (/ (frame-pixel-width) 2) 20) 20))
        (win (or win (selected-window)))
        )
    (if (eq main-win win)
        (progn
          (set-window-buffer main-win (window-buffer (next-window win)))
          (funcall vmacs--delete-window (next-window win)))
      (funcall vmacs--delete-window win))))

(defun vmacs-delete-other-windows()
  (interactive)
  (let ((main-win (window-at-x-y 20 20)))
    (if (eq main-win (get-buffer-window))
        (delete-other-windows)
      (set-window-buffer main-win (current-buffer))
      (delete-other-windows main-win))))

;; 尽量保证光标所在的窗口最大
;; 黄金分隔 多窗口操作时
;; (golden-ratio-mode 1)
;; Work with ediff and helm
;; (setq golden-ratio-adjust-factor 0.91)
;; (setq golden-ratio-extra-commands
;;       (append golden-ratio-extra-commands
;;               '(
;;                 dap-hydra/dap-ui-locals
;;                 dap-ui-locals
;;                 evil-window-left
;;                 evil-window-right
;;                 evil-window-up
;;                 evil-window-down
;;                 avy-goto-char-timer
;;                 avy-goto-word-1
;;                 avy-goto-char-timer
;;                 evil-window-rotate-downwards
;;                 vmacs-split-window-or-other-window
;;                 vmacs-split-window-or-prev-window
;;                 magit-show-commit
;;                 magit-stash-show
;;                 avy-goto-word-1
;;                 ace-jump-mode-pop-mark)))

;; (defun vmacs-evil-window-cmd-p ()
;;   (when (symbolp last-command)
;;     (string-prefix-p "evil-window-" (symbol-name last-command))))

;; (add-to-list 'golden-ratio-inhibit-functions 'vmacs-evil-window-cmd-p)

;; (setq pop-up-windows t)                    ; display-buffer: avoid splitting
;; (setq even-window-sizes nil)               ; display-buffer: avoid resizing

;; ;; fix window splitting behavior when possible
;; https://emacs-china.org/t/display-buffer-alist/8162/4
(setq display-buffer-alist
      '(((lambda (bufname _)
           (memq this-command '( next-error previous-error compile-goto-error)))
         (display-buffer-same-window )
         (inhibit-same-window . nil))
        (vmacs-same-window-buffer
         (display-buffer-same-window)
         (inhibit-same-window . nil))
        ;; ("\\*xref\\*"
        ;;  (display-buffer-reuse-window display-buffer-at-bottom) ;display-buffer-in-direction
        ;;  )
        ;; default
        ;; (".*" (display-buffer-pop-up-frame))
        )
      )
;; (when (eq system-type 'gnu/linux)
;;   (add-to-list 'display-buffer-alist '(".*" (display-buffer--maybe-pop-up-frame)) t))

(defun vmacs-same-window-buffer(bufname _)
  (when (bufferp bufname)
    (setq  bufname (buffer-name bufname))
    )
  (or
   (string-match-p (rx (or
                        "*Agenda Commands*"
                        " *transient*"
                        "*Org Agenda*"
                        "*grep*"))
                   bufname)
   (string-prefix-p "magit:" bufname)
   (string-prefix-p "*Embark " bufname)
   (string-prefix-p "*Annotate " bufname)))

;; 左右分屏
(defun vmacs-display-buffer-pop-up-horizontally (buffer alist)
  "A `display-buffer' ACTION forcing a vertical window split.
    See `split-window-sensibly' and `display-buffer-pop-up-window'."
  (let ((split-width-threshold 0)
        (split-height-threshold nil))
    (display-buffer-pop-up-window buffer alist)))

;; 上下分屏
(defun vmacs-display-buffer-pop-up-vertically (buffer alist)
  "A `display-buffer' ACTION forcing a vertical window split.
    See `split-window-sensibly' and `display-buffer-pop-up-window'."
  (let ((split-width-threshold 0)
        (split-height-threshold nil))
    (display-buffer-pop-up-window buffer alist)))

;; (defun vmacs-helm-alive-p ()
;;   (if (boundp 'helm-alive-p)
;;       (symbol-value 'helm-alive-p)))

;; (add-to-list 'golden-ratio-inhibit-functions 'vmacs-helm-alive-p)

;; q kill buffer,C-uq bury

(defun vmacs-quit-and-kill-window ()
  "Kill buffer and its window on quitting"
  (local-set-key (kbd "q") 'vmacs-kill-buffer-dwim))
(add-hook 'special-mode-hook 'vmacs-quit-and-kill-window)

(provide 'conf-evil-window)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-evil-window.el ends here.
