;;; conf-wayland.el --- Description -*- lexical-binding: t; -*-
;; EWM_MODULE_PATH=~/repos/ewm/compositor/target/debug/libewm_core.so   emacs   --fg-daemon -L ~/repos/ewm/lisp -l ewm

;; (add-to-list 'load-path (expand-file-name "~/repos/ewm/lisp"))
;; (setenv "EWM_MODULE_PATH" (expand-file-name "~/repos/ewm/compositor/target/debug/libewm_core.so"))
;; (setenv "XDG_CONFIG_HOME" (expand-file-name "~/.config"))
(require 'ewm)
(global-set-key (kbd "C-<tab>") #'consult-buffer)
;; Intercepted keys bind in `ewm-mode-map' through `ewm-key'; the
;; `ewm-intercept-prefixes' variable is obsolete since 2026-10.
(keymap-set ewm-mode-map "C-<tab>" (ewm-key :intercept))
(keymap-set ewm-mode-map "C-s-d" (ewm-key :intercept))
(keymap-set ewm-mode-map "C-M-s-d" (ewm-key :intercept))
(keymap-set ewm-mode-map "<f11>" (ewm-key :intercept))
(keymap-set ewm-mode-map "M-:" (ewm-key :intercept))
(define-keymap :keymap ewm-mode-map
  "<swipe-4-up>"     (ewm-key #'ewm-overview-open :fullscreen)  ; the overview on four fingers
  "<swipe-4-down>"     (ewm-key #'ewm-overview-close :fullscreen)  ; the overview off four fingers
  "<swipe-4-right>"     (ewm-key #'ewm-frame-left :fullscreen)  ; the overview off four fingers
  ;; "<swipe-3-up>"     #'my-screenshot                             ; three fingers up, yours
  ;; "s-<swipe-3-down>" #'my-control-panel                          ; with Super held
  "<swipe-4-left>"   (ewm-key #'ewm-frame-right :fullscreen))   ; four fingers left, the next frame when they lift

;; ── helixel 联动：modal 键（SPC/i）按状态进出 compositor 拦截列表 ──
;; normal/motion/visual → 两键被拦截：SPC→leadkey(C-c leader)、i→进 insert；
;; insert 态 → 两键放行直达应用；
;; ESC 任何状态都拦截 → 回 normal + 拉回键盘（SPC leader 由此可用）；
;; 要给应用发 ESC 按 f8；Emacs 侧 f8 仍是 helixel 的 repeat（:filter 透明）。
;; 临时文件日志（排查用）：/tmp/ewm-lisp.log
(defun my/ewm-dbg (fmt &rest args)
  ;; (ignore-errors
  ;;   (append-to-file
  ;;    (concat (format-time-string "[%H:%M:%S.%3N] ")
  ;;            (apply #'format fmt args) "\n")
  ;;    nil "/tmp/ewm-lisp.log"))
  )

;; ;; 送达链路：compositor → Emacs 的 intercepted-command
;; (defun my/ewm-dbg-intercepted (event)
;;   (my/ewm-dbg "intercepted-command: %S" (alist-get 'key event)))
;; (advice-add 'ewm--handle-intercepted-command :before #'my/ewm-dbg-intercepted)

(defun my/ewm-spc ()
  "SPC 兑底：leadkey 不在场时，surface 收空格，否则 self-insert。"
  (interactive)
  (if (ewm-surface-buffer-p (window-buffer))
      (ewm-send-key "SPC")
    (self-insert-command 1)))

(defun my/ewm-modal-key ()
  "compositor 拦截送达的 SPC/i/ESC 分发（SPC 正常时 leadkey 先劫走）。"
  (interactive)
  (my/ewm-dbg "modal-key: ev=%S state=%S buf=%S win-buf=%S"
              last-command-event my/hx-last-state (current-buffer)
              (window-buffer (selected-window)))
  (pcase (event-basic-type last-command-event)
    (32 (my/ewm-spc))
    (?i (my/ewm-insert-to-surface))
    ((or 27 'escape) (my/ewm-focus-emacs))
    ('f8 (my/ewm-send-esc-to-app))))

(defun my/ewm-modal-filter (_)
  "modal 键绑定只在 surface 上下文对 Emacs 可见，其余场合（isearch/minibuffer）完全透明。"
  (and (ewm-surface-buffer-p (window-buffer)) #'my/ewm-modal-key))

(defvar my/ewm-modal-keys '("SPC" "i")
  "随 helixel 状态进出 compositor 拦截列表的键。")

(defun my/ewm-modal-apply ()
  "按 my/hx-last-state 增删 modal 键，并同步重发拦截列表。"
  (let ((def (unless (eq my/hx-last-state 'insert)
               (list 'menu-item "" #'my/ewm-modal-key
                     :filter #'my/ewm-modal-filter))))
    (dolist (key my/ewm-modal-keys)
      (unless (equal (lookup-key ewm-mode-map (kbd key)) def)
        (define-key ewm-mode-map (kbd key) def)))
    (my/ewm-dbg "modal-apply: state=%S SPC-bound=%S"
                my/hx-last-state
                (not (null (lookup-key ewm-mode-map "SPC"))))
    (when (fboundp 'ewm--send-intercept-keys)
      (ewm--send-intercept-keys))))

;; tab-line 背景随 helixel 状态变化：normal=蓝，insert=绿
(defface my/ewm-tab-normal
  '((t :background "#2d7a4a"))
  "Tab-line background for helixel normal/motion/visual state.")
(defface my/ewm-tab-insert
  '((t :background "#27408b"))
  "Tab-line background for helixel insert state.")

(defun my/ewm-apply-state-face ()
  "Copy the state face's background onto the active window's tab-line."
  (let ((bg (face-background (if (eq my/hx-last-state 'insert)
                                 'my/ewm-tab-insert
                               'my/ewm-tab-normal)
                             nil 'default)))
    ;; tab-line-active
    (dolist (target '( tab-line-tab-current))
      (when (facep target)
        (set-face-background target bg)))
    (dolist (frame (frame-list))
      (dolist (win (window-list frame 'never))
        (set-window-parameter win 'tab-line-cache nil)))
    (force-mode-line-update t)))

(defun my/hx-set-state (state)
  "设置全局 helixel 状态并同步 modal 键拦截列表。"
  (my/ewm-dbg "hx-set-state: %S -> %S (boundp-ewm-map=%S)"
              my/hx-last-state state (boundp 'ewm-mode-map))
  (when (and (boundp 'ewm-mode-map) (not (eq my/hx-last-state state)))
    (setq my/hx-last-state state)
    (my/ewm-modal-apply)
    (my/ewm-apply-state-face)))

(defun my/ewm-modal-sync ()
  ;; 只在键盘真在 Emacs frame 上时才同步：
  ;; 否则 xremap 启动键的 emacsclient eval、定时器等在应用聚焦时跑的
  ;; 任意命令，会把全局状态偷改成那些 buffer 的状态（偶发 i 被拦截的根因）。
  (when (and (boundp 'helixel--current-state)
             (not (ewm-surface-buffer-p (window-buffer)))
             (fboundp 'ewm-input--emacs-frame-focused-p)
             (ewm-input--emacs-frame-focused-p))
    (my/hx-set-state helixel--current-state)))
(add-hook 'post-command-hook #'my/ewm-modal-sync)

;; 聚焦应用（点击/s-tab/焦点镜像）默认进入 insert 态，
;; 键直达应用；想要 leader 按 ESC 回 normal。
(defun my/ewm-focus-to-insert (_frame)
  (when (and (boundp 'ewm-mode-map)
             (ewm-surface-buffer-p (window-buffer (selected-window))))
    (my/hx-set-state 'insert)))
(add-hook 'window-selection-change-functions #'my/ewm-focus-to-insert)
;; switch-to-buffer 换到 surface buffer 时只触发 buffer-change，
;; 不触发 selection-change（如 xremap run-or-raise 的聚焦路径）
(add-hook 'window-buffer-change-functions #'my/ewm-focus-to-insert)

(my/ewm-apply-state-face)

;; 从应用拉回键盘，并强制归位 normal 态。
;; 若没有非 surface 窗口，现分一个出来，保证键盘一定能回来。
(defun my/ewm-focus-emacs ()
  (interactive)
  (message "normal")
  (my/ewm-dbg "focus-emacs entered")
  (my/hx-set-state 'normal)
  ;; (let ((win (get-window-with-predicate
  ;;             (lambda (w) (not (ewm-surface-buffer-p (window-buffer w))))
  ;;             'never (selected-frame))))
  ;;   (if win (select-window win)
  ;;     (select-window (split-window))
  ;;     (switch-to-buffer (get-buffer-create "*scratch*"))))
  ;; (with-current-buffer (window-buffer (selected-window))
  ;;   (when (fboundp 'helixel-enter-normal-state)
  ;;     (helixel-enter-normal-state)))
  )

;; 进入 insert 态；当前窗口是 surface 时把键盘交给应用。
;; 之后空格直达应用（SPC 已从拦截列表移除）。
(defun my/ewm-insert-to-surface ()
  (interactive)
  (message "insert")
  (my/hx-set-state 'insert)
  (when (ewm-surface-buffer-p (window-buffer))
    (ewm-layout--refresh)))

;; ESC：所有状态都拦截 → 回 normal + 拉回键盘。
;; 必须绑 "<escape>"（xkb keysym Escape）；绑 "\e"=[27] 是 Latin-1 控制字符，
;; 永远匹配不上物理 Esc 键。:filter 保证 Emacs 里的 Esc 行为完全不受影响。
;; f8：把真 ESC 发给应用（Firefox 关全屏/弹窗）；Emacs 侧 f8 仍是 helixel repeat。
(defun my/ewm-send-esc-to-app ()
  (interactive)
  (ewm-send-key "<escape>"))
(keymap-set ewm-mode-map "<escape>"
            (list 'menu-item "" #'my/ewm-modal-key
                  :filter #'my/ewm-modal-filter :ewm '(:fullscreen)))
(keymap-set ewm-mode-map "<f8>"
            (list 'menu-item "" #'my/ewm-send-esc-to-app
                  :filter #'my/ewm-modal-filter :ewm '(:fullscreen)))

;; 应用聚焦时的 C-c leader 替身（不偷应用的 C-c）
                                        ;  (keymap-set ewm-mode-map "s-SPC" (ewm-key mode-specific-command-prefix :fullscreen))

;; surface 代理 buffer 禁用 helixel normal 态；ewm-edit buffer 归位 normal
(with-eval-after-load 'helixel-state
  (add-to-list 'helixel-major-mode-default-states '(ewm-surface-mode . motion)))
(add-hook 'ewm-edit-mode-hook
          (lambda () (when (fboundp 'helixel-enter-normal-state)
                       (helixel-enter-normal-state))))

;; (ewm--send-intercept-keys)
(setq ewm-output-config ;; wlr-randr
      '(("eDP-1" :width 2880 :height 1800 :scale 1.75 :x 0 :y 0)))

(setopt ewm-input-config
        '((touchpad :natural-scroll t
                    :tap t  :dwt t
                    :accel-speed 0.48
                    :scroll-method "two-finger"
                    :accel-profile "adaptive")
          (mouse :accel-profile "flat" :accel-speed -0.25)
          (trackpoint :accel-speed 0.5)
          ;; libinput list-devices                         # system-wide
          ;; Per-device override (exact name from libinput)
          ("ELAN0676:00 04F3:3195 Touchpad" :tap nil :accel-speed -0.2)))

;; EWM compositor must start immediately in daemon mode (runs on TTY)
;; It doesn't need a graphical frame - the compositor creates the display
(defun vmacs-ewm-init()
  (require 'lazy-wayland)
  (setq wayland-compositor 'ewm)

  (ewm--send-intercept-keys)
  (ewm-start-module)

  (start-process-shell-command
   "import-environment" nil
   "systemctl --user import-environment XCURSOR_SIZE GTK_THEME SSH_AUTH_SOCK GPG_TTY DISPLAY  WAYLAND_DISPLAY  XDG_CURRENT_DESKTOP XDG_SESSION_DESKTOP XDG_RUNTIME_DIR QT_QPA_PLATFORM _JAVA_AWT_WM_NONREPARENTING SDL_VIDEODRIVER")
  (start-process-shell-command "dbus-update-activation-environment" nil "dbus-update-activation-environment --systemd --all")
  (start-process-shell-command     "kpmenu" nil     "kpmenu --daemon")
  (start-process-shell-command     "swaylock" nil     "pidof swaylock || swaylock")
  (start-process-shell-command     "hypridle" nil     "pidof hypridle||hypridle")
  (start-process-shell-command     "wl-paste" nil     "pidof wl-paste ||wl-paste --watch cliphist-store&")
  (start-process-shell-command     "dex" nil     "dex -a -s /etc/xdg/autostart/:~/.config/autostart/")
  (start-process-shell-command     "sway-session" nil     "systemctl --user restart sway-session.target")
  (start-process-shell-command     "xremap" " *xremap*"     "systemctl --user restart xremap")
  (start-process-shell-command     "waybar" " *waybar*"     "systemctl --user restart waybar")
  ;; (setopt ewm-text-input-provider 'external)
  ;; (start-process "fcitx5" nil "fcitx5" "--disable=xim")
  )



(defvar consult-source-xdg-apps
  `(:name "Apps"
          :narrow ?a
          :category app
          :items ,(lambda ()
                    (mapcar #'car (ewm-list-xdg-apps)))
          :action ,#'ewm-launch-xdg-command))
(with-eval-after-load 'conf-icomplete
  (add-to-list 'consult-buffer-sources 'consult-source-xdg-apps))

(provide 'conf-ewm)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-wayland.el ends here.
