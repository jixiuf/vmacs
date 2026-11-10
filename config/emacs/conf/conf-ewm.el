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

;;xremap rename: space to [f18, { escape_next_key: true }]
(keymap-set ewm-mode-map "<Launch9>" (ewm-key :intercept)) ;<Launch9>=f18
(define-key key-translation-map (kbd "<Launch9>") (kbd "C-c")) ;<Launch9>=f18
(keymap-set ewm-mode-map "<f19>" (ewm-key :intercept))

(define-keymap :keymap ewm-mode-map
  "<swipe-4-up>"     (ewm-key #'ewm-overview-open :fullscreen)  ; the overview on four fingers
  "<swipe-4-down>"     (ewm-key #'ewm-overview-close :fullscreen)  ; the overview off four fingers
  "<swipe-4-right>"     (ewm-key #'ewm-frame-left :fullscreen)  ; the overview off four fingers
  ;; "<swipe-3-up>"     #'my-screenshot                             ; three fingers up, yours
  ;; "s-<swipe-3-down>" #'my-control-panel                          ; with Super held
  "<swipe-4-left>"   (ewm-key #'ewm-frame-right :fullscreen))   ; four fingers left, the next frame when they lift


;; (defun my/xremap-ipc (command)
;;   "Send COMMAND to the xremap IPC socket (fire-and-forget)."
;;   (condition-case err
;;       (let ((proc (make-network-process
;;                    :name "xremap-ipc" :family 'local :noquery t
;;                    :service (or (getenv "XREMAP_SOCKET")
;;                                 (expand-file-name
;;                                  "xremap.sock"
;;                                  (or (getenv "XDG_RUNTIME_DIR") "/run/user/1000"))))))
;;         (process-send-string proc (concat command "\n"))
;;         (message "xremap ipc: %s" command)
;;         (delete-process proc))
;;     (error (message "xremap ipc: %s" err))))

;; (defun my/ewm-sync-xremap-mode (state)
;;   "把全局 modal 状态同步给 xremap 的 mode。
;; 通过 xremap 内置 IPC socket 发 set-mode，不再依赖 dotool 合成按键。
;; motion/visual 等中间态沿用原语义归并到 normal（与原 f13 行为一致）。"
;;   (my/xremap-ipc
;;    (concat "set-mode " (if (eq state 'insert) "insert" "normal"))))


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

(run-with-timer 3 nil 'vmacs-ewm-init-app)
  ;; (setopt ewm-text-input-provider 'external)
  ;; (start-process "fcitx5" nil "fcitx5" "--disable=xim")
  )
(defun vmacs-ewm-init-app()
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
  (start-process-shell-command     "waybar" " *waybar*"     "systemctl --user restart waybar"))



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
