;;; conf-wayland.el --- Description -*- lexical-binding: t; -*-
(require 'reka)
(setopt reka-intercept-prefixes '("C-x" "C-u" "C-h" "M-x" "M-:"))

(defun reka-key (key)
  (add-to-list 'reka-intercept-prefixes key)
  (when reka-handle (reka-push-intercept-prefixes))
  (kbd key))

(require 'lazy-wayland)
(setq wayland-compositor 'reka)
(global-set-key (reka-key "s-C-f") (wayland-run-or-raise :name firefox :app-id (rx (or "firefox" "firefox-bin" "firefox-esr")) :command "firefox-bin"))
(global-set-key (reka-key "s-C-d") (wayland-run-or-raise :name term :app-id (rx (or "foot" "alacritty" "foot-ws")) :command "alacritty"))



;; (global-set-key (kbd "s-C-<tab>") (ewm-exec rofi "killall rofi ||rofi -normal-window -show combi -combi-modes 'drun,run,ssh' -modes combi"))
;; (when (require 'reka nil t)  (reka-enable))
;; EWM_MODULE_PATH=~/repos/ewm/compositor/target/debug/libewm_core.so   emacs   --fg-daemon -L ~/repos/ewm/lisp -l ewm -f ewm-start-module
;; (add-to-list 'load-path (expand-file-name "~/repos/ewm/lisp"))
;; (setenv "EWM_MODULE_PATH" (expand-file-name "~/repos/ewm/compositor/target/debug/libewm_core.so"))
;; (setenv "XDG_CONFIG_HOME" (expand-file-name "~/.config"))
;; (when (string-equal (getenv "XDG_SESSION_DESKTOP") "ewm")
;;   (require 'ewm)
;;   (setenv "XDG_SESSION_DESKTOP" "ewm")
;;   (shell-command "/usr/bin/dbus-update-activation-environment --systemd --all;")
;;   ;; /usr/local/bin/xremap-ewm /home/jixiuf/.config/xremap/xremap.yaml /home/jixiuf/.config/xremap/xremap-ewm.yaml --watch=device --ignore=dotool keyboard --ignore=Yubico YubiKey OTP+CCID

;;   ;; /usr/bin/dbus-update-activation-environment --systemd --all
;;   ;; EWM compositor must start immediately in daemon mode (runs on TTY)
;;   ;; It doesn't need a graphical frame - the compositor creates the display
;;   (defun vmacs-ewm-init()
;;     (ewm-start-module)

;;     (start-process-shell-command
;;      "import-environment" nil
;;      "systemctl --user import-environment XCURSOR_SIZE GTK_THEME SSH_AUTH_SOCK GPG_TTY DISPLAY  WAYLAND_DISPLAY  XDG_CURRENT_DESKTOP XDG_SESSION_DESKTOP XDG_RUNTIME_DIR QT_QPA_PLATFORM _JAVA_AWT_WM_NONREPARENTING SDL_VIDEODRIVER")
;;     (start-process-shell-command
;;      "dbus-update-activation-environment" nil
;;      "dbus-update-activation-environment --systemd --all")
;;     ;; (start-process-shell-command     "xremap" " *xremap*"     "/usr/local/bin/xremap-ewm ~/.config/xremap/xremap.yaml ~/.config/xremap/xremap-ewm.yaml --watch=device --ignore='dotool keyboard' ")
;;     (start-process-shell-command     "kpmenu" nil     "kpmenu --daemon")
;;     (start-process-shell-command     "swaylock" nil     "pidof swaylock || swaylock")
;;     (start-process-shell-command     "hypridle" nil     "pidof hypridle||hypridle")
;;     (start-process-shell-command     "wl-paste" nil     "pidof wl-paste ||wl-paste --watch cliphist-store&")
;;     (start-process-shell-command     "dex" nil     "dex -a -s /etc/xdg/autostart/:~/.config/autostart/")
;;     (start-process-shell-command     "sway-session" nil     "systemctl --user restart sway-session.target")
;;     (start-process-shell-command     "xremap" " *xremap*"     "systemctl --user restart xremap")
;;     )

;;   (add-hook 'after-init-hook #'vmacs-ewm-init)

;; ;; niri msg outputs
;; ;; Output "Apple Computer Inc Color LCD Unknown" (eDP-1)
;; ;;   Current mode: 2880x1800 @ 60.001 Hz (preferred)
;; ;;   Variable refresh rate: not supported
;; ;;   Physical size: 330x210 mm
;; ;;   Logical position: 0, 0
;; ;;   Logical size: 1645x1028
;; ;;   Scale: 1.75
;;   (setq ewm-output-config
;;         '(("eDP-1" :width 2880 :height 1800 :scale 1.75 :x 0 :y 0)))

;;   (setq ewm-input-config
;;         '((touchpad :natural-scroll t :tap t :dwt t)
;;           (mouse :accel-profile "flat")
;;           (trackpoint :accel-speed 0.5)
;;           ;; libinput list-devices                         # system-wide
;;           ;; Per-device override (exact name from libinput)
;;           ("ELAN0676:00 04F3:3195 Touchpad" :tap nil :accel-speed -0.2)))
;;   (ewm--send-input-config)

;;   (defvar consult-source-xdg-apps
;;     `(:name "Apps"
;;             :narrow ?a
;;             :category app
;;             :items ,(lambda ()
;;                       (mapcar #'car (ewm-list-xdg-apps)))
;;             :action ,#'ewm-launch-xdg-command))
;;   (with-eval-after-load 'conf-icomplete
;;     (add-to-list 'consult-buffer-sources 'consult-source-xdg-apps)
;;   )
;;   )

(provide 'conf-wayland)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-wayland.el ends here.
