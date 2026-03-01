;;; conf-ewm.el --- Description -*- lexical-binding: t; -*-

;; EWM_MODULE_PATH=~/repos/ewm/compositor/target/debug/libewm_core.so   emacs   --fg-daemon -L ~/repos/ewm/lisp -l ewm -f ewm-start-module
(add-to-list 'load-path (expand-file-name "~/repos/ewm/lisp"))
(setenv "EWM_MODULE_PATH" (expand-file-name "~/repos/ewm/compositor/target/debug/libewm_core.so"))
(setenv "XDG_CONFIG_HOME" (expand-file-name "~/.config"))
(when (string-equal (getenv "XDG_SESSION_DESKTOP") "ewm")
  (require 'ewm)
  (setenv "XDG_SESSION_DESKTOP" "ewm")
  (shell-command "/usr/bin/dbus-update-activation-environment --systemd --all;")
  ;; /usr/local/bin/xremap-ewm /home/jixiuf/.config/xremap/xremap.yaml /home/jixiuf/.config/xremap/xremap-ewm.yaml --watch=device --ignore=dotool keyboard --ignore=Yubico YubiKey OTP+CCID

  ;; /usr/bin/dbus-update-activation-environment --systemd --all
  ;; EWM compositor must start immediately in daemon mode (runs on TTY)
  ;; It doesn't need a graphical frame - the compositor creates the display
  (defun vmacs-ewm-init()
    (ewm-start-module)

    (start-process-shell-command
     "import-environment" nil
     "systemctl --user import-environment XCURSOR_SIZE GTK_THEME SSH_AUTH_SOCK GPG_TTY DISPLAY  WAYLAND_DISPLAY  XDG_CURRENT_DESKTOP XDG_SESSION_DESKTOP XDG_RUNTIME_DIR QT_QPA_PLATFORM _JAVA_AWT_WM_NONREPARENTING SDL_VIDEODRIVER")
    (start-process-shell-command
     "dbus-update-activation-environment" nil
     "dbus-update-activation-environment --systemd --all")
    (start-process-shell-command     "xremap" " *xremap*"     "/usr/local/bin/xremap-ewm ~/.config/xremap/xremap.yaml ~/.config/xremap/xremap-ewm.yaml --watch=device --ignore='dotool keyboard' ")
    (start-process-shell-command     "kpmenu" nil     "kpmenu --daemon")
    (start-process-shell-command     "swaylock" nil     "pidof swaylock || swaylock")
    (start-process-shell-command     "hypridle" nil     "pidof hypridle||hypridle")
    (start-process-shell-command     "wl-paste" nil     "pidof wl-paste ||wl-paste --watch cliphist-store&")
    (start-process-shell-command     "dex" nil     "dex -a -s /etc/xdg/autostart/:~/.config/autostart/")
    (start-process-shell-command     "sway-session" nil     "systemctl --user restart sway-session.target")
    )

  (add-hook 'after-init-hook #'vmacs-ewm-init)

  (setq ewm-output-config
        '(("DP-1" :width 2560 :height 1440 :scale 1.0)
          ("eDP-1" :width 1920 :height 1200 :scale 1.55 :x 0 :y 0)))

  (setq ewm-input-config
        '((touchpad :natural-scroll t :tap t :dwt t)
          (mouse :accel-profile "flat")
          (trackpoint :accel-speed 0.5)
          ;; libinput list-devices                         # system-wide
          ;; Per-device override (exact name from libinput)
          ("ELAN0676:00 04F3:3195 Touchpad" :tap nil :accel-speed -0.2)))
  (ewm--send-input-config)

  (defvar consult-source-xdg-apps
    `(:name "Apps"
            :narrow ?a
            :category app
            :items ,(lambda ()
                      (mapcar #'car (ewm-list-xdg-apps)))
            :action ,#'ewm-launch-xdg-command))
  (with-eval-after-load 'conf-icomplete
    (add-to-list 'consult-buffer-sources 'consult-source-xdg-apps)
  )
  )

(provide 'conf-ewm)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-ewm.el ends here.
