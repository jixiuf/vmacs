;;; conf-wayland.el --- Description -*- lexical-binding: t; -*-
;; EWM_MODULE_PATH=~/repos/ewm/compositor/target/debug/libewm_core.so   emacs   --fg-daemon -L ~/repos/ewm/lisp -l ewm

(add-to-list 'load-path (expand-file-name "~/repos/ewm/lisp"))
(setenv "EWM_MODULE_PATH" (expand-file-name "~/repos/ewm/compositor/target/debug/libewm_core.so"))
(setenv "XDG_CONFIG_HOME" (expand-file-name "~/.config"))
(when (string-equal (getenv "XDG_SESSION_DESKTOP") "ewm")
  (require 'ewm)
  (global-set-key (kbd "C-<tab>") #'consult-buffer)
  (add-to-list 'ewm-intercept-prefixes "C-<tab>") 
  (add-to-list 'ewm-intercept-prefixes ?\M-:)
  (ewm--send-intercept-keys)
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

  (setenv "XDG_SESSION_DESKTOP" "ewm")
  (shell-command "/usr/bin/dbus-update-activation-environment --systemd --all;")
  ;; /usr/local/bin/xremap-ewm /home/jixiuf/.config/xremap/xremap.yaml /home/jixiuf/.config/xremap/xremap-ewm.yaml --watch=device --ignore=dotool keyboard --ignore=Yubico YubiKey OTP+CCID

  ;; /usr/bin/dbus-update-activation-environment --systemd --all
  ;; EWM compositor must start immediately in daemon mode (runs on TTY)
  ;; It doesn't need a graphical frame - the compositor creates the display
  (defun ewm-key (key)
    (add-to-list 'ewm-intercept-prefixes `(,(kbd key) :fullscreen))
    (kbd key))

  (defun vmacs-ewm-init()
    (require 'lazy-wayland)
    (setq wayland-compositor 'ewm)

    (ewm--send-intercept-keys)
    (ewm-start-module)
    ;; meep need this 
    (setq interprogram-paste-function ewm--saved-interprogram-paste-function)

    (start-process-shell-command
     "import-environment" nil
     "systemctl --user import-environment XCURSOR_SIZE GTK_THEME SSH_AUTH_SOCK GPG_TTY DISPLAY  WAYLAND_DISPLAY  XDG_CURRENT_DESKTOP XDG_SESSION_DESKTOP XDG_RUNTIME_DIR QT_QPA_PLATFORM _JAVA_AWT_WM_NONREPARENTING SDL_VIDEODRIVER")
    (start-process-shell-command
     "dbus-update-activation-environment" nil
     "dbus-update-activation-environment --systemd --all")
    ;; (start-process-shell-command     "xremap" " *xremap*"     "/usr/local/bin/xremap-ewm ~/.config/xremap/xremap.yaml ~/.config/xremap/xremap-ewm.yaml --watch=device --ignore='dotool keyboard' ")
    (start-process-shell-command     "kpmenu" nil     "kpmenu --daemon")
    (start-process-shell-command     "swaylock" nil     "pidof swaylock || swaylock")
    (start-process-shell-command     "hypridle" nil     "pidof hypridle||hypridle")
    (start-process-shell-command     "wl-paste" nil     "pidof wl-paste ||wl-paste --watch cliphist-store&")
    (start-process-shell-command     "dex" nil     "dex -a -s /etc/xdg/autostart/:~/.config/autostart/")
    (start-process-shell-command     "sway-session" nil     "systemctl --user restart sway-session.target")
    (start-process-shell-command     "xremap" " *xremap*"     "systemctl --user restart xremap")
    )
  (when after-init-time
    (vmacs-ewm-init)
    (add-hook 'after-init-hook #'vmacs-ewm-init))



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

;;; conf-wayland.el ends here.
