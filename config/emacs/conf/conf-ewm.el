;;; conf-ewm.el --- Description -*- lexical-binding: t; -*-

;; EWM_MODULE_PATH=~/repos/ewm/compositor/target/debug/libewm_core.so   emacs   --fg-daemon -L ~/repos/ewm/lisp -l ewm -f ewm-start-module
(add-to-list 'load-path (expand-file-name "~/repos/ewm/lisp"))
(setenv "EWM_MODULE_PATH" (expand-file-name "~/repos/ewm/compositor/target/debug/libewm_core.so"))
(when (string-equal (getenv "XDG_SESSION_DESKTOP") "ewm")
  (require 'ewm)
  ;; EWM compositor must start immediately in daemon mode (runs on TTY)
  ;; It doesn't need a graphical frame - the compositor creates the display
  (add-hook 'after-init-hook #'ewm-start-module)

  ;; (ewm-start-module)
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
