#!/usr/bin/env bash
export XDG_SESSION_DESKTOP=ewm
export EWM_MODULE_PATH=~/repos/ewm/compositor/target/debug/libewm_core.so
systemctl --user stop emacs
systemctl --user import-environment XCURSOR_SIZE GTK_THEME SSH_AUTH_SOCK GPG_TTY DISPLAY  WAYLAND_DISPLAY  XDG_CURRENT_DESKTOP XDG_SESSION_DESKTOP XDG_RUNTIME_DIR QT_QPA_PLATFORM _JAVA_AWT_WM_NONREPARENTING SDL_VIDEODRIVER
dbus-update-activation-environment --systemd --all
emacs --fg-daemon -L ~/repos/ewm/lisp -l ewm
