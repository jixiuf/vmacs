#!/usr/bin/env bash
export XDG_SESSION_DESKTOP=ewm
export EWM_MODULE_PATH=~/repos/ewm/compositor/target/debug/libewm_core.so
export XDG_CONFIG_HOME=~/.config
# key classification logs go to journald: journalctl --user -t ewm -f
export RUST_LOG="${RUST_LOG:-ewm=debug,smithay=warn}"
systemctl --user stop emacs
systemctl --user import-environment XCURSOR_SIZE GTK_THEME SSH_AUTH_SOCK GPG_TTY DISPLAY  WAYLAND_DISPLAY  XDG_CURRENT_DESKTOP XDG_SESSION_DESKTOP XDG_RUNTIME_DIR QT_QPA_PLATFORM _JAVA_AWT_WM_NONREPARENTING SDL_VIDEODRIVER
dbus-update-activation-environment --systemd --all

# graphical-session.target 配置了 RefuseManualStart=yes，
# 只能作为依赖被拉起。创建一个常驻的临时单元 Requires 它：
# - 它活着 → target 保持 active → waybar/swayidle/swaync 自动启动
# - 退出时 stop 它 → target 变 unneeded 自动停 → PartOf 的服务跟着停
systemd-run --user --unit=ewm-session \
  --property=Requires=graphical-session.target \
  --property=After=graphical-session.target \
  sleep infinity

cleanup() {
  systemctl --user stop ewm-session.service
}
trap cleanup EXIT

emacs --fg-daemon -L ~/repos/ewm/lisp -l ewm
