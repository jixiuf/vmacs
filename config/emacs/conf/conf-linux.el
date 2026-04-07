;;; -*- lexical-binding: t -*-
;; 获取当前输入法
;; ibus engine
;; xkb:us::eng  or rime
;; ibus engine rime  # 将输入法更改为

(global-set-key  (kbd "s-C-q") 'save-buffers-kill-emacs)
(global-set-key  (kbd "s-C-c") 'kill-ring-save)
(global-set-key  (kbd "s-C-v") 'yank)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq-default initial-frame-alist
              '(
                ;; (alpha-background . 75)
                ;; (alpha . 100)

                (font . "Sarasa Term SC Nerd Light-16")
                (ns-appearance . dark)
                (foreground-color . "#ffffff")
                (background-color . "#000000")))
(setq-default default-frame-alist initial-frame-alist)

(defun vmacs-set-font(&optional f)
  ;; 当 font 设置为单一字体的时候，遇到当前字体处理不了的，则使用 fontset-default 来解析
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fontsets.html
  ;; 👉
  ;; (unless (display-graphic-p)
  ;; (term-title-mode 1))
  ;; emerge -av media-fonts/noto-emoji
  (when (display-graphic-p)
    (with-selected-frame (or f (selected-frame))
      (set-fontset-font "fontset-default" 'emoji "Noto Color Emoji-12")
      ;; (set-fontset-font "fontset-default" 'symbol "LXGW WenKai Mono")
      (setq default-text-properties '(line-spacing 0.06 line-height 1.15))
      ;; (set-face-attribute 'default nil :font "LXGW WenKai Mono" :height 175)
      ;; (set-face-attribute 'fixed-pitch nil :font "Sarasa Term SC Nerd Light" :height 1.0)
      ;; (set-face-attribute 'default nil :font "Sarasa Term SC Nerd Light" )

      ;; https://github.com/lxgw/LxgwWenKai
      ;; (set-face-attribute 'fixed-pitch nil :font "LXGW WenKai Mono" :height 1.0)
      )))

(add-hook 'after-init-hook #'vmacs-set-font)
(add-hook 'after-make-frame-functions #'vmacs-set-font)

(setq auth-sources '("~/.authinfo.gpg" ))
(setq pgtk-selection-timeout 300)


(provide 'conf-linux)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-linux.el ends here.
