;; 获取当前输入法
;; ibus engine
;; xkb:us::eng  or rime
;; ibus engine rime  # 将输入法更改为
(defvar ime (cond
             ((executable-find "fcitx5")    'fcitx5)
             ((executable-find "ibus")    'ibus)))

(defun switch-to-english-input-method ()
  "Switch to English input method."
  (interactive)
  (cond
   ((eq ime 'fcitx5)
    (call-process "fcitx5-remote" nil nil nil "-s" "keyboard-us"))
   ((eq ime 'ibus)
    (call-process "ibus" nil nil nil "engine" "xkb:us::eng"))))

(defun switch-to-rime-input-method ()
  "Switch to English input method."
  (interactive)
  (cond
   ((eq ime 'fcitx5)
    (call-process "fcitx5-remote" nil nil nil "-s" "rime"))
   ((eq ime 'ibus)
    (call-process "ibus" nil nil nil "engine" "rime"))))

(defun get-input-method-state()
  (cond
   ((eq ime 'fcitx5)
    (string-trim (shell-command-to-string "fcitx5-remote -n")))
   ((eq ime 'ibus)
    (string-trim (shell-command-to-string "ibus engine")))))

(defun vmacs-input-method-hook()
  (when (member this-command '(vmacs-pop-selection meow-insert-exit evil-force-normal-state evil-normal-state keyboard-quit))
    (switch-to-english-input-method)))
(add-hook 'meow-normal-mode-hook #'vmacs-input-method-hook)

(defun linux-toggle-input-method()
  (interactive)
  (if (string-equal (get-input-method-state) "rime")
      (switch-to-english-input-method)
    (switch-to-rime-input-method)
    (meow-insert)))

(global-set-key (kbd "<f11>") #'linux-toggle-input-method)
(define-key isearch-mode-map (kbd  "<f11>") #'linux-toggle-input-method)
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "<f11>")   #'linux-toggle-input-method))

(global-set-key  (kbd "s-C-q") 'save-buffers-kill-emacs)
(global-set-key  (kbd "s-C-c") 'kill-ring-save)
(global-set-key  (kbd "s-C-v") 'yank)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq-default initial-frame-alist
              '(
                (alpha-background . 75)
                (alpha . 100)

                (font . "Sarasa Term SC Nerd Light-15")
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
    (set-fontset-font "fontset-default" 'emoji "Noto Color Emoji")
    (set-fontset-font "fontset-default" 'symbol "Noto Color Emoji")
    (with-selected-frame (or f (selected-frame))
      (setq default-text-properties '(line-spacing 0.06 line-height 1.15))
      ;; (set-face-attribute 'default nil :font "LXGW WenKai Mono" :height 175)
      (set-face-attribute 'fixed-pitch nil :font "Sarasa Term SC Nerd Light" :height 1.0)
      (set-face-attribute 'default nil :font "Sarasa Term SC Nerd Light" )
      ;; https://github.com/lxgw/LxgwWenKai
      ;; (set-face-attribute 'fixed-pitch nil :font "LXGW WenKai Mono" :height 1.0)
      )))

(add-hook 'after-init-hook #'vmacs-set-font)
(add-hook 'after-make-frame-functions #'vmacs-set-font)

(setq auth-sources '("~/.authinfo.gpg" ))

(provide 'conf-linux)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-linux.el ends here.
