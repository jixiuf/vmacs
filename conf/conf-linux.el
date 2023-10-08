;; 获取当前输入法
;; ibus engine
;; xkb:us::eng  or rime
;; ibus engine rime  # 将输入法更改为
(defvar ime (if (executable-find "fcitx5") 'fcitx5 'ibuf))
(defun switch-to-english-input-method ()
  "Switch to English input method."
  (interactive)
  (if (eq ime 'fcitx5)
      (call-process "fcitx5-remote" nil nil nil "-s" "keyboard-us")
    (call-process "ibus" nil nil nil "engine" "xkb:us::eng")))
(defun switch-to-rime-input-method ()
  "Switch to English input method."
  (interactive)
  (if (eq ime 'fcitx5)
      (call-process "fcitx5-remote" nil nil nil "-s" "rime")
    (call-process "ibus" nil nil nil "engine" "rime")))
(defun get-input-method-state()
  (if (eq ime 'fcitx5)
      (string-trim (shell-command-to-string "fcitx5-remote -n"))
    (string-trim (shell-command-to-string "ibus engine"))))

(defadvice evil-normal-state (before input-method activate)
  "C-g back to normal state"
  (when (member this-command '(evil-force-normal-state evil-normal-state keyboard-quit))
    (switch-to-english-input-method)))

(defun vmacs-evil-input()
  (switch-to-english-input-method)
  (print evil-state))
;; (add-hook 'evil-normal-state-entry-hook #'vmacs-evil-input)
;; (add-hook 'evil-insert-state-exit-hook #'vmacs-evil-input)
(defun linux-toggle-input-method()
  (interactive)
  (if (string-equal (get-input-method-state) "rime")
      (switch-to-english-input-method)
    (switch-to-rime-input-method)
    (evil-insert-state)))

(global-set-key (kbd "C-s-<SPC>") #'linux-toggle-input-method)
(global-set-key (kbd "C-<SPC>") #'linux-toggle-input-method)
(define-key isearch-mode-map (kbd  "C-s-<SPC>") #'linux-toggle-input-method)
(define-key isearch-mode-map (kbd  "C-<SPC>") #'linux-toggle-input-method)
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-s-<SPC>")   #'linux-toggle-input-method)
  (define-key vterm-mode-map (kbd "C-<SPC>")   #'linux-toggle-input-method))

(global-set-key  (kbd "s-C-q") 'save-buffers-kill-emacs)
(global-set-key  (kbd "s-C-c") 'kill-ring-save)
(global-set-key  (kbd "s-C-v") 'yank)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq-default initial-frame-alist
              '((alpha-background . 75)

                (font . "Sarasa Term SC Nerd-15")
                (ns-appearance . dark)
                (foreground-color . "#ffffff")
                (background-color . "#000000")))
(setq-default default-frame-alist initial-frame-alist)

(defun vmacs-set-font(&optional f)
  ;; 当 font 设置为单一字体的时候，遇到当前字体处理不了的，则使用 fontset-default 来解析
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fontsets.html
  ;; 👉
  ;; emerge -av media-fonts/noto-emoji
  (set-fontset-font "fontset-default" 'emoji "Noto Color Emoji")
  (set-fontset-font "fontset-default" 'symbol "Noto Color Emoji")
  (with-selected-frame (or f (selected-frame))
    ;; (set-face-attribute 'default nil :font "Sarasa Term SC Nerd-15")
    (set-face-attribute 'fixed-pitch nil :font "Sarasa Term SC Nerd" :height 1.0)))
(add-hook 'after-init-hook #'vmacs-set-font)
(add-hook 'after-make-frame-functions #'vmacs-set-font)
(provide 'conf-linux)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-linux.el ends here.
