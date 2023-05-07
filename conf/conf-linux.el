;; 获取当前输入法
;; ibus engine
;; xkb:us::eng  or rime
;; ibus engine rime  # 将输入法更改为
(defun switch-to-english-input-method ()
  "Switch to English input method."
  (interactive)
  (call-process "ibus" nil nil nil "engine" "xkb:us::eng"))
(defun switch-to-rime-input-method ()
  "Switch to English input method."
  (interactive)
  (call-process "ibus" nil nil nil "engine" "rime"))
(defun get-input-method-state()
  (string-trim (shell-command-to-string "ibus engine")))

(add-hook 'evil-normal-state-entry-hook #'switch-to-english-input-method)
(defun linux-toggle-input-method()
  (interactive)
  (if (string-equal (get-input-method-state) "rime")
      (switch-to-english-input-method)
    (switch-to-rime-input-method)
    (evil-insert-state)))

(global-set-key (kbd "C-<SPC>") #'linux-toggle-input-method)
(define-key isearch-mode-map (kbd  "C-<SPC>") #'linux-toggle-input-method)
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-<SPC>")   #'linux-toggle-input-method))


(global-set-key  (kbd "s-M-C-q") 'save-buffers-kill-emacs)
(global-set-key  (kbd "s-M-C-c") 'kill-ring-save)
(global-set-key  (kbd "s-M-C-v") 'yank)

(defun vmacs-set-font()
  ;; 当 font 设置为单一字体的时候，遇到当前字体处理不了的，则使用 fontset-default 来解析
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fontsets.html
  ;; (set-fontset-font "fontset-default" 'emoji "Apple Color Emoji")
  ;; (set-fontset-font "fontset-default" 'symbol "Apple Color Emoji")
  ;; (when (>= emacs-major-version 29)
  ;;   (set-fontset-font t 'emoji "Apple Color Emoji-17"))
  ;; (set-fontset-font t 'symbol "Apple Symbols")
    ;; https://github.com/laishulu/Sarasa-Term-SC-Nerd
  (set-face-attribute 'default nil :font "Sarasa Term SC Nerd" :height 180)
  (set-face-attribute 'fixed-pitch nil :font "Sarasa Term SC Nerd" :height 1.0)
  )

(vmacs-set-font)
(add-hook 'after-init-hook #'vmacs-set-font)


(provide 'conf-linux)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-linux.el ends here.
