;;; Code:  -*- lexical-binding: t; -*-
;; https://github.com/DogLooksGood/emacs-rime/blob/master/README_CN.org
;; https://gitlab.com/liberime/liberime
;; RIME_PATH=~/repos/squirrel/librime/ make liberime
;; (add-to-list 'load-path "~/.emacs.d/submodule/emacs-rime/")
;;

;; (package-vc-install '(rimel :url "https://github.com/jixiuf/rimel.git"))

(defvar ime (cond
             ((string-equal (getenv "XDG_SESSION_DESKTOP") "ewm") 'rime)
             ((eq system-type 'darwin) 'rime)
             ((eq system-type 'gnu/linux) 'rime)
             ((executable-find "fcitx5")    'fcitx5)
             ((executable-find "ibus")    'ibus)))

(if (eq system-type 'gnu/linux)
    (setq liberime-user-data-dir (expand-file-name "~/.local/share/fcitx5/rime/"))
  (setq liberime-user-data-dir (expand-file-name "~/Library/Rime/")))

(if (string-equal (getenv "XDG_SESSION_DESKTOP") "ewm")
    (setq rimel-show-candidate 'echo-area)
  (setq rimel-show-candidate 'posframe))
(setq rimel-posframe-style 'horizontal)
(setq liberime-auto-build t)
(setq default-input-method "rimel")
(defun rimel-predicate-in-code-p ()
  "Return non-nil when cursor is in code (not string/comment).
Only active in `prog-mode' derived buffers."
  (and (derived-mode-p 'prog-mode 'conf-mode)
       (not (equal "*scratch*" (buffer-name)))
       (let ((ppss (syntax-ppss)))
         (not (or (nth 3 ppss)    ; in string
                  (nth 4 ppss)))))) ; in comment

(setq rimel-disable-predicates
      '(rimel-predicate-in-code-p
        rimel-predicate-after-alphabet-char-p
        rimel-predicate-current-uppercase-letter-p))


(with-eval-after-load 'meep
  (add-hook 'input-method-activate-hook 'meep-insert t))

(defun switch-to-english-input-method (&optional im)
  "Switch to English input method."
  (interactive)
  (cond
   ((eq (or im ime) 'fcitx5)
    (call-process "fcitx5-remote" nil nil nil "-s" "keyboard-us"))
   ((eq (or im ime) 'rime)
    (deactivate-input-method))
   ((eq (or im ime) 'ibus)
    (call-process "ibus" nil nil nil "engine" "xkb:us::eng")))
  (message "en"))

(defun switch-to-rime-input-method (&optional im)
  "Switch to English input method."
  (interactive)
  (cond
   ((eq (or im ime) 'fcitx5)
    (call-process "fcitx5-remote" nil nil nil "-s" "rime"))
   ((eq (or im ime) 'rime)
    (activate-input-method default-input-method))
   ((eq (or im ime) 'ibus)
    (call-process "ibus" nil nil nil "engine" "rime")))
  (message "zh"))

(defun get-input-method-state(&optional im)
  (cond
   ((eq (or im ime) 'rime)
    (if current-input-method  "rime" ""))
   ((eq (or im ime) 'fcitx5)
    (string-trim (shell-command-to-string "fcitx5-remote -n")))
   ((eq (or im ime) 'ibus)
    (string-trim (shell-command-to-string "ibus engine")))))

(defun vmacs-input-method-hook()
  (when (member this-command '(vmacs-cancel-selection
                               bray-state-stack-pop
                               meow-insert-exit evil-force-normal-state evil-normal-state keyboard-quit))
    (switch-to-english-input-method)));
(add-hook 'meep-state-hook-normal-enter #'vmacs-input-method-hook)



(defun vmacs-toggle-input-method()
  (interactive)
  (when (and (eq system-type 'gnu/linux) (eq ime 'rime))
    (switch-to-english-input-method 'fcitx5))
  (if (string-equal (get-input-method-state) "rime")
      (switch-to-english-input-method)
    (switch-to-rime-input-method)
    (meep-insert)))

(global-set-key (kbd "<f11>") #'vmacs-toggle-input-method)
(global-set-key (kbd "<f18>") #'vmacs-toggle-input-method)
(define-key isearch-mode-map (kbd  "<f11>") #'vmacs-toggle-input-method)
(define-key isearch-mode-map (kbd  "<f18>") #'vmacs-toggle-input-method)
(with-eval-after-load 'ghostel
  (define-key ghostel-mode-map (kbd "<f18>")   #'vmacs-toggle-input-method)
  (define-key ghostel-mode-map (kbd "<f11>")   #'vmacs-toggle-input-method))
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "<f18>")   #'vmacs-toggle-input-method)
  (define-key vterm-mode-map (kbd "<f11>")   #'vmacs-toggle-input-method))


;;
;; (setq rime-disable-predicates           ;临时英文模式
;;       '(rime-predicate-evil-mode-p
;;         rime-predicate-prog-in-code-p   ;在 prog-mode 和 conf-mode 中除了注释和引号内字符串之外的区域
;;         ;; rime-predicate-in-code-string-p ;在代码的字符串中，不含注释的字符串。
;;         rime-predicate-space-after-cc-p ;在中文字符且有空格之后
;;         ;; rime-predicate-space-after-ascii-p ;在任意英文字符且有空格之后
;;         rime-predicate-after-alphabet-char-p
;;         rime-predicate-after-ascii-char-p
;;         ;; rime-predicate-auto-english-p
;;         ;; rime-predicate-current-uppercase-letter-p
;;         ;; rime-predicate-punctuation-after-space-cc-p
;;         ))
;; (setq rime-inline-predicates '(rime-predicate-space-after-cc-p))

;; (add-hook 'input-method-activate-hook 'vmacs-evil-input-method-activate t)
;; (add-hook 'input-method-deactivate-hook 'vmacs-evil-input-method-deactive t)
;; (defun vmacs-evil-input-method-activate()
;;   (call-process  "open" nil nil nil "-g" "hammerspoon://input_method_switch?id=Squirrel" ))
;; (defun vmacs-evil-input-method-deactive()
;;   (call-process  "open" nil nil nil "-g" "hammerspoon://input_method_switch?id=U.S." ))

;; (global-set-key (kbd "C-\\") 'toggle-input-method)
;; (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)

;; ;; 输入法进入normal state时，关闭输入法的中文模式
;; (add-hook 'evil-motion-state-entry-hook 'disable-input-method-hook)
;; 结合 hammerspoon 实现按下shift 切换输入法时，当切换到中文中自动进入insert state
;; https://github.com/jixiuf/dotfiles/blob/master/mac/hammerspoon/init.lua
;; 当切换输入法到中文状态时的时候会回调到这里
;;回调的时候有可能是上面disable-input-method-hook 里调，此时不应该切到insert模式
;; (defun vmacs-evil-normal-state()
;;   (interactive)
;;   (vmacs-evil-input-method-deactive)
;;   (evil-normal-state))

;; "Non-nil means random control characters terminate incremental search."
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'conf-input-method)
;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-evil-input-method.el ends here.
