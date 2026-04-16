;;; Code:  -*- lexical-binding: t; -*-
;; (package-vc-install '(rimel :url "https://github.com/jixiuf/rimel.git" :branch "main"))

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
(setq rimel-highlight-first t)
(setq rimel-auto-build t)
(setq default-input-method "rimel")
(with-eval-after-load 'rimel
  (add-hook 'after-init-hook #'liberime-sync)
  (add-to-list 'rimel-keymap '(?\C-o . "<pageup>"))
  (add-to-list 'rimel-keymap '("C-s" . "<down>"))
  (add-to-list 'rimel-keymap '("C-h" . "C-h"))
  (add-to-list 'rimel-keymap '("C-l" . "C-l")))
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
    (require 'rimel)
    (meep-insert)
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

(provide 'conf-input-method)
;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-input-method.el ends here.
