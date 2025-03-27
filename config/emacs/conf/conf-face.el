;; 如果配置好了， 下面20个汉字与40个英文字母应该等长
;;  这个工具不错 https://blindwith.science/2019/07/443.html/
;; here are 20 hanzi and 40 english chars, see if they are the same width
;;
;; Sarasa Mono SC Nerd-18 等高等宽字体  更纱黑体
;; https://ericzhuochen.com/blog/post/emacs-1-font/
;; https://github.com/be5invis/Sarasa-Gothic
;; https://github.com/jonz94/Sarasa-Gothic-Nerd-Fonts
;; ;; 1l0oOLiI
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|
;; 你你你你你你你你你你你你你你你你你你你你|
;; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,|
;; 。。。。。。。。。。。。。。。。。。。。|
;; 👈👈👈👈👈👈👈👈👈👈👈👈👈👈👈👈👈👈👈👈|
;; 1111111111111111111111111111111111111111|
;; iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii|
;; llllllllllllllllllllllllllllllllllllllll|
;; oooooooooooooooooooooooooooooooooooooooo|
;; 0000000000000000000000000000000000000000|
;; 東東東東東東東東東東東東東東東東東東東東|
;; ここここここここここここここここここここ|
;; ｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺ|
;; 까까까까까까까까까까까까까까까까까까까까|

;; (create-frame-font-middle-mac)
;; (create-fontset-from-fontset-spec
;;    "-apple-Menlo-medium-normal-normal-*-12-*-*-*-m-0-fontset-mymac,
;;  ascii:-apple-Menlo-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1,
;; han:-*-PingFang SC-normal-normal-normal-*-14-*-*-*-p-0-iso10646-1,
;; cjk-misc:-*-PingFang SC-normal-normal-normal-*-14-*-*-*-p-0-iso10646-1,
;; kana:-*-PingFang SC-normal-normal-normal-*-14-*-*-*-p-0-iso10646-1,
;; hangul:-*-Apple SD Gothic Neo-normal-normal-normal-*-16-*-*-*-p-0-iso10646-1")
;; (add-to-list 'default-frame-alist '(font . "fontset-mymac"))
;; (set-frame-font "fontset-mymac" )
(defface font-lock-todo-face
  '((t (:foreground "Red" :box (:line-width 2 :color "grey75" :style released-button) :height 1.2
        :inherit default)))
  "Font Lock mode face used to highlight TODO."
  :group 'font-lock-faces)
(defface font-lock-done-face
  '((t (:inherit default :foreground "Green" :box (:line-width 2 :color "grey75" :style released-button) :height 1.2)))
  "Font Lock mode face used to highlight DONE."
  :group 'font-lock-faces)
(dolist (mode '(c-mode c++-mode java-mode lisp-mode emacs-lisp-mode
                       go-ts-mode
                       lisp-interaction-mode sh-mode))
  (font-lock-add-keywords
   mode
   '(("\\<\\(FIXME\\|TODO\\|Todo\\|HACK\\|todo\\):" 1  'font-lock-todo-face prepend)
     ("@\\<\\(FIXME\\|TODO\\|Todo\\|HACK\\|todo\\)" 1  'font-lock-todo-face prepend)
     ("\\<\\(DONE\\|Done\\|done\\):" 1 'font-lock-done-face t)
     ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)
     )))

;; show some functions as keywords
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("\\<\\(quote\\|add-hook\\|equal\\)" .
                           font-lock-keyword-face)))

(defun my-modus-themes-custom-faces (&optional f)
  (run-with-timer 0.01 nil
                  #'(lambda()
                      (modus-themes-with-colors
                        (custom-set-faces
                         '(font-lock-comment-face ((t (:inherit modus-themes-slant :slant italic ))))
                         ;; '(italic ((t (:slant italic :weight light :height 151 :width normal ))))
                         '(show-paren-match ((t (:foreground "SpringGreen3" :weight bold))))
                         '(gnus-summary-normal-ancient ((t (:extend t :foreground "gray"))))
                         '(gnus-summary-normal-read ((t (:inherit italic :extend t :foreground "gray58"))))
                         `(region ((t (:extend t :foreground unspecified :background "#5a5a5a"))))
                         `(secondary-selection ((t (:extend t :foreground unspecified :background "#020202"))))
                         ;; `(meow-region-cursor-1 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-char-0)))
                         ;; `(meow-region-cursor-2 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-char-1)))
                         ;; `(meow-region-cursor-3 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-char-2)))

                         `(meow-position-highlight-number-1 ((t :inherit (bold modus-themes-reset-soft) :foreground "yellow")))
                         `(meow-position-highlight-number-2 ((t :inherit (bold modus-themes-reset-soft) :foreground "magenta")))
                         `(meow-position-highlight-number-3 ((t :inherit (bold modus-themes-reset-soft) :background ,bg-char-2)))))
                      )))
(my-modus-themes-custom-faces)
(add-hook 'after-make-frame-functions #'my-modus-themes-custom-faces)

;; (setq-default undo-tree-mode-lighter " Ü") ;undo
;; (setq-default helm-completion-mode-string " H")

;; (setq-default mode-line-cleaner-alist
;;               `((auto-complete-mode . " á")
;;                 (company-mode . "")
;;                 (yas-minor-mode . "")
;;                 (undo-tree-mode . "")
;;                 (golden-ratio-mode . "")
;;                 (flymake-mode . " Fly")
;;                 (ivy-mode . "")
;;                 ;; major mode
;;                 (dired-mode . " Dired")
;;                 (fundamental-mode . "Fd")
;;                 (ibuffer-mode . "iBuf")
;;                 (python-mode . "Py")
;;                 (lisp-interaction-mode . "iEL")
;;                 (emacs-lisp-mode . "EL")))

;; (defun clean-mode-line ()
;;   (interactive)
;;   (dolist (cleaner mode-line-cleaner-alist)
;;     (let* ((mode (car cleaner))
;;            (mode-str (cdr cleaner))
;;            (old-mode-str (cdr (assq mode minor-mode-alist))))
;;       (when old-mode-str
;;         (setcar old-mode-str mode-str))
;;       ;; major mode
;;       (when (eq mode major-mode)
;;         (setq mode-name mode-str)))))

;; (add-hook 'after-change-major-mode-hook 'clean-mode-line)
;; ;; 然后将mode-line 的face header调成0.1，变成一条线
(provide 'conf-face)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-face.el ends here.
