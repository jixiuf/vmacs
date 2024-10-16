(autoload #'viper-ex  "viper" t)
(setq meow--kbd-kill-region "C-k")
(setq meow-motion-remap-prefix "s-M-")
(setq meow-expand-hint-remove-delay 3)
(setq meow-use-clipboard t)
(setq meow-keypad-self-insert-undefined nil)
;; (setq meow-use-cursor-position-hack t)  ;a 的行为向后一字符,行尾会到下一行
(setq meow-char-thing-table
      '((?r . round)                    ;()
        (?s . square)                   ;[]
        (?c . curly)                    ;{}
        (?g . string)
        (?e . symbol)
        ;; (?w . window)
        (?b . buffer)
        (?p . paragraph)
        (?s . line)
        (?v . visual-line)
        (?f . defun)
        (?. . sentence)))
(setq meow-thing-selection-directions
  '((inner . backward)
    (bounds . backward)
    (beginning . backward)
    (end . forward)))


(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . "C-c Mj")
   '("k" . "C-c Mk")
   '("g" . "C-c G")
   '("G" . "C-c MG")
   '("/" . "C-c M/")
   '("z" . "C-c Mz")
   '("q" . "C-c Mq")
   '("<escape>" . "C-c M<escape>"))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "s-M-j")
   '("k" . "s-M-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  
  (meow-normal-define-key
   '("g" . "C-c G")
   ;; see vmacs-normal-mode-map in conf=keybind.el
   '("i" . "C-c Ni")
   '("G" . "C-c NG")
   '("n" . "C-c Nn")
   '("N" . "C-c NN")
   '("/" . "C-c N/")
   '("z" . "C-c Nz")
   '("=" . "C-c N=")
   '("q" . "C-c q")
   '("m" . "C-c Nm")
   '("0" . meow-expand-or-digit-argument)
   '("9" . meow-expand-or-digit-argument)
   '("8" . meow-expand-or-digit-argument)
   '("7" . meow-expand-or-digit-argument)
   '("6" . meow-expand-or-digit-argument)
   '("5" . meow-expand-or-digit-argument)
   '("4" . meow-expand-or-digit-argument)
   '("3" . meow-expand-or-digit-argument)
   '("2" . meow-expand-or-digit-argument)
   '("1" . meow-expand-or-digit-argument)
   '("-" . negative-argument)
   '("M-j" . vmacs-meow-join)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("<" . indent-rigidly-left-to-tab-stop)
   '(">" . indent-rigidly-right-to-tab-stop)
   '("M-s-," . meow-beginning-of-thing)
   '("M-s-." . meow-end-of-thing)
   '("a" . vmacs-meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("v" . meow-back-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("E" . meow-next-word)
   '("e" . meow-next-symbol)
   '("f" . meow-find)
   '("F" . meow-negative-find)
   ;; '("g" . meow-cancel-selection)
   '("h" . meow-left)
   ;; '("h" . backward-char)
   '("H" . meow-left-expand)
   '("I" . meow-open-above)
   ;; '("j" . next-line)
   '("j" . vmacs-meow-next)
   '("J" . meow-next-expand)
   ;; '("k" . previous-line)
   '("k" . vmacs-meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   ;; '("l" . forward-char)
   '("L" . meow-right-expand)
   '("*" . vmacs-meow-search-symbol)
   '("#" . vmacs-meow-search-symbol-prev)
   '("o" . meow-open-below)
   '("C-o" . meow-block)
   '("O" . meow-to-block)
   '("c" . vmacs-meow-change)
   '("r" . vmacs-meow-replace)
   '("p" . vmacs-meow-yank)
   '("Q" . meow-goto-line)
   '("d" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("?" . isearch-backward)
   '("W" . meow-mark-word)
   '("w" . meow-mark-symbol)
   '("C-r" . vmacs-meow-reverse)
   '("%" . vmacs-meow-reverse)
   '(";" . vmacs-meow-reverse)
   '("s" . vmacs-meow-line)
   '("S" . meow-goto-line)
   '("y" . meow-save)
   '("`" . meow-end-or-call-kmacro)
   '("R" . meow-swap-grab)
   '("Y" . meow-sync-grab)
   '("\\" . just-one-space-or-delete-horizontal-space)
   '(":" . viper-ex)
   '("<escape>" . vmacs-pop-selection)
   ))
(global-set-key (kbd "C-8") #'vmacs-meow-search-symbol)
(global-set-key (kbd "C-3") #'vmacs-meow-search-symbol-prev)
(global-set-key (kbd "C-s") #'isearch-forward)
(global-set-key (kbd "M-y") #'vmacs-yank-pop)


(require 'meow)
(meow-setup)

(when (require 'which-key nil t)
  (add-hook 'after-init-hook #'which-key-mode))
(define-key   meow-beacon-state-keymap (kbd "C-c C-c") #'meow-beacon-apply-kmacro)
(add-to-list 'meow-selection-command-fallback '(meow-save . vmacs-meow-line)) ;support: yy y3y
(add-to-list 'meow-selection-command-fallback '(meow-replace . vmacs-meow-yank))
(add-to-list 'meow-selection-command-fallback '(meow-kill . vmacs-meow-line)) ;suppert: dd d3d
(add-to-list 'meow-selection-command-fallback '(meow-change . vmacs-meow-line)) ;suppert: cc c3c
(define-key  meow-beacon-state-keymap "a" 'meow-beacon-append)


(meow-thing-register 'quoted
                    '(regexp "`\\|'" "`\\|'")
                    '(regexp "`\\|'" "`\\|'"))
(meow-thing-register 'go-package
                    '(regexp "[[:space:]\"{}(),\n]" "[[:space:]\"(),{}\n]")
                    '(regexp "[[:space:]\"{}(),\n]" "[[:space:]\"(),{}\n]"))

;; org-mode begin_src or markdown ``` block
(meow-thing-register 'org-block
                    '(regexp "^[ \\|\t]*\\(#\\+begin_\\|```\\)[^\n]*\n" "^[ \\|\t]*\\(#\\+end_[^\n]*\\|```\\)$")
                    '(regexp "^[ \\|\t]*\\(#\\+begin_\\|```\\)[^\n]*\n" "^[ \\|\t]*\\(#\\+end_[^\n]*\\|```\\)$")
                    )

(add-to-list 'meow-char-thing-table '(?` . quoted))
(add-to-list 'meow-char-thing-table '(?o . org-block))
(add-to-list 'meow-char-thing-table '(?, . go-package))

(setq meow-keypad-ctrl-meta-prefix ?e)
(setq meow-keypad-start-keys
  '((?c . ?c)
    ;; (?h . ?h)
    (?x . ?x))
)

(add-to-list 'meow-mode-state-list '(reb-mode . insert))
(add-to-list 'meow-mode-state-list '(text-mode . insert))
(add-to-list 'meow-mode-state-list '(comint-mode . insert))
(add-to-list 'meow-mode-state-list '(org-mode . normal))
(add-to-list 'meow-mode-state-list '(yaml-mode . normal))
(add-to-list 'meow-mode-state-list '(yaml-ts-mode . normal))
(add-to-list 'meow-mode-state-list '(messages-buffer-mode . normal))
(add-to-list 'meow-mode-state-list '(grep-mode . normal))
(add-to-list 'meow-mode-state-list '(markdown-mode . normal))

(with-eval-after-load 'org-agenda
  (set-keymap-parent org-agenda-mode-map meow-normal-state-keymap))
(add-to-list 'meow-mode-state-list '(org-agenda-mode . motion))
(meow-global-mode 1)

(defun vmacs-meow-frame(&optional f)
  (with-selected-frame (or f (selected-frame))
    (meow--prepare-face)))

(add-hook 'after-make-frame-functions #'vmacs-meow-frame)
;; (global-display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(meow-setup-line-number)
;; (add-hook 'meow-insert-exit-hook 'corfu-quit)

;; (add-to-list 'meow-selection-command-fallback '(meow-save . meow-line)) ;support: yy y3y
(define-advice meow-save (:around (orig-fun &rest args) yy-old-pos)
  "goto origin position after `yy',need fallback (meow-save . meow-line) "
  (let ((region (region-active-p)))
    (apply orig-fun args)
    (when region
      (meow-pop-all-selection))))

;; 处理 文件最后一行无换行符时 p针对yy后的行为
(defun meow-p-kill-ring-save (beg end &optional region)
  "make sure `p' after `yy' a whole line is pasted,even last line doesn't has a newline"
  (when (and (equal 'line (cdr (meow--selection-type)))
             (meow--direction-forward-p)
             (= (point) (point-max)))
    (with-temp-buffer
      (insert "\n")
      (append-next-kill)
      (kill-region (point-min)(point-max)))))
(advice-add 'kill-ring-save :after #'meow-p-kill-ring-save)


(define-advice meow-yank (:around (orig-fun &rest args) vim-p-and-auto-selection)
  "Make `yank' behave like paste (p) command in vim."
  (when-let ((clip (condition-case nil (current-kill 0 t) (error ""))))
    (set-text-properties 0 (length clip) nil clip)
    (let ((charp (= 1 (length clip)))
          (linep (string-suffix-p "\n" clip)))
      (if charp
          (forward-char)
        (when linep
          (forward-line)
          (goto-char (line-beginning-position))))
      (apply orig-fun args)
      ;; 下面代码 自动选中粘贴的内容，如果粘贴的是整行(即 yy p)
      ;; 则光标移动到行首（类似vim），否则行尾
      (when linep (exchange-point-and-mark)))))

(global-set-key (kbd "C-h") 'negative-argument)
(global-set-key (kbd "C-u") 'meow-universal-argument)

(defun vmacs-pop-all-selection ()
  (when (region-active-p)
    (meow-pop-all-selection)))

(advice-add 'keyboard-quit :before #'vmacs-pop-all-selection)

(define-advice meow-mark-symbol (:around (orig-fun &rest args) dwim)
  "makr symbol or word"
  (if (not (eq last-command 'meow-mark-symbol))
      (apply orig-fun args)
    (meow-pop-all-selection)
    (call-interactively #'meow-mark-word)))

;; (lambda () (interactive)
;;    (if (or defining-kbd-macro executing-kbd-macro)
;;        (call-interactively #'end-kbd-macro)
;;      (call-interactively #'kmacro-start-macro)))

(provide 'conf-meow)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-meow.el ends here.
