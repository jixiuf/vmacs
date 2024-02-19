(autoload #'viper-ex  "viper" t)
(setq meow-motion-remap-prefix "s-M-")
(setq meow-expand-hint-remove-delay 3)
(setq meow-keypad-leader-dispatch vmacs-space-leader-mode-map)
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
;; meow-save


(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . "C-c mj")
   '("k" . "C-c mk")
   '("g" . "C-c g")
   '("G" . "C-c mG")
   '("/" . "C-c m/")
   '("z" . "C-c mz")
   '("<escape>" . "C-c m<escape>"))
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
   '("g" . "C-c g")
   '("G" . "C-c nG")
   '("n" . "C-c nn")
   '("N" . "C-c nN")
   '("/" . "C-c n/")
   '("z" . "C-c nz")
   '("=" . "C-c n=")
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '("M-j" . vmacs-meow-join)
   '("m" . meow-bounds-of-thing)
   '("," . meow-inner-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . vmacs-meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("v" . meow-back-symbol)
   '("c" . meow-change)
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
   '("i" . meow-insert)
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
   '("p" . meow-yank)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("M-r" . meow-replace-pop)
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
   '("s" . meow-line)
   '("S" . meow-goto-line)
   '("y" . meow-save)
   '("q" . meow-start-kmacro-or-insert-counter)
   '("`" . meow-end-or-call-kmacro)

   '("R" . meow-swap-grab)
   '("Y" . meow-sync-grab)
   '("P" . vmacs-meow-insert-secondary)
   '("\\" . just-one-space-or-delete-horizontal-space)
   '("." . repeat)
   '(":" . viper-ex)
   '("<escape>" . meow-cancel-selection)
   ))
(global-set-key (kbd "C-8") #'vmacs-meow-search-symbol)
(global-set-key (kbd "C-3") #'vmacs-meow-search-symbol-prev)
(global-set-key (kbd "C-s") #'meow-isearch)


(require 'meow)
(meow-setup)
(define-key   meow-beacon-state-keymap (kbd "C-c C-c") #'meow-beacon-apply-kmacro)
(add-to-list 'meow-selection-command-fallback '(meow-save . meow-line)) ;support: yy y3y
(add-to-list 'meow-selection-command-fallback '(meow-replace . meow-yank))
(add-to-list 'meow-selection-command-fallback '(meow-kill . meow-line)) ;suppert: dd d3d
(add-to-list 'meow-selection-command-fallback '(meow-change . meow-line)) ;suppert: cc c3c
(define-key  meow-beacon-state-keymap "a" 'meow-beacon-append)


(meow-thing-register 'quoted
                    '(regexp "`\\|'" "`\\|'")
                    '(regexp "`\\|'" "`\\|'"))
(add-to-list 'meow-char-thing-table '(?` . quoted))

(setq meow-keypad-ctrl-meta-prefix ?e)
(setq meow-keypad-start-keys
  '((?c . ?c)
    ;; (?h . ?h)
    (?x . ?x))
)
(add-to-list 'meow-mode-state-list '(text-mode . insert))
(add-to-list 'meow-mode-state-list '(messages-buffer-mode . normal))
(meow-global-mode 1)

(defvar vmacs-meow-save-marker nil)
;; (add-to-list 'meow-selection-command-fallback '(meow-save . meow-line)) ;support: yy y3y
(define-advice meow-save (:around (orig-fun &rest args) yy-old-pos)
  "goto origin position after `yy',need fallback (meow-save . meow-line) "
  (let ((activep (region-active-p)))
    (when (not activep)
      (setq vmacs-meow-save-marker (point-marker)))
    (apply orig-fun args)
    (when (and activep (region-active-p)
               (eq last-command 'meow-save))
      (goto-char vmacs-meow-save-marker))))

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
    (let ((linep (string-suffix-p "\n" clip)))
      (when linep
        (forward-line)
        (goto-char (line-beginning-position)))
      (apply orig-fun args)
      ;; 下面代码 自动选中粘贴的内容，如果粘贴的是整行(即 yy p)
      ;; 则光标移动到行首（类似vim），否则行尾
      (run-with-timer 0.1 nil
                      (lambda(linep)
                        (exchange-point-and-mark)
                        (unless linep
                          (exchange-point-and-mark)))
                      linep))))

(global-set-key (kbd "C-h") 'negative-argument)
(global-set-key (kbd "C-u") 'meow-universal-argument)
;; (lambda () (interactive)
;;    (if (or defining-kbd-macro executing-kbd-macro)
;;        (call-interactively #'end-kbd-macro)
;;      (call-interactively #'kmacro-start-macro)))

(provide 'conf-meow)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-meow.el ends here.
