;;; -*- lexical-binding: t; -*-
(autoload #'viper-ex  "viper" t)
;https://github.com/meow-edit/meow/discussions/661
(setq meow-next-thing-include-syntax
      '((word "w" "w")(symbol "w" "w")))
(setq meow-expand-hint-counts
  '((word . 4)
    (line . 30)
    (block . 30)
    (find . 30)
    (till . 30)
    (symbol . 4)))

;; (setq meow--delete-region-function #'kill-region)
(setq meow--kbd-kill-region "C-k")
(setq meow-expand-hint-remove-delay 3)
(setq meow-use-clipboard t)
(setq meow-select-on-append t)
(setq meow-select-on-insert t)
(setq meow-keypad-self-insert-undefined nil)
;; (setq meow-use-cursor-position-hack t)  ;a 的行为向后一字符,行尾会到下一行
;; (setq meow-keypad-leader-dispatch "C-c")


(defun meow-setup ()
  (meow-motion-define-key
   '("j" . "C-c Mj")
   '("h" . "C-c Mh")
   '("l" . "C-c Ml")
   '("k" . "C-c Mk")
   '("g" . "C-c G")
   '("G" . "C-c MG")
   '("/" . "C-c M/")
   '("n" . "C-c Mn")
   '("N" . "C-c MN")
   '("z" . "C-c Mz")
   '("q" . "C-c Mq")
   '(":" . "C-c M:")
   '("<escape>" . vmacs-cancel-selection)
   )

  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "s-M-j")
   '("k" . "s-M-k")
   '("s" . "C-x C-s")
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
   '("q" . meow-beginning-of-thing)
   '("p" . meow-end-of-thing)
   '(";" . meow-mark-symbol)
   '("<" . indent-rigidly-left-to-tab-stop)
   '(">" . indent-rigidly-right-to-tab-stop)
   '("a" . meow-append)
   '("A" . meow-open-below)
   ;; '("v" . meow-back-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("W" . meow-mark-word)
   '("w" . meow-next-word)
   ;; '("w" . vmacs-forward-word)
   '("E" . "M-f")
   '("e" . meow-next-symbol)
   ;; '("e" . vmacs-forward-symbol)
   '("v" . meow-back-symbol)
   ;; '("v" . vmacs-backward-symbol)
   '("b" . meow-back-word)
   ;; '("b" . "M-b")
   ;; '("b" . meow-back-word)
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
   '("C-o" . meow-inner-block)
   '("O" . meow-to-block)
   '("c" . meow-change)
   '("r" . meow-replace)
   ;; '("p" . meow-yank)
   '("Q" . meow-goto-line)
   '("d" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("?" . isearch-backward)
   '("C-r" . vmacs-meow-reverse)
   '("%" . vmacs-meow-reverse)
   '("s" . meow-mark-line)
   '("S" . meow-goto-line)
   '("y" . meow-save)
   '("`" . meow-start-kmacro-or-insert-counter)
   '("R" . meow-swap-grab)
   '("Y" . meow-sync-grab)
   '("\\" . just-one-space-or-delete-horizontal-space)
   '(":" . viper-ex)
   '("<escape>" . vmacs-cancel-selection)
   ))
(global-set-key (kbd "C-8") #'vmacs-meow-search-symbol)
(global-set-key (kbd "C-3") #'vmacs-meow-search-symbol-prev)
(global-set-key (kbd "C-s") #'isearch-forward)
(global-set-key (kbd "M-y") #'vmacs-yank-pop)
(vmacs-leader "," #'meow-inner-of-thing)
(vmacs-leader "." #'meow-bounds-of-thing)

(require 'meow)
(meow-setup)

(when (require 'which-key nil t)
  (setq which-key-max-description-length 36)
  (add-hook 'after-init-hook #'which-key-mode))
(add-to-list 'meow-selection-command-fallback '(meow-save . meow-mark-line)) ;support: yyy
(add-to-list 'meow-selection-command-fallback '(meow-replace . meow-yank))
(add-to-list 'meow-selection-command-fallback '(meow-kill . meow-mark-line)) ;suppert: dd d3d
(add-to-list 'meow-selection-command-fallback '(meow-change . meow-mark-line)) ;suppert: cc c3c
(add-to-list 'meow-selection-command-fallback '(meow-pop-selection . meow-selection)) ;suppert: cc c3c
(add-to-list 'meow-selection-command-fallback '(vmacs-cancel-selection . meow-grab)) ;for cancel meow--cancel-second-selection
;; (define-key  meow-beacon-state-keymap "a" 'meow-beacon-append)
;; (define-key meow-beacon-state-keymap [remap meow-save] 'meow-bounds-of-thing)
(define-key meow-beacon-state-keymap (kbd "C-c C-c") #'meow-beacon-apply-kmacro)


(meow-thing-register 'quoted
                     '(regexp "`\\|'\\|\"" "`\\|'\\|\"")
                     '(regexp "`\\|'\\|\"" "`\\|'\\|\""))
(meow-thing-register 'grave-quoted
                     '(regexp "`" "`\\|'")
                     '(regexp "`" "`\\|'"))
(meow-thing-register 'go-package
                     '(regexp "[[:space:]\"{}(),\n]" "[[:space:]\"(),{}\n]")
                     '(regexp "[[:space:]\"{}(),\n]" "[[:space:]\"(),{}\n]"))
(meow-thing-register 'arguments
                     '(regexp "[(,]" "[),]")
                     '(regexp "[(,]" "[),]"))
;; org-mode begin_src or markdown ``` block
(meow-thing-register 'org-block
                     '(pair-regexp (
                                    "\\(^[ \\|\t]*#\\+begin_[^\n]*\n\\)"
                                    "\\(^[ \\|\t]*```[^\n]*\n\\)"
                                    )
                                   (
                                    "\\(^[ \\|\t]*\\(#\\+end_[^\n]*\\)$\\)"
                                    "\\(^[ \\|\t]*```$\\)"
                                    ))
                     '(pair-regexp (
                                    "\\(^[ \\|\t]*#\\+begin_[^\n]*\n\\)"
                                    "\\(^[ \\|\t]*```[^\n]*\n\\)"
                                    )
                                   (
                                    "\\(^[ \\|\t]*\\(#\\+end_[^\n]*\\)$\\)"
                                    "\\(^[ \\|\t]*```$\\)"
                                    )))

(meow-thing-register 'code-block
                     '(pair-regexp ("([\n\t ]*" "\\[[\n\t ]*" "{[\n\t ]*" )
                                   ("[\n\t ]*)" "[\n\t ]*\\]" "[\n\t ]*}" ) )
                     '(pair-regexp ("(" "\\[" "{" )
                                   (")" "\\]" "}" )))
 
(meow-thing-register 'sexp 'sexp 'sexp)
(meow-thing-register 'word 'word 'word)
(setq meow-char-thing-table
      '((?w . word)
        (?. . word)
        (?r . go-package)
        (?, . symbol)
        (?e . symbol)
        (?x . sexp)
        (?f . defun)
        (?` . grave-quoted)
        (?' . quoted)
        (?q . quoted)
        (?g . string)
        ;; (?a . arguments)
        (?c . code-block)
        (?o . org-block)
        (?\( . round)                    ;()
          (?8 . round)                    ;()
          (?9 . round)                    ;()
          (?0 . round)
          (?\) . round)                    ;()
        (?\[ . square)                   ;[]
          (?\] . square)                   ;[]
        (?{ . curly)                    ;{}
        (?c . code-block)                    ;{}
        ;; (?c . curly)                    ;{}
        ;; (?w . window)
        (?b . buffer)
        (?p . paragraph)
        (?s . line)
        (?y . line)
        (?d . line)
        (?v . visual-line)))
(meow-tree-sitter-register-thing ?a "parameter")
(meow-tree-sitter-register-thing ?\; "comment")
(meow-tree-sitter-register-thing ?m "function") ;method

;; (setq meow-thing-selection-directions
;;       '((inner . forward)
;;         (bounds . backward)
;;         (beginning . backward)
;;         (end . forward)))


(setq meow-keypad-ctrl-meta-prefix ?e)
(setq meow-keypad-meta-prefix ?M)
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
(add-to-list 'meow-mode-state-list '(dape-repl-mode . normal))
(add-to-list 'meow-mode-state-list '(org-agenda-mode . motion))

(defmacro meow-set-keymap-parent (map-or-mode &optional parent)
  "Set the parent keymap for MAP-OR-MODE to PARENT.

MAP-OR-MODE can be a map or a mode
Returns PARENT. PARENT should be nil or another keymap.
Default is 'meow-normal-state-keymap' when PARENT is nil."
  `(let ((map (if (keymapp ,map-or-mode)
                  ,map-or-mode
                (let ((map-name (intern (concat (symbol-name ,map-or-mode) "-map"))))
                  (when (boundp map-name)
                    (symbol-value map-name))))))
     (when (keymapp map)
       (set-keymap-parent map (make-composed-keymap (keymap-parent map)
                                                    (or ,parent meow-normal-state-keymap))))))
;; (keymap-unset occur-mode-map "l" t)

(defvar meow-motion-parent-keymaps (make-hash-table :test #'equal))
(defun meow-motion-set-keymap-parent()
  (when (and meow-motion-mode
             (not (gethash major-mode meow-motion-parent-keymaps)))
    (puthash major-mode t meow-motion-parent-keymaps)
    (meow-set-keymap-parent major-mode meow-normal-state-keymap)))

(add-hook 'meow-motion-mode-hook #'meow-motion-set-keymap-parent)

(meow-global-mode 1)

(defun vmacs-meow-frame(&optional f)
  (with-selected-frame (or f (selected-frame))
    (meow--prepare-face)))

(add-hook 'after-make-frame-functions #'vmacs-meow-frame)
;; (global-display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(meow-setup-line-number)
;; (add-hook 'meow-insert-exit-hook 'corfu-quit)

(define-advice meow-change (:around (orig-fun &rest args) rect)
  (if (bound-and-true-p rectangle-mark-mode)
      (call-interactively #'string-rectangle)
    (apply orig-fun args)))
(define-advice meow-yank (:around (orig-fun &rest args) rect)
  (if (bound-and-true-p rectangle-mark-mode)
      (call-interactively #'yank-rectangle)
    (apply orig-fun args)))

(define-advice meow-replace (:around (orig-fun &rest args) rect-and-grab)
  (interactive)
  (if (bound-and-true-p rectangle-mark-mode)
      (progn
        (call-interactively #'delete-rectangle)
        (rectangle-exchange-point-and-mark)
        (call-interactively #'yank-rectangle))
    (apply orig-fun args)))

(define-advice meow-inner-of-thing (:around (orig-fun &rest args) mark-thing)
  (let* ((thing (cdr (assoc (car args) meow-char-thing-table)))
         (func (intern(format "meow-mark-%s" thing)))
         (back (equal 'backward (meow--thing-get-direction 'inner))))
    (if (fboundp func)
        (progn
          (call-interactively func)
          (setq this-command func))
      (apply orig-fun args)
      (let ((search (regexp-quote (buffer-substring-no-properties (region-beginning)(region-end)))))
        (unless (string-empty-p search)
          (meow--push-search search)
          (meow--highlight-regexp-in-buffer search))))))

(define-advice meow-bounds-of-thing (:around (orig-fun &rest args) mark-thing)
  (let* ((thing (cdr (assoc (car args) meow-char-thing-table)))
         (func (intern(format "meow-mark-%s" thing)))
         (back (equal 'backward (meow--thing-get-direction 'inner))))
    (if (fboundp func)
        (progn
          (call-interactively func)
          (setq this-command func))
      (apply orig-fun args)
      (let ((search (regexp-quote (buffer-substring-no-properties (region-beginning)(region-end)))))
        (unless (string-empty-p search)
          (meow--push-search search)
          (meow--highlight-regexp-in-buffer search))))))


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
  (when-let* ((clip (condition-case nil (current-kill 0 t) (error ""))))
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

;; (global-set-key (kbd "C-h") 'negative-argument)
(global-set-key (kbd "C-u") 'meow-universal-argument)

(defun vmacs-pop-all-selection ()
  (when (region-active-p)
    (meow-pop-all-selection)))

(advice-add 'keyboard-quit :before #'vmacs-pop-all-selection)

(define-advice meow-mark-symbol (:around (orig-fun &rest args) dwim)
  "makr symbol or word"
  (cond
   ((eq last-command 'meow-mark-word)
    (meow-pop-all-selection)
    (setq this-command 'meow-pop-all-selection))
   ((eq last-command 'meow-mark-symbol)
    (meow-pop-all-selection)
    (call-interactively #'meow-mark-word)
    (setq this-command 'meow-mark-word))
   (t (apply orig-fun args))))


;; C-c u
(define-advice backward-up-list (:around (orig-fun &rest args) mark)
  "makr block"
  (let ((oldpos (point))
        beginning end)
    (apply orig-fun args)
    (setq beginning (point))
    (setq end (save-excursion (forward-list) (point)))
    (when (>= end oldpos beginning)
      (goto-char beginning)
      (set-mark end)
      (activate-mark))))


(defun meow-inner-block (arg)
  "Mark the block or expand to parent block."
  (interactive "P")
  (let ((ra (region-active-p))
        (back (xor (meow--direction-backward-p) (< (prefix-numeric-value arg) 0)))
        (depth (car (syntax-ppss)))
        (orig-pos (point))
        (orig-mark (mark))
        p m p-inner m-inner)
    (save-mark-and-excursion
      (while (and (if back (re-search-backward "\\s(" nil t) (re-search-forward "\\s)" nil t))
                  (or (meow--in-string-p)
                      (if ra (>= (car (syntax-ppss)) depth) (> (car (syntax-ppss)) depth)))))
      (when (and (if ra (< (car (syntax-ppss)) depth) (<= (car (syntax-ppss)) depth))
                 (not (= (point) orig-pos)))
        (setq p (point))
        (when (ignore-errors (forward-list (if back 1 -1)))
          (setq m (point)))))
    (when (and p m)
      (save-mark-and-excursion
        (if back
            (progn
              (goto-char p)
              (when (looking-at (format"%c[ \t\n]*" (following-char)))
                (setq p-inner (match-end 0)))
              (goto-char m)
              (when (looking-back (format "[ \t\n]*%c" (preceding-char)) nil t)
                (setq m-inner (match-beginning 0))))
          (goto-char p)
          (when (looking-back (format "[ \n\t]*%c" (preceding-char) ) nil t)
            (setq p-inner (match-beginning 0)))
          (goto-char m)
          (when (looking-at (format "%c[ \n\t]*" (following-char)))
            (setq m-inner (match-end 0))))
        (unless (and (equal orig-mark m-inner)(equal orig-pos p-inner))
          (setq m m-inner)
          (setq p p-inner)))
      (thread-first
        (meow--make-selection '(expand . block) m p)
        (meow--select))
      (meow--maybe-highlight-num-positions '(meow--backward-block . meow--forward-block)))))

(provide 'conf-meow)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-meow.el ends here.
