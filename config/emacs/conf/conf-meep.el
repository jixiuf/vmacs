;;; init.el --- Testing -*- lexical-binding: t -*-
(global-set-key (kbd "M-y") #'vmacs-yank-pop)
(global-set-key (kbd "C-s-v") #'clipboard-yank)             ;paste from clipboard
(global-set-key (kbd "C-y") #'meep-clipboard-killring-yank) ;paste from kill ring

(defun meep-clipboard-killring-yank-ad(&optional arg)
  (activate-mark))
(advice-add 'meep-clipboard-killring-yank-pop-stack :before #'meep-clipboard-killring-yank-ad)

;; (advice-add 'meep--wrap-current-kill :override
;;             (defun meep--wrap-current-kill-ad (n &optional do-not-move)
;;               "Wrap `current-kill', only ever use the kill ring. Forward N & DO-NOT-MOVE."
;;               (current-kill n do-not-move)))
;; (advice-add 'meep-clipboard-killring-copy :before #'meep-clipboard-killring-copy-ad)
;; (defun meep-clipboard-killring-copy-ad(&optional arg)
;;   (meep-clipboard-only-copy)
;;   (activate-mark))


(defun isearch--search (reverse &optional match not-regexp
                             inhibit-lazy-highlight-and-count not-repeat)
  "Search for the next occurrence of MATCH using isearch.
If found, move point to the end of the occurrence,and return point.

REVERSE - If non-nil, the search direction is backward. Otherwise, it is forward.
MATCH - The string to search for, if nil `isearch-repeat' is called.
NOT-REGEXP - If non-nil, do a regular string search instead.
INHIBIT-LAZY-HIGHLIGHT-AND-COUNT - If non-nil disable lazy highlighting and
lazy counting features.
NOT-REPEAT - do not use `isearch-repeat' even MATCH equals to last `isearch-string'."
  (interactive "P")
  (when (and (string-equal match isearch-string)
             (equal isearch-regexp (not not-regexp))
             (not not-repeat))
    (setq match nil))
  (let ((inhibit-redisplay t)
        ;; we call `isearch-lazy-highlight-new-loop' at the end
        ;; of function,so set `isearch-lazy-count' and `isearch-lazy-highlight'
        ;; to nil is costless.
        (isearch-lazy-count nil)
        (isearch-lazy-highlight nil))
    (if (not match)
        ;; if MATCH is nil, just call `isearch-repeat'
        (isearch-repeat (if reverse 'backward 'forward))
      (if reverse
          (isearch-backward-regexp not-regexp t)
        (isearch-forward-regexp not-regexp t))
      (isearch-process-search-string
       match
       (mapconcat 'isearch-text-char-description match "")))
    (isearch-update)
    (isearch-done))
  ;; highlight after isearch-done
  ;; M-x:lazy-highlight-cleanup to cleanup highlight
  (when (and isearch-success
             (not inhibit-lazy-highlight-and-count))
    (isearch-lazy-highlight-new-loop))
  isearch-success)


(define-advice meep-isearch-repeat-next (:around (orig-fun &rest args) search-region)
  (if (and (region-active-p)
           (< (- (region-end) (region-beginning)) 100))
      (let* ((text-bounds (meep--isearch-bounds-at-point-impl))
             (text (meep--isearch-extract-regex-from-bounds text-bounds)))
        (deactivate-mark)
        (isearch--search nil text))
    (apply orig-fun args)))
(define-advice meep-isearch-repeat-prev (:around (orig-fun &rest args) search-region)
  (if (and (region-active-p)
           (< (- (region-end) (region-beginning)) 100))
      (let* ((text-bounds (meep--isearch-bounds-at-point-impl))
             (text (meep--isearch-extract-regex-from-bounds text-bounds)))
        (deactivate-mark)
        (isearch--search t text))
    (apply orig-fun args)))


(defun my-key-free ()
  (interactive)
  (let ((keys (this-command-keys-vector)))
    (message "Key Free: %s" (format-kbd-macro keys))))

;; (key-binding (kbd "C-c")  nil)
(defun my-meep-keymap-set-many (map &rest keybinds)
  (declare (indent 1))
  (pcase-dolist (`(,key . ,def) keybinds)
    (if (stringp def)
        ;; TODO: fix this
        (let ((cmd (key-binding def)))
        (keymap-set map key
                    `(lambda()
                       ,(format "Execute the command `%s' which is bound to %s."
                                cmd (buttonize def 'describe-key (kbd def)))
                      (interactive)
                        (setq unread-command-events (listify-key-sequence (kbd ,def)))
                        )))
        (keymap-set map key def))))

(defun my-meep-basis-keys ()
  (my-meep-keymap-set-many meep-state-keymap-motion
   '("<SPC>" . "C-c")
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
    '("<escape>" . keyboard-quit)
    )

  (my-meep-keymap-set-many meep-state-keymap-normal
    '("i" . "C-c Ni")
    '("G" . "C-c NG")
    '("n" . "C-c Nn")
    '("N" . "C-c NN")
    '("/" . "C-c N/")
    '("z" . "C-c Nz")
    '("=" . "C-M-\\")

    '("m $" . toggle-truncate-lines)
    '("m f" . narrow-to-defun)
    '("m w" . widen)
    '("m h" . mark-defun)
    '("m q" . fill-paragraph)
    '("m s" . gt-do-translate)
    '("m ," . pop-to-mark-command)
    '("m t" .  org-capture)
    '("m z" .  hs-toggle-hiding)
    ;; '("m" . "C-c Nm")
    '("P" . my-key-free)
    '("1" . meep-digit-argument-repeat)
    '("2" . meep-digit-argument-repeat)
    '("3" . meep-digit-argument-repeat)
    '("4" . meep-digit-argument-repeat)
    '("5" . meep-digit-argument-repeat)
    '("6" . meep-digit-argument-repeat)
    '("7" . meep-digit-argument-repeat)
    '("8" . meep-digit-argument-repeat)
    '("9" . meep-digit-argument-repeat)
    '("0" . meep-digit-argument-repeat)
    '("-" . meep-digit-argument-repeat)

    '("t" . meep-region-to-secondary-selection)
    '("T" . meep-region-swap)

    ;; ----
    ;; Left Hand: Row 1.
    ;; ----

     '("<f8>" . repeat-fu-execute)

    '("Q" . meep-clipboard-register-actions)
    ;; '("W" . my-key-free)

    '("d" . meep-clipboard-killring-cut)
    '("D" . meep-clipboard-only-cut)

    '("r" . meep-clipboard-killring-yank)
    '("M-r" . meep-clipboard-killring-yank-pop-stack)
    '("R" . meep-clipboard-only-yank)

    '("y" . meep-clipboard-killring-copy)
    '("Y" . meep-clipboard-only-copy)

    ;; Left Hand: Row 2.

    ;; NOTE: a more comprehensive surround map is really needed.
    ;; This is only character level surround insertion.
    '("S" . meep-char-surround-insert)
    '("f s" . meep-char-surround-insert-lines)

    '("m i" . meep-insert-at-last)
    '("m v" . rectangle-mark-mode)

    '("o" . meep-insert-open-below)
    '("O" . meep-insert-open-above)
    '("m e" . meep-insert-line-end)
    '("m a" . meep-insert-line-beginning)

    '("m d" . downcase-region)
    '("m u" . upcase-region)

    '("m <return>" . fill-region)
    '("m <end>" . end-of-buffer)
    '("m <home>" . beginning-of-buffer)

    ;; Run commands "after" numeric has been set.
    ;; Unlike the default to re-running N times.
    '("m 1" . digit-argument)
    '("m 2" . digit-argument)
    '("m 3" . digit-argument)
    '("m 4" . digit-argument)
    '("m 5" . digit-argument)
    '("m 6" . digit-argument)
    '("m 7" . digit-argument)
    '("m 8" . digit-argument)
    '("m 9" . digit-argument)
    '("m 0" . digit-argument)
    '("m -" . negative-argument)

    '("C-2" . meep-region-toggle)
    '("m m" . meep-region-toggle)
    '("s" . meep-region-expand-to-line-bounds)

    '("f h" . meep-move-find-char-on-line-at-prev)
    '("f b" . meep-move-find-char-on-line-till-prev)
    ;; '("f j" . meep-isearch-regexp-next)
    ;; '("f k" . meep-isearch-regexp-prev)
    '("?" . meep-isearch-regexp-prev)
    '("f l" . meep-move-find-char-on-line-at-next)
    '("f f" . meep-move-find-char-on-line-till-next)

    ;; Find "repeat" are below the keys for find.
    '("f ." . meep-move-find-char-on-line-repeat-at-next)
    '("f ," . meep-move-find-char-on-line-repeat-at-prev)
    '("f o" . meep-move-find-char-on-line-repeat-till-next)
    '("f i" . meep-move-find-char-on-line-repeat-till-prev)

    '("C-3" . meep-isearch-at-point-prev)
    '("C-8" . meep-isearch-at-point-next)

    ;; '("f u" . avy-goto-symbol-1-below)
    ;; '("f i" . avy-goto-symbol-1-above)

    ;; Alternative to VIM's ":" to go to line numbers (frees up a key).
    '("f ;" . goto-line)
    '("f :" . goto-char)

    '("F" . my-key-free)

    ;; '("s r" . meep-char-replace)
    ;; '("s i" . meep-char-insert)

    ;; Left Hand: Row 3.
    '("u" . undo)
    '("U" . undo-redo)

    '("i" . meep-insert)
    '("I" . meep-insert-overwrite)
    '("a" . meep-insert-append)

    '("x" . meep-delete-char-ring-next)
    '("X" . meep-delete-char-ring-prev)

    '("z" . meep-delete-char-ring-yank)
    '("Z" . meep-clipboard-killring-cut-line) ; Odd-one out, locate for convenience.

    '("c" . meep-insert-change)
    '("C" . meep-insert-change-lines)

    ;; Right Hand: Row 1.
    '("f a" . meep-move-line-non-space-beginning)
    '("f e" . meep-move-line-non-space-end)
    '("f v" . meep-move-by-sexp-any-prev)

    '("C-r" . meep-exchange-point-and-mark)
    '("f x" . meep-move-by-sexp-over-next)
    '("f X" . meep-move-by-sexp-over-prev)

    '(";" . meep-exchange-point-and-mark-motion)
    '("f u" . meep-move-by-sexp-any-next)


    ;; Right Hand: Row 2.
    '("H" . meep-move-same-syntax-or-symbol-prev)

    '("J" . meep-move-by-sexp-out-next)

    '("K" . meep-move-by-sexp-out-prev)

    '("L" . meep-move-same-syntax-or-symbol-next)
    
    '("q" . meep-move-matching-bracket-inner)
    '("p" . meep-move-matching-bracket-outer)
    '("[" . meep-move-matching-bracket-inner)
    '("]" . meep-move-matching-bracket-outer)

    '("'" . meep-move-matching-syntax-inner)
    '("\"" . meep-move-matching-syntax-outer)

    ;; Right Hand: Row 3.
    '("v" . meep-move-symbol-prev)
    '("V" . meep-move-same-syntax-and-space-prev)

    ;; '(">" . meep-move-paragraph-next)

    ;; '("<" . meep-move-paragraph-prev)
   '("<" . indent-rigidly-left-to-tab-stop)
   '(">" . indent-rigidly-right-to-tab-stop)

    '("e" . meep-move-symbol-next-end)
    '("E" . meep-move-same-syntax-and-space-next)

    '("w" . meep-move-word-next-end)
    '("b" . meep-move-word-prev)
    '("W" . meep-move-same-syntax-and-space-next-end)

    ;; Other keys.
    '("\\" . meep-register-jump-to)
    '("`" . meep-register-kmacro-start-or-end)

    '("," . meep-move-to-bounds-of-thing-beginning)
    '("." . meep-move-to-bounds-of-thing-end)

    '("-" . meep-region-syntax-contract)
    '("=" . meep-region-syntax-expand)

    ;; '("<tab>" . meep-indent-rigidly)

    '("S-<delete>" . meep-join-line-prev)

    '("S-<backspace>" . meep-join-line-next)

    '("^" . meep-move-line-beginning)
    '("$" . meep-move-line-end)
)

  (my-meep-keymap-set-many meep-state-keymap-visual)

  (my-meep-keymap-set-many meep-state-keymap-insert '("<escape>" . bray-state-stack-pop))

  (my-meep-keymap-set-many meep-clipboard-register-map
    '("d" . meep-clipboard-register-cut)
    '("r" . meep-clipboard-register-yank)
    '("y" . meep-clipboard-register-copy)))

(when (require 'which-key nil t)
  (setq which-key-max-description-length 36)
  (add-hook 'after-init-hook #'which-key-mode))

(setq repeat-fu-preset 'meep)
(add-hook 'bray-mode-hook (lambda () (repeat-fu-mode (or bray-mode 0))))


(defun my-meep-setup-once ()
  ;; Extended functions.
  (meep-bootstrap-once)

  (setq meep-state-insert 'insert)
  (setq bray-state-default 'normal)

  (defvar meep-state-hook-insert-enter nil)
  (defvar meep-state-hook-insert-exit nil)

  (defvar meep-state-hook-normal-enter nil)
  (defvar meep-state-hook-normal-exit nil)

  ;; Visual mode.
  (defun meep-mark-hook-activate ()
    "Activate visual state."
    (when (bray-state-derived-p 'normal)
      (bray-state-stack-push 'visual)))
  (defun meep-mark-hook-deactivate ()
    "Activate visual state."
    (when (bray-state-derived-p 'visual)
      (bray-state-stack-pop)))

  (add-hook
   'bray-mode-hook
   (lambda ()
     (cond
      (bray-mode
       (add-hook 'activate-mark-hook #'meep-mark-hook-activate)
       (add-hook 'deactivate-mark-hook #'meep-mark-hook-deactivate))
      (t
       (remove-hook 'activate-mark-hook #'meep-mark-hook-activate)
       (remove-hook 'deactivate-mark-hook #'meep-mark-hook-deactivate)))))
  ;; End visual mode support.

  (add-hook
   'meep-state-hook-insert-enter
   (lambda ()
     (set-mark (point))
     (deactivate-mark)))

  (add-hook
   'meep-state-hook-insert-exit
   (lambda ()
     ;; (set-mark (point))
     (deactivate-mark)

     ;; Testing this out!
     ;; VIM style '^' register for when we leave insert mode.
     (let ((reg ?^))
       (let ((reg-val (get-register reg)))
         (cond
          ((and reg-val (markerp reg-val))
           (set-marker reg-val (point) (current-buffer)))
          (t
           (set-register reg (point-marker))))))))


  (defvar meep-state-keymap-motion (make-keymap))
  (defvar meep-state-keymap-normal (make-keymap))
  (defvar meep-state-keymap-visual (make-keymap))
  (defvar meep-state-keymap-insert (make-keymap))

  ;; Optional, a quick way to mask insertion.
  (define-key meep-state-keymap-motion [remap self-insert-command] 'my-key-free)

  (setq bray-state-definitions
        (list
         (list
          :id 'normal
          ;; Define.
          :cursor-type 'box
          :lighter "<N>"
          :keymaps (list (cons t 'meep-state-keymap-motion) (cons t 'meep-state-keymap-normal))

          :enter-hook 'meep-state-hook-normal-enter
          :exit-hook 'meep-state-hook-normal-exit)
         (list
          :id 'motion
          ;; Define.
          :cursor-type 'hollow
          :lighter "<M>"
          :keymaps (list (cons t 'meep-state-keymap-motion))

          :enter-hook 'meep-state-hook-normal-enter
          :exit-hook 'meep-state-hook-normal-exit)

         (list
          :id 'visual
          ;; Define.
          :cursor-type 'bar
          :lighter "<V>"
          :keymaps (list (cons t 'meep-state-keymap-motion) (cons t 'meep-state-keymap-normal) (cons t 'meep-state-keymap-visual))

          :enter-hook 'meep-state-hook-visual-enter
          :exit-hook 'meep-state-hook-visual-exit)

         (list
          :id 'insert
          ;; Define.
          :cursor-type 'bar
          :lighter "<I>"
          :keymaps (list (cons t 'meep-state-keymap-insert))

          :enter-hook 'meep-state-hook-insert-enter
          :exit-hook 'meep-state-hook-insert-exit

          ;; Optional.
          :is-input t)))

  (my-meep-basis-keys))

(my-meep-setup-once)


;; Enable MEEP for most buffers.
(add-hook
 'after-change-major-mode-hook
 (lambda ()
   ;; Enable these modes for all non-special buffers (files).
   (unless (minibufferp)
     (bray-mode 1)
     (when (derived-mode-p
              (list 'special-mode 'gud-mode 'term-mode 'inferior-emacs-lisp-mode 'dired-mode))
        (bray-state-stack-push 'motion)
       ))))

(with-current-buffer "*Messages*"  (bray-mode))


(provide 'conf-meep)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-meep.el ends here.
