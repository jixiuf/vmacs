;;; init.el --- Testing -*- lexical-binding: t -*-
(global-set-key (kbd "M-y") #'vmacs-yank-pop)
(global-set-key (kbd "C-s-v") #'meep-clipboard-only-yank)             ;paste from clipboard
(global-set-key (kbd "C-y") #'meep-clipboard-killring-yank) ;paste from kill ring
(global-set-key (kbd "C-c ,") #'meep-move-to-bounds-of-thing-beginning)
(global-set-key (kbd "C-c .") #'meep-move-to-bounds-of-thing-end)

(defun meep-clipboard-killring-yank-ad(&optional arg)
  (activate-mark))
(advice-add 'meep-clipboard-killring-yank-pop-stack :before #'meep-clipboard-killring-yank-ad)

(defun meep-clipboard-killring-copy-ad(&optional arg)
  (meep-clipboard-only-copy)
  (activate-mark))
(advice-add 'meep-clipboard-killring-copy :before #'meep-clipboard-killring-copy-ad)

;; (advice-add 'meep--wrap-current-kill :override
;;             (defun meep--wrap-current-kill-ad (n &optional do-not-move)
;;               "Wrap `current-kill', only ever use the kill ring. Forward N & DO-NOT-MOVE."
;;               (current-kill n do-not-move)))

(defun meep-next-line-dwim(&optional arg)
  (interactive)
  (if (and (region-active-p)
           (> (point) (mark)))
      (meep-move-line-next (or arg 1))
    (meep-move-line-prev (or arg 1))))

(defun meep-push-markers(&optional arg)
  (require 'xref)
  (xref--push-markers (current-buffer)
                      (point-marker)
                      (selected-window)))


(defun meep-append ()
  "meow like append."
  (interactive)
  (cond
   ((region-active-p)
    (when (< (point) (mark))
      (goto-char (mark))))
   (t
    (unless (eobp)
      (forward-char 1))))
  (meep--insert-impl))

(defun meep-insert-before ()
  "meow like insert mode."
  (when (region-active-p)
    (when (> (point) (mark))
      (goto-char (mark)))))

(advice-add 'meep-insert :before #'meep-insert-before)
(defun meep--search-match (match)
  "Check if MATCH can be matched by the current `isearch-string'."
  (when (and match
             (not (string-empty-p match))
             (not (string-empty-p isearch-string)))
    (save-match-data
      (save-mark-and-excursion
        (let ((case-fold-search isearch-case-fold-search)
              (search-invisible isearch-invisible))
          (with-temp-buffer
            (insert match)
            (when isearch-forward
              (goto-char (point-min)))
            (and (isearch-search-string isearch-string nil t)
                 (string-equal (match-string 0) match))))))))

(define-advice meep-isearch-repeat-next (:around (orig-fun &rest args) search-region)
  (if (and (region-active-p)
           (= (line-number-at-pos (region-end))
              (line-number-at-pos (region-beginning))))
      (if (< (point) (mark))
          (if (meep--search-match (buffer-substring (region-beginning)(region-end)))
              (call-interactively #'meep-isearch-repeat-prev)
            (call-interactively #'meep-isearch-at-point-prev))
        (if (meep--search-match (buffer-substring (region-beginning)(region-end)))
            (apply orig-fun args)
          (call-interactively #'meep-isearch-at-point-next)))
    (apply orig-fun args)))

(define-advice meep-isearch-repeat-prev (:around (orig-fun &rest args) search-region)
  (if (and (region-active-p)
           (= (line-number-at-pos (region-end))
              (line-number-at-pos (region-beginning))))
      (if (meep--search-match (buffer-substring (region-beginning)(region-end)))
          (apply orig-fun args)
        (call-interactively #'meep-isearch-at-point-prev))
    (apply orig-fun args)))


;; (defun my-key-free ()
;;   (interactive)
;;   (let ((keys (this-command-keys-vector)))
;;     (message "Key Free: %s" (format-kbd-macro keys))))

;; (key-binding (kbd "C-c")  nil)

(defun meep-kbd (def)
  "Command that converts current key."
  `(lambda (&rest args)
     ;; Keys that apply only to a major mode aren't currently functioning, as =describe-key= is
     ;; executed when =meep-kbd= is called.
     ,(format "Execute the command which is bound to %s."
              (buttonize def 'describe-key (kbd def)))
     (interactive)
     (setq prefix-arg current-prefix-arg)
     (cond
      ( (string-suffix-p "-" ,def)
        (let ((ch (char-to-string(read-event ,def))))
          (setq unread-command-events (listify-key-sequence (kbd (concat ,def ch))))))
      (t
       (setq unread-command-events (listify-key-sequence (kbd ,def)))))))

(defun my-meep-basis-keys ()
 (global-set-key (kbd "C-c x") (meep-kbd "C-x")) ;this space+x = C-c x
 ;; (global-set-key (kbd "C-c ,") (meep-kbd "M-"))	;this space+, = M-
  (defvar-keymap meep-state-keymap-motion
    "<SPC>"     (meep-kbd "C-c" )
    "j"         (meep-kbd "C-c Mj")
    "h"         (meep-kbd "C-c Mh")
    "l"         (meep-kbd "C-c Ml")
    "k"         (meep-kbd "C-c Mk")
    "g"         (meep-kbd "C-c G")
    "G"         (meep-kbd "C-c MG")
    "/"         (meep-kbd  "C-c M/")
    "n"         (meep-kbd "C-c Mn")
    "N"         (meep-kbd "C-c MN")
    "z"         (meep-kbd "C-c Mz")
    "q"         (meep-kbd "C-c Mq")
    ":"         (meep-kbd "C-c M:")
    "i"         (meep-kbd "C-c Mi")
    "="         (meep-kbd "C-M-\\")
    "<escape>"  #'keyboard-quit)

  (defvar-keymap meep-state-keymap-normal
    "1"           #'meep-digit-argument-repeat
    "2"           #'meep-digit-argument-repeat
    "3"           #'meep-digit-argument-repeat
    "4"           #'meep-digit-argument-repeat
    "5"           #'meep-digit-argument-repeat
    "6"           #'meep-digit-argument-repeat
    "7"           #'meep-digit-argument-repeat
    "8"           #'meep-digit-argument-repeat
    "9"           #'meep-digit-argument-repeat
    "0"           #'meep-digit-argument-repeat
    "-"           #'meep-digit-argument-repeat
    "t"           #'meep-region-to-secondary-selection
    "T"           #'meep-region-swap
    "<f8>"        #'repeat-fu-execute
    "Q"           #'meep-clipboard-register-actions
    "d"           #'meep-clipboard-killring-cut
    "D"           #'meep-clipboard-only-cut
    "r"           #'meep-clipboard-killring-yank
    "M-r"         #'meep-clipboard-killring-yank-pop-stack
    "R"           #'meep-clipboard-only-yank
    "y"           #'meep-clipboard-killring-copy
    "Y"           #'meep-clipboard-only-copy
    "S"           #'meep-char-surround-insert
    "f s"         #'meep-char-surround-insert-lines
    "o"           #'meep-insert-open-below
    "O"           #'meep-insert-open-above
    "m i"         #'meep-insert-at-last
    "m v"         #'rectangle-mark-mode
    "m $"         #'toggle-truncate-lines
    "m f"         #'narrow-to-defun
    "m n"         #'narrow-to-region
    "m w"         #'widen
    "m h"         #'mark-defun
    "m q"         #'fill-paragraph
    "m s"         #'gt-do-translate
    "m ,"         #'pop-to-mark-command
    "m t"         #'org-capture
    "m z"         #'hs-toggle-hiding
    "m e"         #'meep-insert-line-end
    "m a"         #'meep-insert-line-beginning
    "m d"         #'downcase-region
    "m u"         #'upcase-region
    "m <return>"  #'fill-region
    "m <end>"     #'end-of-buffer
    "m <home>"    #'beginning-of-buffer
    "m 1"         #'digit-argument
    "m 2"         #'digit-argument
    "m 3"         #'digit-argument
    "m 4"         #'digit-argument
    "m 5"         #'digit-argument
    "m 6"         #'digit-argument
    "m 7"         #'digit-argument
    "m 8"         #'digit-argument
    "m 9"         #'digit-argument
    "m 0"         #'digit-argument
    "m -"         #'negative-argument
    "C-2"         #'meep-region-toggle
    "m m"         #'meep-region-toggle
    "s"           #'meep-region-expand-to-line-bounds
    "f h"         #'meep-move-find-char-on-line-at-prev
    "f b"         #'meep-move-find-char-on-line-till-prev
    "?"           #'meep-isearch-regexp-prev
    "f l"         #'meep-move-find-char-on-line-at-next
    "f f"         #'meep-move-find-char-on-line-till-next
    "f ."         #'meep-move-find-char-on-line-repeat-at-next
    "f ,"         #'meep-move-find-char-on-line-repeat-at-prev
    "f o"         #'meep-move-find-char-on-line-repeat-till-next
    "f i"         #'meep-move-find-char-on-line-repeat-till-prev
    "C-3"         #'meep-isearch-at-point-prev
    "C-8"         #'meep-isearch-at-point-next
    "f ;"         #'goto-line
    "f :"         #'goto-char
    "u"           #'undo
    "U"           #'undo-redo
    "I"          #'meep-insert-overwrite
    "a"          #'meep-append
    ;; "         z" #'meep-transpose
    "x"          #'meep-delete-char-ring-next
    "X"          #'meep-delete-char-ring-prev
    "c"          #'meep-insert-change
    "C"          #'meep-insert-change-lines
    "f a"        #'meep-move-line-non-space-beginning
    "f e"        #'meep-move-line-non-space-end
    "f v"        #'meep-move-by-sexp-any-prev
    "f x"        #'meep-move-by-sexp-over-next
    "f X"        #'meep-move-by-sexp-over-prev
    ";"          #'meep-exchange-point-and-mark-motion
    "f u"        #'meep-move-by-sexp-any-next
    "H"          #'meep-move-same-syntax-or-symbol-prev
    "J"          #'meep-join-line-next
    "K"          #'meep-join-line-prev
    "C-o"        #'meep-move-by-sexp-out-prev
    "L"          #'meep-move-same-syntax-or-symbol-next
    "q"          #'meep-move-matching-bracket-inner
    "C-r"        #'meep-move-matching-bracket-outer
    "["          #'meep-move-matching-bracket-inner
    "]"          #'meep-move-matching-bracket-outer
    "'"          #'meep-move-matching-syntax-inner
    "\""         #'meep-move-matching-syntax-outer
    "v"          #'meep-move-symbol-prev
    "V"          #'meep-move-same-syntax-and-space-prev
    "<"          #'indent-rigidly-left-to-tab-stop
    ">"          #'indent-rigidly-right-to-tab-stop
    "e"          #'meep-move-symbol-next-end
    "E"          #'meep-move-same-syntax-and-space-next
    "w"          #'meep-move-word-next-end
    "b"          #'meep-move-word-prev
    "W"          #'meep-move-same-syntax-and-space-next-end
    "\\"         #'meep-register-jump-to
    "`"          #'meep-register-kmacro-start-or-end
    ","          #'meep-move-to-bounds-of-thing-beginning
    "."          #'meep-move-to-bounds-of-thing-end
    "^"          #'meep-move-line-beginning
    "$"          #'meep-move-line-end)

  (defvar-keymap meep-state-keymap-visual
    "s" #'meep-next-line-dwim
    "<SPC>" #'meep-region-expand-to-line-bounds
    "x" #'meep-exchange-point-and-mark)

  (defvar-keymap meep-state-keymap-insert "<escape>"  #'bray-state-stack-pop)

  (defvar-keymap meep-clipboard-register-map
    "d"  #'meep-clipboard-register-cut
    "r"  #'meep-clipboard-register-yank
    "y"  #'meep-clipboard-register-copy))

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
  ;; visual mode End support.

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

  (my-meep-basis-keys)


  ;; Optional, a quick way to mask insertion.
  ;; (define-key meep-state-keymap-motion [remap self-insert-command] 'my-key-free)

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
          :enter-hook 'meep-state-hook-motion-enter
          :exit-hook 'meep-state-hook-motion-exit)

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

  )

(my-meep-setup-once)


;; Enable MEEP for most buffers.
(defun meep-mode-hook()
  (unless (minibufferp)
    (bray-mode 1)
    (cond
     ((derived-mode-p '(reb-mode calc-mode))
      (bray-state-stack-push 'insert))
     ((derived-mode-p
       '(special-mode gud-mode term-mode
                      org-agenda-mode
                      inferior-emacs-lisp-mode dired-mode))
      (bray-state-stack-push 'motion)))))

(add-hook 'after-change-major-mode-hook #'meep-mode-hook)


(with-current-buffer "*Messages*"  (bray-mode))

(defmacro meep-set-keymap-parent (map-or-mode &optional parent)
  "Set the parent keymap for MAP-OR-MODE to PARENT.

MAP-OR-MODE can be a map or a mode
Returns PARENT. PARENT should be nil or another keymap.
Default is 'meep-state-keymap-normal' when PARENT is nil."
  `(let ((map (if (keymapp ,map-or-mode)
                  ,map-or-mode
                (let ((map-name (intern (concat (symbol-name ,map-or-mode) "-map"))))
                  (when (boundp map-name)
                    (symbol-value map-name))))))
     (when (keymapp map)
       (set-keymap-parent map (make-composed-keymap (keymap-parent map)
                                                    (or ,parent meep-state-keymap-normal))))))
;; (keymap-unset occur-mode-map "l" t)

(defvar meep-motion-parent-keymaps (make-hash-table :test #'equal))
(defun meep-motion-set-keymap-parent()
  (unless (member major-mode '(special-mode))
    (when (and (equal bray-state 'motion)
               (not (gethash major-mode meep-motion-parent-keymaps)))
      (puthash major-mode t meep-motion-parent-keymaps)
      (meep-set-keymap-parent major-mode meep-state-keymap-normal))))

(add-hook 'meep-state-hook-motion-enter #'meep-motion-set-keymap-parent)

;; gopkg
(require 'thingatpt)
(define-thing-chars gopkg "-/[:alnum:]_.@:")
(put 'grave-quoted 'bounds-of-thing-at-point
     (lambda ()
       (let ((thing (thing-at-point-looking-at
		     "[^`]+" 500)))
         (if thing
             (let ((beginning (match-beginning 0))
                   (end (match-end 0)))
               (cons beginning end))))))
(put 'org-block 'beginning-op (lambda()
                                (when (re-search-backward
                                    "\\(^[ \\|\t]*\\(?:#\\+begin_\\|```\\)[^\n]*\n\\)"
                                    nil t)
                                 (goto-char  (match-end 1)))))
(put 'org-block 'end-op (lambda()
                                (when (re-search-forward
                                    "\\(^[ \\|\t]*\\(?:#\\+end_[^\n]*\\|```\\)$\\)"
                                    nil t)
                                  (goto-char  (match-beginning 1)))))

;;;###autoload
(defun meep-move-to-bounds-of-thing (arg &optional thing mark)
  "Move to the thing start/end (start when ARG is negative)."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (cond
     (bounds
      (meep--move-to-bounds-endpoint bounds arg)
      (when mark
        (meep-mark-thing-at-point))
      (message "go to %s of %s" (or (and (< arg 0) "beginning") "end") thing)
      t)
     (t
      (message "Not found: bounds of %s" thing)
      nil))))
(defun meep-mark-thing-at-point (&optional arg _)
  (when (member last-command '(meep-move-to-bounds-of-thing-beginning
                               meep-move-to-bounds-of-thing-end))
    (run-with-timer 0.001 nil (lambda()
                                ;; (setq this-command last-command)
                                (meep-exchange-point-and-mark-motion)
                                (exchange-point-and-mark)))))

(advice-add 'meep-move-to-bounds-of-thing-beginning :before #'meep-push-markers)
(advice-add 'meep-region-expand-to-line-bounds :before #'(lambda(&optional arg)
                                                           (unless (region-active-p)
                                                             (meep-push-markers)
                                                             )))

(defun meep-move-to-bounds-of-gopkg (arg)
  (interactive "^p")
  (meep-move-to-bounds-of-thing  arg 'gopkg t))

(defun meep-move-to-bounds-of-word (arg)
  (interactive "^p")
  (meep-move-to-bounds-of-thing  arg 'word t)
  )
(defun meep-move-to-bounds-of-symbol (arg)
  (interactive "^p")
  (meep-move-to-bounds-of-thing  arg 'symbol t))
(defun meep-move-to-bounds-of-grave-quoted (arg )
  (interactive "^p")
  (meep-move-to-bounds-of-thing  arg 'grave-quoted t))
(defun meep-move-to-bounds-of-org-block (arg)
  (interactive "^p")
  (meep-move-to-bounds-of-thing  arg 'org-block t))

(dolist (cmd
         (list
          'meep-move-to-bounds-of-gopkg
          'meep-move-to-bounds-of-word
          'meep-move-to-bounds-of-symbol
          'meep-move-to-bounds-of-grave-quoted
          'meep-move-to-bounds-of-org-block))
         (meep-command-prop-set cmd :mark-on-motion t))
(advice-add 'meep-move-matching-bracket-inner :after #'meep-mark-thing-at-point)
(advice-add 'meep-move-matching-bracket-outer :after #'meep-mark-thing-at-point)
(advice-add 'meep-move-to-bounds-of-defun :after #'meep-mark-thing-at-point)
(advice-add 'meep-move-to-bounds-of-string :after #'meep-mark-thing-at-point)
(advice-add 'meep-move-to-bounds-of-string-inner :after #'meep-mark-thing-at-point)

(add-to-list 'meep-bounds-commands '(?r meep-move-to-bounds-of-gopkg "gopkg"))
(add-to-list 'meep-bounds-commands '(?. meep-move-to-bounds-of-word "word"))
(add-to-list 'meep-bounds-commands '(?, meep-move-to-bounds-of-symbol "symbol"))
(add-to-list 'meep-bounds-commands '(?` meep-move-to-bounds-of-grave-quoted "`"))
(add-to-list 'meep-bounds-commands '(?o meep-move-to-bounds-of-org-block "org-block"))
(add-to-list 'meep-bounds-commands '(?c meep-move-matching-bracket-inner "src-block inner"))
(add-to-list 'meep-bounds-commands '(?x meep-move-matching-bracket-outer "src-block"))
(add-to-list 'meep-bounds-commands '(?f meep-move-to-bounds-of-defun "defun"))
(add-to-list 'meep-bounds-commands '(?g meep-move-to-bounds-of-string "str"))
(add-to-list 'meep-bounds-commands '(?/ meep-move-to-bounds-of-comment-inner "comment inner"))

(provide 'conf-meep)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-meep.el ends here.
