;;; -*- lexical-binding: t -*-
(global-set-key (kbd "M-y") #'vmacs-yank-pop)
(global-set-key (kbd "C-s-v") #'meep-clipboard-only-yank)             ;paste from clipboard
(global-set-key (kbd "C-y") #'meep-clipboard-killring-yank) ;paste from kill ring
(global-set-key (kbd "C-c ,") #'meep-move-to-bounds-of-thing-beginning)
(global-set-key (kbd "C-c .") #'meep-move-to-bounds-of-thing-end)

(defun my-key-free ()
  (interactive)
  (let ((keys (this-command-keys-vector)))
    (message "Key Free: %s" (format-kbd-macro keys))))
(defun my-meep-basis-keys ()
  (defvar-keymap m-map
    "i"         #'meep-insert-at-last
    "v"         #'rectangle-mark-mode
    "$"         #'toggle-truncate-lines
    "f"         #'narrow-to-defun
    "n"         #'narrow-to-region
    "w"         #'widen
    "h"         #'mark-defun
    "s"         #'gt-translate
    ","         #'pop-to-mark-command
    "t"         #'org-capture
    "z"         #'hs-toggle-hiding
    "e"         #'meep-insert-line-end
    "a"         #'meep-insert-line-beginning
    "q"         #'fill-paragraph
    "<return>"  #'fill-region
    "1"         #'meep-digit-argument-repeat
    "2"         #'meep-digit-argument-repeat
    "3"         #'meep-digit-argument-repeat
    "4"         #'meep-digit-argument-repeat
    "5"         #'meep-digit-argument-repeat
    "6"         #'meep-digit-argument-repeat
    "7"         #'meep-digit-argument-repeat
    "8"         #'meep-digit-argument-repeat
    "9"         #'meep-digit-argument-repeat
    "0"         #'meep-digit-argument-repeat
    "-"         #'meep-digit-argument-repeat
    "m"         #'meep-region-toggle)

  (global-set-key (kbd "C-c m") m-map)
  (defvar-keymap g-map
    "4" #'query-replace ;space g4
    "5" #'re-builder                                 ;;query-replace-regexp
    "g" #'vmacs-goto-line
    "T" #'consult-grep
    "t" #'consult-ripgrep
    "e" #'grep
    "w" (vmacs-defun consult-ripgrep-default (consult-ripgrep default-directory))
    "x" (vmacs-defun consult-ripgrep-root-symbol (consult-ripgrep(vc-root-dir)  (concat "\\b" (thing-at-point 'symbol) "\\b")))
    "X" #'consult-ripgrep-root-symbol
    "s" (vmacs-defun consult-ripgrep-default-symbol (consult-ripgrep default-directory (concat "\\b" (thing-at-point 'symbol) "\\b")))
    "/" #'consult-focus-lines
    "z" #'consult-hide-lines
    "r" #'revert-buffer
    ";" #'goto-line
    ":" #'goto-char
    "n" #'next-error
    "p" #'previous-error
    "b" #'pop-global-mark
    "u" #'upcase-dwim
    "U" #'downcase-dwim
    "m" #'push-mark-command
    "P" #'project-or-external-find-file
    "d" #'xref-find-definitions
    "," #'goto-last-change
    "." #'goto-last-change-reverse
    "f" #'gptel-rewrite
    )
  (global-set-key (kbd "C-c g") g-map)
  (defvar-keymap meep-state-keymap-motion
    "g" g-map
    "j"         #'meep-move-line-next
    "k"         #'meep-move-line-prev
    "h"         #'meep-move-char-prev
    "l"         #'meep-move-char-next
    "G"         #'vmacs-goto-line
    "/"         #'meep-isearch-regexp-next
    "n"         #'meep-isearch-repeat-next
    "N"         #'meep-isearch-repeat-prev
    ":"	        #'viper-ex
    "z"         #'meep-transpose
    "q"         #'meep-move-matching-bracket-inner
    "<escape>"  #'keyboard-quit)
  
  
  (define-key meep-state-keymap-motion [remap self-insert-command] #'my-key-free)
  (defvar-keymap meep-state-keymap-normal
    "m" m-map
    "i"         #'meep-insert
    "1"           #'digit-argument
    "2"           #'digit-argument
    "3"           #'digit-argument
    "4"           #'digit-argument
    "5"           #'digit-argument
    "6"           #'digit-argument
    "7"           #'digit-argument
    "8"           #'digit-argument
    "9"           #'digit-argument
    "0"           #'digit-argument
    "-"           #'negative-argument
    "C-2"         #'meep-region-toggle
    "t"           #'meep-region-to-secondary-selection
    "T"           #'meep-region-swap
    "<f8>"        #'repeat-fu-execute
    "Q"           #'meep-clipboard-register-actions
    "p"           #'meep-region-syntax-expand
    "d"           #'meep-clipboard-killring-cut
    "D"           #'meep-clipboard-only-cut
    "r"           #'meep-clipboard-killring-yank
    "M-r"         #'meep-clipboard-killring-yank-pop-stack
    "R"           #'meep-clipboard-only-yank
    "y"           #'meep-clipboard-killring-copy
    "Y"           #'meep-clipboard-only-copy
    "S"           #'meep-char-surround-insert
    "o"           #'meep-insert-open-below
    "O"           #'meep-insert-open-above
    "s"           #'meep-region-expand-to-line-bounds
    "?"           #'meep-isearch-regexp-prev
    ;; "f s"         #'meep-char-surround-insert-lines
    ;; "f h"         #'meep-move-find-char-on-line-at-prev
    ;; "f b"         #'meep-move-find-char-on-line-till-prev
    ;; "f l"         #'meep-move-find-char-on-line-at-next
    ;; "f f"         #'meep-move-find-char-on-line-till-next
    ;; "f ."         #'meep-move-find-char-on-line-repeat-at-next
    ;; "f ,"         #'meep-move-find-char-on-line-repeat-at-prev
    ;; "f o"         #'meep-move-find-char-on-line-repeat-till-next
    ;; "f i"         #'meep-move-find-char-on-line-repeat-till-prev
    ;; "f t"         #'toggle-case-fold-search
    ;; "f a"        #'meep-move-line-non-space-beginning
    ;; "f e"        #'meep-move-line-non-space-end
    ;; "f v"        #'meep-move-by-sexp-any-prev
    ;; "f x"        #'meep-move-by-sexp-over-next
    ;; "f X"        #'meep-move-by-sexp-over-prev
    ;; "f u"        #'meep-move-by-sexp-any-next
    "C-3"         #'meep-isearch-at-point-prev
    "C-8"         #'meep-isearch-at-point-next
    "u"           #'undo
    "U"           #'undo-redo
    "I"          #'meep-insert-overwrite
    "a"          #'meep-append
    ;; "         z" #'meep-transpose
    "x"          #'meep-delete-char-ring-next
    "X"          #'meep-delete-char-ring-prev
    "c"          #'meep-insert-change
    "C"          #'meep-insert-change-lines
    ";"          #'meep-region-activate-and-reverse-motion
    "H"          #'meep-move-same-syntax-or-symbol-prev
    "J"          #'meep-join-line-next
    "K"          #'meep-join-line-prev
    "C-o"        #'meep-move-by-sexp-out-prev
    "L"          #'meep-move-same-syntax-or-symbol-next
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
    "x" #'meep-region-activate-and-reverse)

  (defvar-keymap meep-state-keymap-insert "<escape>"  #'bray-state-stack-pop)

  (keymap-set meep-clipboard-register-map "d"  #'meep-clipboard-register-cut)
  (keymap-set meep-clipboard-register-map "r"  #'meep-clipboard-register-yank)
  (keymap-set meep-clipboard-register-map "y"  #'meep-clipboard-register-copy))

(when (require 'which-key nil t)
  (setq which-key-max-description-length 36)
  (add-hook 'after-init-hook #'which-key-mode))

(setq repeat-fu-preset 'meep)
(add-hook 'bray-mode-hook (lambda () (repeat-fu-mode (or bray-mode 0))))


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
        :cursor-type 'box
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
(defun meep-clipboard-killring-yank-1(&optional arg)
  "forward 1 line for line-wise paste"
  (let* ((text (meep--wrap-current-kill 0 t))
         (yank-handler (get-text-property 0 'yank-handler text))
         ;; A NOP if yank-handler is nil (harmless).
         (region-type (meep--yank-handler-to-region-type (car yank-handler))))
    (when (eq region-type 'line-wise)
      (when (= (point) (point-max))
        (insert "\n"))
      (forward-line 1))))

(advice-add 'meep-clipboard-killring-yank :before #'meep-clipboard-killring-yank-1)

(defun meep-clipboard-killring-yank-ad(&optional arg)
  (activate-mark))
(advice-add 'meep-clipboard-killring-yank-pop-stack :before #'meep-clipboard-killring-yank-ad)

(defun meep-clipboard-killring-copy-ad(&optional arg)
  (vmacs-isearch-unhighlight)
  (meep-clipboard-only-copy)
  (activate-mark))
(advice-add 'meep-clipboard-killring-copy :before #'meep-clipboard-killring-copy-ad)

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
  "This advice customizes `meep-isearch-repeat-next' for single-line active regions. If the region
content doesn't match `isearch-string', it is used to search the region content using
`meep-isearch-at-point-next' or `meep-isearch-at-point-prev'. Otherwise, it delegates to the
original function."
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

(defun meep-exchange-point-and-mark-motion-ad (&optional arg ex-pt-mark)
  "auto push single-line active region string to isearch history."
  (when (= (line-number-at-pos (region-end))
           (line-number-at-pos (region-beginning)))
    (let ((search (buffer-substring (region-beginning)(region-end))))
      (unless (string-equal search (car regexp-search-ring))
        (save-mark-and-excursion
          (meep-isearch-repeat-next 1)
          (meep-isearch-repeat-prev -1)
          )
        (when ex-pt-mark
          (exchange-point-and-mark))))))


(advice-add 'meep-region-activate-and-reverse-motion :after #'meep-exchange-point-and-mark-motion-ad)
(advice-add 'meep-region-syntax-expand :after #'(lambda(arg) (meep-exchange-point-and-mark-motion-ad nil t)))
;;


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
(defun meep-move-to-bounds-of-thing (arg &optional thing mark ex-pt-mark)
  "Move to the thing start/end (start when ARG is negative)."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (cond
     (bounds
      (meep--move-to-bounds-endpoint bounds arg)
      (when mark
        (meep-mark-thing-at-point arg nil ex-pt-mark))
      (message "go to %s of %s" (or (and (< arg 0) "beginning") "end") thing)
      t)
     (t
      (message "Not found: bounds of %s" thing)
      nil))))
(defun meep-mark-thing-at-point (&optional arg _ ex-pt-mark)
  (when (member last-command '(meep-move-to-bounds-of-thing-beginning
                               meep-move-to-bounds-of-thing-end))
    (run-with-timer 0.001 nil (lambda()
                                ;; (setq this-command last-command)
                                (meep-region-activate-and-reverse-motion)
                                (when ex-pt-mark
                                  (exchange-point-and-mark))))))

(defun meep-mark-thing-at-point-reverse (&optional arg arg2 )
  (meep-mark-thing-at-point arg arg2 t))

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
;;
(defun meep-move-to-bounds-of-parameter (arg )
  "from evil-textobj-tree-sitter"
  (interactive "^p")
  (require 'evil-textobj-tree-sitter-thing-at-point)
  (meep-move-to-bounds-of-thing  arg 'parameter t t))
(defun meep-move-to-bounds-of-loop (arg )
  "from evil-textobj-tree-sitter"
  (interactive "^p")
  (require 'evil-textobj-tree-sitter-thing-at-point)
  (meep-move-to-bounds-of-thing  arg 'loop t t))

(defun meep-move-to-bounds-of-class (arg )
  "from evil-textobj-tree-sitter"
  (interactive "^p")
  (require 'evil-textobj-tree-sitter-thing-at-point)
  (meep-move-to-bounds-of-thing  arg 'class t t))
(defun meep-move-to-bounds-of-conditional (arg )
  "from evil-textobj-tree-sitter"
  (interactive "^p")
  (require 'evil-textobj-tree-sitter-thing-at-point)
  (meep-move-to-bounds-of-thing  arg 'conditional t t))

(defun meep-move-to-bounds-of-comment (&optional arg _)
  "from evil-textobj-tree-sitter"
  (interactive "^p")
  (require 'evil-textobj-tree-sitter-thing-at-point)
  (meep-move-to-bounds-of-thing  arg 'comment t t))

(defun meep-move-to-bounds-of-org-block (arg)
  (interactive "^p")
  (meep-move-to-bounds-of-thing  arg 'org-block t))

(dolist (cmd
         (list
          'meep-move-to-bounds-of-gopkg
          'meep-move-to-bounds-of-conditional
          'meep-move-to-bounds-of-comment
          'meep-move-to-bounds-of-parameter
          'meep-move-to-bounds-of-loop
          'meep-move-to-bounds-of-class
          'meep-move-to-bounds-of-word
          'meep-move-to-bounds-of-symbol
          'meep-move-to-bounds-of-grave-quoted
          'meep-move-to-bounds-of-org-block))
  (meep-command-prop-set cmd :mark-on-motion t))
(advice-add 'meep-move-matching-bracket-inner :after #'meep-mark-thing-at-point-reverse)
(advice-add 'meep-move-matching-bracket-outer :after #'meep-mark-thing-at-point-reverse)
(advice-add 'meep-move-to-bounds-of-defun :after #'meep-mark-thing-at-point-reverse)
(advice-add 'meep-move-to-bounds-of-string :after #'meep-mark-thing-at-point)

(add-to-list 'meep-bounds-commands '(?r meep-move-to-bounds-of-gopkg "gopkg"))
(add-to-list 'meep-bounds-commands '(?, meep-move-to-bounds-of-word "word"))
(add-to-list 'meep-bounds-commands '(?. meep-move-to-bounds-of-symbol "symbol"))
(add-to-list 'meep-bounds-commands '(?` meep-move-to-bounds-of-grave-quoted "`"))
(add-to-list 'meep-bounds-commands '(?b meep-move-to-bounds-of-org-block "org-block"))
(add-to-list 'meep-bounds-commands '(?c meep-move-matching-bracket-inner "src-block inner"))
(add-to-list 'meep-bounds-commands '(?x meep-move-matching-bracket-outer "src-block"))
(add-to-list 'meep-bounds-commands '(?f meep-move-to-bounds-of-defun "defun"))
(add-to-list 'meep-bounds-commands '(?g meep-move-to-bounds-of-string "str"))
(add-to-list 'meep-bounds-commands '(?/ meep-move-to-bounds-of-comment-inner "comment inner"))
(add-to-list 'meep-bounds-commands '(?i meep-region-mark-bounds-of-char-inner "pair inner"))
(add-to-list 'meep-bounds-commands '(?o meep-region-mark-bounds-of-char-outer "pair"))

(add-to-list 'meep-bounds-commands '(?a meep-move-to-bounds-of-parameter "param"))
(add-to-list 'meep-bounds-commands '(?d meep-move-to-bounds-of-loop "loop"))
(add-to-list 'meep-bounds-commands '(?t meep-move-to-bounds-of-class "type"))
(add-to-list 'meep-bounds-commands '(?w meep-move-to-bounds-of-conditional "conditional"))
;; (add-to-list 'meep-bounds-commands '(?h meep-move-to-bounds-of-comment "comment"))


;; for repeat
(meep-command-prop-set #'meep-isearch-regexp-next :mark-on-motion t)

(defvar-local meep-local-keymap nil
  "Local keymap for meep minor mode in the current buffer.")

(defvar-local meep-minor-mode-map-alist nil
  "Alist to keep track of minor mode keymaps.")

;; (add-to-list 'emulation-mode-map-alists 'meep-minor-mode-map-alist)
(add-to-list 'emulation-mode-map-alists 'meep-minor-mode-map-alist)

(defun meep-register-local-map ()
  "Register the local keymap in `emulation-mode-map-alists`."
  (setq-local meep-minor-mode-map-alist
              (cons (cons t meep-local-keymap) meep-minor-mode-map-alist)))

(defun meep-unregister-local-map ()
  "Unregister the local keymap from `emulation-mode-map-alists`."
  (setq-local meep-minor-mode-map-alist nil))
(add-hook 'meep-state-hook-motion-enter #'meep-register-local-map)
(add-hook 'meep-state-hook-motion-exit #'meep-unregister-local-map)
(add-hook 'meep-state-hook-normal-enter #'meep-register-local-map)
(add-hook 'meep-state-hook-normal-exit #'meep-unregister-local-map)

(defun meep-local-set-key (&rest keybinds)
  "Set KEY to DEF in the local keymap."
  (declare (indent 1))
  (unless meep-local-keymap
    (setq meep-local-keymap (make-sparse-keymap))
    (meep-register-local-map))
  (while keybinds
    (let ((key (pop keybinds))
          (def (pop keybinds)))
      (keymap-set meep-local-keymap key def))))

(defcustom meep-keypad-leader-key "<SPC>"
  "The leader key for keypad mapping. Defaults to space key.
When set, automatically updates the key translation map
to bind this key to `meep-keypad' function."
  :type 'string
  :set (lambda (var val)
         (set-default var val)
         (keymap-set key-translation-map val 'meep-keypad))
  :initialize 'custom-initialize-set)

(defcustom meep-keypad-dispatch
  '((?r . "M-")
    (?h . "C-h")
    (?e . "C-M-")
    ;; (?x . "C-x")
    (?x . "C-x"))
  "Association list for key dispatch mapping.
When a character key is pressed, its corresponding key sequence is dispatched.
Example: pressing 'c' dispatches 'C-c', pressing 'm' dispatches 'M-' prefix."
  :type '(alist :key-type (character :tag "From key")
                :value-type (string :tag "To sequence")))

(defun meep-keypad (prompt)
  "Core function for handling keypad mapping.
Maps the leader key defined in `meep-keypad-leader-key' to `C-c' prefix key.
Behavior depends on current state:
- In insert mode or minibuffer: returns the leader key directly.
- In other modes: reads user input and dispatches key sequences based on `meep-keypad-dispatch'.
  Supports sequences ending with '-' (like 'M-') to combine with subsequent keys."
  (let* ((leader (aref (kbd meep-keypad-leader-key) 0))
         (default-prefix [?\C-c])
         (default-prefix-desc (key-description default-prefix)))
    (cond
     ((or (eq (meep-state) meep-state-insert)
          (bound-and-true-p isearch-mode)
          (minibufferp))
      ;; Return leader key directly in insert state or minibuffer
      (vector leader))
     ;; If leader key is pressed
     ((equal (this-command-keys-vector)
             (vector leader))
      (let* ((keys default-prefix)
             (which-key-this-command-keys-function
              (lambda () keys))
             (char (read-event default-prefix-desc))
             (val (alist-get char meep-keypad-dispatch))
             char2)
        (cond
         ;; Handle sequences ending with "-" (like "M-"), read second key and combine
         ((and val (string-suffix-p "-" val))
          (let* ((parts (split-string val " "))
                 (prefix-for-keys
                  (when (cdr parts)
                    (string-join (butlast parts) " "))))
            (setq keys (kbd prefix-for-keys)))
          (setq char2 (read-event (concat "keypad: " val)))
          (kbd (concat val (single-key-description char2))))
         ;; Direct match for sequences like "C-x"
         (val (kbd val))
         ;; Default: return Ctrl-C plus pressed key
         (t (vconcat default-prefix (vector char))))))
     ;; Other cases: return leader key
     (t (vector leader)))))


;; (defun meep-kbd (def)
;;   "Command that converts current key."
;;   `(lambda (&rest args)
;;      ;; Keys that apply only to a major mode aren't currently functioning, as =describe-key= is
;;      ;; executed when =meep-kbd= is called.
;;      ,(format "Execute the command which is bound to %s."
;;               (buttonize def 'describe-key (kbd def)))
;;      (interactive)
;;      (setq prefix-arg current-prefix-arg)
;;      (cond
;;       ( (string-suffix-p "-" ,def)
;;         (let ((ch (char-to-string(read-event ,def))))
;;           (setq unread-command-events (listify-key-sequence (kbd (concat ,def ch))))))
;;       (t
;;        (setq unread-command-events (listify-key-sequence (kbd ,def)))))))
;; (defun meep--search (reverse &optional match not-regexp
;;                              inhibit-lazy-highlight-and-count not-repeat)
;;   "Search for the next occurrence of MATCH using isearch.
;; If found, move point to the end of the occurrence,and return point.

;; REVERSE - If non-nil, the search direction is backward. Otherwise, it is forward.
;; MATCH - The string to search for, if nil `isearch-repeat' is called.
;; NOT-REGEXP - If non-nil, do a regular string search instead.
;; INHIBIT-LAZY-HIGHLIGHT-AND-COUNT - If non-nil disable lazy highlighting and
;; lazy counting features.
;; NOT-REPEAT - do not use `isearch-repeat' even MATCH equals to last `isearch-string'."
;;   (interactive "P")
;;   (when (and (string-equal match isearch-string)
;;              (equal isearch-regexp (not not-regexp))
;;              (not not-repeat))
;;     (setq match nil))
;;   (let ((inhibit-redisplay t)
;;         ;; we call `isearch-lazy-highlight-new-loop' at the end
;;         ;; of function,so set `isearch-lazy-count' and `isearch-lazy-highlight'
;;         ;; to nil is costless.
;;         (isearch-lazy-count nil)
;;         (isearch-lazy-highlight nil))
;;     (if (not match)
;;         ;; if MATCH is nil, just call `isearch-repeat'
;;         (isearch-repeat (if reverse 'backward 'forward))
;;       (if reverse
;;           (isearch-backward-regexp not-regexp t)
;;         (isearch-forward-regexp not-regexp t))
;;       (isearch-process-search-string
;;        match
;;        (mapconcat 'isearch-text-char-description match "")))
;;     (isearch-update)
;;     (isearch-done))
;;   ;; highlight after isearch-done
;;   ;; M-x:lazy-highlight-cleanup to cleanup highlight
;;   (when (and isearch-success
;;              (not inhibit-lazy-highlight-and-count))
;;     (isearch-lazy-highlight-new-loop))
;;   isearch-success)


(provide 'conf-meep)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-meep.el ends here.
