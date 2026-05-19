;;; conf-lxel.el --- Description -*- lexical-binding: t; -*-
;; (package-vc-install '(helixel-mode :url "https://github.com/jixiuf/helixel-mode.git" :branch "main"))
;;  (package-vc-install '(keypad :url "https://github.com/jixiuf/emacs-keypad.git" :branch "main"))
;;; Code:
(require 'helixel)
(setq helixel-replace-yanked-delete-char-p nil)
(setq helixel-major-mode-default-states
      (append helixel-major-mode-default-states
      '((reb-mode . insert)
        ;; (pi-coding-agent-input-mode . insert)
        (ghostel-mode . insert)
        (calc-mode . insert))))

(helixel-define-key 'motion "j" #'helixel-next-line)
(helixel-define-key 'motion "k" #'helixel-previous-line)
(helixel-define-key 'motion "g" helixel-goto-map)

(keymap-unset helixel-goto-map "r" t)
(keymap-unset helixel-normal-map "C-c" t)
(keymap-unset helixel-normal-map "C-f" t)
(keymap-unset helixel-normal-map "C-b" t)
(keymap-unset helixel-normal-map  "C-v" t)
(keymap-unset helixel-normal-map "C-w" t)
(keymap-unset helixel-normal-map "<SPC>" t)
(keymap-unset helixel-normal-map "C-i" t)
(keymap-unset helixel-normal-map "C-o" t)
(global-set-key (kbd "C-c ,") #'helixel-jump-backward)
(global-set-key (kbd "C-c .") #'helixel-jump-forward)

(helixel-define-key 'normal (kbd "<f8>") #'repeat)
(helixel-define-key 'normal (kbd "C-2") #'helixel-begin-selection)
(helixel-define-key 'motion (kbd "C-2") #'helixel-begin-selection)
(helixel-define-key 'normal (kbd "M-y") #'helixel-replace-pop)
;; (helixel-define-key 'normal "v" #'helixel-backward-long-word)
;; (helixel-define-key 'normal "e" #'helixel-forward-long-word-start)
;; (helixel-define-key 'normal "s" #'helixel-select-line)
;; (helixel-define-key 'normal "S" #'helixel-select-line-up)
(helixel-define-key 'normal "G" #'end-of-buffer)
;; (helixel-define-key 'normal "," #'backward-sexp)
;; (helixel-define-key 'normal "." #'forward-sexp)
(helixel-define-key 'normal (kbd "C-r") #'exchange-point-and-mark)
(global-set-key (kbd "C-3") 'helixel-search-at-point-prev)
(global-set-key (kbd "C-8") 'helixel-search-at-point-next)



(define-key helixel-goto-map "4" #'query-replace)
(define-key helixel-goto-map "5" #'re-builder)
(define-key helixel-goto-map "T" #'consult-grep)
(define-key helixel-goto-map "r" #'revert-buffer)
(define-key helixel-goto-map "t" #'consult-ripgrep)
(define-key helixel-goto-map "w" (vmacs-defun consult-ripgrep-default (consult-ripgrep default-directory)))
(define-key helixel-goto-map "x" (vmacs-defun consult-ripgrep-root-symbol (consult-ripgrep(vc-root-dir)  (concat "\\b" (thing-at-point 'symbol) "\\b"))))
(define-key helixel-goto-map "X" #'consult-ripgrep-root-symbol)
(define-key helixel-goto-map "s" (vmacs-defun consult-ripgrep-default-symbol (consult-ripgrep default-directory (concat "\\b" (thing-at-point 'symbol) "\\b"))))
(define-key helixel-goto-map "/" #'consult-focus-lines)
(define-key helixel-goto-map "z" #'consult-hide-lines)
(define-key helixel-goto-map "b" #'pop-global-mark)
(define-key helixel-goto-map "u" #'upcase-dwim)
(define-key helixel-goto-map "U" #'downcase-dwim)
(define-key helixel-goto-map "m" #'push-mark-command)
(define-key helixel-goto-map "P" #'project-or-external-find-file)
(define-key helixel-goto-map "," #'goto-last-change)
(define-key helixel-goto-map "." #'goto-last-change-reverse)
(define-key helixel-goto-map "f" #'gptel-rewrite)

(global-set-key (kbd "C-c g") helixel-goto-map)

(defvar-keymap m-map
  "$"         #'toggle-truncate-lines
  "n"         #'narrow-to-region
  "r"         #'revert-buffer
  "."         #'widen
  "d"         #'narrow-to-defun
  "f"         #'mark-defun
  "e"         #'gt-translate
  ","         #'pop-to-mark-command
  "t"         #'org-capture
  "m"         #'helixel-begin-selection
  "v"         #'helixel-select-rectangle
  "z"         #'hs-toggle-hiding
  "q"         #'fill-paragraph
  "<return>"  #'fill-region)
(global-set-key (kbd "C-c m") helixel-textobj-map)
(set-keymap-parent helixel-textobj-inner-map m-map)

(defmacro helixel-set-keymap-parent (map-or-mode &optional parent)
  "Set the parent keymap for MAP-OR-MODE to PARENT.

MAP-OR-MODE can be a map or a mode
Returns PARENT. PARENT should be nil or another keymap.
Default is 'helixel-normal-map' when PARENT is nil."
  `(let ((map (if (keymapp ,map-or-mode)
                  ,map-or-mode
                (let ((map-name (intern (concat (symbol-name ,map-or-mode) "-map"))))
                  (when (boundp map-name)
                    (symbol-value map-name))))))
     (when (keymapp map)
       (set-keymap-parent map (make-composed-keymap (keymap-parent map)
                                                    (or ,parent helixel-normal-map))))))
;; (keymap-unset occur-mode-map "l" t)

(defvar helixel-motion-parent-keymaps (make-hash-table :test #'equal))
(defun helixel-motion-set-keymap-parent()
  (unless (member major-mode '(special-mode dired-mode wdired-mode Custom-mode))
    (when (and (equal helixel--current-state 'motion)
               (not (gethash major-mode helixel-motion-parent-keymaps)))
      (puthash major-mode t helixel-motion-parent-keymaps)
      (helixel-set-keymap-parent major-mode helixel-normal-map))))

(add-hook 'helixel-motion-state-hook #'helixel-motion-set-keymap-parent)

(helixel-mode)

(require 'keypad)
(require 'keypad-which-key)
(add-to-list 'keypad-pass-through-predicates
             (lambda () (eq helixel--current-state 'insert)))

(setq keypad-keys
      '((:key "<SPC>" :prefix "C-c" :modifier "" :fallback "C-"
              :dispatch ((?x . (:prefix "C-x" :modifier "C-" :fallback "C-"))
                         (?h . (:prefix "C-h" :modifier nil  :fallback "C-"))
                         (?s . (:prefix "M-s" :modifier nil  :fallback "M-"))))
        ;; (:key "," :prefix "" :modifier "M-" :fallback nil)
        (:key "s" :prefix "" :modifier "C-M-" :fallback nil
              :pass-through-predicates (minibufferp
                                        isearch-mode
                                        vc-dir-mode dired-mode
                                        package-menu-mode
                                        ibuffer-mode
                                        vc-annotate-mode
                                        vc-git-log-view-mode
                                        (lambda () (eq helixel--current-state 'insert))))))

;; (setq keypad-dispatch-priority t)
(setq keypad-toggle-priority t)
(keypad-mode 1)

(define-thing-chars gopkg "-/[:alnum:]_.@:*")
(put 'gopkg 'forward-op
     (lambda (&optional count)
       (helixel-forward-chars "-/[:alnum:]_.@:*" count)))
(helixel-define-mark-object "gopkg" 'gopkg "gopkg" 'gopkg t)
(define-key helixel-textobj-outer-map "y" #'helixel-mark-a-gopkg)
(define-key helixel-textobj-inner-map "y" #'helixel-mark-inner-gopkg)

(define-key helixel-textobj-outer-map ";" #'helixel-mark-a-double-quote)
(define-key helixel-textobj-inner-map ";" #'helixel-mark-inner-double-quote)
;; (define-key evil-outer-text-objects-map "f" (helixel-get-tree-sitter-textobj "function.outer"))
;; (define-key evil-inner-text-objects-map "f" (helixel-get-tree-sitter-textobj "function.inner"))

(define-key helixel-textobj-inner-map "q" (helixel-get-tree-sitter-textobj '("parameter.inner" "assignment.inner" )))
(define-key helixel-textobj-outer-map "q" (helixel-get-tree-sitter-textobj '("parameter.outer"  "assignment.outer")))
(define-key helixel-textobj-outer-map "x" (helixel-get-tree-sitter-textobj '("call.outer" "statement.outer" "block.outer" "loop.outer" "conditional.outer")))
(define-key helixel-textobj-inner-map "x" (helixel-get-tree-sitter-textobj '("call.inner" "statement.inner" "block.inner" "loop.inner" "conditional.inner" )))
(define-key helixel-textobj-outer-map "/" (helixel-get-tree-sitter-textobj "comment.outer" ))
(define-key helixel-textobj-inner-map "/" (helixel-get-tree-sitter-textobj "comment.inner" ))
(provide 'conf-helixel)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-helixel.el ends here.
