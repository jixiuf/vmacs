;;; conf-lxel.el --- Description -*- lexical-binding: t; -*-
;; (package-vc-install '(helixel-mode :url "https://github.com/jixiuf/helixel-mode.git" :branch "main"))
;;  (package-vc-install '(leadkey :url "https://github.com/jixiuf/emacs-leadkey.git" :branch "main"))
;;; Code:
 (when (file-directory-p "~/vmacs/config/emacs/elpa/helixel")
   (add-to-list 'load-path "~/vmacs/config/emacs/elpa/helixel"))

(require 'helixel)
(helixel-treesit-setup)
(setq helixel-default-register ?`)
(setq helixel-register-yank-char ?y)
(setq helixel-register-small-delete-char ?x)
(setq helixel-register-delete-registers '(?d ?f ?g))
(setq helixel-register-backends
  '((?` . kill-ring)
    (?c . clipboard)
    (?s . primary)))
(setq helixel-search-use-region t)
(setq helixel-replace-delete-char-p nil)

;; (add-to-list  'helixel-major-mode-default-states '(reb-mode . insert))

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



(global-set-key (kbd "C-c /") #'helixel-comment-toggle)
;; (helixel-define-key 'normal (kbd "C-c /") #'helixel-comment-toggle)
(helixel-define-key 'normal (kbd "C-h") #'negative-argument)
(helixel-define-key 'normal (kbd "`") #'helixel-select-register)
(helixel-define-key 'normal (kbd "RET") #'helixel-select-register)
;; (helixel-define-key 'normal (kbd "<return>") #'helixel-select-register )
(helixel-define-key 'normal (kbd "<f8>") #'repeat)
(helixel-define-key 'normal (kbd "C-2") #'helixel-begin-selection)
;; (helixel-define-key 'motion (kbd "C-2") #'helixel-begin-selection)
(helixel-define-key 'normal "G" #'helixel-go-end-buffer)
(helixel-define-key 'normal (kbd "C-r") #'helixel-jump-to-match)
(helixel-define-key 'normal "q" helixel-left-map)
(helixel-define-key 'normal "Q" helixel-inner-left-map)
(helixel-define-key 'normal "v" helixel-right-map)
(helixel-define-key 'normal "V" helixel-inner-right-map)
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
;; (define-key helixel-goto-map "u" #'upcase-dwim)
;; (define-key helixel-goto-map "U" #'downcase-dwim)
(define-key helixel-goto-map "m" #'push-mark-command)
(define-key helixel-goto-map "P" #'project-or-external-find-file)
(define-key helixel-goto-map "," #'goto-last-change)
;; (define-key helixel-goto-map "." #'goto-last-change-reverse)
(define-key helixel-goto-map "f" #'gptel-rewrite)
;; (define-key helixel-goto-map ";" #'goto-line)
;; (define-key helixel-goto-map ":" #'goto-char)

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

;; (keymap-unset occur-mode-map "l" t)

(helixel-mode)
(helixel-mc-mark-all-for-real-cursor-only '(vmacs-other-window))

(require 'leadkey)
(require 'leadkey-which-key)
(add-to-list 'leadkey-pass-through-predicates #'helixel-insert-state-p)
(global-unset-key (kbd "C-x C-SPC"))
(global-unset-key (kbd "C-x SPC"))

(setq leadkey-keys
      '((:key "<SPC>" :prefix "C-c" :modifier "" :fallback "C-"
              :dispatch ((?x . (:prefix "C-x" :modifier "C-" :fallback nil
                             :dispatch ((?\s . :toggle))))
                         (?h . (:prefix "<f1>" :modifier nil  :fallback "C-"))
                         (?s . (:prefix "M-s" :modifier nil  :fallback "M-"))))
        ;; (:key "," :prefix "" :modifier "M-" :fallback nil)
        ;; (:key "s" :prefix "" :modifier "C-M-" :fallback nil
        ;;       :pass-through-predicates (minibufferp
        ;;                                 isearch-mode
        ;;                                 vc-dir-mode dired-mode
        ;;                                 package-menu-mode
        ;;                                 ibuffer-mode
        ;;                                 vc-annotate-mode
        ;;                                 vc-git-log-view-mode
        ;;                                 (lambda () (helixel-insert-state-p))))
        ))

;; (setq keypad-dispatch-priority t)
(setq leadkey-toggle-priority t)
(leadkey-mode 1)

(define-thing-chars gopkg "-/[:alnum:]_.@:*")
(put 'gopkg 'forward-op
     (lambda (&optional count)
       (helixel-forward-chars "-/[:alnum:]_.@:*" count)))
(helixel-define-mark-object "gopkg" 'gopkg "gopkg" 'gopkg t)
(define-key helixel-textobj-outer-map "u" #'helixel-mark-a-gopkg)
(define-key helixel-textobj-inner-map "u" #'helixel-mark-inner-gopkg)

(define-key helixel-textobj-outer-map "g" #'helixel-mark-a-double-quote)
(define-key helixel-textobj-inner-map "g" #'helixel-mark-inner-double-quote)
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
