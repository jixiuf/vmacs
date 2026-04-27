;;; conf-helix.el --- Description -*- lexical-binding: t; -*-
;; (package-vc-install '(helix-mode :url "https://github.com/jixiuf/helix-mode.git" :branch "main"))
;;  (package-vc-install '(leader :url "https://github.com/jixiuf/leader.git" :branch "main"))
;;; Code:
(require 'helix)
(setq helix-major-mode-default-states
      '((rob-mode . insert)
        (ghostel-mode . insert)
        (calc-mode . insert)
        (special-mode . motion)
        (gud-mode . motion)
        (vc-dir-mode . motion)
        (diff-mode . motion)
        (term-mode . motion)
        (gnus-summary-mode . motion)
        (gnus-article-mode . motion)
        (gnus-group-mode . motion)
        (org-agenda-mode . motion)
        (inferior-emacs-lisp-mode . motion)
        (dired-mode . motion)
        (help-mode . motion)
        (info-mode . motion)))

(helix-define-key 'motion "j" #'helix-next-line)
(helix-define-key 'motion "k" #'helix-previous-line)
(helix-define-key 'motion "g" helix-goto-map)

(keymap-unset helix-goto-map "r" t)
(keymap-unset helix-normal-state-keymap "C-c" t)
(keymap-unset helix-normal-state-keymap "C-f" t)
(keymap-unset helix-normal-state-keymap "C-b" t)
(keymap-unset helix-normal-state-keymap "<SPC>" t)

(helix-define-key 'normal (kbd "C-2") #'helix-begin-selection)
(helix-define-key 'motion (kbd "C-2") #'helix-begin-selection)
(helix-define-key 'normal "R" #'helix-replace)
(helix-define-key 'normal "r" #'helix-replace-yank)
(helix-define-key 'normal "v" #'helix-backward-long-word)
(helix-define-key 'normal "e" #'helix-forward-long-word-start)
(helix-define-key 'normal "s" #'helix-select-line)
(helix-define-key 'normal "S" #'helix-select-line-up)
(helix-define-key 'normal "x" #'helix-kill-thing-at-point)
(helix-define-key 'normal "/" #'lazy-isearch-regexp-next)
(helix-define-key 'normal "?" #'lazy-isearch-regexp-prev)
(helix-define-key 'normal "n" #'lazy-isearch-repeat-next)
(helix-define-key 'normal "N" #'lazy-isearch-repeat-prev)
(helix-define-key 'normal "," #'backward-sexp)
(helix-define-key 'normal "." #'forward-sexp)
(helix-define-key 'normal (kbd "C-r") #'exchange-point-and-mark)



(defvar-keymap g-map
  "4" #'query-replace ;space g4
  "5" #'re-builder                                 ;;query-replace-regexp
  "g" #'vmacs-goto-line
  "T" #'consult-grep
  "r" #'revert-buffer
  "t" #'consult-ripgrep
  "e" #'grep
  "w" (vmacs-defun consult-ripgrep-default (consult-ripgrep default-directory))
  "x" (vmacs-defun consult-ripgrep-root-symbol (consult-ripgrep(vc-root-dir)  (concat "\\b" (thing-at-point 'symbol) "\\b")))
  "X" #'consult-ripgrep-root-symbol
  "s" (vmacs-defun consult-ripgrep-default-symbol (consult-ripgrep default-directory (concat "\\b" (thing-at-point 'symbol) "\\b")))
  "/" #'consult-focus-lines
  "z" #'consult-hide-lines
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
(set-keymap-parent helix-goto-map g-map)

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
  "m"         #'helix-begin-selection
  "z"         #'hs-toggle-hiding
  "q"         #'fill-paragraph
  "<return>"  #'fill-region)
(global-set-key (kbd "C-c m") helix-textobj-map)
(set-keymap-parent helix-textobj-inner-map m-map)


(defcustom meep-keypad-dispatch
  '((?h . "C-h")
    ;; (?r . "M-")
    ;; (?e . "C-M-")
    (?c . "C-c")
    (?x . "C-x"))
  "Association list for key dispatch mapping.
When a character key is pressed, its corresponding key sequence is dispatched.
Example: pressing 'c' dispatches 'C-c', pressing 'm' dispatches 'M-' prefix."
  :type '(alist :key-type (character :tag "From key")
                :value-type (string :tag "To sequence")))

(defun my-meep-keypad (_)
  "Core function for handling keypad mapping.
Maps the leader key to `C-c' prefix key by binding the leader key in `key-translation-map'.
(keymap-set key-translation-map \"<SPC>\" #\='meep-keypad)
Behavior depends on current state:
- In insert mode or minibuffer: returns the leader key directly.
- In other modes: reads user input and dispatches key sequences based on `meep-keypad-dispatch'.
  Supports sequences ending with `-' (like `M-') to combine with subsequent keys."
  (let* ((vkeys (this-command-keys-vector))
         (len (length vkeys))
         (leader (aref vkeys (1- len))) ;latest one
         (default-prefix "C-c"))
    (cond
     ((or (eq helix--current-state 'insert) (bound-and-true-p isearch-mode) (minibufferp))
      ;; Return leader key directly in insert state or minibuffer
      (vector leader))
     ;; If leader key is pressed
     ((= len 1)
      (let* ((keys default-prefix)
             (which-key-this-command-keys-function (lambda () (kbd keys)))
             (char (read-event keys))
             (val (alist-get char meep-keypad-dispatch))
             binding
             char2)
        (cond
         ;; Handle sequences ending with "-" (like "M-"), read second key and combine
         ((and val (string-suffix-p "-" val))
          (let* ((parts (split-string val " "))
                 (prefix
                  (when (cdr parts)
                    (string-join (butlast parts) " "))))
            (setq keys prefix))
          (setq char2 (read-event val))
          (setq keys (concat val (single-key-description char2))))
         ;; Direct match for sequences like "C-x"
         (val
          (setq keys (concat val)))
         ;; Default: return C-c plus pressed key
         (t
          (setq keys (concat default-prefix " " (single-key-description char)))))
        (setq binding (key-binding (kbd keys)))

        (while (not (or (commandp binding t) (null binding)))
          (setq char (read-event keys))
          (setq val (alist-get char meep-keypad-dispatch))
          (cond
           ;; Handle sequences ending with "-" (like "M-"), read second key and combine
           ((and val (string-suffix-p "-" val))
            (let* ((parts (split-string val " "))
                   (prefix
                    (when (cdr parts)
                      (string-join (butlast parts) " "))))
              (setq keys (concat keys " " prefix))
              (setq char2 (read-event (concat keys (car (last parts))))))
            (setq keys (concat keys " " val (single-key-description char2))))
           ;; Direct match for sequences like "C-x" in meep-keypad-dispatch
           (val
            (setq keys (concat keys " " val)))
           ((eq leader char) ;leader+x+leader+f = C-x f
            (setq keys (concat keys " " (single-key-description (read-event keys)))))
           ;; try C-char first (leader+x+f = C-x C-f)
           ((key-binding (kbd (concat keys " C-" (single-key-description char))))
            (setq keys (concat keys " C-" (single-key-description char))))
           (t
            (setq keys (concat keys " " (single-key-description char)))))
          (setq binding (key-binding (kbd keys))))
        (kbd keys)))
     ;; Other cases: return leader key
     (t
      (vector leader)))))

;; (keymap-set key-translation-map "<SPC>" 'my-meep-keypad)


(defmacro helix-set-keymap-parent (map-or-mode &optional parent)
  "Set the parent keymap for MAP-OR-MODE to PARENT.

MAP-OR-MODE can be a map or a mode
Returns PARENT. PARENT should be nil or another keymap.
Default is 'helix-normal-state-keymap' when PARENT is nil."
  `(let ((map (if (keymapp ,map-or-mode)
                  ,map-or-mode
                (let ((map-name (intern (concat (symbol-name ,map-or-mode) "-map"))))
                  (when (boundp map-name)
                    (symbol-value map-name))))))
     (when (keymapp map)
       (set-keymap-parent map (make-composed-keymap (keymap-parent map)
                                                    (or ,parent helix-normal-state-keymap))))))
;; (keymap-unset occur-mode-map "l" t)

(defvar helix-motion-parent-keymaps (make-hash-table :test #'equal))
(defun helix-motion-set-keymap-parent()
  (unless (member major-mode '(special-mode))
    (when (and (equal helix--current-state 'motion)
               (not (gethash major-mode helix-motion-parent-keymaps)))
      (puthash major-mode t helix-motion-parent-keymaps)
      (helix-set-keymap-parent major-mode helix-normal-state-keymap))))

(add-hook 'helix-motion-mode-hook #'helix-motion-set-keymap-parent)

(helix-mode)

(require 'leader)
(add-to-list 'leader-pass-through-predicates
             (lambda () (eq helix--current-state 'insert)))
(leader-mode 1) 

(provide 'conf-helix)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-helix.el ends here.
