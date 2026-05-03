;;; conf-lxel.el --- Description -*- lexical-binding: t; -*-
;; (package-vc-install '(hxel-mode :url "https://github.com/jixiuf/hxel-mode.git" :branch "main"))
;;  (package-vc-install '(keypad :url "https://github.com/jixiuf/emacs-keypad.git" :branch "main"))
;;; Code:
(require 'hxel)
(setq hxel-replace-yanked-delete-char-p nil)
(setq hxel-major-mode-default-states
      '((reb-mode . insert)
        (ghostel-mode . insert)
        (calc-mode . insert)))

(hxel-define-key 'motion "j" #'hxel-next-line)
(hxel-define-key 'motion "k" #'hxel-previous-line)
(hxel-define-key 'motion "g" hxel-goto-map)

(keymap-unset hxel-goto-map "r" t)
(keymap-unset hxel-normal-state-keymap "C-c" t)
(keymap-unset hxel-normal-state-keymap "C-f" t)
(keymap-unset hxel-normal-state-keymap "C-b" t)
(keymap-unset hxel-normal-state-keymap  "C-v" t)
(keymap-unset hxel-normal-state-keymap "C-w" t)
(keymap-unset hxel-normal-state-keymap "<SPC>" t)

(hxel-define-key 'normal (kbd "<f8>") #'repeat)
(hxel-define-key 'normal (kbd "C-2") #'hxel-begin-selection)
(hxel-define-key 'motion (kbd "C-2") #'hxel-begin-selection)
(hxel-define-key 'normal "R" #'hxel-replace)
(hxel-define-key 'normal "r" #'hxel-replace-yanked)
(hxel-define-key 'normal "v" #'hxel-backward-long-word)
(hxel-define-key 'normal "e" #'hxel-forward-long-word-start)
(hxel-define-key 'normal "s" #'hxel-select-line)
(hxel-define-key 'normal "S" #'hxel-select-line-up)
(hxel-define-key 'normal "x" #'hxel-kill-thing-at-point)
(hxel-define-key 'normal "G" #'end-of-buffer)
(hxel-define-key 'normal "," #'backward-sexp)
(hxel-define-key 'normal "." #'forward-sexp)
(hxel-define-key 'normal (kbd "C-r") #'exchange-point-and-mark)
(global-set-key (kbd "C-3") 'hxel-search-at-point-prev)
(global-set-key (kbd "C-8") 'hxel-search-at-point-next)



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
(set-keymap-parent hxel-goto-map g-map)

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
  "m"         #'hxel-begin-selection
  "v"         #'hxel-select-rectangle
  "z"         #'hs-toggle-hiding
  "q"         #'fill-paragraph
  "<return>"  #'fill-region)
(global-set-key (kbd "C-c m") hxel-textobj-map)
(set-keymap-parent hxel-textobj-inner-map m-map)


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
     ((or (eq hxel--current-state 'insert) (bound-and-true-p isearch-mode) (minibufferp))
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


(defmacro hxel-set-keymap-parent (map-or-mode &optional parent)
  "Set the parent keymap for MAP-OR-MODE to PARENT.

MAP-OR-MODE can be a map or a mode
Returns PARENT. PARENT should be nil or another keymap.
Default is 'hxel-normal-state-keymap' when PARENT is nil."
  `(let ((map (if (keymapp ,map-or-mode)
                  ,map-or-mode
                (let ((map-name (intern (concat (symbol-name ,map-or-mode) "-map"))))
                  (when (boundp map-name)
                    (symbol-value map-name))))))
     (when (keymapp map)
       (set-keymap-parent map (make-composed-keymap (keymap-parent map)
                                                    (or ,parent hxel-normal-state-keymap))))))
;; (keymap-unset occur-mode-map "l" t)

(defvar hxel-motion-parent-keymaps (make-hash-table :test #'equal))
(defun hxel-motion-set-keymap-parent()
  (unless (member major-mode '(special-mode dired-mode wdired-mode))
    (when (and (equal hxel--current-state 'motion)
               (not (gethash major-mode hxel-motion-parent-keymaps)))
      (puthash major-mode t hxel-motion-parent-keymaps)
      (hxel-set-keymap-parent major-mode hxel-normal-state-keymap))))

(add-hook 'hxel-motion-mode-hook #'hxel-motion-set-keymap-parent)

(hxel-mode)

(require 'keypad)
(require 'keypad-which-key)
(add-to-list 'keypad-pass-through-predicates
             (lambda () (eq hxel--current-state 'insert)))

(setq keypad-keys
      '((:key "<SPC>" :prefix "C-c" :modifier "" :fallback "C-"
              :dispatch ((?x . (:prefix "C-x" :modifier "C-" :fallback "C-"))
                         (?h . (:prefix "C-h" :modifier nil  :fallback "C-"))
                         (?s . (:prefix "M-s" :modifier nil  :fallback "M-"))
                         (?g . (:prefix "M-g" :modifier nil  :fallback "M-"))
                         (?m . (:prefix  nil  :modifier "M-" :fallback  nil))))
        (:key "," :prefix "" :modifier "M-" :fallback nil)
        (:key "x" :prefix "" :modifier "C-M-" :fallback nil
              :pass-through-predicates (minibufferp isearch-mode
                                        vc-dir-mode dired-mode
                                        (lambda () (eq hxel--current-state 'insert))
                                        vc-annotate-mode))))

;; (setq keypad-dispatch-priority t)
(setq keypad-toggle-priority t)
(keypad-mode 1)

(provide 'conf-hxel)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-hxel.el ends here.
