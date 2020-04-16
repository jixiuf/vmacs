;;; multi-substring-complete.el --- Completion style for matching regexps in any order  -*- lexical-binding: t; -*-
;; https://github.com/oantolin/orderless
(require 'cl-lib)

(defgroup orderless nil
  "Completion method that matches space-separated regexps in any order."
  :group 'completion)

(defface orderless-match-face-0
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#72a4ff")
    (((class color) (min-colors 88) (background light)) :foreground "#223fbf")
    (t :foreground "blue"))
  "Face for matches of components numbered 0 mod 4."
  :group 'orderless)

(defface orderless-match-face-1
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#ed92f8")
    (((class color) (min-colors 88) (background light)) :foreground "#8f0075")
    (t :foreground "magenta"))
  "Face for matches of components numbered 1 mod 4."
  :group 'orderless)

(defface orderless-match-face-2
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#90d800")
    (((class color) (min-colors 88) (background light)) :foreground "#145a00")
    (t :foreground "green"))
  "Face for matches of components numbered 2 mod 4."
  :group 'orderless)

(defface orderless-match-face-3
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#f0ce43")
    (((class color) (min-colors 88) (background light)) :foreground "#804000")
    (t :foreground "yellow"))
  "Face for matches of components numbered 3 mod 4."
  :group 'orderless)

(defcustom orderless-regexp-separator " +"
  "Regexp to match component separators for orderless completion.
This is passed to `split-string' to divide the pattern into
component regexps."
  :type '(choice (const :tag "Spaces" " +")
                 (const :tag "Spaces, hyphen or slash" " +\\|[-/]")
                 (regexp :tag "Custom regexp"))
  :group 'orderless)

(let ((faces [orderless-match-face-0
              orderless-match-face-1
              orderless-match-face-2
              orderless-match-face-3]))
  (defun orderless--highlight-match (regexp string face)
    ;; only call this when the match has already been checked!
    (string-match regexp string)
    (font-lock-prepend-text-property
     (match-beginning 0)
     (match-end 0)
     'face (aref faces (mod face 4))
     string)))

(defun orderless-all-completions (string table pred _point)
  (save-match-data
    (let* ((limit (car (completion-boundaries string table pred "")))
           (prefix (substring string 0 limit))
           (all (all-completions prefix table pred))
           (regexps (split-string (substring string limit)
                                  orderless-regexp-separator
                                  t)))
      (when minibuffer-completing-file-name
        (setq all (completion-pcm--filename-try-filter all)))
      (condition-case nil
          (progn
            (setq all
                  (cl-loop for original in all
                           when
                           (cl-loop for regexp in regexps
                                    always (string-match-p regexp original))
                           collect      ; it's a match, copy and highlight
                           (cl-loop with candidate = (copy-sequence original)
                                    for regexp in regexps and face from 0 do
                                    (orderless--highlight-match
                                     regexp candidate face)
                                    finally (return candidate))))
            (when all (nconc all (length prefix))))
        (invalid-regexp nil)))))

(defun orderless-try-completion (string table pred point &optional _metadata)
  (let* ((limit (car (completion-boundaries string table pred "")))
         (prefix (substring string 0 limit))
         (all (orderless-all-completions string table pred point)))
    (cl-flet ((measured (string) (cons string (length string))))
      (cond
       ((null all) nil)
       ((atom (cdr all)) (measured (concat prefix (car all))))
       (t (measured string))))))

(cl-pushnew '(orderless
              orderless-try-completion orderless-all-completions
              "Completion of multiple regexps, in any order.")
            completion-styles-alist
            :test #'equal)
(provide 'multi-substring-complete)

;; Local Variables:
;; coding: utf-8
;; End:

;;; multisubstring-complete.el ends here.
