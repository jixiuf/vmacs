;;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require  'ediff)
  (require  'vc-hooks)
  (require  'log-edit)
  (require  'org)
  (require  'term)
  ;; (require  'helm)
  (require  'ibuffer)
  (require  'log-view)
  (require 'cc-mode)
  ;; (require 'hippie-exp)
  )

;; (require 'clang-format)

(declare-function novel-fill "lazy-novel-mode")
(defun term-complete-tab()
  (interactive)
  ;; (require 'term)
  (term-send-raw-string "\t")
  )

(defvar smart-tab-completion-functions
  '(
    (emacs-lisp-mode company-complete)
    ;; (python-mode jedi:complete)
    (Custom-mode  widget-forward)
    (magit-status-mode magit-section-toggle)
    (magit-mode magit-section-toggle)
    (magit-process-mode magit-section-toggle)
    (term-mode term-complete-tab)
    (epa-key-list-mode widget-forward)
    (minibuffer-local-completion minibuffer-complete)
    (minibuffer-local-map minibuffer-complete)
    (org-mode org-cycle)
    (Info-mode Info-next-reference)
    (novel-mode org-cycle)
    (text-mode novel-fill)
    (help-mode forward-button)
    (nxml-mode nxml-complete)
    (json-mode vmacs-json-pretty)
    (objc-mode company-complete)
    (go-mode company-complete))
  "List of major modes in which to use a mode specific completion
  function.")

(defvar smart-tab-mode-for-indent-tab-mode
  '(applescript-mode))

(defun get-completion-function()
  "Get a completion function according to current major mode."
  (let ((completion-function
         (cadr (assq major-mode smart-tab-completion-functions))))
    (if (null completion-function)
        'hippie-expand

      completion-function)))


;;;###autoload
(defun smart-tab (&optional _arg)
  (interactive "P")
  ;;根据buffer内容自动设置major-mode
  (when (equal "*scratch*" (buffer-name)) (set-auto-mode))
  (cond
   ;; (looking-at "\\_>") at end of symbol
   ;; (looking-at "\\>") at end of word
   ((minibufferp)
    (call-interactively 'minibuffer-complete))
   ((and (not buffer-read-only)
         (not (member major-mode '(term-mode org-mode json-mode novel-mode Custom-mode Info-mode text-mode)))
         (or (looking-back "^[ \t]*" (point-at-bol))        ;at bol 在行首 不适合补全
             (looking-back (string-trim (concat comment-start ".*")) (point-at-bol)) ;前面是注释 不适合补全
             (and (looking-at "[ \t]*$")        ;at eol
                  (or (looking-back "[,;)}]" (point-at-bol))   ;前面是,;)} 几种符号 ，则不适全补全
                      (looking-back "]" (point-at-bol))        ;前面是] 也不适全补全
                      ))))
    (if (and mark-active)
        (if (member major-mode '(c-mode c++-mode))
            (progn
              (call-interactively #'eglot-format)
              (deactivate-mark))
          (indent-region (region-beginning) (region-end)))
      (if (or indent-tabs-mode
              (or (member major-mode smart-tab-mode-for-indent-tab-mode)))
          (call-interactively 'indent-for-tab-command)
        (call-interactively 'indent-according-to-mode))))
   (t
    ;; Hippie also expands yasnippets, due to `yas-hippie-try-expand' in
    ;; `hippie-expand-try-functions-list'.
    (call-interactively (get-completion-function))
    )))

(provide 'lazy-smart-tab)
;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-smart-tab.el ends here.
