(eval-when-compile
  (require  'ediff)
  (require  'vc-hooks)
  (require  'log-edit)
  (require  'org)
  (require  'term)
  (require  'helm)
  (require  'ibuffer)
  (require  'log-view)
  (require 'cc-mode)
  (require 'hippie-exp))

(declare-function novel-fill "lazy-novel-mode")
(defun term-complete-tab()
  (interactive)
  ;; (require 'term)
  (term-send-raw-string "\t")
  )
(defvar smart-tab-completion-functions
  '((emacs-lisp-mode helm-lisp-completion-at-point)
    (python-mode jedi:complete)
    (magit-status-mode magit-section-toggle)
    (magit-mode magit-section-toggle)
    (magit-process-mode magit-section-toggle)
    (term-mode term-complete-tab)
    (epa-key-list-mode widget-forward)
    ;; (org-mode novel-fill)
    (novel-mode novel-fill)
    (text-mode novel-fill)
    (help-mode forward-button)
    (nxml-mode nxml-complete)
    (objc-mode company-complete)
    (go-mode auto-complete))
  "List of major modes in which to use a mode specific completion
  function.")

(defvar smart-tab-mode-for-indent-tab-mode
  '(applescript-mode))

(defun get-completion-function()
  "Get a completion function according to current major mode."
  (let ((completion-function
         (second (assq major-mode smart-tab-completion-functions))))
    (if (null completion-function)
        'hippie-expand

      completion-function)))


;;;###autoload
(defun smart-tab (&optional arg)
  (interactive "P")
  (cond
   ;; (looking-at "\\_>") at end of symbol
   ;; (looking-at "\\>") at end of word
   ((and (not buffer-read-only)
         (not (member major-mode '(term-mode org-mode novel-mode text-mode)))
         (or (looking-back "^[ \t]*" (point-at-bol))        ;at bol 在行首 不适合补全
             (looking-back (string-trim (concat comment-start ".*")) (point-at-bol)) ;前面是注释 不适合补全
             (and (looking-at "[ \t]*$")        ;at eol
                  (or (looking-back "[,;)}]" (point-at-bol))   ;前面是,;)} 几种符号 ，则不适全补全
                      (looking-back "]" (point-at-bol))        ;前面是] 也不适全补全
                      ))))
    (if (and mark-active )
        (indent-region (region-beginning) (region-end))

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
