;;; Code:
(global-corfu-mode)
(corfu-history-mode)
(require 'kind-icon)
(setq kind-icon-default-face 'corfu-default)
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
(define-key corfu-map (kbd "SPC") #'corfu-insert-separator)
(define-key corfu-map (kbd "C-j") #'corfu-next)
(define-key corfu-map (kbd "C-k") #'corfu-previous)
(define-key corfu-map (kbd "M-.") #'corfu-first)
(define-key corfu-map (kbd "M-,") #'corfu-last)

(setq completion-category-overrides '((eglot (styles orderless))))

(setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
(setq corfu-auto t)                 ;; Enable auto completion
;; (setq corfu-separator ?\s)          ;; Orderless field separator
(setq corfu-quit-no-match 'separator) ;; or t
;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;; (corfu-preview-current nil)    ;; Disable current candidate preview
;; (corfu-preselect-first nil)    ;; Disable candidate preselection
;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
;; (corfu-scroll-margin 5)        ;; Use scroll margin
;; (setq completion-cycle-threshold 3)
;; Enable auto completion and configure quitting

;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
;; Corfu commands are hidden, since they are not supposed to be used via M-x.
(setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; (defun corfu-enable-in-minibuffer ()
;;   "Enable Corfu in the minibuffer if `completion-at-point' is bound."
;;   (when (where-is-internal #'completion-at-point (list (current-local-map)))
;;     ;; (setq-local corfu-auto nil) Enable/disable auto completion
;;     (corfu-mode 1)))

;; (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-symbol)

(global-set-key (kbd "C-.") #'completion-at-point)

(provide 'conf-corfu)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-corfu.el ends here.
