;; (eval-when-compile (require 'company))


(with-eval-after-load 'company
  ;; 调整默认backends
  (setq-default company-backends
                `(company-elisp
                  (company-files company-dabbrev company-keywords company-yasnippet)
                company-nxml
                company-css
                company-clang
                company-xcode
                company-cmake
                company-eclim
                company-semantic
                company-capf
                (
                 ;; company-dabbrev-code
                 company-gtags
                 company-etags



                 )
                ;; company-oddmuse ;
                ;; company-bbdb
                ))


  (setq-default company-idle-delay 0.3)
  (setq company-echo-delay 0)
  (setq company-tooltip-minimum-width 50)
  (setq company-search-regexp-function 'company-search-flex-regexp)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)

  (setq-default company-minimum-prefix-length 2)
  (add-to-list 'company-begin-commands  'backward-delete-char-untabify)
  (add-to-list 'company-begin-commands  'backward-kill-word)
  (setq company-require-match nil)      ;不为nil的话，则如果输入的内容导致无匹配的选项，则不允许此输入
  ;; (define-key company-mode-map (kbd "C-i") 'company-other-backend) ;iterm map to C-i
  ;; (define-key company-mode-map (kbd "C-[ [ a h") (key-binding (kbd "C-i"))) ;iterm map to C-i
  (define-key company-active-map (kbd "C-e") #'company-other-backend)
  (define-key company-active-map (kbd "C-s") #'company-filter-candidates)
  (define-key company-active-map (kbd "M-s") #'company-search-candidates)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map [tab] 'vmacs-company-complete-common-or-selection)
  (define-key company-active-map (kbd "TAB") 'vmacs-company-complete-common-or-selection)
  ;; (define-key company-active-map [return]  'vmacs-company-complete-common-or-selection)
  (define-key company-active-map (kbd "C-j")  'company-complete-selection)
  (define-key company-active-map [(meta tab)] 'company-complete-common)
  (define-key company-active-map (kbd "<C-m>") 'company-complete-selection)
  (define-key company-active-map (kbd "C-[ [ a m")  'company-complete-selection)
  (define-key company-active-map  (kbd "<return>") nil)
  (define-key company-active-map  (kbd "RET") nil)

  ;; (define-key company-mode-map (kbd "C-:") 'helm-company)
  ;; (define-key company-active-map (kbd "C-:") 'helm-company)
  )
(make-variable-buffer-local 'company-backends)
;; make evil repeat . work with company

(when (fboundp 'evil-declare-change-repeat)
  (mapc #'evil-declare-change-repeat
        '(vmacs-company-complete-common-or-selection)))

(global-company-mode 1)
;; (add-hook 'after-init-hook (lambda() (global-company-mode 1)))

(defun vmacs-company-complete-common-or-selection()
  (interactive)
  (call-interactively 'company-complete-common)
  (setq this-command 'company-complete-common)
  (when (equal company-prefix company-common)
  (setq this-command 'company-complete-selection)
    (call-interactively 'company-complete-selection)))

(provide 'conf-company-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-company-mode.el ends here.
