;; (eval-when-compile (require 'company))

(with-eval-after-load 'company
  ;; 调整默认backends
  (setq-default company-backends `(,@(unless (version< "24.3.51" emacs-version)
                                       (list 'company-elisp))
                                   company-files
                                   company-nxml
                                   company-css
                                   company-clang
                                   company-xcode
                                   company-cmake
                                   company-eclim
                                   company-semantic
                                   company-capf
                                   (company-dabbrev-code company-gtags company-etags
                                                         company-keywords)
                                        company-dabbrev
                                        company-oddmuse ;
                                        company-bbdb))

  (setq-default company-idle-delay 0.08)
  (setq-default company-minimum-prefix-length 2)
  ;; (define-key company-mode-map (kbd "C-i") 'company-other-backend) ;iterm map to C-i
  ;; (define-key company-mode-map (kbd "C-[ [ a h") (key-binding (kbd "C-i"))) ;iterm map to C-i
  (define-key company-active-map (kbd "C-e") #'company-other-backend)
  (define-key company-active-map (kbd "C-s") #'company-filter-candidates)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  ;; (define-key company-mode-map (kbd "C-:") 'helm-company)
  ;; (define-key company-active-map (kbd "C-:") 'helm-company)
  )
(global-company-mode 1)
;; (add-hook 'after-init-hook (lambda() (global-company-mode 1)))

(provide 'conf-company-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-company-mode.el ends here.
