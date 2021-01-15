;;; Code:
;; C-u0 只限本函数内
;; C-u 恢复上次的iedit
(with-eval-after-load 'iedit
  (setq iedit-search-invisible nil)
  (add-hook 'iedit-mode-hook #'(lambda() (electric-pair-mode -1)))
  (add-hook 'iedit-mode-end-hook #'electric-pair-mode)
  (evil-define-minor-mode-key '(motion visual normal insert) 'iedit-mode (kbd "C-;")  'iedit--quit)
  (evil-define-minor-mode-key '(motion visual normal) 'iedit-mode "t"   'iedit-show/hide-occurrence-lines)

  (evil-define-minor-mode-key '(motion visual normal) 'iedit-mode "gg"  'iedit-goto-first-occurrence)
  (evil-define-minor-mode-key '(motion visual normal) 'iedit-mode "G"   'iedit-goto-last-occurrence)

  (evil-define-minor-mode-key '(motion visual normal) 'iedit-mode "n"   'iedit-next-occurrence)
  (evil-define-minor-mode-key '(motion visual normal) 'iedit-mode "N"   'iedit-prev-occurrence)
  (evil-define-minor-mode-key '(motion visual normal) 'iedit-mode (kbd "TAB") 'iedit-toggle-selection)
  (evil-define-minor-mode-key '(motion visual normal) 'iedit-mode [tab]       'iedit-toggle-selection)
  (evil-define-minor-mode-key '(motion visual normal) 'iedit-mode  (kbd "gU") 'iedit-upcase-occurrences)
  (evil-define-minor-mode-key '(motion visual normal) 'iedit-mode  (kbd "gu") 'iedit-downcase-occurrences)
  (evil-define-minor-mode-key '(motion visual normal) 'iedit-mode  (kbd "mf") 'iedit-restrict-function)
  (evil-define-minor-mode-key '(motion visual normal) 'iedit-mode  (kbd "ml") 'iedit-restrict-current-line)
  (evil-define-minor-mode-key '(motion visual normal) 'iedit-mode  (kbd "zc") 'iedit-toggle-case-sensitive)
  (evil-define-minor-mode-key '(motion visual normal) 'iedit-mode  (kbd "zb") 'iedit-toggle-buffering)
  (evil-define-minor-mode-key '(motion visual normal) 'iedit-mode  (kbd "zi") 'iedit-toggle-search-invisible)
  (evil-define-minor-mode-key '(motion visual normal) 'iedit-mode  (kbd "C－x r n") 'iedit-number-occurrences)
  (evil-normalize-keymaps) ;bug of evil-define-minor-mode-key https://github.com/emacs-evil/evil/issues/301
  )

(provide 'conf-iedit)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-iedit.el ends here.
