;;; Code:
(setq-default iedit-toggle-key-default nil)
(global-set-key (kbd "C-;") 'evil-iedit-state/iedit-mode)
(require 'evil-iedit-state)
(define-key evil-iedit-state-map (kbd "C-;")  'evil-iedit-state/quit-iedit-mode)

(define-key evil-iedit-state-map "t"   'iedit-show/hide-unmatched-lines)
(define-key evil-iedit-state-map "s"   nil)

(define-key evil-iedit-state-map  (kbd "gU") 'iedit-upcase-occurrences)
(define-key evil-iedit-state-map  (kbd "gu") 'iedit-downcase-occurrences)
(define-key evil-iedit-state-map  (kbd "mf") 'iedit-restrict-function)
(define-key evil-iedit-state-map  (kbd "ml") 'iedit-restrict-current-line)
(define-key evil-iedit-state-map  (kbd "zc") 'iedit-toggle-case-sensitive)
(define-key evil-iedit-state-map  (kbd "zb") 'iedit-toggle-buffering)
(define-key evil-iedit-state-map "#"   nil)
(define-key evil-iedit-state-map  (kbd "N") 'iedit-number-occurrences)
(define-key evil-iedit-state-map  (kbd "SPC") 'iedit-blank-occurrences)

;; (autoload 'iedit-mode-from-isearch "iedit" "enable iedit-mode when in isearch mode")

;; (define-key evil-normal-state-map "s;" 'iedit-mode)
;; (with-eval-after-load 'iedit
;;   (define-key iedit-mode-keymap (kbd "M-;") 'iedit-toggle-selection))


;; (define-key global-map iedit-toggle-key-default 'iedit-mode)
;; (define-key global-map (kbd "C-[ [ 1 f") 'iedit-mode) ;iterm map C-; to this
;; (define-key isearch-mode-map iedit-toggle-key-default 'iedit-mode-from-isearch)


(provide 'conf-iedit)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-iedit.el ends here.
