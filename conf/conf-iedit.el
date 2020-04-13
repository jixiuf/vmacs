;;; Code:
(autoload 'iedit-mode "iedit-mode" nil t)
(setq-default iedit-toggle-key-default nil)
(global-set-key (kbd "C-;") 'evil-iedit-state-iedit-mode)
(evil-define-state iedit
  "`iedit state' interfacing iedit mode."
  :tag " <E> "
  :enable (normal)
  :cursor box
  :message "-- IEDIT --"
  ;; force iedit mode
  (if (evil-replace-state-p) (call-interactively 'iedit-mode)))
(set-keymap-parent  evil-iedit-state-map evil-normal-state-map)


(defun evil-iedit-state-iedit-mode (&optional arg)
  "Start `iedit-mode'."
  (interactive "P")
  (if (fboundp 'ahs-clear) (ahs-clear))
  (iedit-mode arg)
  (evil-iedit-state))
;; (require 'evil-iedit-state)
(define-key evil-iedit-state-map (kbd "C-;")  'evil-iedit-state-quit-iedit-mode)
(define-key evil-iedit-state-map (kbd "C-g")  'evil-iedit-state-quit-iedit-mode)

(defun evil-iedit-state-quit-iedit-mode ()
  "Quit iedit-mode and return set state `evil-iedit-state-default-state'."
  (interactive)
  (iedit-done)
  (evil-normal-state))


(define-key evil-iedit-state-map "t"   'iedit-show/hide-unmatched-lines)

(define-key evil-iedit-state-map "gg"  'iedit-goto-first-occurrence)
(define-key evil-iedit-state-map "G"   'iedit-goto-last-occurrence)

(define-key evil-iedit-state-map "n"   'iedit-next-occurrence)
(define-key evil-iedit-state-map "N"   'iedit-prev-occurrence)
(define-key evil-iedit-state-map (kbd "TAB") 'iedit-toggle-selection)
(define-key evil-iedit-state-map [tab]       'iedit-toggle-selection)
(define-key evil-iedit-state-map  (kbd "gU") 'iedit-upcase-occurrences)
(define-key evil-iedit-state-map  (kbd "gu") 'iedit-downcase-occurrences)
(define-key evil-iedit-state-map  (kbd "mf") 'iedit-restrict-function)
(define-key evil-iedit-state-map  (kbd "ml") 'iedit-restrict-current-line)
(define-key evil-iedit-state-map  (kbd "zc") 'iedit-toggle-case-sensitive)
(define-key evil-iedit-state-map  (kbd "zb") 'iedit-toggle-buffering)
(define-key evil-iedit-state-map  (kbd "C－x r n") 'iedit-number-occurrences)

(provide 'conf-iedit)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-iedit.el ends here.
