;;; Code:
(require 'icomplete)

;; (setq icomplete-max-delay-chars 3)
(setq icomplete-delay-completions-threshold 2000)
(setq icomplete-compute-delay 0)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-hide-common-prefix nil)
(setq icomplete-in-buffer t)
(setq icomplete-tidy-shadowed-file-names t)

(setq icomplete-prospects-height 15)
(setq icomplete-separator "\n")
;; (setq icomplete-separator (propertize " ⚫ " 'face  '(foreground-color . "SlateBlue1")))
(setq completion-styles '(basic partial-completion substring initials  flex))

(defun vmacs-fido-mode-hook()
  (when (and icomplete-mode (icomplete-simple-completing-p))
    (setq-local completion-styles '(orderless partial-completion initials flex))))

(when (require 'orderless nil t)
  (add-hook 'minibuffer-setup-hook #'vmacs-fido-mode-hook 99)
  (setq orderless-component-separator " +")
  (setq orderless-matching-styles '(orderless-regexp orderless-literal)))



(fido-mode 1)
(define-key icomplete-fido-mode-map (kbd "C-n") #'icomplete-forward-completions)

(define-key icomplete-minibuffer-map (kbd "C-.") 'next-history-element)
(define-key icomplete-fido-mode-map (kbd "C-l") #'icomplete-fido-backward-updir)
(define-key icomplete-fido-mode-map (kbd "C-e") #'(lambda(&optional argv)(interactive)(if (eolp) (call-interactively #'icomplete-fido-exit) (end-of-line))) )



(when (require 'embark nil t)
  (when (require 'marginalia nil t) (marginalia-mode 1))
  (setq embark-occur-initial-view-alist '((t . list)))
  (define-key icomplete-fido-mode-map (kbd "C-o") 'embark-act)
  (define-key icomplete-fido-mode-map (kbd "C-o") 'embark-act)
  (define-key icomplete-fido-mode-map (kbd "C-c C-o") 'embark-occur)
  (define-key icomplete-fido-mode-map (kbd "C-c C-c") 'embark-export)
  (define-key icomplete-fido-mode-map (kbd "C-c C-e") 'embark-live-occur)
  (define-key embark-occur-mode-map (kbd "h") nil)
  (define-key embark-occur-mode-map (kbd "v") nil)
  (define-key embark-occur-mode-map (kbd "e") nil)
  (global-set-key (kbd "C-o") 'embark-act)
  )

(fset 'imenu 'consult-imenu)
(setq consult-async-default-split nil)
(vmacs-leader "gg" #'consult-ripgrep)
(vmacs-leader "gt" #'(lambda()(interactive) (require 'magit)(consult-ripgrep (magit-toplevel))))
(vmacs-leader "g." #'(lambda()(interactive) (consult-ripgrep nil (thing-at-point 'symbol))))
(vmacs-leader "g," #'(lambda()(interactive) (require 'magit) (consult-ripgrep (magit-toplevel) (thing-at-point 'symbol))))

(defadvice yank-pop (around icomplete-mode (arg) activate)
  (interactive "p")
  (let ((icomplete-separator (concat "\n" (propertize (make-string 60 ?— ) 'face 'vertical-border) "\n ")))
    ad-do-it))



(provide 'conf-icomplete)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-icomplete.el ends here.
