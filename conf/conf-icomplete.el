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

(when (require 'orderless nil t)
  (setq completion-styles (cons 'orderless completion-styles))
  (setq orderless-component-separator "[ /]")
  (setq orderless-matching-styles '(orderless-regexp orderless-literal orderless-initialism orderless-flex))
  (defun without-if-$! (pattern _index _total)
    (when (or (string-prefix-p "$" pattern) ;如果以! 或$ 开头，刚表示否定
              (string-prefix-p "!" pattern))
      `(orderless-without-literal . ,(substring pattern 1))))
  (defun flex-if-comma (pattern _index _total) ;如果以逗号结尾，则以flex 算法匹配此组件
    (when (string-suffix-p "," pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  (defun literal-if-= (pattern _index _total) ;如果以=结尾，则以literal  算法匹配此组件
    (when (or (string-suffix-p "=" pattern)
              (string-suffix-p "-" pattern)
              (string-suffix-p ";" pattern))
      `(orderless-literal . ,(substring pattern 0 -1))))


  (setq orderless-style-dispatchers '(literal-if-= flex-if-comma without-if-$!)))




(icomplete-mode 1)

(define-key icomplete-minibuffer-map (kbd "RET") 'icomplete-fido-ret)
(define-key icomplete-minibuffer-map (kbd "C-m") 'icomplete-fido-ret)
(define-key icomplete-minibuffer-map (kbd "C-n") #'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-p") #'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-s") #'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-r") #'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-.") 'next-history-element)
(define-key icomplete-minibuffer-map (kbd "C-l") #'icomplete-fido-backward-updir)
(define-key icomplete-minibuffer-map (kbd "C-e") #'(lambda(&optional argv)(interactive)(if (eolp) (call-interactively #'icomplete-fido-exit) (end-of-line))) )



(when (require 'embark nil t)
  (when (require 'marginalia nil t) (marginalia-mode 1))
  (setq embark-collect-initial-view-alist '((t . list)))
  (define-key icomplete-minibuffer-map (kbd "C-o") 'embark-act)
  (define-key icomplete-minibuffer-map (kbd "C-o") 'embark-act)
  (define-key icomplete-minibuffer-map (kbd "C-c C-o") 'embar-collect)
  (define-key icomplete-minibuffer-map (kbd "C-c C-c") 'embark-export)
  (define-key icomplete-minibuffer-map (kbd "C-c C-e") 'embark-live-occur)
  (define-key embark-collect-mode-map (kbd "h") nil)
  (define-key embark-collect-mode-map (kbd "v") nil)
  (define-key embark-collect-mode-map (kbd "e") nil)
  (global-set-key (kbd "C-o") 'embark-act)
  )


(defun vmacs-minibuffer-space ()
  (interactive)
  (if (and (string-prefix-p consult-async-default-split (minibuffer-contents))
           (= 2 (length (split-string (minibuffer-contents) consult-async-default-split))))
      (insert consult-async-default-split)
    (insert " ")))

(define-key icomplete-minibuffer-map (kbd "SPC") 'vmacs-minibuffer-space)

(fset 'imenu 'consult-imenu)
(setq consult-async-default-split "#")
(vmacs-leader "gg" #'consult-ripgrep)
(vmacs-leader "gG" #'consult-grep)
(vmacs-leader "gt" #'(lambda()(interactive) (require 'magit) (consult-ripgrep (magit-toplevel))))
(vmacs-leader "g." #'(lambda()(interactive) (consult-ripgrep nil (thing-at-point 'symbol))))
(vmacs-leader "g," #'(lambda()(interactive) (require 'magit) (consult-ripgrep (magit-toplevel) (thing-at-point 'symbol))))
(vmacs-define-key  'global "g/" 'consult-keep-lines nil 'normal)
(vmacs-define-key  'global "gz" 'consult-flush-lines nil 'normal)

(defadvice yank-pop (around icomplete-mode (arg) activate)
  (interactive "p")
  (let ((icomplete-separator (concat "\n" (propertize (make-string 60 ?— ) 'face 'vertical-border) "\n ")))
    ad-do-it))



(provide 'conf-icomplete)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-icomplete.el ends here.
