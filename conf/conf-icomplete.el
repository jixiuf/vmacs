 ;;; Code:
(require 'icomplete)

;; (setq icomplete-max-delay-chars 3)
(setq icomplete-delay-completions-threshold 2000)
(setq icomplete-compute-delay 0)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-hide-common-prefix nil)
(setq icomplete-in-buffer t)
(setq icomplete-tidy-shadowed-file-names t)

(setq icomplete-prospects-height 2)
(setq icomplete-separator (propertize " ☯" 'face  '(foreground-color . "SlateBlue1")))

(setq completion-styles '(basic partial-completion substring initials  flex))

(when (require 'orderless nil t)
  (setq completion-styles (cons 'orderless completion-styles)) ;把orderless放到completion-styles 开头
  ;; 默认按空格开隔的每个关键字支持regexp/literal/initialism 3种算法
  (setq orderless-matching-styles '(orderless-regexp orderless-literal orderless-initialism ))
  (defun without-if-$! (pattern _index _total)
    (when (or (string-prefix-p "$" pattern) ;如果以! 或$ 开头，则表示否定，即不包含此关键字
              (string-prefix-p "!" pattern))
      `(orderless-without-literal . ,(substring pattern 1))))
  (defun flex-if-comma (pattern _index _total) ;如果以逗号结尾，则以flex 算法匹配此组件
    (when (string-suffix-p "," pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  (defun literal-if-= (pattern _index _total) ;如果以=结尾，则以literal  算法匹配此关键字
    (when (or (string-suffix-p "=" pattern)
              (string-suffix-p "-" pattern)
              (string-suffix-p ";" pattern))
      `(orderless-literal . ,(substring pattern 0 -1))))
  (setq orderless-style-dispatchers '(literal-if-= flex-if-comma without-if-$!)))




(icomplete-mode 1)
(require 'icomplete-vertical nil t)

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
  (global-set-key (kbd "C-o") 'embark-act)
  (define-key icomplete-minibuffer-map (kbd "C-o") 'embark-act)
  (define-key icomplete-minibuffer-map (kbd "C-o") 'embark-act)
  (define-key icomplete-minibuffer-map (kbd "C-c C-o") 'embar-collect)
  (define-key icomplete-minibuffer-map (kbd "C-c C-c") 'embark-export)
  (define-key icomplete-minibuffer-map (kbd "C-c C-e") 'embark-live-occur)
  (defun vmacs-embark-collect-mode-hook ()
    (evil-local-mode)
    (evil-define-key 'normal 'local "/" #'consult-focus-lines)
    (evil-define-key 'normal 'local "z" #'consult-hide-lines)
    (evil-define-key 'normal 'local "r" #'consult-reset-lines))
  (add-hook 'tabulated-list-mode-hook 'vmacs-embark-collect-mode-hook))


(defun vmacs-minibuffer-space ()
  (interactive)
  (require 'consult)
  (if (and (string-prefix-p consult-async-default-split (minibuffer-contents))
           (= 2 (length (split-string (minibuffer-contents) consult-async-default-split))))
      (insert consult-async-default-split)
    (when (looking-back consult-async-default-split) (delete-char -1))
    (insert " ")))

(define-key icomplete-minibuffer-map (kbd "SPC") 'vmacs-minibuffer-space)

(setq consult-project-root-function #'vc-root-dir)
(with-eval-after-load 'consult
  (with-eval-after-load 'embark (require 'embark-consult nil t))
  (setq consult-ripgrep-command (format "%s %s"consult-ripgrep-command " -z"))
  (add-to-list 'consult-buffer-sources 'vmacs-consult--source-dired t)
  (add-to-list 'consult-buffer-sources 'vmacs-consult--source-git t)
  (setq consult-config `((consult-buffer :preview-key ,(kbd "C-v")) ;disable auto preview for consult-buffer
                        )))


(vmacs-leader (kbd "fh") (vmacs-defun find-file-home (let ((default-directory "~/"))(call-interactively 'find-file))))
(vmacs-leader (kbd "ft") (vmacs-defun find-file-tmp (let ((default-directory "/tmp/"))(call-interactively 'find-file))))
(setq ffap-machine-p-known 'accept)  ; no pinging
;; (vmacs-leader (kbd "ff") (icomplete-horizontal find-file  (find-file-at-point)))
(vmacs-leader (kbd "ff") #'find-file-at-point)
(vmacs-leader (kbd "fc") #'(lambda()(interactive) (find-file (expand-file-name "http.txt" dropbox-dir))))

(vmacs-leader " " 'consult-buffer)
(vmacs-leader "fo" 'consult-buffer-other-window)
(vmacs-leader "gG" #'consult-grep)
(vmacs-leader "gg" (vmacs-defun consult-ripgrep-default (consult-ripgrep default-directory)))
(vmacs-leader "gt" #'consult-ripgrep)
(vmacs-leader "g." (vmacs-defun consult-ripgrep-default-symbol (consult-ripgrep default-directory (thing-at-point 'symbol))))
(vmacs-leader "g," (vmacs-defun consult-ripgrep-root-symbol (consult-ripgrep(vc-root-dir) (thing-at-point 'symbol)) ))
(vmacs-define-key  'global "g/" 'consult-focus-lines nil 'normal)
(global-set-key [remap goto-line] 'consult-goto-line)
(global-set-key (kbd "C-c C-s") 'consult-line)
(global-set-key (kbd "<help> a") 'consult-apropos)
(vmacs-leader (kbd "wi") 'consult-imenu)

(global-set-key [remap yank-pop] 'consult-yank-pop)

(defun vmacs-icomplete-mode-hook()
  (cond
   ((cl-find this-command '(consult-ripgrep-root-symbol
                            consult-ripgrep-default-symbol
                            consult-imenu consult-line
                            consult-ripgrep execute-extended-command
                            project-switch-project vmacs-magit-status-list
                            project-or-external-find-file
                            consult-ripgrep-default consult-grep
                            evil-project-find-regexp
                            magit-status
                            xref-find-references
                            dired consult-buffer consult-buffer-other-window))
    (progn
      (when (boundp icomplete-vertical-mode)(icomplete-vertical-mode 1))
      (setq-local icomplete-separator "\n")
      (setq-local icomplete-prospects-height 15)))
   ((cl-find this-command '(yank-pop consult-yank-pop))
    (setq-local icomplete-prospects-height 15)
    (setq-local icomplete-separator (concat
                                     (propertize "\n" 'face '(:height 1))
                                     (propertize " " 'face '(:inherit vertical-border :underline t :height 1)
                                                 'display '(space :align-to right))
                                     (propertize "\n" 'face '(:height 1))))
    )
   (t
    (when (boundp icomplete-vertical-mode)(icomplete-vertical-mode -1))
    (setq-local icomplete-separator (propertize " ☚ " 'face  '(foreground-color . "lightgreen")))
    (setq-local icomplete-prospects-height 2))))


(add-hook 'icomplete-minibuffer-setup-hook #'vmacs-icomplete-mode-hook)


(provide 'conf-icomplete)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-icomplete.el ends here.
