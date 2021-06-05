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
;; (concat
;;                                      (propertize "\n" 'face '(:height 1))
;;                                      (propertize " " 'face '(:inherit vertical-border :underline t :height 1)
;;                                                  'display '(space :align-to right))
;;                                      (propertize "\n" 'face '(:height 1)))
(setq icomplete-separator (propertize " ☚ " 'face  '(foreground-color . "lightgreen")))

(setq completion-styles '(basic partial-completion substring initials  flex))

(when (require 'orderless nil t)
  (setq completion-styles (cons 'orderless completion-styles)) ;把orderless放到completion-styles 开头
  ;; 默认按空格开隔的每个关键字支持regexp/literal/initialism 3种算法
  (setq orderless-matching-styles '(orderless-regexp orderless-literal orderless-initialism ))
    ;; Recognizes the following patterns:
  ;; * ;flex flex;
  ;; * =literal literal=
  ;; * `initialism initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun vmacs-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern) `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ((string-match-p "\\`\\.." pattern) `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ((string-suffix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 0 -1)))
     ((string-prefix-p "@" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ((string-suffix-p "@" pattern) `(orderless-without-literal . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ((string-prefix-p "," pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "," pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p ";" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p ";" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))
  (setq orderless-style-dispatchers '(vmacs-orderless-dispatch)))




(icomplete-mode 1)
(icomplete-vertical-mode 1)
;; (setq icomplete-scroll t)

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
  ;; (setq marginalia-margin-min 18)

  (setq embark-collect-initial-view-alist '((t . list)))
  (global-set-key (kbd "C-o") 'embark-act)
  (define-key icomplete-minibuffer-map (kbd "C-o") 'embark-act)
  (define-key icomplete-minibuffer-map (kbd "C-c C-o") 'embar-collect)
  (define-key icomplete-minibuffer-map (kbd "C-c C-c") 'embark-export)
  (defun vmacs-embark-collect-mode-hook ()
    (evil-local-mode)
    (evil-define-key 'normal 'local "/" #'consult-focus-lines)
    (evil-define-key 'normal 'local "z" #'consult-hide-lines)
    (evil-define-key 'normal 'local "r" #'consult-reset-lines))
  (add-hook 'tabulated-list-mode-hook 'vmacs-embark-collect-mode-hook))


(defun vmacs-minibuffer-space ()
  (interactive)
  (require 'consult)
  (if (and (string-prefix-p "#" (minibuffer-contents))
           (= 2 (length (split-string (minibuffer-contents) "#"))))
      (insert "#")
    (when (looking-back "#") (delete-char -1))
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
(vmacs-leader (kbd "ff") #'find-file)
(global-set-key (kbd "C-x C-f") #'find-file-at-point)
(vmacs-leader (kbd "fc") #'(lambda()(interactive) (find-file (expand-file-name "http.txt" dropbox-dir))))
(autoload #'mu4e-headers-search-bookmark  "mu4e" t)
(vmacs-leader (kbd "fm") #'(lambda()(interactive) (mu4e-headers-search-bookmark)(mu4e)))

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

(defun vmacs-icomplete()
  (setq-local truncate-lines t)
  ;;  remove the truncated lines indicator
  ;; (setq-default fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist))
  )

(add-hook 'icomplete-minibuffer-setup-hook #'vmacs-icomplete)

(provide 'conf-icomplete)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-icomplete.el ends here.
