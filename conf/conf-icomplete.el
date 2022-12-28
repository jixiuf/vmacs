  ;; -*- lexical-binding: t -*-
;;; Code:
(require 'icomplete)
(require 'recentf)

;; (setq icomplete-max-delay-chars 3)
(setq icomplete-delay-completions-threshold 2000)
(setq icomplete-compute-delay 0)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-hide-common-prefix nil)
(setq icomplete-in-buffer nil)
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
  (setq completion-styles '(orderless basic partial-completion initials))
  ;; 默认按空格开隔的每个关键字支持regexp/literal/initialism 3种算法
  (setq orderless-matching-styles '(orderless-regexp orderless-literal orderless-initialism ))
    ;; Recognizes the following patterns:
  ;; * ;flex flex;
  ;; * =literal literal=
  ;; * `initialism initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  ;; https://github.com/minad/consult/wiki
  (defun fix-dollar (args)
    (if (string-suffix-p "$" (car args))
        (list (format "%s[%c-%c]*$"
                      (substring (car args) 0 -1)
                      consult--tofu-char
                      (+ consult--tofu-char consult--tofu-range -1)))
      args))
  (advice-add #'orderless-regexp :filter-args #'fix-dollar)
  ;; (advice-add #'prescient-regexp-regexp :filter-args #'fix-dollar)
  (defun +orderless--suffix-regexp ()
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * %char-fold char-fold%
  ;; * `initialism initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun vmacs-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern) `(orderless-regexp . ,(concat (substring pattern 0 -1) (+orderless--suffix-regexp))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) (+orderless--suffix-regexp))))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))

  (setq orderless-style-dispatchers '(vmacs-orderless-dispatch))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  (setq completion-category-overrides '(
                                        (eglot (styles orderless))
                                        (file (styles partial-completion)) ;; partial-completion is tried first
                                        ;; enable initialism by default for symbols
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism))
                                        ))

  ;; 支持拼间首字母过滤中文， 不必切输入法
  (defun completion--regex-pinyin (str)
    (require 'pinyinlib)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin)
  (setq orderless-component-separator #'orderless-escapable-split-on-space) ;; allow escaping space with \
  )




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
  (vmacs-define-key  'global (kbd "C-t") #'embark-act nil 'normal)

  (define-key icomplete-minibuffer-map (kbd "C-t") 'embark-act)
  (define-key icomplete-minibuffer-map (kbd "C-c C-o") 'embark-collect-snapshot)
  (define-key icomplete-minibuffer-map (kbd "C-c C-c") 'embark-export)
  (defun vmacs-embark-collect-mode-hook ()
    (evil-local-mode)
    (evil-define-key 'normal 'local "/" #'consult-focus-lines)
    (evil-define-key 'normal 'local "z" #'consult-hide-lines)
    (evil-define-key 'normal 'local "r" #'consult-reset-lines))
  (add-hook 'tabulated-list-mode-hook 'vmacs-embark-collect-mode-hook))


(setq consult-async-split-style 'perl)
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
  (with-eval-after-load 'embark
    (require 'embark-consult nil t)
    (setf (alist-get 'xref-location embark-exporters-alist) #'vmacs-embark-consult-export-grep)
    (setf (alist-get 'consult-grep embark-exporters-alist) #'vmacs-embark-consult-export-grep)
    (defun vmacs-embark-consult-export-grep(lines)
      (dolist (buf (buffer-list))
        (when (string-prefix-p "*grep" (buffer-name buf))
          (kill-buffer buf)))
      (let* ((default-directory default-directory)
             dir file)
        (cl-find-if
         (lambda (line)
           (setq file (car (split-string line ":")))
           (unless (file-name-absolute-p file)
             (setq dir (locate-dominating-file default-directory file))
             (when dir (setq default-directory dir)))
           ) lines)
        (embark-consult-export-grep lines)
        ))
    )
  (add-hook 'embark-after-export-hook #'(lambda()(rename-buffer "*grep*" t)))

  (setq consult-ripgrep-args (format "%s %s"consult-ripgrep-args " -z"))
  ;; (add-to-list 'consult-buffer-sources 'vmacs-consult--source-dired t)
  (setq consult-buffer-sources
        '(consult--source-buffer
          consult--source-recent-file
          vmacs-consult--source-git))

  (setq consult-config `((consult-buffer :preview-key ,(kbd "C-v")) ;disable auto preview for consult-buffer
                         )))


(vmacs-leader (kbd "fh") (vmacs-defun find-file-home (let ((default-directory "~/"))(call-interactively 'find-file))))
(vmacs-leader (kbd "ft") (vmacs-defun find-file-tmp (let ((default-directory "/tmp/"))(call-interactively 'find-file))))
(setq ffap-machine-p-known 'accept)  ; no pinging
;; (vmacs-leader (kbd "ff") (icomplete-horizontal find-file  (find-file-at-point)))
(vmacs-leader (kbd "ff") #'find-file)
(global-set-key (kbd "C-x C-f") #'find-file-at-point)
(global-set-key (kbd "C-x r b") #'consult-bookmark)
(global-set-key (kbd "C-x r x" ) #'consult-register)(global-set-key (kbd "C-x r b") #'consult-bookmark)

(vmacs-leader (kbd "fc") #'(lambda()(interactive) (find-file (expand-file-name "http.txt" dropbox-dir))))
(autoload #'mu4e-headers-search-bookmark  "mu4e" t)
(vmacs-leader (kbd "i") #'(lambda()(interactive)(shell-command "killall mbsync") (mu4e-headers-search-bookmark)(mu4e)))

(autoload #'consult-buffer  "consult" t)
(vmacs-leader " " 'consult-buffer)
(vmacs-leader "fo" 'consult-buffer-other-window)
(vmacs-leader "fl" 'consult-find)
(vmacs-leader "gh" #'consult-grep)
(vmacs-leader "gg" (vmacs-defun consult-ripgrep-default (consult-ripgrep default-directory)))
(vmacs-leader "gt" #'consult-ripgrep)
(vmacs-leader "g." (vmacs-defun consult-ripgrep-default-symbol (consult-ripgrep default-directory (concat "\\b" (thing-at-point 'symbol) "\\b"))))
(vmacs-leader "g," (vmacs-defun consult-ripgrep-root-symbol (consult-ripgrep(vc-root-dir)  (concat "\\b" (thing-at-point 'symbol) "\\b"))))
(vmacs-define-key  'global "g/" 'consult-focus-lines nil 'normal)
(global-set-key [remap goto-line] 'consult-goto-line)
(global-set-key (kbd "C-c C-s") 'consult-line)
(global-set-key (kbd "<help> a") 'consult-apropos)
(vmacs-leader (kbd "wi") 'consult-imenu)


(defun vmacs-recentf-keep-p (file)
  "Return non-nil if FILE should be kept in the recent list.
It handles the case of remote files as well."
  (cond
   ((file-remote-p file nil t) (file-readable-p file))
   ((file-remote-p file) nil)           ;不记录tramp path
   ((file-readable-p file))))
(setq recentf-keep '(vmacs-recentf-keep-p))

;; Track opened directories
(defun recentf-track-opened-dir ()
  (and default-directory
       (recentf-add-file default-directory)))

(add-hook 'dired-mode-hook #'recentf-track-opened-dir)

;; Track closed directories
;; (advice-add 'recentf-track-closed-file :override
;;             (defun recentf-track-closed-advice ()
;;               (cond (buffer-file-name (recentf-remove-if-non-kept buffer-file-name))
;;                     ((equal major-mode 'dired-mode)
;;                      (recentf-remove-if-non-kept default-directory)))))

;; (require 'consult-dired-history)
(setq-default consult-dir-sources
              '(consult-dir--source-default
                consult-dir--source-recentf
                consult-dir--source-project
                consult-dir--source-tramp-ssh
                consult-dir--source-bookmark))

(define-key minibuffer-local-completion-map (kbd "C-M-s-j") #'consult-dir)
(define-key minibuffer-local-completion-map (kbd "C-M-s-l") #'consult-dir-jump-file) ;locate
(define-key global-map (kbd "C-x d") #'consult-dir)
(setq consult-dir-shadow-filenames nil)
(setq consult-dir-default-command #'consult-dir-dired)

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
