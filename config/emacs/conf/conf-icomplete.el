;; -*- lexical-binding: t -*-
;;; Code:
(require 'icomplete)
(require 'recentf)
(require 'consult)
(setq enable-recursive-minibuffers t)        ;在 minibuffer 中也可以再次使用 minibuffer
(setq history-delete-duplicates t)          ;minibuffer 删除重复历史
(setq minibuffer-prompt-properties;minibuffer prompt 只读，且不允许光标进入其中
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(setq completion-cycle-threshold 3)
;; (setq completion-flex-nospace t)
(setq completion-pcm-complete-word-inserts-delimiters t) ;partial-completion in completion-styles
;; (setq completion-pcm-word-delimiters "-_/:| ")
(setq completion-auto-help t)         ;不主动弹出 *Completions*
(setq completions-format 'one-column)   ; *Completions* buffer M-v 跳到*Completions* buffer
(setq completions-header-format nil)
(setq max-mini-window-height 5)        ;selectrum-num-candidates-displayed 受影响
(setq completions-max-height 5)
(setq icomplete-prospects-height 5)
;; (setq completion-auto-select nil)
(setq completions-detailed t)
(setq completion-show-help nil) ;*Completions* show help
(setq eldoc-echo-area-use-multiline-p 2)
(setq resize-mini-windows 'grow-only)
;; (setq read-answer-short t)
(setq minibuffer-eldef-shorten-default t)
(minibuffer-electric-default-mode 1)    ;当输入内容后，prompt 的 default 值就会被隐藏
(file-name-shadow-mode 1)
(minibuffer-depth-indicate-mode 1)                   ;显示 minibuffer 深度

(define-key completion-list-mode-map (kbd "C-g") 'quit-window) ;*Completions*

(defface vmacs-minibuffer-font
  `((t :inherit default :height 1.0))
  "The default font for minibuffer buffer.
Monospaced font whihc is fixed idth and height is recommended."
  :group 'minibuffer)

(defun vmacs-minibuffer-hook()
  ;; (set (make-local-variable 'buffer-face-mode-face) 'vmacs-minibuffer-font)
  ;; (buffer-face-mode t)
  (local-set-key (kbd "C-.") 'completion-at-point)
  (local-set-key (kbd "<C-m>") 'exit-minibuffer)
  (local-set-key (kbd "<C-h>") 'backward-delete-char-untabify)
  (local-set-key (kbd "C-l") 'backward-kill-word)
  (local-set-key [escape] 'abort-recursive-edit)
  (local-set-key (kbd "TAB") 'minibuffer-complete)
  (local-set-key (kbd "<tab>") 'minibuffer-complete)
  (local-set-key  (kbd "<f19>") #'ignore) ;详见 isearch-pre-command-hook
  ;; (define-key minibuffer-local-completion-map (kbd "C-e") 'minibuffer-complete)
  (define-key minibuffer-local-map (kbd "M-p") 'previous-history-element)
  (define-key minibuffer-local-map (kbd "M-n") 'next-history-element)
  ;; (define-key minibuffer-local-must-match-map (kbd "<C-m>") 'exit-minibuffer)
  ;; (define-key minibuffer-local-map (kbd "<C-m>") 'exit-minibuffer)
  ;; (define-key minibuffer-local-completion-map (kbd "<C-m>") 'exit-minibuffer)
  ;; (define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)

  ;; (autoload 'minibuffer-keyboard-quit "delsel" "" t nil)
  ;; (define-key minibuffer-local-map [escape]  'minibuffer-keyboard-quit)
  )

(add-hook 'minibuffer-setup-hook #'vmacs-minibuffer-hook)




;; (setq icomplete-max-delay-chars 3)
(setq icomplete-delay-completions-threshold 2000)
(setq icomplete-compute-delay 0)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-hide-common-prefix nil)
;; (setq icomplete-in-buffer t)
;; ;; https://github.com/emacs-mirror/emacs/blob/master/etc/NEWS.30#L135
;; (advice-add 'completion-at-point :after #'minibuffer-hide-completions)

(setq tab-always-indent 'complete)
(setq completion-in-region-function #'consult-completion-in-region)
(global-completion-preview-mode)
;; (setq read-buffer-completion-ignore-case nil)
;; (setq read-file-name-completion-ignore-case nil)
(setq completion-preview-ignore-case t)
(setq completion-ignore-case t)
;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
;; Corfu commands are hidden, since they are not supposed to be used via M-x.
(setq read-extended-command-predicate #'command-completion-default-include-p)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(defun vmacs-complete()
  (interactive)
  (completion-preview-hide)
  (indent-for-tab-command))
(define-key completion-preview-active-mode-map (kbd "C-n") #'vmacs-complete)
(define-key completion-preview-active-mode-map (kbd "C-i") #'vmacs-complete)
(define-key completion-preview-active-mode-map (kbd "C-o") #'completion-preview-complete)
(define-key completion-preview-active-mode-map (kbd "C-m") #'completion-preview-insert)
(define-key completion-preview-active-mode-map (kbd "C-s") #'completion-preview-next-candidate)
(define-key completion-preview-active-mode-map (kbd "M-f") #'completion-preview-insert-word)
(setq completion-preview-minimum-symbol-length nil)
(setq completion-preview-completion-styles '(basic partial-completion initials orderless))
(setq icomplete-tidy-shadowed-file-names t)
(setq icomplete-separator (propertize " 👈 " 'face  '(foreground-color . "lightgreen")))
(setq completion-styles '(basic partial-completion substring initials  flex))

(when (require 'orderless nil t)
  (setq completion-styles '(basic partial-completion initials orderless))
  ;; 支持拼间首字母过滤中文， 不必切输入法
  (defun completion--regex-pinyin (str)
    (require 'pinyinlib)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  ;; 默认按空格开隔的每个关键字支持 regexp/literal/initialism 3 种算法
  (setq orderless-matching-styles '(completion--regex-pinyin orderless-regexp
                                    orderless-literal orderless-initialism
                                    orderless-flex))
  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp orderless-flex)))
  (setq completion-category-overrides '((eglot (styles orderless))
                                        (multi-category (styles . (basic partial-completion orderless)))
                                        ;; (buffer (styles +orderless-with-initialism)) ;; partial-completion is tried first
                                        (file (styles partial-completion)) ;; partial-completion is tried first
                                        ;; enable initialism by default for symbols
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism))))

  (setq orderless-component-separator #'orderless-escapable-split-on-space) ;; allow escaping space with \
  ;; Recognizes the following patterns:
  ;; * regexp$ (regexp matching at end)
  ;; consult-buffer 等 支持  el$
  ;; https://github.com/minad/consult/wiki#orderless-style-dispatchers-ensure-that-the--regexp-works-with-consult-buffer
  (defun +orderless-fix-dollar (word &optional _index _total)
    (let ((consult-suffix
           (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
               (format "[%c-%c]*$"
                       consult--tofu-char
                       (+ consult--tofu-char consult--tofu-range -1))
             "$")))
      (concat word consult-suffix)))
  (add-to-list 'orderless-affix-dispatch-alist
               '(?$ . +orderless-fix-dollar)))




(icomplete-mode 1)
;; (icomplete-vertical-mode 1)
(add-hook 'minibuffer-setup-hook #'vmacs-icomplete-vertical-mode)
;; (add-hook 'completion-in-region-mode-hook #'vmacs-icomplete-vertical-mode)
(defun vmacs-icomplete-vertical-mode()
  (setq-local truncate-lines t)
  (when  (member this-command '(xref-find-references
                                xref-find-definitions xref-find-apropos
                                eglot-find-declaration eglot-find-implementation
                                eglot-code-actions execute-extended-command
                                project-or-external-find-file completion-at-point
                                indent-for-tab-command vmacs-yank-pop
                                describe-function describe-variable
                                yank-pop vmacs-complete
                                consult-dir consult-ripgrep
                                consult-grep consult-completion-in-region
                                consult-line consult-ripgrep-default-symbol
                                consult-ripgrep-root-symbol consult-ripgrep-default))
    (when (minibufferp)
      (setq-local icomplete-vertical-mode t)
      (add-hook 'icomplete-minibuffer-setup-hook
                #'icomplete--vertical-minibuffer-setup nil t)
      ;; for complete-in-buffer
      ;; (local-set-key  (kbd "<escape>") 'keyboard-quit)
      ;; (setq-local icomplete-hide-common-prefix nil
      ;;             ;; Ask `icomplete-completions' to return enough completions candidates.
      ;;             max-mini-window-height 8
      ;;             icomplete-prospects-height 25
      ;;             redisplay-adhoc-scroll-in-resize-mini-windows nil)
      )))


;; (add-hook 'minibuffer-exit-hook '(lambda() (icomplete-vertical-mode -1)))

;; (setq icomplete-scroll t)
(define-key icomplete-minibuffer-map (kbd "RET") 'icomplete-fido-ret)
(define-key icomplete-minibuffer-map (kbd "C-m") 'icomplete-fido-ret)
(define-key icomplete-minibuffer-map (kbd "C-n") #'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-p") #'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-s") #'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-r") #'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "M-.") 'next-history-element)
(define-key icomplete-minibuffer-map (kbd "C-l") #'icomplete-fido-backward-updir)
(define-key icomplete-minibuffer-map (kbd "C-e") #'(lambda(&optional argv)(interactive)(if (eolp) (call-interactively #'icomplete-fido-exit) (end-of-line))) )
;; for consult-rg 忽略 gitignore
(define-key icomplete-minibuffer-map (kbd "C-h") #'(lambda()(interactive) (insert " -- -uuu")))


(when (require 'embark nil t)
  (when (require 'marginalia nil t)
    (marginalia-mode 1)
    (advice-add 'marginalia--annotate-local-file :override
                (defun marginalia--annotate-local-file-advice (cand)
                  (marginalia--fields
                   ((marginalia--full-candidate cand)
                    :face 'marginalia-size )))))
  ;; (setq marginalia-margin-min 18)

  ;; (setq embark-collect-initial-view-alist '((t . list)))
  ;; (vmacs-define-key  'global (kbd "C-t") #'embark-act nil 'normal)

  (global-set-key  (kbd "C-w") 'embark-act)
  (define-key icomplete-minibuffer-map (kbd "C-w") 'embark-act)
  (define-key icomplete-minibuffer-map (kbd "C-c C-o") 'embark-collect-snapshot)
  (define-key icomplete-minibuffer-map (kbd "C-c C-c") 'embark-export)
  (defun vmacs-embark-collect-mode-hook ()
    (local-set-key "/" #'consult-focus-lines)
    (local-set-key  "z" #'consult-hide-lines)
    )
  (add-hook 'tabulated-list-mode-hook 'vmacs-embark-collect-mode-hook))


(setq consult-async-split-style 'perl)
(defun vmacs-minibuffer-space ()
  (interactive)
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
        ;; (run-with-timer
        ;;  0.03 nil (lambda()
        ;;             (with-current-buffer (get-buffer "*grep*")
        ;;               (grep-change-to-grep-edit-mode))))
        )))
  (add-hook 'embark-after-export-hook #'(lambda()(rename-buffer "*grep*" t)))

  ;; (setq consult-ripgrep-args (format "%s %s"consult-ripgrep-args " -z"))
  ;; (add-to-list 'consult-buffer-sources 'vmacs-consult--source-dired t)
  (setq consult-buffer-sources
        '(consult--source-buffer
          consult--source-recent-file
          vmacs-consult--source-git))
  (defun vmacs-consult--source-recentf-items ()
    (let ((ht (consult--buffer-file-hash))
          file-name-handler-alist ;; No Tramp slowdown please.
          items)
      (dolist (file recentf-list (nreverse items))
        ;; Emacs 29 abbreviates file paths by default, see
        ;; `recentf-filename-handlers'.
        (unless (eq (aref file 0) ?/)
          (setq file (expand-file-name file)))
        (unless (gethash file ht)
          (push (propertize
                 (vmacs-short-filename file)
                 'multi-category `(file . ,file))
                items)))))

  (defun vmacs-short-filename(file)
    "return filename with one parent directory.
/a/b/c/d-> c/d"
    (let* ((file (directory-file-name file))
           (filename (file-name-nondirectory file))
           (dir (file-name-directory file))
           (short-name filename)
           ;; (display short-name)
           )
      (when dir
        (setq short-name
              ;; 这段是想实现 只展示最后 1 级目录/文件.ext 的功能
              (format "%s\\%s" filename (file-name-nondirectory
                                         (directory-file-name dir))
                      )))
      (propertize short-name 'multi-category `(file . ,file))))

  (plist-put consult--source-recent-file
             :items #'vmacs-consult--source-recentf-items)

  ;; (consult-customize
  ;;  consult-buffer
  ;;  ;; my/command-wrapping-consult    ;; disable auto previews inside my command
  ;;  :preview-key '(:debounce 0.4 any)) ;; Option 1: Delay preview
  ;; ;; :preview-key (kbd "M-."))      ;; Option 2: Manual preview

  )


(vmacs-leader (kbd "fh") (vmacs-defun find-file-home (let ((default-directory "~/"))(call-interactively 'find-file))))
(vmacs-leader (kbd "ft") (vmacs-defun find-file-tmp (let ((default-directory "/tmp/"))(call-interactively 'find-file))))
(vmacs-leader (kbd "fu") (vmacs-defun find-file-http (find-file (expand-file-name "http/" dropbox-dir))))
(vmacs-leader (kbd "fn") (vmacs-defun find-file-note (find-file org-default-notes-file)))

(setq ffap-machine-p-known 'accept)  ; no pinging
;; (vmacs-leader (kbd "ff") (icomplete-horizontal find-file  (find-file-at-point)))
(vmacs-leader (kbd "ff") #'find-file)
(global-set-key (kbd "C-x C-f") #'find-file-at-point)
(global-set-key (kbd "C-x r b") #'consult-bookmark)
(global-set-key (kbd "C-x r x" ) #'consult-register)


(vmacs-leader (kbd "i") (vmacs-defun vmacs-mu4e (call-process "killall" nil nil nil  "mbsync")(require 'mu4e)(mu4e t)(mu4e-search-bookmark)))

(vmacs-leader "<SPC>" 'consult-buffer)
(vmacs-leader "fo" 'consult-buffer-other-window)
(vmacs-leader "fl" 'consult-find)
(vmacs-leader "gh" #'consult-grep)
(vmacs-leader "ge" #'grep)
(vmacs-leader "gg" (vmacs-defun consult-ripgrep-default (consult-ripgrep default-directory)))
(vmacs-leader "gt" #'consult-ripgrep)
(vmacs-leader "g." (vmacs-defun consult-ripgrep-default-symbol (consult-ripgrep default-directory (concat "\\b" (thing-at-point 'symbol) "\\b"))))
(vmacs-leader "g," (vmacs-defun consult-ripgrep-root-symbol (consult-ripgrep(vc-root-dir)  (concat "\\b" (thing-at-point 'symbol) "\\b"))))
(global-set-key [remap goto-line] 'consult-goto-line)
(global-set-key (kbd "C-c C-s") 'consult-line)
(vmacs-leader (kbd "wi") 'consult-imenu)


(defun vmacs-recentf-keep-p (file)
  "Return non-nil if FILE should be kept in the recent list.
It handles the case of remote files as well."
  (cond
   ((file-remote-p file nil t) (file-readable-p file))
   ((file-remote-p file) nil)           ;不记录 tramp path
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
(setq consult-dir-project-list-function #'(lambda()nil)) ;不使用 consult-dir--source-project
;; (setq consult-dir-project-list-function #'consult-dir-project-dirs)
(setq-default consult-dir-sources
              '(consult-dir--source-default
                consult-dir--source-recentf
                ;; consult-dir--source-project
                consult-dir--source-tramp-ssh))

(define-key minibuffer-local-completion-map (kbd "s-C-j") #'consult-dir)
(define-key minibuffer-local-completion-map (kbd "s-j") #'consult-dir)
(define-key minibuffer-local-completion-map (kbd "s-C-l") #'consult-dir-jump-file) ;locate
(define-key minibuffer-local-completion-map (kbd "s-l") #'consult-dir-jump-file) ;locate
(define-key global-map (kbd "C-x d") #'consult-dir)
(setq consult-dir-shadow-filenames nil)
(setq consult-dir-default-command #'consult-dir-dired)
(with-eval-after-load 'consult-dir
  (defvar consult-dir--source-project-items (plist-get consult-dir--source-project :items))
  (plist-put consult-dir--source-project
             :items #'(lambda() (mapcar #'vmacs-short-filename  (funcall consult-dir--source-project-items))))
  (plist-put consult-dir--source-recentf
             :items #'(lambda() (mapcar #'vmacs-short-filename  (consult-dir--recentf-dirs))))
  (plist-put consult-dir--source-default
             :items #'(lambda() (mapcar #'vmacs-short-filename  (consult-dir--default-dirs))))

  )




(provide 'conf-icomplete)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-icomplete.el ends here.
