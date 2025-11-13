;;; -*- lexical-binding: t -*-
;; (add-hook 'grep-setup-hook 'grep-mode-fun)
;; Then M-g n or M-g p for the next/previous match.
;; With (repeat-mode) consequent next/previous are just n and p.
;; (repeat-mode 1)
(setq grep-command "rg -nS --no-heading -e "
      grep-use-null-device nil)

(with-eval-after-load 'replace
  ;; (keymap-unset occur-mode-map "l" t)
  ;; (define-key occur-mode-map "l" (kbd "C-f"))
  (define-key occur-edit-mode-map (kbd "C-n") 'next-error-no-select)
  (define-key occur-edit-mode-map (kbd "C-p") 'previous-error-no-select)
  (when (boundp 'meow-normal-state-keymap)
    (set-keymap-parent occur-mode-map meow-normal-state-keymap)
    )
  (add-hook 'occur-hook #'occur-edit-mode))
(defun vmacs-grep-mode-hook()
  (meep-local-set-key "/" #'consult-focus-lines)
  (meep-local-set-key "z" #'consult-hide-lines))

(with-eval-after-load 'grep
  ;; (define-key grep-mode-map (kbd "e") nil)
  ;; (set-keymap-parent grep-mode-map meow-normal-state-keymap)
  
  (define-key grep-mode-map (kbd "C-c Mi") #'wgrep-change-to-wgrep-mode)
  (advice-add 'grep-exit-message :after #'wgrep-change-to-wgrep-mode)
  
  (add-hook 'grep-mode-hook 'vmacs-grep-mode-hook)
  ;; (when (boundp 'grep-edit-mode-map)
  ;;   (define-key grep-mode-map (kbd "C-c Ni") #'grep-change-to-grep-edit-mode) ;i
  ;;   (advice-add 'grep-change-to-grep-edit-mode :after #'meow--switch-to-normal)
  ;;   (advice-add 'grep-edit-save-changes :after #'meow--switch-to-normal)


  ;;   (define-key grep-edit-mode-map (kbd "C-c N/") #'consult-focus-lines)
  ;;   (define-key grep-edit-mode-map (kbd "C-c Nz") #'consult-hide-lines)
  ;;   (define-key grep-edit-mode-map (kbd "C-x C-s") 'grep-edit-save-changes)
  ;;   (define-key grep-edit-mode-map (kbd "M-n") 'compilation-next-error)
  ;;   (define-key grep-edit-mode-map (kbd "M-p") 'compilation-previous-error)
  ;;   (define-key grep-edit-mode-map (kbd "M-s-n") 'compilation-next-file)
  ;;   (define-key grep-edit-mode-map (kbd "M-s-p") 'compilation-previous-file))
  )


(with-eval-after-load 'wgrep
  (add-hook 'wgrep-mode-hook 'vmacs-grep-mode-hook)
  (setq-default wgrep-auto-save-buffer nil ;真正的打开文件，会处理各种find-file save-file的hook,慢，如gofmt引入package
              wgrep-too-many-file-length 1
              wgrep-enable-key "C-c Mi"
              wgrep-change-readonly-file t)
  (define-key wgrep-mode-map (kbd "C-g") 'wgrep-abort-changes)
  (define-key wgrep-mode-map (kbd "C-c C-c") 'vmacs-wgrep-finish-edit)
  (define-key wgrep-mode-map (kbd "C-x C-s") 'vmacs-wgrep-finish-edit)
  (define-key wgrep-mode-map (kbd "M-n") 'compilation-next-error)
  (define-key wgrep-mode-map (kbd "M-p") 'compilation-previous-error)
  (define-key wgrep-mode-map (kbd "M-s-n") 'compilation-next-file)
  (define-key wgrep-mode-map (kbd "M-s-p") 'compilation-previous-file)
  (defun vmacs-wgrep-finish-edit()
    (interactive)
    (if  current-prefix-arg
        (let ((wgrep-auto-save-buffer t))
          (call-interactively #'wgrep-finish-edit)
          )
      (call-interactively #'wgrep-finish-edit)
      (let ((count 0))
        (dolist (b (buffer-list))
          (with-current-buffer b
            (when (buffer-file-name)
              (let ((ovs (wgrep-file-overlays)))
                (when (and ovs (buffer-modified-p))
                  (basic-save-buffer)
                  ;; (kill-this-buffer) ;for xref project-find-regexp
                  (setq count (1+ count)))))))
        (cond
         ((= count 0)
          (message "No buffer has been saved."))
         ((= count 1)
          (message "Buffer has been saved."))
         (t
          (message "%d buffers have been saved." count))))))
  )


(defvar my/re-builder-positions nil  "Store point and region bounds before calling re-builder")
;; https://karthinks.com/software/bridging-islands-in-emacs-1/
(with-eval-after-load 're-builder
  (setq reb-re-syntax 'string)          ; read/string/rx 写正则的时候  group写成\(\) 而非默认的 \\(\\)
  (define-advice re-builder (:around (orig-fun &rest args) query-replace)
    (setq my/re-builder-positions
          (cons (point)
                (if  (region-active-p)
                    (list (region-beginning)
                          (region-end))
                  (list (point-min)
                        (point-max)))))
    (apply orig-fun args)
    (message "Return:Replace,C-cC-k:quit C-r:()    g:all, c:act e:edit replacement C-e:临时退出 C-cC-c:恢复"))

  (define-key reb-mode-map (kbd "M-n") #'reb-next-match)
  (define-key reb-mode-map (kbd "M-p") #'reb-prev-match)
  (define-key reb-mode-map (kbd "C-r") #'(lambda()(interactive) (insert "\\(\\)") (backward-char 2)))
  ;; 可以使用 \(.*\)  后使用\1 来指定原始group值
  ;; Define
  (define-key reb-mode-map (kbd "RET") #'reb-replace-regexp)
  (define-key reb-mode-map [(control return) ] #'reb-replace-regexp)
  (define-key reb-mode-map (kbd "C-c C-c") #'reb-replace-regexp)
  (define-key reb-mode-map (kbd "C-c C-t") #'reb-toggle-case)
  ;; (define-key reb-lisp-mode-map (kbd "RET") #'reb-replace-regexp) # rx 模式
  (define-key reb-mode-map (kbd "C-c C-k") #'reb-quit))

;;
(add-hook 'isearch-mode-hook (lambda()(require 'xref)
                               (if (equal emacs-major-version 30)
                                   (xref--push-markers (current-buffer) (point) )
                                 (xref--push-markers (current-buffer) (point) (selected-window))
                                 )))
(setq isearch-lazy-count t)
(setq lazy-highlight-cleanup nil)
(setq isearch-wrap-pause 'no)
(define-key isearch-mode-map  (kbd "M-n")   'isearch-forward-thing-at-point)
(define-key isearch-mode-map  (kbd "M-p")   'consult-isearch-history)
(define-key isearch-mode-map  (kbd "C-f")   'isearch-yank-word-or-char)
(define-key isearch-mode-map  (kbd "C-,")   'isearch-beginning-of-buffer)
(define-key isearch-mode-map  (kbd "C-.")   'isearch-end-of-buffer)
(define-key isearch-mode-map  (kbd "M-t")   'isearch-toggle-regexp)
(define-key isearch-mode-map  (kbd "C-e")   'isearch-edit-string)
(global-set-key (kbd "C-c w c") 'toggle-case-fold-search)
(global-set-key (kbd "C-c C-s") 'toggle-case-fold)
(setq isearch-message-prefix-add "(C-t:rx C-e:edit M-c/C-cC-s:case)")
(global-set-key (kbd "C--")   #'(lambda() (interactive)(insert "_")))
(defun vmacs-isearch-insert_()
  (interactive)
  (isearch-printing-char ?_))
(define-key isearch-mode-map  (kbd "C--")   'vmacs-isearch-insert_)


;; (add-hook 'isearch-mode-end-hook
;;           (lambda()
;;             (when isearch-success ; 只有在搜索成功时
;;               (set-mark isearch-other-end)
;;               (activate-mark))))

(with-eval-after-load 'isearch (define-key isearch-mode-map [escape] 'isearch-abort))
;; isearch lazy counter
(defvar vmacs--search-indicator-overlay nil
  "Overlays used to display search indicator in current line.")

(defvar-local vmacs--search-indicator-state nil
  "The state for search indicator.

Value is a list of (last-regexp last-pos idx cnt).")
(defface vmacs-search-indicator
  '((((class color) (background dark))
     ( :foreground "yellow"))
    (((class color) (background light))
     :background "blue" :foreground "green yellow"))
  "Face for search indicator."
  :group 'vmacs)

(defun vmacs--lazy-count-hook ()
    (save-mark-and-excursion
      (vmacs--remove-search-indicator)
      (when isearch-lazy-count-current
        (vmacs--show-indicator (point) (isearch-lazy-count-format)))
      ;; If the current search hits one, then there is no need to highlight it.
      (when (number-or-marker-p isearch-success)
        (dolist (ov isearch-lazy-highlight-overlays)
          (let ((ov-start (overlay-start ov))
                (ov-end (overlay-end ov)))
            ;; Check if point `isearch-success' is within the overlay region.
            (when (and (<= ov-start isearch-success) (>= ov-end isearch-success))
              (overlay-put ov 'priority 0)))))))

  (add-hook 'lazy-count-update-hook #'vmacs--lazy-count-hook)

(defun vmacs--remove-search-indicator ()
  (vmacs--remove-search-highlight)
  (vmacs--clean-search-indicator-state))

(defun vmacs--clean-search-indicator-state ()
  (setq vmacs--search-indicator-overlay nil
        vmacs--search-indicator-state nil))

(defun vmacs--remove-search-highlight ()
  (when vmacs--search-indicator-overlay
    (delete-overlay vmacs--search-indicator-overlay)))

(defun vmacs--show-indicator (pos msg)
  (goto-char pos)
  (goto-char (line-end-position))
  (setq msg (string-trim-right msg))
  (if (= (point) (point-max))
      (let ((ov (make-overlay (point) (point))))
        (overlay-put ov 'after-string (propertize (format " %s " msg) 'face 'vmacs-search-indicator))
        (setq vmacs--search-indicator-overlay ov))
    (let ((ov (make-overlay (point)  (1+ (point)))))
      (overlay-put ov 'display (propertize (format " %s\n" msg) 'face 'vmacs-search-indicator))
      (setq vmacs--search-indicator-overlay ov))))
;; end of isearch (from meow)

(defun vmacs-isearch-unhighlight ()
  (isearch-dehighlight)
  (lazy-highlight-cleanup t)
  (vmacs--remove-search-indicator))

(advice-add 'keyboard-quit :before #'vmacs-isearch-unhighlight)


;;
(global-set-key (kbd "C-c C-c") #'exit-recursive-edit) ;query-replace C-r临时退出replace 后，可C-cC-c 继续replace

;; https://emacs.stackexchange.com/questions/80484/query-replace-ignore-events-not-binded-in-query-replace-map
(defvar vmacs-do-nothing-map
  (let ((map (make-keymap)))
    (set-char-table-range (nth 1 map) t 'ignore)
    map))
(set-keymap-parent query-replace-map vmacs-do-nothing-map)
(define-key query-replace-map "g" 'automatic) ;old ! replace all automatic
(define-key query-replace-map "p" 'backup)
(define-key query-replace-map "c" 'act)     ;old y
(define-key query-replace-map "\C-e" 'edit) ;临时退出
(setq query-replace-read-from-default #'vmacs-query-replace-read-from-default)
(defvar vmacs-query-replace-read-from-def nil)
(defun vmacs-query-replace-read-from-default()
  (if (eq this-command 'vmacs-replace-all)
      vmacs-query-replace-read-from-def
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (thing-at-point 'symbol))))


(provide 'conf-search)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-search.el ends here.
