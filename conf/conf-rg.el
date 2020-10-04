;; https://github.com/dajva/rg.el
;; doc https://rgel.readthedocs.io
;;; Code:
(require 'rg)
(setq rg-show-header t)
(setq rg-command-line-flags '("-z" "--pcre2"))
(setq rg-group-result nil)
(evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>g") rg-global-map)
(define-key rg-global-map (kbd "C-.") #'vmacs-rg-dwim-current-dir)
(define-key rg-global-map (kbd ".") #'vmacs-rg-dwim-current-dir)
(define-key rg-global-map (kbd ",") #'vmacs-rg-dwim-project-dir)
(define-key rg-global-map "g" #'vmacs-rg-word-current-dir)
(define-key rg-global-map "p" #'vmacs-rg-word-root-dir)
(define-key rg-global-map "m" #'rg-menu)
;; (rg-define-toggle "-w" "w" t) ;word edge ,等同于前后加\b
;; 0宽断言
;; https://leongfeng.github.io/2017/03/10/regex-java-assertions/
;;  '(?<![a-zA-Z0-9_-])(world)(?![a-zA-Z0-9-_])' ;
;; 搜索world ，只是它前后不能包含a-z等字符， 基本等价于\bworld\b
;; 但是\b 对于_- 也当作单词边界,并不符合我的预期
;;
;; 利用0宽断言作多关键字匹配，
;; rg --pcre2    '(defun).*(?=vmacs-rg-query).*(?=val)'
(defun vmacs-rg-query(&optional val wrap)
  (print (prefix-numeric-value current-prefix-arg))
  (unless val (setq val (thing-at-point 'symbol)))
  (if wrap
      (format "%s%s%s" "(?<![a-zA-Z0-9_-])" val "(?![a-zA-Z0-9-_])")
    val))

;; 默认 使用有单词边界的正则，C-u 使用非正则，C-uC-u 使用用户输入的正则（不含边界）
(rg-define-search vmacs-rg-word-current-dir
  :query (vmacs-rg-query (rg-read-pattern  (= 4 (prefix-numeric-value current-prefix-arg)))
                         (= 1 (prefix-numeric-value current-prefix-arg)))
  :format (= 4 (prefix-numeric-value current-prefix-arg))      ;无prefix arg 或prefix>4 使用regex，否是非正则
  :flags ("--type=all ")
  :files current :dir current)
(rg-define-search vmacs-rg-word-root-dir
  :query (vmacs-rg-query (rg-read-pattern  (= 4 (prefix-numeric-value current-prefix-arg)))
                         (= 1 (prefix-numeric-value current-prefix-arg)))
  :format (= 4 (prefix-numeric-value current-prefix-arg))      ;无prefix arg 或prefix>4 使用regex，否是非正则
  :flags ("--type=all")
  :files current :dir project)

(rg-define-search vmacs-rg-dwim-current-dir
  "Search for thing at point in files matching the current file
under the current directory."
  :query (vmacs-rg-query nil (= 1 (prefix-numeric-value current-prefix-arg)))
  :format (= 4 (prefix-numeric-value current-prefix-arg))      ;无prefix arg 或prefix>4 使用regex，否是非正则
  :flags ("--type=all")
  :files current
  :dir current)

(rg-define-search vmacs-rg-dwim-project-dir
  "Search for thing at point in files matching the current file
under the project root directory."
  :query (vmacs-rg-query nil (= 1 (prefix-numeric-value current-prefix-arg)))
  :format (= 4 (prefix-numeric-value current-prefix-arg))      ;无prefix arg 或prefix>4 使用regex，否是非正则
  :flags ("--type=all")
  :files current
  :dir project)

;; c toggle case
(defun vmacs-rg-hook()
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)
  (setq-local compilation-scroll-output 'first-error)
  (setq-local compilation-always-kill t)
  (define-key rg-mode-map "g" nil)
  (define-key rg-mode-map "e" nil)
  (define-key rg-mode-map "i" nil)
  (define-key rg-mode-map "s" nil)
  (define-key rg-mode-map "l" nil)
  (define-key rg-mode-map "h" nil)
  (define-key rg-mode-map (kbd "M-n") 'next-error-no-select)
  (define-key rg-mode-map (kbd "M-p") 'previous-error-no-select)
  (define-key rg-mode-map "n" 'compilation-next-error)
  (define-key rg-mode-map "p" 'compilation-previous-error)
  (define-key rg-mode-map (kbd "L") 'rg-forward-history)
  (define-key rg-mode-map (kbd "H") 'rg-back-history)
  (define-key rg-mode-map (kbd "M-p") 'previous-error-no-select)
  (define-key rg-mode-map "I" #'rg-rerun-toggle-ignore)
  (define-key rg-mode-map (kbd "z") 'rg-occur-hide-lines-matching)
  (define-key rg-mode-map (kbd "/") 'rg-occur-hide-lines-not-matching)
  (evil-define-key 'normal 'local "gr" 'rg-recompile))

(add-hook 'rg-mode-hook #'vmacs-rg-hook)

;;;###autoload
(defun rg-occur-hide-lines-not-matching (search-text)
  "Hide lines that don't match the specified regexp."
  (interactive "MHide lines not matched by regexp: ")
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (save-excursion
    (goto-char (point-min))
    (forward-line 5)
    (let ((inhibit-read-only t)
          line)
      (while (not (looking-at-p "^\nrg finished "))
        (setq line (buffer-substring-no-properties (point) (point-at-eol)))
        (if (string-match-p search-text line)
            (forward-line)
          (when (not (looking-at-p "^\nrg finished "))
            (delete-region (point) (1+ (point-at-eol)))))))))

;;;###autoload
(defun rg-occur-hide-lines-matching  (search-text)
  "Hide lines matching the specified regexp."
  (interactive "MHide lines matching regexp: ")
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (save-excursion
    (goto-char (point-min))
    (forward-line 5)
    (let ((inhibit-read-only t)
          line)
      (while (not (looking-at-p "^\nrg finished "))
        (setq line (buffer-substring-no-properties (point) (point-at-eol)))
        (if (not (string-match-p search-text line))
            (forward-line)
          (when (not (looking-at-p "^\nrg finished "))
            (delete-region (point) (1+ (point-at-eol)))))))))

;;wgrep
;; (add-hook 'grep-setup-hook 'grep-mode-fun)
(setq-default wgrep-auto-save-buffer nil ;真正的打开文件，会处理各种find-file save-file的hook,慢，如gofmt引入package
              wgrep-too-many-file-length 1
              ;; wgrep-enable-key "i"
              wgrep-change-readonly-file t)

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
                (kill-this-buffer)
                (setq count (1+ count)))))))
      (cond
       ((= count 0)
        (message "No buffer has been saved."))
       ((= count 1)
        (message "Buffer has been saved."))
       (t
        (message "%d buffers have been saved." count))))))

(with-eval-after-load 'wgrep
  (define-key wgrep-mode-map (kbd "C-g") 'wgrep-abort-changes)
  (define-key wgrep-mode-map (kbd "C-c C-c") 'vmacs-wgrep-finish-edit))

(defun enable-wgrep-when-entry-insert()
  (when (derived-mode-p 'ivy-occur-mode 'rg-mode 'grep-mode
                        'ivy-occur-grep-mode 'helm-grep-mode)
    (wgrep-change-to-wgrep-mode)
    (when (equal last-command 'iedit-mode)
      ;; 恢复iedit bug导致rg iedit在进入wgrep 模式下 iedit 消失
      (run-with-timer 0.001 nil 'iedit-mode '(4)))))

(add-hook 'evil-insert-state-entry-hook 'enable-wgrep-when-entry-insert)

(provide 'conf-rg)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-rg.el ends here.
