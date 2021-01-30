;;; -*- lexical-binding: t; -*-
;; -*- coding:utf-8 -*-
;; eval-when-compile
(eval-when-compile
  (require 'savehist)
  (require  'cl-seq)
  (require  'ediff)
  (require  'vc-hooks)
  (require 'recentf)
  (require 'saveplace)
  (require  'log-edit)
  (require  'org)
  (require  'ibuffer)
  (require  'log-view)
  (require 'cc-mode)
  (require 'lazy-camelize)
  ;;(require 'hippie-exp)
  )

(declare-function org-end-of-line "org")
(declare-function org-beginning-of-line "org")
(declare-function org-kill-line "org")
(declare-function upcase-first-char "lazy-camelize")

;;;###autoload
(defun vmacs-idle-timer()
  (require 'savehist)
  (savehist-autosave)
  (require 'recentf)
  (recentf-save-list)
  (require 'saveplace)
  (save-place-kill-emacs-hook)
  ;;recentf-save-list里有bug会导致cursor恢复成默认color,而不能与evil配置的cursor相配
  (evil-refresh-cursor)

  (message ""))


;;vim 有o 与O 命令，用于在下一行与上一行插入一个空行，并定位光标到空格后
;; 此功能类似，当光标在行首时，则在上一行添加空行，当在行尾时，则在下一行添加空行
;; 否则将当前行分成两行
;; 即只需要与C-a与C-e命令合并使用即可实现o O的功能，
;; 即C-aC-j=o,C-eC-j=O(假如此命令绑定到C-j的话)
;;;###autoload
(defun open-line-or-new-line-dep-pos()
  "binding this to `C-j' if point is at head of line then
open-line if point is at end of line , new-line-and-indent"
  (interactive)
  (let ((pos))
    (if (or (and (= (point) (point-at-bol))
                 (not (looking-at "^[ \t]*$")))
            (looking-back "^[ \t]*" (point-at-bol)))
        (progn
          (open-line 1)
          (indent-for-tab-command)
          (setq pos (point))
          (forward-line)
          (indent-for-tab-command)
          (goto-char pos))
      (when (member last-command '(evil-open-line-or-new-line-dep-pos
                                   move-end-of-line
                                   evil-move-end-of-line
                                   smart-end-of-line))
        (end-of-line))
      (newline-and-indent))))


;; 若光标不在行首则跳转到行首，若在行首则跳转到行首第一个非空字符处
;; 一般绑在C-a上
;;;###autoload
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
Move point to beginning-of-line ,if point was already at that position,
  move point to first non-whitespace character. "
  (interactive)
  (cond
   ((derived-mode-p 'vterm-mode)
    (vterm-beginning-of-line))
   ((derived-mode-p 'eshell-mode)
    (let ((oldpos (point)))
      (eshell-bol)
      (and (= oldpos (point))
           (beginning-of-line) ))
    )
   (t
    (let ((oldpos (point)))
      (beginning-of-line)
      (and (= oldpos (point))
           (back-to-indentation) )))))


;; 若光标不在行首则跳转到行首，若在行首则跳转到行首第一个非空字符处
;; 一般绑在C-a上
;;;###autoload
(defun org-mode-smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
Move point to beginning-of-line ,if point was already at that position,
  move point to first non-whitespace character. "
  (interactive)
  (let ((oldpos (point)))
    (org-beginning-of-line)
    (and (= oldpos (point))
         (back-to-indentation) )))
;; 一般绑定在C-e上，跳转到行尾
;; 如果当前行是个非常长的行，以至于一屏显示不下，
;; 则此命令只会向后移动一屏的距离而不是移到行尾
;; C-uC-e则直接跳到行尾
;;;###autoload
(defun smart-end-of-line(&optional arg)
  "like `org-end-of-line' move point to
   virtual end of line
or Move point to end of line (ignore white space)
or end-of-line.
Move point to end-of-line ,if point was already at end of line (ignore white space)
  move point to end of line .if `C-u', then move to end of line directly."
  (interactive "^P")
  (if (equal major-mode 'vterm-mode)
      (vterm-end-of-line)
    (if arg (end-of-line)
      (let ((oldpos (point)) (new-pos)) (beginning-of-line) (if (re-search-forward "[ \t]*$" (point-at-eol) t 1) (setq new-pos  (match-beginning 0)) (setq new-pos (point-at-eol)))
           (when (= oldpos new-pos)
             (setq new-pos (point-at-eol))
             )
           (when (> new-pos (+ (frame-width) oldpos))
             (setq new-pos (+ (frame-width) oldpos)))
           (goto-char new-pos)
           )
      )
    )
  )

;; 同smart-end-of-line
;;;###autoload
(defun org-mode-smart-end-of-line()
  "Move point to first non-whitespace character or end-of-line.
Move point to end-of-line ,if point was already at that position,
  move point to first non-whitespace character."
  (interactive)
  (let ((oldpos (point)))
    (org-end-of-line)
    (if  (equal (point-at-eol) (point))
        (progn
          (beginning-of-line)
          (when (re-search-forward "[ \t]*$" (point-at-eol) t)
            (goto-char (match-beginning 0)))
          )
      (when (equal oldpos (point))
        (end-of-line)
        )
      )
    ))

;;;###autoload
;; (defun kill-syntax-forward ()
;;   "Kill characters with syntax at point."
;;   (interactive)
;;   (kill-region (point)
;;                (progn (skip-syntax-forward (string (char-syntax (char-after))))
;;                       (point))))
;;;###autoload
;; (defun kill-syntax-backward ()
;;   "Kill characters with syntax at point."
;;   (interactive)
;;   (kill-region (point)
;;                (progn (skip-syntax-backward (string (char-syntax (char-before))))
;;                       (point))))

;; ;;;###autoload
;; (defun vmacs-jump-to-space-forward()
;;   (interactive)
;;   (let ((old-pos (point))
;;         m-end m-begin
;;         )
;;     (when (re-search-forward "[ \t]+"  nil t)
;;       (setq m-begin (match-beginning 0))
;;       (setq m-end (match-end 0))
;;       (goto-char m-begin)
;;       (if (equal old-pos m-end)
;;           (progn
;;             (re-search-forward "[ \t]+"  nil t)
;;             (goto-char (match-beginning 0)))
;;         (if (equal m-begin old-pos)
;;             (goto-char m-end)
;;             )))))

;; 绑定在C-xC-v上，切换成*scratch* 中，若已经在*scratch*中，则切换成上一个buffer
;;;###autoload
(defun switch-to-scratch-buffer ()
  "Toggle between *scratch* buffer and the current buffer.
     If the *scratch* buffer does not exist, create it."
  (interactive)
  (let ((scratch-buffer-name  "*scratch*"))
    (if (equal (buffer-name (current-buffer)) scratch-buffer-name)
        (switch-to-buffer (other-buffer))
      (with-current-buffer
          (switch-to-buffer  scratch-buffer-name)
        ;; (when (functionp prev-major-mode) (funcall prev-major-mode ))
        (when (equal major-mode 'fundamental-mode )(emacs-lisp-mode))
        (goto-char (point-max))))))


;; ;;;###autoload
;; (defun move-backward-paren()
;;   (interactive)
;;   (re-search-backward "\\s[\\|\\s(\\|\\s{" nil t)
;;   )

;; ;;;###autoload
;; (defun move-forward-paren()
;;   (interactive)
;;   (re-search-forward "\\s]\\|\\s)\\|\\s}" nil t)
;;   )


;;;;;###autoload
;; (defun query-stardict ()
;;   "Serch dict in stardict."
;;   (interactive)
;;   (let ((begin (point-min))
;;         (end (point-max)))
;;     (if mark-active
;;         (setq begin (region-beginning)
;;               end (region-end))
;;       (save-excursion
;;         (backward-word)
;;         (mark-word)
;;         (setq begin (region-beginning)
;;               end (region-end))))
;;     (message "searching  %s ... using stardicr" (buffer-substring begin end))
;;     (shell-command "notify-send \"`sdcv -n -u '朗道英汉字典5.0' %s`\"" (buffer-substring begin end) )
;;     (message "finished searching  朗道英汉字典5.0'")
;;     ))

;; linux 上使用星际译王的命令行版sdcv 进行翻译
;;;###autoload
(defun sdcv-to-buffer ()
  "Search dict in region or world."
  (interactive)
  (let* ((word (if mark-active
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (current-word nil t)))
         (buf-name (buffer-name))
         (mp3-file (concat "/usr/share/OtdRealPeopleTTS/" (downcase (substring word 0 1 )) "/" word ".mp3"))
         )
    ;; (setq word (read-string (format "Search the dictionary for (default %s): " word)
    ;;                         nil nil word))
    (set-buffer (get-buffer-create "*sdcv*"))
    (buffer-disable-undo)
    (erase-buffer)
    (when (file-exists-p mp3-file)(shell-command (concat "mpg123 "  mp3-file " >/dev/null 2>/dev/null")))
    (insert (shell-command-to-string  (format "sdcv --data-dir %s --utf8-input --utf8-output -n %s " (expand-file-name "~/.emacs.d/bin/sdcv/dic/")  word)))
    ;;
    (if (equal buf-name "*sdcv*")
        (switch-to-buffer "*sdcv*")
      (pop-to-buffer "*sdcv*" t nil))
    (goto-char (point-min))
    ))

;; (shell-command "notify-send \"`sdcv -n  %s`\"" (buffer-substring begin end))
;; (tooltip-show
;;      (shell-command-to-string
;;       (concat "sdcv -n "
;;               (buffer-substring begin end))))

;;只留光标处一个空格或者删除所有空格(按一次与按两次效果不同)
;;;###autoload
(defun just-one-space-or-delete-horizontal-space()
  "just one space or delete all horizontal space."
  (interactive)
  (if (equal last-command 'just-one-space-or-delete-horizontal-space)
      (delete-horizontal-space)
    (just-one-space)
    )
  (message "just-one-space or delete-horizontal-space(if press twice)")
  )

;; (defsubst at-term-end-of-line()            ;跳过后面的空格等，判断是否在最后一行
;;   (let ((p (point)) e)
;;     (goto-char (point-max))
;;     (when (re-search-backward "[ \t]+\\|\n" nil 'move 1)
;;       (goto-char (match-beginning 0))) ;skip space
;;     (setq e (equal (line-number-at-pos p)(line-number-at-pos )))
;;     (goto-char p)                       ;go back to init pos
;;     e))

;;如果有选中区域,则kill选区,否则删除当前行
;;注意当前行并不代表整行,它只删除光标到行尾的内容,也就是默认情况下
;;C-k 所具有的功能
;;;###autoload
(defun vmacs-kill-region-or-line(&optional arg)
  "this function is a wrapper of (kill-line).
   When called interactively with no active region, this function
  will call (kill-line) ,else kill the region."
  (interactive "P")
  (if mark-active
      (if (= (region-beginning) (region-end) ) (kill-line arg)
        (kill-region (region-beginning) (region-end) )
        )
    (kill-line arg)
    )
  )
;;;###autoload
(defun vmacs-kill-region-or-org-kill-line(&optional arg)
  "this function is a wrapper of (kill-line).
   When called interactively with no active region, this function
  will call (kill-line) ,else kill the region."
  (interactive "P")
  (if mark-active
      (if (= (region-beginning) (region-end) ) (org-kill-line arg)
        (kill-region (region-beginning) (region-end) )
        )
    (org-kill-line arg)
    )
  )
;; ;;;;(global-unset-key "\C-w")  ;C-k 现在完全具有C-w的功能, 所以取消C-w的键定义
;; (defvar vmacs-trailing-whitespace-modes '(c++-mode c-mode haskell-mode emacs-lisp-mode scheme-mode erlang-mode))
;; ;;;###autoload
;; (defun vmacs-trailing-whitespace-hook ()
;;   (when (member major-mode vmacs-trailing-whitespace-modes)
;;     (delete-trailing-whitespace)))

;; (defvar vmacs-untabify-modes '(haskell-mode lisp-mode scheme-mode erlang-mode clojure-mode java-mode ))

;; ;;;###autoload
;; (defun vmacs-untabify-hook ()
;;   (when (member major-mode vmacs-untabify-modes)
;;     (untabify (point-min) (point-max))))

;; kill buffer的包装，对于emacsclient连上来的buffer,则表示编辑完了
;; 退出emacsclient,否则就是普通的关闭文件
(autoload 'server-edit "server")
;;;###autoload
(defun vmacs-kill-buffer-dwim(&optional buf)
  (interactive)
  (with-current-buffer (or buf (current-buffer))
    (cond
     ((equal (buffer-name buf) "*scratch*")
      (copy-region-as-kill (point-min)(point-max))
      (kill-this-buffer))
     ((and (featurep 'server)
           (boundp 'server-buffer-clients)
           server-buffer-clients)
      (server-edit))
     ((derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)
      (kill-this-buffer)
      ;; (vterm-toggle-hide )
      ;; (vmacs-add-to-killed-file-list)
      ;; (centaur-tabs-forward-group)
      ;; (centaur-tabs-backward-group)
      )
     ((equal major-mode 'magit-status-mode)
      (magit-mode-bury-buffer 16))
     ( (derived-mode-p 'magit-mode)
       (let ((process (magit-section-value-if 'process)))
         (if (and process
                  (eq (process-status process) 'run))
             (magit-mode-bury-buffer)
           (magit-mode-bury-buffer 4))))
     ( (derived-mode-p 'org-agenda-mode)
       (dolist (f org-agenda-files)
         (kill-buffer (get-file-buffer f)))
       (kill-this-buffer)
       )
     ( (derived-mode-p 'calc-mode)
       (call-interactively 'calc-quit))
     ( (derived-mode-p 'diff-mode)
       (call-interactively 'kill-buffer-and-window))
     ( (derived-mode-p 'Info-mode)
       (call-interactively 'kill-buffer-and-window))
     ( (derived-mode-p 'special-mode)
       (if (get-buffer-process buf)
           (bury-buffer-and-window)
         (kill-buffer-and-window)))
     (t
      (message "kill buffer %s" (buffer-name buf))
      (kill-this-buffer)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar vmacs-killed-file-list nil
  "List of recently killed files.")
(defun vmacs-add-to-killed-file-list()
  "If buffer is associated with a file name, add that file to the
`vmacs-killed-file-list' when killing the buffer."
  (let ((default-directory default-directory))
    (cond
     (buffer-file-name
      (unless (or (string-match-p "COMMIT_EDITMSG" buffer-file-name)
                  (string-match-p "/cache/recentf" buffer-file-name))
        (push buffer-file-name vmacs-killed-file-list)))
     ((equal major-mode 'magit-status-mode)
      (put-text-property 0 1 'type 'magit default-directory))
     ((member major-mode '(eshell-mode term-mode ))
      (push (current-buffer) vmacs-killed-file-list))
     ((equal major-mode 'dired-mode)
      (push default-directory vmacs-killed-file-list)))))



(add-hook 'kill-buffer-hook #'vmacs-add-to-killed-file-list)

;;;###autoload
(defun vmacs-undo-kill-buffer()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (when vmacs-killed-file-list
    (let* ((file (pop vmacs-killed-file-list))
           type)
      (while (and file (bufferp file) (not (buffer-live-p file)))
        (setq file (pop vmacs-killed-file-list)))
      (when file
        (when (stringp file)
          (setq type (get-text-property 0 'type file)))
        (cond
         ((bufferp file)
          (switch-to-buffer file))
         ((equal type 'magit)
          (magit-status-setup-buffer file)
          (message "reopen magit-status in %s" file))

         (t
          (find-file file)
          (message "reopen file: %s" file))
         )
        )
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(defun kill-other-buffers ()
  "kill all buffer which not showing in window."
  (interactive)
  (mapc 'kill-buffer
        (cl-remove-if 'get-buffer-window (buffer-list)))
  (message "all other buffers are killed."))

;;;###autoload
(defun bury-buffer-and-window()
  "bury buffer and window"
  (interactive)
  (bury-buffer)
  (when (< 1 (count-windows))
    (delete-window)))

;; ;;;###autoload
;; (defun keyboard-quit-or-bury-buffer-and-window()
;;   "C-gC-g (bury buffer and window)"
;;   (interactive)
;;   (if (equal last-command 'keyboard-quit)
;;       (bury-buffer-and-window)
;;     (setq this-command 'keyboard-quit)
;;     (call-interactively 'keyboard-quit)
;;     )
;;   )

;; 在当前行任何位置输入分号都在行尾添加分号，除非本行有for 这个关键字，
;; 如果行尾已经有分号则删除行尾的分号，将其插入到当前位置,就是说输入两次分号则不在行尾插入而是像正常情况一样.
;;;###autoload
(defun vmacs-append-semicolon-at-eol(&optional _arg)
  (interactive "*p")
  (let* ( ( init_position (point))
          (b (line-beginning-position))
          (e (line-end-position))
          (line_str (buffer-substring b e))
          (semicolon_end_of_line (string-match ";[ \t]*$" line_str ))
          )
    (if semicolon_end_of_line ;;;;如果行尾已经有分号，则删除行尾的分号，并在当前位置输入分号;;;;;;
        (progn
          (save-excursion
            (goto-char (+ semicolon_end_of_line b))
            (delete-char 1) )
          (insert ";") )
      ;;在整行内容中搜索有没有关键字for的存在,或者当前位置已经是行尾,直接插入分号
      (if   (or (string-match "^[ \t]*$" (buffer-substring init_position e))
                (string-match "\\bfor\\b" line_str))
          (insert ";")
        (save-excursion ;;如果搜索不到 for 则在行尾插入分号;
          (end-of-line)
          (delete-trailing-whitespace)
          (insert ";")
          )))))
;;代码注释工作，如果有选中区域，则注释或者反注释这个区域
;;如果，没选中区域，则注释或者注释当前行，如果光标在行末，则在行末添加或删除注释
;;;###autoload
(defun vmacs-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at the end of the line,
then comment current line.
Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;; "run ediff with marked buffer in ibuffer mode
;; 如果有两个marked 的buffer,对这两个进行ediff ,默认在merge 模式,
;; `C-u'的话,即普通的ediff,不进行merge
;; 如果是mark了三个buffer ,则会让你选哪一个是ancestor(祖先),然后进行三方合并
;; `C-u'的话,不进行合并,仅进行三方合并"
;;;###autoload
(defun ibuffer-ediff-merge(&optional arg)
  (interactive "P")
  (let ((marked-buffers  (ibuffer-marked-buffer-names))
        ancestor
        )
    (cond ((= (length marked-buffers) 2)
           (if arg
               (ediff-buffers (car marked-buffers) (nth 1 marked-buffers))
             (ediff-merge-buffers  (car marked-buffers) (nth 1 marked-buffers)))
           )
          ((= (length marked-buffers) 3)
           (setq ancestor (completing-read
                           "which is ancestor (for  Ediff 3 merge):" marked-buffers
                           nil nil nil nil (last marked-buffers)))
           (setq marked-buffers (delete ancestor marked-buffers))
           (if arg
               (ediff-buffers3 (car marked-buffers )(nth 1 marked-buffers) ancestor)
             (ediff-merge-buffers-with-ancestor (car marked-buffers )(nth 1 marked-buffers) ancestor)))
          (t (call-interactively 'ediff-buffers))
          )))

;;;###autoload
(defun minibuffer-quit ()
  "Quit the minibuffer command, even when the minibuffer loses focus."
  (interactive)
  (when (active-minibuffer-window)
    (save-window-excursion
      (select-window (minibuffer-window))
      (keyboard-escape-quit))))

;;;###autoload
(defun minibuffer-refocus ()
  "Refocus the minibuffer if it is waiting for input."
  (interactive)
  (when (active-minibuffer-window)
    (message "") ;; clear the echo area, in case it overwrote the minibuffer
    (select-window (minibuffer-window))))

                                        ;convert a buffer from dos ^M end of lines to unix end of lines
;;;###autoload
(defun dos2unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match "")))

;;;###autoload
(defun unix2dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "\r\n")))

;; mac上打开iterm2，并cd到当前编辑的文件所在目录
;;;###autoload
(defun cd-iterm2()
  (interactive)
  (let ((cmd (format "
tell application \"iTerm\"
	activate
	if (count of windows) = 0 then
		set w to (create window with default profile)
		delay 0.3
	else
		set w to current window
	end if

	tell w
		set targetSession to null

		activate current session
		tell current session of w
			if is at shell prompt then
				set targetSession to current session of w
			end if
		end tell
		if targetSession is null then
			repeat with aTab in tabs
				if targetSession is null then
					tell aTab
						select
						repeat with aSession in sessions
							if targetSession is null then
								tell aSession
									select
									if is at shell prompt then
										set targetSession to aSession
									end if
								end tell
							end if
						end repeat
					end tell
				end if
			end repeat
		end if
		if targetSession is null then
			create tab with default profile
			delay 0.1
			set targetSession to current session of w
		end if

		if targetSession is not null then
			tell targetSession
				select
				set cmd to \" cd \" & quote & \"%s\" & quote & \"; clear\"
				write text cmd
			end tell

		end if
	end tell
end tell
" (expand-file-name default-directory))))
    (ns-do-applescript cmd)))

;; mac上打开iterm2，并cd到当前编辑的文件所在目录
;;;###autoload
(defun cd-iterm2-new-tab()
  (interactive)
  (let ((cmd (format "tell application \"iTerm\"
    activate
    if (count of windows) = 0 then
        set w to (create window with default profile)
    else
        set w to current window
    end if
    tell w
        create tab with default profile
        tell current session of w
            set cmd to \"cd \" & quote & \"%s\" & quote & \";clear\"
            -- do script with command cmd
            write text cmd
            -- exec command (cmd)
        end tell

    end tell
end tell" (expand-file-name default-directory))))
    (ns-do-applescript cmd)))

;;;###autoload
(defun toggle-case-fold()
  (interactive)
  (if case-fold-search
      (progn
        (setq
         ;; helm-case-fold-search nil ;nil=case sensitive
         case-fold-search nil ;nil=case sensitive
         ;; ivy-case-fold-search-default  nil
         ;; counsel-rg-base-command  "rg -s --no-heading --line-number --color never  -z %s ."
         evil-ex-search-case 'sensitive)

        (message "case sensitive"))
    (setq
     ;; ivy-case-fold-search-default  'always
     ;; helm-case-fold-search t ;nil=case sensitive
     ;; counsel-rg-base-command  "rg -i --no-heading --line-number --color never  -z %s ."
     case-fold-search t ;nil=case sensitive
     evil-ex-search-case 'insensitive)
    (message "case insensitive")))

;;;###autoload
(defun consult-hide-lines ()
  (interactive)
  (consult-focus-lines nil (consult--completion-filter 'consult-location nil) "! "))

;;;###autoload
(defun consult-reset-lines ()
  (interactive)
  (consult-focus-lines t))


;; ;; this macro works
;; ;; (macroexpand '(with-mode-on icomplete-mode (message "ss")))
;; (defmacro with-mode-on (mode &rest body)
;;   (declare (indent defun)
;;            (doc-string 3))
;;   (macroexp-let2 nil mode-p mode
;;     `(progn
;;        (unless ,mode-p (,mode 1))
;;        ,@body
;;        (unless ,mode-p (,mode -1)))))

;; ;; (macroexpand '(with-mode-off icomplete-mode (message "ss")))
;; (defmacro with-mode-off (mode &rest body)
;;   (declare (indent defun)
;;            (doc-string 3))
;;   (macroexp-let2 nil mode-p `(bound-and-true-p ,mode)
;;     `(progn
;;        (when ,mode-p (,mode -1))
;;        ,@body
;;        (when ,mode-p (,mode 1)))))


;; ;;;###autoload
;; (defun my-complete()
;;   (interactive)
;;   (cond ((and (boundp 'company-mode) company-mode)
;;          (call-interactively 'company-complete)
;;          )
;;         ((and (boundp 'auto-complete-mode) auto-complete-mode)
;;          (call-interactively 'auto-complete)
;;          )
;;         (t
;;          (call-interactively 'hippie-expand))))


;; ;;;###autoload
;; (defun vmacs_compile_current_el_outside()
;;   (when (buffer-file-name)
;;     (let ((command (format  " emacs  -batch    -l %s -f batch-byte-compile %s "
;;                             (expand-file-name "~/.emacs.d/site-lisp/vmacs/vmacs_byte_compile_include.el")
;;                             (buffer-file-name))))
;;       (with-current-buffer (get-buffer-create "*vmacs_compile_current_el*")
;;         (insert (shell-command-to-string command)))
;;       (switch-to-buffer (get-buffer-create "*vmacs_compile_current_el*")))
;;     )
;;   )


;; ;;;###autoload
;; (defun vmacs-hide-frame()
;;   "hide current frame"
;;   (interactive)
;;   (make-frame-invisible nil t))

;; ;;;###autoload
;; (defun scroll-other-window-up-or-previous-buffer(&optional ARG)
;;   "if there is an `other-window' ,then scroll it up ,if
;;  not ,call (previous-buffer)"
;;   (interactive)
;;   (if (equal 1 (length (window-list nil nil))) ;;if don't exist other window
;;       (previous-buffer)
;;     (scroll-other-window ARG)
;;       ))

;; ;;;###autoload
;; (defun scroll-other-window-down-or-next-buffer(&optional lines)
;;   "if there is an `other-window' ,then scroll it down ,if
;;  not ,call (next-buffer)"
;;   (interactive)
;;   (if (equal 1 (length (window-list nil nil))) ;;if don't exist other window
;;       (next-buffer)
;;     (scroll-other-window-down  lines)
;;     ))

;; ;;;###autoload
;; (defun vmacs-forward-4-line() (interactive) (forward-line 4) (scroll-up   4))
;; ;;;###autoload
;; (defun vmacs-backward-4-line() (interactive) (forward-line -4)(scroll-down 4))


;; ;;;###autoload
;; (defun toggle-menu-bar-tool-bar()
;;   "toggle menu-bar and tool-bar"
;;   (interactive)
;;   (if menu-bar-mode
;;       (progn
;;         (menu-bar-mode 0)
;;         (tool-bar-mode 0)
;;         )
;;     (menu-bar-mode 1)
;;     (tool-bar-mode 1)
;;     )
;;   )
;; ;;让hipperextend不仅可以匹配开头,也可以匹配字符串的内部
;; ;;将这个函数加入到hippie-expand-try-functions-list中，
;; ;;;###autoload
;; (defun try-vmacs-dabbrev-substring (old)
;;   (let ((old-fun (symbol-function 'he-dabbrev-search)))
;;     (fset 'he-dabbrev-search (symbol-function 'vmacs-dabbrev-substring-search))
;;     (unwind-protect
;;         (try-expand-dabbrev old)
;;       (fset 'he-dabbrev-search old-fun))))

;; (defun vmacs-dabbrev-substring-search (pattern &optional reverse limit)
;;   (let ((result ())
;;         (regpat (cond ((not hippie-expand-dabbrev-as-symbol)
;;                        (concat (regexp-quote pattern) "\\sw+"))
;;                       ((eq (char-syntax (aref pattern 0)) ?_)
;;                        (concat (regexp-quote pattern) "\\(\\sw\\|\\s_\\)+"))
;;                       (t
;;                        (concat (regexp-quote pattern)
;;                                "\\(\\sw\\|\\s_\\)+")))))
;;     (while (and (not result)
;;                 (if reverse
;;                     (re-search-backward regpat limit t)
;;                   (re-search-forward regpat limit t)))
;;       (setq result (buffer-substring-no-properties (save-excursion
;;                                                      (goto-char (match-beginning 0))
;;                                                      (skip-syntax-backward "w_")
;;                                                      (point))
;;                                                    (match-end 0)))
;;       (if (he-string-member result he-tried-table t)
;;           (setq result nil)))     ; ignore if bad prefix or already in table
;;     result))


(provide 'lazy-command)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-command.el ends here.
