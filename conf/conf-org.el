;; org-mode 相关
;;(require 'ox-re-reveal)
;;(setq org-re-reveal-root (format "file://%sreveal.js-3.0.0" (expand-file-name user-emacs-directory)))
;;(setq org-re-reveal-single-file t)
;; C-cC-eRR

(vmacs-leader (kbd "t") 'org-agenda)   ;列出todo list等
(vmacs-leader (kbd "T") 'org-capture)  ;新加一个todo 条目等
(define-key evil-normal-state-map "mt" 'org-capture)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c e") 'org-edit-special)
  (define-key org-mode-map (kbd "C-c C-k") 'org-babel-remove-result-one-or-many)
  (define-key org-mode-map (kbd "<drag-n-drop>") 'vmacs-org-insert-image))

(setq verb-auto-kill-response-buffers t)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(defun vmacs-verb-handler-json ()
  "Standard handler for the \"application/json\" text content type."
  (when verb-json-use-mode
    (funcall verb-json-use-mode))
  (when (< (oref verb-http-response body-bytes)
           (or verb-json-max-pretty-print-size 0))
    (unwind-protect
        (unless (zerop (buffer-size))
          (vmacs-json-pretty))
      (buffer-enable-undo))
    (goto-char (point-min))))

(setq verb-content-type-handlers
  '(;; Text handlers
    ("text/html" html-mode)
    ("\\(application\\|text\\)/xml" xml-mode)
    ("application/xhtml\\+xml" xml-mode)
    ("application/json" vmacs-verb-handler-json)
    ("application/javascript" js-mode)
    ("application/css" css-mode)
    ("text/plain" text-mode)
    ;; Binary handlers
    ("application/pdf" doc-view-mode t)
    ("image/png" image-mode t)
    ("image/svg\\+xml" image-mode t)
    ("image/x-windows-bmp" image-mode t)
    ("image/gif" image-mode t)
    ("image/jpe?g" image-mode t)))

(define-key org-mode-map (kbd "C-c C-u") #'verb-export-request-on-point-curl)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (verb . t)))
;; https://github.com/zweifisch/ob-http
(with-eval-after-load 'org-src

  (add-to-list 'org-src-lang-modes (cons "go" 'go))
  (add-to-list 'org-src-lang-modes (cons "golang" 'go))
  (setq org-src-ask-before-returning-to-edit-buffer nil)
  (define-key org-src-mode-map "\C-c\C-c" 'org-edit-src-exit)
  (define-key org-src-mode-map "\C-x\C-s" 'org-edit-src-exit))

;; (run-with-idle-timer 300 t 'show-todo-list-after-init) ;idle 300=5*60s,show todo list


;; (define-key global-map [(control meta ?r)] 'org-agenda)

(setq-default
 ;; inhibit-startup-screen t;隐藏启动显示画面
 calendar-date-style 'iso
 calendar-day-abbrev-array ["七" "一" "二" "三" "四" "五" "六"]
 calendar-day-name-array ["七" "一" "二" "三" "四" "五" "六"]
 calendar-month-name-array ["一月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"]
 calendar-week-start-day 1

 org-agenda-deadline-leaders (quote ("最后期限:  " "%3d 天后到期: " "%2d 天前: "))
 ;; (setq-default org-agenda-format-date (quote my-org-agenda-format-date-aligned))
 org-agenda-inhibit-startup t
 org-agenda-scheduled-leaders (quote ("计划任务:" "计划任务(第%2d次激活): "))
 org-agenda-window-setup (quote current-window)
 ;; org-clock-string "计时:"
 ;; org-closed-string "已关闭:"
 ;; org-deadline-string "DEADLINE:"
 ;; org-scheduled-string "SCHEDULED:"
 org-time-stamp-formats  '("<%Y-%m-%d 周%u>" . "<%Y-%m-%d 周%u %H:%M>")
 org-agenda-files  (list (expand-file-name "todo.txt" dropbox-dir))
 org-deadline-warning-days 5;;最后期限到达前5天即给出警告
 org-agenda-show-all-dates t
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-reverse-note-order t ;;org.el
 org-link-file-path-type  'relative
 org-log-done 'time
 ;; code执行免应答（Eval code without confirm）
 org-confirm-babel-evaluate nil
 org-image-actual-width '(600)
 org-default-notes-file (expand-file-name "notes.txt" dropbox-dir)
 org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
                     (sequence "Info(i)")
                     (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
                     (sequence "|" "CANCELED(c)"))
 org-capture-templates `(("t" "Todo" entry (file+headline ,(expand-file-name "todo.txt" dropbox-dir) "Tasks")
                          "* TODO %? :%T\n  %i\n")
                         ("i" "Info" entry (file+headline ,(expand-file-name "todo.txt" dropbox-dir) "Info")
                          "* Info %? :%T\n  %i\n")
                         ("h" "Note" item (file ,(expand-file-name "http.txt" dropbox-dir))
                          " %? ")
                         ("n" "Note" item (file ,org-default-notes-file)
                          " %? "))
 org-agenda-custom-commands '(
                              ("n"  "[Note] Go to  Target(Note )" ( (find-file org-default-notes-file) (undo)))
                              ("h"  "[Note] Go to  http.txt (Note )" ((find-file (expand-file-name "http.txt" dropbox-dir)) (undo)))
                              ;; ("b" . "show item of tags prefix") ; describe prefix "h"
                              ;; ("be" tags "+Emacs")
                              ;; ("bj" tags "+Java")
                              ;; ("ba" tags "+AutoHotKey")
                              ;; ("bl" tags "+Linux")
                              ;; ("bd" tags "+Daily")
                              ;; ("bw" tags "+Windows")
                              ("i" todo "Info" nil)
                              ("c" todo "DONE|DEFERRED|CANCELLED" nil)
                              ("W" todo "WAITING" nil)
                              ("w" agenda "" ((org-agenda-start-on-weekday 1) ;start form Monday
                                              (org-agenda-ndays 14)))
                              ("A" agenda ""
                               ((org-agenda-skip-function
                                 (lambda nil
                                   (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
                                (org-agenda-ndays 1)
                                (org-agenda-overriding-header "Today's Priority #A tasks: ")))
                              ("u" alltodo ""
                               ((org-agenda-skip-function
                                 (lambda nil
                                   (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                                             (quote regexp) "\n]+>")))
                                (org-agenda-overriding-header "Unscheduled TODO entries: ")))
                              )
 org-emphasis-regexp-components
 ;; markup 记号前后允许中文
 ;; https://emacs-china.org/t/org-mode/597/11
 ;; Org 里中文/斜体/、*粗体*、_下划线_、+删除+、~代码~、=常量=。
 (list (concat " \t('\"{"            "[:nonascii:]")
       (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
       " \t\r\n,\"'"
       "."
       1)



 )
;; How to automatically save all org files after marking a repeating item as DONE in the org agenda?
(add-hook 'org-trigger-hook 'save-buffer)
(with-eval-after-load 'org-agenda
  (org-defkey org-agenda-mode-map "m"        nil))

;; C-c C-e export to github markdown
(eval-after-load "ox" '(require 'ox-gfm nil t))
;;;###autoload
(define-derived-mode novel-mode org-mode "Novel"
  "novel mode")

(defun vmacs-org-mode-hook()
  (modify-syntax-entry ?， "." ) ;; 识别中文标点
  (modify-syntax-entry ?。 "." ) ;; 识别中文标点
  (modify-syntax-entry ?！ "." ) ;; 识别中文标点
  (modify-syntax-entry ?？ "." ) ;; 识别中文标点
  (modify-syntax-entry ?、 "." ) ;; 识别中文标点
  (modify-syntax-entry ?； "." ) ;; 识别中文标点
  ;; (iimage-mode 1)
  ;; (auto-fill-mode 1)
  ;; (paragraph-indent-minor-mode 1)
  ;; (org-set-local 'fill-paragraph-function 'fill-paragraph-function-org-mode-hook)
  ;; (create-frame-font-mac)
  ;; (create-frame-font-big-mac)
  )
(defface vmacs-org-font
  `((t :inherit default :height 1.2))
  "The default font for vterm buffer.
Monospaced font whihc is fixed idth and height is recommended."
  :group 'vterm)

(defun vmacs-novel-mode-hook()
  (vmacs-org-mode-hook)
  ;; (create-frame-font-large-mac)
  ;; 不起作用
  (face-remap-add-relative 'default 'vmacs-org-font)
  (evil-define-key 'normal 'local (kbd "gw") 'novel-fill)
  ;; (local-set-key [(tab)]       'smart-tab)
  ;; (local-set-key (kbd "TAB")   'smart-tab)
  )

(add-hook 'org-mode-hook 'vmacs-org-mode-hook)
(add-hook 'novel-mode-hook 'vmacs-novel-mode-hook)




;; #+OPTIONS:   H:2 num:nil toc:t \n:nil @:t ::t |:t ^:nil -:t f:t *:t <:t

;; H:         set the number of headline levels for export
;; num:       turn on/off section-numbers
;; toc:       turn on/off table of contents, or set level limit (integer)
;; \n:        turn on/off line-break-preservation (DOES NOT WORK)
;; @:         turn on/off quoted HTML tags
;; ::         turn on/off fixed-width sections
;; |:         turn on/off tables
;; ^:         turn on/off TeX-like syntax for sub- and superscripts.  If
;; you write "^:{}", a_{b} will be interpreted, but
;; the simple a_b will be left as it is.   Org 里中文/斜体/、*粗体*、_下划线_、+删除+、~代码~、=常量=。
;; -:         turn on/off conversion of special strings.
;; f:         turn on/off footnotes like this[1].
;; todo:      turn on/off inclusion of TODO keywords into exported text
;; tasks:     turn on/off inclusion of tasks (TODO items), can be nil to remove
;; all tasks, todo to remove DONE tasks, or list of kwds to keep
;; pri:       turn on/off priority cookies
;; tags:      turn on/off inclusion of tags, may also be not-in-toc
;; <:         turn on/off inclusion of any time/date stamps like DEADLINES
;; *:         turn on/off emphasized text (bold, italic, underlined)
;; TeX:       turn on/off simple TeX macros in plain text
;; LaTeX:     configure export of LaTeX fragments.  Default auto
;; skip:      turn on/off skipping the text before the first heading
;; author:    turn on/off inclusion of author name/email into exported file
;; email:     turn on/off inclusion of author email into exported file
;; creator:   turn on/off inclusion of creator info into exported file
;; timestamp: turn on/off inclusion creation time into exported file
;; d:         turn on/off inclusion of drawers




;; `<TAB>'      子树的折叠
;; ,-> FOLDED -> CHILDREN -> SUBTREE --.
;; '-----------------------------------'
;; `S-<TAB>' 整个buffer的折叠
;; `C-u <TAB>'
;; ,-> OVERVIEW -> CONTENTS -> SHOW ALL --.
;; '--------------------------------------'
;; `C-u C-u C-u <TAB>' 显示所有

;;org buffer 打开时的初始状态是Overview ,可`org-startup-folded'进行配置
;; 也可在每个文件头部加入下面内容设置
;; #+STARTUP: overview
;; #+STARTUP: content
;; #+STARTUP: showall
;; #+STARTUP: showeverything
;; `C-u C-u <TAB>' 切换到buffer 初化的状态

;;可以 通过VISIBILITY 属性设置某一个节点的可视化状态，如：
;;                         ** adef
;;                         ** abc
;;                         :PROPERTIES:
;;                         :VISIBILITY: children
;;                         :END:
;;

;;与属性设置相关的键绑定

;;`C-cC-xp'插入一个属性

;; `S-<left>/<right>'
;; 在某个属性可取的值之间循环

;;在一个属性行上按`C-cC-c' 执行与属性相关的操作，如重新设置值，删除这个属性等,如
;; `C-c C-c d' 删除一个property
;; `C-c C-c D' 全局删除一个property
;; `C-c C-c c'
;; Compute the property at point, using the operator and scope from
;; the nearest column format definition.

;;`M-x org-insert-property-drawer'
;;插入 设置属性的开始与结束标记，即
;;                         :PROPERTIES:
;;                          这中间设置属性
;;                         :END:
;;一个属性可以设置它可以取哪些值，比如
;; prop1 可以属1 2 3 ,这样设置
;; prop1_ALL 1 2 3
;;这个属性如果像这样放在文件头部，此文件中有效
;; #+PROPERTY: NDisks_ALL 1 2 3 4
;; `org-global-properties' 所有文件中有效
;; 在一个节点设置，则此子点下性所有子节点有效
;;比如：
;;                         * CD collection
;;                         :PROPERTIES:
;;                         :NDisks_ALL:  1 2 3 4
;;                         :Publisher_ALL: "Deutsche Grammophon" Philips EMI
;;                         :END:
;;                         ** a cd
;;                         :PROPERTIES:
;;                         :NDisks: 1             --- 这里只能取值1 2 3 4
;;                         :Publisher: EMI         ----同理
;;                         :END:





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;`C-cC-xf'  脚注相关的操作 ,插入脚注，在脚注间跳转
;;`C-uC-cC-xf'  脚注相关的操作

;;Todo相关
;; `C-c C-t'
;; ,-> (unmarked) -> TODO -> DONE --.
;; '--------------------------------'
;;竖线之后表示处于完成状态
;; (setq org-todo-keywords
;;       '((sequence "待办" "结果反馈" "最终检查" "|" "完成" "DELEGATED")))

;;(setq org-todo-keywords '((type "homewordToDo" "JobToDo"  "|" "DONE")))

;;多个序列 (序列间切换)
;; `C-u C-u C-c C-t'
;; `C-S-<right>'
;; `C-S-<left>'
;;q所有keyword 间切换
;; `S-<right>'
;; `S-<left>'
;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "|" "DONE(d)")
;;         (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
;;         (sequence "|" "CANCELED(c)")))

;; (setq org-todo-keyword-faces
;;       '(("TODO"      . org-warning)
;;         ("DEFERRED"  . shadow)
;;         ("CANCELED"  . (:foreground "blue" :weight bold))))

;;每个keyword 后有字母，可以用`C-cC-t'后跟这个字母迅速切换到这种状态
;;只在某一文件有效的todo 设置
;; #+TODO: TODO | DONE
;; #+TODO: REPORT BUG KNOWNCAUSE | FIXED
;; #+TODO: | CANCELED

;; (setq org-startup-folded t)

;; (setq-default org-enforce-todo-dependencies t) ;; 子节点若有未完成事项，则父节点不能标记为Done
;;记录Done 的时刻
;;(setq org-log-done 'time)

;; (setq-default org-log-done 'time)
;; (setq org-log-done 'note) ;; 与(setq org-log-done 'time)相同，并且提示你输入一条note
;;默认情况下，只有Done 的时候才记录时刻或note ,也可以设置在处于某个关键字状态时也进行此操作
;;d在每个关键字后的括号中加入这两个标记`!' (for a timestamp) and `@' (for a note)

;;已经标记为“@” 了，后面却跟着一个'/!' ,表示 在从这个状态切换到其他状态时，当仅仅当
;;目标状态没有设置@也没! 时，它会记录此时的时刻，表示从这个状态切换为其他状态的时刻。
;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
;;比如这个例子，从wait 切换为todo 状态时，它会记录时刻，因为todo状态，没有! 或@ 标记
;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
;; (setq-default org-todo-keywords
;;       '((sequence "TODO(t!)" "|" "DONE(d@/!)")
;;         (sequence "REPORT(r!)" "BUG(b!)" "KNOWNCAUSE(k!)" "|" "FIXED(f@)")
;;         (sequence "|" "CANCELED(c@)")))
;;local同样的语法
;; #+TODO: TODO(t) WAIT(w@/!) | DONE(d!) CANCELED(c@)

;;如果仅想对革一特定的子节点进行定制，使用LOGGING属性
;;                        * TODO Log each state with only a time
;;                         :PROPERTIES:
;;                         :LOGGING: TODO(!) WAIT(!) DONE(!) CANCELED(!)
;;                         :END:
;;                         * TODO Only log when switching to WAIT, and when repeating
;;                         :PROPERTIES:
;;                         :LOGGING: WAIT(@) logrepeat
;;                         :END:
;;                         * TODO No logging at all
;;                         :PROPERTIES:
;;                         :LOGGING: nil
;;                         :END:
;设置优先级
;; `C-c ,'
;; `S-<up>'
;; `S-<down>'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;-- Tag --
;设置了
;; #+FILETAGS: :Peter:Boss:Secret:
;;相当于本文档中所有标题中都含这几个tag
;;操作Tag 的操作
;; `C-c C-q' 插入xc
;; `C-c C-c'

;; `org-tag-alist'
;; #+TAGS: @work @home @tennisclub
;; #+TAGS: laptop car pc sailboat
;;(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("laptop" . ?l)))
;; (setq-default org-tag-alist '(("@Erlang" . ?r)("@Emacs" . ?e) ("@AutoHotKey" . ?a) ("@SVN" . ?S) ("@SQL" . ?s) ("@Daily" . ?d)("@Java" . ?j)("@Windows" . ?w)  ("@Novel" . ?n)("@Oracle" . ?o) ("@DB" . ?b) ("@Linux" . ?l)))

;;或者：
;;#+TAGS: @work(w)  @home(h)  @tennisclub(t)  laptop(l)  pc(p)

;分组：同组只择其一
;;#+TAGS: { @work(w)  @home(h)  @tennisclub(t) }  laptop(l)  pc(p)
;;全局 如果要设置分组 要用`:startgroup' `:endgroup'来分组
;; (setq org-tag-alist '((:startgroup . nil)
;;                       ("@work" . ?w) ("@home" . ?h)
;;                       ("@tennisclub" . ?t)
;;                       (:endgroup . nil)
;;                       ("laptop" . ?l) ("pc" . ?p)))



;; C-caa
;;deadline and schedules 最后期限于 任务安排
;;(setq org-deadline-warning-days 5);;最后期限到达前5天即给出警告
;; *** TODO write article about the Earth for the Guide
;; The editor in charge is [[bbdb:Ford Prefect]]
;; DEADLINE: <2004-02-29 Sun>
;; `DEADLINE: <2004-02-29 Sun -5d>'.这种格式可以指定5天前警告
;;; schedules 则用于指定在未来的一天开始执行某任务
;; *** TODO Call Trillian for a date on New Years Eve.
;; SCHEDULED: <2004-12-25 Sat>
;; `C-cC-s'  插入schedule
;; # C-cC-s 计划任务 ，加一个 日期 C-caa显示计划任务
;; # C-uC-cC-s 删除此计划任务
;; # C-uC-uC-tc-s 延期此计划任务
;; # C-cC-d 插入一个deadline

;; `C-cC-xC-k' Mark the current entry for agenda action.
;;press `k s' or `k d' to schedule the marked item.


(provide 'conf-org)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-org.el ends here.
