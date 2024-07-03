;; org-mode 相关
;;(require 'ox-re-reveal)
;;(setq org-re-reveal-root (format "file://%sreveal.js-3.0.0" (expand-file-name user-emacs-directory)))
;;(setq org-re-reveal-single-file t)
;; C-cC-eRR

(with-eval-after-load 'org-capture  (add-to-list 'org-capture-mode-hook #'meow-insert))
(vmacs-leader (kbd "t") 'org-agenda)   ;列出 todo list 等
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "C-c Gt") 'org-capture)
  (define-key org-agenda-mode-map (kbd "C-c Gr") 'org-agenda-redo))

(setq verb-auto-kill-response-buffers t)
(defun uid() (interactive) (completing-read "uid: " '("10064589" "545473" "60682172")))
(with-eval-after-load 'org
  (with-eval-after-load 'verb
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
    (define-key org-mode-map (kbd "C-M-h") 'org-babel-mark-block)
    (define-key org-mode-map (kbd "C-,") nil)
    (define-key org-mode-map (kbd "C-c C-u") #'verb-export-request-on-point-curl))
  (define-key org-mode-map (kbd "C-c e") 'org-edit-special)
  (define-key org-mode-map (kbd "C-a") 'org-mode-smart-beginning-of-line)
  (define-key org-mode-map (kbd "C-e") 'org-mode-smart-end-of-line)
  (define-key org-mode-map (kbd "C-k") 'vmacs-kill-region-or-org-kill-line)
  (define-key org-mode-map (kbd "C-c C-k") 'org-babel-remove-result-one-or-many)
  (define-key org-mode-map (kbd "<drag-n-drop>") 'vmacs-org-insert-image))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (verb . t)))

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes (cons "go" 'go))
  (add-to-list 'org-src-lang-modes (cons "golang" 'go))
  (setq org-src-ask-before-returning-to-edit-buffer nil)
  (define-key org-src-mode-map "\C-c\C-c" 'org-edit-src-exit)
  (define-key org-src-mode-map "\C-x\C-s" 'org-edit-src-exit))

;; (run-with-idle-timer 300 t 'show-todo-list-after-init) ;idle 300=5*60s,show todo list

;; path /Library/TeX/texbin
;; https://github.com/kimim/kimim-emacs#org-to-pdf
;; https://mirrors.tuna.tsinghua.edu.cn/CTAN/language/chinese/ctex/ctex.pdf
;; brew install mactex-no-gui
;; sudo tlmgr init-usertree
;; sudo tlmgr --usermode install ctex titlesec enumitem ms fontspec abstract    \
;;                          zhnumber fandol lastpage pdftexcmds infwarerr  \
;;                          minted fvextra etoolbox fancyvrb upquote       \
;;                          lineno catchfile xstring framed float          \
;;                          grffile wrapfig ulem lettrine minifp           \
;;                          capt-of xcolor svg koma-script trimspaces      \
;;                          titling layaureo parskip extsizes pgf          \
;;                          moderncv microtype
;; sudo fmtutil-sys --all

;;  语法高亮包 minted 它用到了 python 的 pygments
;;  brew install pygments

(require 'ox-latex)
(setq org-latex-compiler "xelatex")
;; 下面用到的 minted（语法高亮 需要 -shell-escape  参数，它用到了 python 的 pygments ，故传参需要）
;; minted(语法高亮) 需要 -8bit 否则 tab 会显示为^^I 则 minted.pdf 的 FAQ
;; 详见 https://ctan.math.utah.edu/ctan/tex-archive/macros/latex/contrib/minted/minted.pdf
(setq org-latex-pdf-process '("xelatex  -8bit -shell-escape -interaction nonstopmode %f"
                              "xelatex  -8bit -shell-escape -interaction nonstopmode %f"))
;; 可以通过此指令 指定使用哪个  #+LATEX_CLASS: ctexart
;; 或用 org-latex-default-class 指定默认值(原默认:article)
;; ;中文更友好(如日期格式等) https://mirrors.tuna.tsinghua.edu.cn/CTAN/language/chinese/ctex/ctex.pdf
;; https://github.com/kimim/kimim-emacs/blob/master/site-lisp/latex-classes.el
(progn
  (setq class-ctexart '("ctexart"                ;ctexart 对应 ctex 版的 article,需要使用上面注释中的 tlmgr 安装 ctex 待 中文模版
                        "
\\documentclass[10pt,a4paper,UTF8]{ctexart}
\\ctexset{today=small} % 日期格式 small|big|old small=2021 年 6 月 20 日
\\renewcommand\\thesection{\\chinese{section}、} % H1 的序号用 中文一二三
\\renewcommand\\thesubsection{\\arabic{section}.\\arabic{subsection}.} % H2 阿拉伯数字
\\renewcommand\\thesubsubsection{\\arabic{section}.\\arabic{subsection}.\\arabic{subsubsection}} % H3 阿拉伯数字

\\usepackage{hyperref} % 超链接的样式
\\hypersetup{hidelinks}
\\hypersetup{
colorlinks = true,
linkcolor=blue,
urlcolor=blue,
citecolor=[rgb]{0,0.47,0.68},
filecolor=[rgb]{0,0.37,0.53},
linktoc=all
}

%\\setCJKmainfont{微软雅黑} % sets the roman font
%\\setCJKsansfont{微软雅黑} % sets the sans font
%\\setCJKmonofont{Consolas} % otherwise FangSong is not found

\\usepackage{adjustbox} %  缩放 table 会用到  #+LATEX: \\adjustbox{max width=\\linewidth}{ tablehere  #+LATEX: \\end{adjustbox}
\\usepackage{caption} % 图表 加一句话标题

% https://ctan.math.utah.edu/ctan/tex-archive/macros/latex/contrib/geometry/geometry.pdf
\\usepackage{geometry} % 控制页面边距，
\\geometry{a4paper,left=2cm,right=2cm,top=1.5cm,bottom=1cm} % scale=0.8 是设置可用区比例

% https://ctan.math.utah.edu/ctan/tex-archive/macros/latex/contrib/enumitem/enumitem.pdf
\\usepackage{enumitem} % 设置 列表的上下间距 ，这几个值调大有效果，0依然很大
\\setlist[1]{labelindent=\\parindent} % < Usually a good idea
\\setlist[itemize]{topsep=5pt, partopsep=0,parsep=1pt ,itemsep=1pt}
\\setlist[enumerate]{topsep=5pt, partopsep=0,parsep=1pt ,itemsep=1pt}

\\usepackage[table]{xcolor} % 表格按行使用不同的颜色 表头上使用 #+LATEX: \\rowcolors[]{2}{blue!10}{blue!25}

"
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}")
                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; ;默认提供了 article/report/book,article=工作论文写作而设计,以及报告类、出书类相应的版式
(setq org-latex-classes nil)           ;for debug
(add-to-list 'org-latex-classes class-ctexart))
;; #+LATEX_HEADER: \usepackage{fontspec}
;; #+LATEX_HEADER: \setmainfont{PingFang SC}

(setq org-latex-default-class "ctexart") ;(原默认:article)


;; (setq org-latex-listings t)             ;https://mirrors.rit.edu/CTAN/macros/latex/contrib/listings/listings.pdf
;; (add-to-list 'org-latex-packages-alist '("" "listings"))
;; (add-to-list 'org-latex-packages-alist '("" "xcolor"))
;; (setq org-latex-listings-options
;;       '(("basicstyle" "\\small")
;;         ("commentstyle" "\\color{violet}")
;;         ("frame" "lines")              ;none, leftline, topline, bottomline, lines (top and bottom), single for single frames, or shadowbox.
;;         ("keywordstyle" "\\color{black}\\bfseries\\underbar")))

;; (add-to-list 'org-latex-minted-langs '(go "go")) ;pygmentize -L lexers|grep go
;; (add-to-list 'org-latex-minted-langs '(ditaa "text"))
;; (add-to-list 'org-latex-minted-langs '(plantuml "text"))

;; ;;  源代码语法高亮
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))
;;\usemintedstyle{name} #pygmentize -L styles #选不同的 style
(setq org-latex-minted-options ;https://ctan.math.utah.edu/ctan/tex-archive/macros/latex/contrib/minted/minted.pdf
      '(
        ("fontsize" "\\small")
        ("frame=lines")                 ;代码块首尾加 none | leftline | topline | bottomline | lines=上下有线 | single=框
        ;;  ("framesep=2mm") ; frame 与内容之间的间距
        ;; ("linenos=true");显示行号 ,frame=single 有框隔着 不会出现复制代码不方便
        ;; ("bgcolor" "green")
        ("breaklines" "true")           ;长行 换行展示
        ;; ("breaksymbolleft" "")          ;default 􏰀→
        ;; ("breakanywheresymbolpre" "")
        ;; ("breakbeforesymbolpre" "")
        ("breakanywhere" "true")        ;默认只在空白等地方换行，若一直无空白，则后面内容 trunc 掉了
        ("autogobble" "true") ;; 缩进相关
        ("showtabs" "false")
        ;; ("style=zenburn" )          ;default:"default" ;pygmentize -L styles
        ;; ("gobble=2") ;; 自动移除每行前 n 个字符
        ;; ("tabsize=16")
        ;; ("showspaces" "true") ;; 显示空白字符
        ;;  ("mathescape=true")
        ;;  ("numbersep=5pt")
        ))


(setq-default
 ;; inhibit-startup-screen t;隐藏启动显示画面
 org-src-window-setup 'current-window   ;C-c'
 org-src-preserve-indentation t
 calendar-date-style 'iso
 calendar-day-abbrev-array ["日" "一" "二" "三" "四" "五" "六"]
 calendar-day-name-array ["日" "一" "二" "三" "四" "五" "六"]
 calendar-month-name-array ["一月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"]
 calendar-week-start-day 1
 ;; org-clock-string "计时:"
 ;; org-closed-string "已关闭:"
 ;; org-deadline-string "DEADLINE:"
 ;; org-scheduled-string "SCHEDULED:"
 org-timestamp-formats  '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>") ;%a 要在 %H 前，否则时间区间的 11:00-12:00 会被分开
 org-deadline-warning-days 5;;最后期限到达前 5 天即给出警告
 org-icalendar-timezone "Asia/Shanghai"

 org-reverse-note-order t ;;org.el
 org-link-file-path-type  'relative
 org-log-done 'time
 ;; code 执行免应答（Eval code without confirm）
 org-confirm-babel-evaluate nil
 org-image-actual-width '(600)
 org-agenda-show-all-dates t
 org-deadline-past-days 1
 org-agenda-deadline-leaders (quote ("最后期限:  " "%3d 天后到期: " "%2d 天前截止: "))
 ;; (setq-default org-agenda-format-date (quote my-org-agenda-format-date-aligned))
 org-agenda-inhibit-startup t
 org-agenda-scheduled-leaders (quote ("计划任务:" "计划任务(第%2d 次激活): "))
 org-agenda-start-with-log-mode nil
 org-agenda-format-date "%A %Y-%m-%d"
 org-agenda-window-setup (quote current-window)
 org-agenda-confirm-kill nil            ;C-k 删除条目
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-span 'week
  org-default-notes-file (expand-file-name "notes.txt.gpg" dropbox-dir)
 org-agenda-files  (list (expand-file-name "todo.txt.gpg" dropbox-dir) (expand-file-name "caldav.txt.gpg" dropbox-dir))
 org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
                     (sequence "Info(i)"))
 org-capture-templates `(("t" "Todo" entry (file ,(expand-file-name "todo.txt.gpg" dropbox-dir))
                          "* TODO %?\n%T\n  %i\n")
                         ("i" "Info" entry (file+headline ,(expand-file-name "todo.txt.gpg" dropbox-dir) "Info")
                          "* Info %? :%T\n  %i\n")
                         ("n" "Note" item (file ,org-default-notes-file)
                          " %? "))
 org-agenda-custom-commands '(
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
       1))

;; How to automatically save all org files after marking a repeating item as DONE in the org agenda?
(add-hook 'org-trigger-hook 'save-buffer)


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
  (push '("[X]" . ?✅) prettify-symbols-alist)
  (push '("[ ]" . ?❎) prettify-symbols-alist)
  (prettify-symbols-mode 1)
  (setq truncate-lines nil)
  (remove-hook 'completion-at-point-functions 'ispell-completion-at-point t)
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
  ;; (when (string= khalel-import-org-file (buffer-file-name))
    ;; (local-set-key (kbd "C-c C-e") 'khalel-edit-calendar-event)
    ;; (local-set-key (kbd "C-c C-r") 'khalel-import-events)
    ;; (local-set-key (kbd "C-c C-u") 'khalel-refresh)
    ;; (local-set-key (kbd "C-c C-y") 'khalel-sync-summary)

    ;; (defun khal-edit-hook()
    ;;   (when (equal (buffer-name) "*khal-edit*") (setq-local truncate-lines nil)))
    ;; (add-hook 'comint-mode-hook #'khal-edit-hook)
    ;; )
  )

(add-hook 'org-mode-hook 'vmacs-org-mode-hook)
(add-hook 'novel-mode-hook 'vmacs-novel-mode-hook)
(when (require 'org-caldav nil t)
  ;; (setq org-agenda-files  (list (expand-file-name "todo.txt.gpg" dropbox-dir) (expand-file-name "caldav.txt" dropbox-dir)))
  (setq org-caldav-delete-org-entries 'always)
  (setq org-caldav-delete-calendar-entries 'always)
  (setq org-caldav-files nil)
  (setq org-caldav-show-sync-results nil)
  (setq org-caldav-debug-level 0)
  (setq org-caldav-debug-buffer " *org-caldav-debug*")
  (setq org-caldav-calendars
        `(
          (:calendar-id "60BADA72-D892-4002-60BA-DA72D8924002"
                        :get-event-by-report t
                        :uuid-extension ".ics"
                        ;; :sync-direction "cal->org"
                        :sync-direction "twoway"
                        :url "https://caldav.feishu.cn/jixiufeng_luojilab"
                        :inbox ,(expand-file-name "caldav.txt.gpg" dropbox-dir))
          (:calendar-id "primary"
                        :get-event-by-report nil
                        :uuid-extension ""
                        :sync-direction "twoway"
                        :url "https://calendar.dingtalk.com/dav/u_fukx3svp"
                        :inbox ,(expand-file-name "todo.txt.gpg" dropbox-dir))))

  (defun vmacs-org-caldav-sync()
    ;; f and b 前一周
    (print this-command)
    (when (member this-command
                  '(org-agenda-redo
                    org-agenda-todo
                    ;; org-agenda
                    org-agenda-date-prompt))
      (save-excursion
      (org-caldav-sync))))
  (add-hook 'org-agenda-mode-hook #'vmacs-org-caldav-sync)
  (run-with-idle-timer 300 t 'org-caldav-sync) ;idle 300=5*60s,


  ;; Usually a good idea to set the timezone manually
  ;; (setq org-icalendar-date-time-format ":%Y%m%dT%H%M%S")
  (setq org-icalendar-timezone "Asia/Shanghai")
  ;; ali dingding 对todo 的支持有问题，暂不同步todo
  (setq org-icalendar-include-todo nil
        org-caldav-sync-todo nil)  )
(when (require 'org-alert nil t)
  ;; support  for khalel calendar
  ;; - When: <2024-07-02 14:00>--<2024-07-02 15:00>
  ;; `TIMESTAMP_IA` 代表不活跃的时间戳（即不会影响 agenda 视图的纯时间戳）。
  ;; TIMESTAMP 时间戳是活跃的,您应该使用 `TIMESTAMP` 替换 `TIMESTAMP_IA`。
  (setq org-alert-match-string
        "SCHEDULED>=\"<today>\"+SCHEDULED<\"<tomorrow>\"|DEADLINE>=\"<today>\"+DEADLINE<\"<tomorrow>\"|TIMESTAMP>=\"<today>\"+TIMESTAMP<\"<tomorrow>\"")
  (setq org-alert-time-match-string
        "\\(?:SCHEDULED\\|DEADLINE\\|When\\)?:?.*<20[0-9][0-9]-[0-9][0-9]-[0-9][0-9] [日一二三四五六] ?\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)?.*>")

  (setq alert-default-style 'libnotify)
  (setq org-alert-interval 300
        org-alert-notify-cutoff 10
        org-alert-notify-after-event-cutoff 10)
  (org-alert-enable))



;; C-cC-xC-i C-cC-xC-o
;; (add-hook 'org-clock-in-hook #'(lambda()(call-process  "open" nil nil nil "-g" "hammerspoon://org-clock?id=org-clock-in")))
;; (add-hook 'org-clock-out-hook #'(lambda()(call-process  "open" nil nil nil "-g" "hammerspoon://org-clock?id=org-clock-out")))
;; (setq khalel-import-org-file (expand-file-name "caldav.txt" dropbox-dir))
;; (autoload 'khalel-import-events "khalel" "" t)
;; (defun khalel-refresh ()
;;   (interactive)
;;   (let( (mode major-mode))
;;     (require 'khalel)
;;     (khalel-run-vdirsyncer)
;;     (if (equal mode 'org-agenda-mode)
;;         (org-agenda-redo)
;;       (khalel-import-events)
;;       )))
;; (defun khalel-sync-summary()
;;   (interactive)
;;   (let ((buf (current-buffer)))
;;     (when (equal major-mode 'org-agenda-mode)
;;       (call-interactively #'org-agenda-switch-to))
;;     (when (or(equal major-mode 'org-mode) (equal major-mode 'novel-mode))
;;       (let* ((inhibit-read-only t)
;;              (heading (org-get-heading t))
;;              (calendar (org-entry-get (point) "CALENDAR"))
;;              (id (org-entry-get (point) "ID"))
;;              (icsfile (expand-file-name (format "~/.calendars/%s/%s.ics" calendar id)))
;;              )
;;         (when (file-exists-p icsfile)
;;           (with-temp-buffer
;;             (insert-file-contents icsfile)
;;             (goto-char (point-min))
;;             (while (re-search-forward "^SUMMARY:\\(.*\\)$" nil t)
;;               (replace-match heading nil nil nil 1))
;;             (write-file icsfile)))
;;         )
;;       )
;;     (switch-to-buffer buf)))
;;   (define-advice org-agenda-todo (:around (orig-fun &rest args) sync-summary)
;;     (apply orig-fun args)
;;     (khalel-sync-summary))
;; (add-hook 'org-agenda-mode-hook #'khalel-import-events)
;; (with-eval-after-load 'khalel
;;   (setq khalel-import-start-date "-7d")
;;   (setq khalel-import-end-date "+100d")
;;   (setq khalel-import-org-file-confirm-overwrite nil)
;;   (setq khalel-import-format "* {title} {cancelled} :{calendar}:\n\
;; :PROPERTIES:\n:CALENDAR: {calendar}\n\
;; :LOCATION: {location}\n\
;; :ID: {uid}\n\
;; :END:\n\
;; - When: <{start-long}>--<{end-long}>\n\
;; - Where: {location}\n\
;; - Description: {description}\n\
;; - URL: {url}\n- Organizer: {organizer}\n\n\
;; [[elisp:(khalel-edit-calendar-event)][Edit(C-cC-e)]]\
;;     [[elisp:(progn (khalel-run-vdirsyncer) (khalel-import-events))]\
;; [Sync]]\n"
;;         )
;;   (setq khalel-import-org-file-header "#+TITLE: 日历\n\
;; #+COLUMNS: %ITEM %TIMESTAMP %LOCATION %CALENDAR\n\n\
;; *NOTE*: 本文件使用 [[elisp:(khalel-import-events)][khalel-import-events]] 生成 \
;; 不要直接编辑，\n请用 =khalel-edit-calendar-event= 或者 =khal edit= 来编辑\n\
;; [[elisp:(khalel-run-vdirsyncer)][占此同步]]\n")
;;   (setq khalel-capture-key "w")
;;   (khalel-add-capture-template)

;;   (unless (file-exists-p khalel-import-org-file)(khalel-import-events))
;;   (setq khalel-default-calendar "primary")
;;   (define-advice khalel--delete-process-window-when-done (:around (orig-fun &rest args) refresh)
;;     (let ((buf (process-buffer (car args))))
;;       (when (equal (buffer-name buf) "*khal-edit*")
;;         (khalel-import-events)
;;         ;; (khalel-run-vdirsyncer)
;;         ;; (khalel-import-events)
;;         (with-current-buffer (get-file-buffer khalel-import-org-file)
;;           (revert-buffer nil t))
;;         ))
;;     (apply orig-fun args)
;;     )
;;   (define-advice khalel--sanitize-ics (:around (orig-fun &rest args) ali)
;;     "When called interactively with no active region, copy a single line instead."

;;     (apply orig-fun args)
;;     (with-temp-file (car args)
;;       (insert-file-contents (car args))
;;       (goto-char (point-min))
;;       (while (re-search-forward "^\\(UID:[[:blank:]]*\\)SC-" nil t)
;;         (replace-match "\\1" nil nil))
;;       (goto-char (point-min))
;;       (while (re-search-forward "^\\(UID:[[:blank:]]*\\)DL-" nil t)
;;         (replace-match "\\1" nil nil))
;;       )
;;     (car args)))


;; 见 org-export-options-alist 对应哪些全局变量
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
;; `S-<TAB>' 整个 buffer 的折叠
;; `C-u <TAB>'
;; ,-> OVERVIEW -> CONTENTS -> SHOW ALL --.
;; '--------------------------------------'
;; `C-u C-u C-u <TAB>' 显示所有

;;org buffer 打开时的初始状态是 Overview ,可`org-startup-folded'进行配置
;; 也可在每个文件头部加入下面内容设置
;; #+STARTUP: overview
;; #+STARTUP: content
;; #+STARTUP: showall
;; #+STARTUP: showeverything
;; `C-u C-u <TAB>' 切换到 buffer 初化的状态

;;可以 通过 VISIBILITY 属性设置某一个节点的可视化状态，如：
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
;; `C-c C-c d' 删除一个 property
;; `C-c C-c D' 全局删除一个 property
;; `C-c C-c c'
;; Compute the property at point, using the operator and scope from
;; the nearest column format definition.

;;`M-x org-insert-property-drawer'
;;插入 设置属性的开始与结束标记，即
;;                         :PROPERTIES:
;;                          这中间设置属性
;;                         :END:
;;一个属性可以设置它可以取哪些值，比如
;; prop1 可以属 1 2 3 ,这样设置
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
;;                         :NDisks: 1             --- 这里只能取值 1 2 3 4
;;                         :Publisher: EMI         ----同理
;;                         :END:





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;`C-cC-xf'  脚注相关的操作 ,插入脚注，在脚注间跳转
;;`C-uC-cC-xf'  脚注相关的操作

;;Todo 相关
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
;;q 所有 keyword 间切换
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

;;每个 keyword 后有字母，可以用`C-cC-t'后跟这个字母迅速切换到这种状态
;;只在某一文件有效的 todo 设置
;; #+TODO: TODO | DONE
;; #+TODO: REPORT BUG KNOWNCAUSE | FIXED
;; #+TODO: | CANCELED

;; (setq org-startup-folded t)

;; (setq-default org-enforce-todo-dependencies t) ;; 子节点若有未完成事项，则父节点不能标记为 Done
;;记录 Done 的时刻
;;(setq org-log-done 'time)

;; (setq-default org-log-done 'time)
;; (setq org-log-done 'note) ;; 与(setq org-log-done 'time)相同，并且提示你输入一条 note
;;默认情况下，只有 Done 的时候才记录时刻或 note ,也可以设置在处于某个关键字状态时也进行此操作
;;d 在每个关键字后的括号中加入这两个标记`!' (for a timestamp) and `@' (for a note)

;;已经标记为“@” 了，后面却跟着一个'/!' ,表示 在从这个状态切换到其他状态时，当仅仅当
;;目标状态没有设置@也没! 时，它会记录此时的时刻，表示从这个状态切换为其他状态的时刻。
;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
;;比如这个例子，从 wait 切换为 todo 状态时，它会记录时刻，因为 todo 状态，没有! 或@ 标记
;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
;; (setq-default org-todo-keywords
;;       '((sequence "TODO(t!)" "|" "DONE(d@/!)")
;;         (sequence "REPORT(r!)" "BUG(b!)" "KNOWNCAUSE(k!)" "|" "FIXED(f@)")
;;         (sequence "|" "CANCELED(c@)")))
;;local 同样的语法
;; #+TODO: TODO(t) WAIT(w@/!) | DONE(d!) CANCELED(c@)

;;如果仅想对革一特定的子节点进行定制，使用 LOGGING 属性
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
;;相当于本文档中所有标题中都含这几个 tag
;;操作 Tag 的操作
;; `C-c C-q' 插入 xc
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
;;(setq org-deadline-warning-days 5);;最后期限到达前 5 天即给出警告
;; *** TODO write article about the Earth for the Guide
;; The editor in charge is [[bbdb:Ford Prefect]]
;; DEADLINE: <2004-02-29 Sun>
;; `DEADLINE: <2004-02-29 Sun -5d>'.这种格式可以指定 5 天前警告
;;; schedules 则用于指定在未来的一天开始执行某任务
;; *** TODO Call Trillian for a date on New Years Eve.
;; SCHEDULED: <2004-12-25 Sat>
;; `C-cC-s'  插入 schedule
;; # C-cC-s 计划任务 ，加一个 日期 C-caa 显示计划任务
;; # C-uC-cC-s 删除此计划任务
;; # C-uC-uC-tc-s 延期此计划任务
;; # C-cC-d 插入一个 deadline

;; `C-cC-xC-k' Mark the current entry for agenda action.
;;press `k s' or `k d' to schedule the marked item.


(provide 'conf-org)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-org.el ends here.
