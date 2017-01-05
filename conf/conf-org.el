;; org-mode 相关
(evil-leader/set-key "t" 'org-agenda)   ;列出todo list等
(evil-leader/set-key "T" 'org-capture)  ;新加一个todo 条目等
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
 org-clock-string "计时:"
 org-closed-string "已关闭:"
 org-deadline-string "最后期限:"
 org-scheduled-string "计划任务:"
 org-time-stamp-formats  '("<%Y-%m-%d 周%u>" . "<%Y-%m-%d 周%u %H:%M>")
 org-agenda-files  (list (expand-file-name "todo.txt" dropbox-dir))
 org-deadline-warning-days 5;;最后期限到达前5天即给出警告
 org-agenda-show-all-dates t
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-reverse-note-order t ;;org.el
 org-log-done 'time
 ;; code执行免应答（Eval code without confirm）
 org-confirm-babel-evaluate nil
 org-default-notes-file (expand-file-name "notes.txt" dropbox-dir)
 org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
                     (sequence "Info(i)")
                     (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
                     (sequence "|" "CANCELED(c)"))
 org-capture-templates `(("t" "Todo" entry (file+headline ,(expand-file-name "todo.txt" dropbox-dir) "Tasks")
                          "* TODO %? 创建于:%T\n  %i\n")
                         ("i" "Info" entry (file+headline ,(expand-file-name "todo.txt" dropbox-dir) "Info")
                          "* Info %? 创建于:%T\n  %i\n")
                         ("n" "Note" item (file ,org-default-notes-file)
                          " %? "))
 org-agenda-custom-commands '(("n"  "[Note] Go to  Target(Note )" ( (find-file org-default-notes-file)))
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


 )

;;;###autoload
(define-derived-mode novel-mode org-mode "Novel"
  "novel mode")

(defun txt-mode-hook()
  (modify-syntax-entry ?， "." ) ;; 识别中文标点
  (modify-syntax-entry ?。 "." ) ;; 识别中文标点
  (modify-syntax-entry ?！ "." ) ;; 识别中文标点
  (modify-syntax-entry ?？ "." ) ;; 识别中文标点
  (modify-syntax-entry ?、 "." ) ;; 识别中文标点
  (modify-syntax-entry ?； "." ) ;; 识别中文标点
  (iimage-mode 1)
  ;; (auto-fill-mode 1)
  ;; (paragraph-indent-minor-mode 1)
  ;; (org-set-local 'fill-paragraph-function 'fill-paragraph-function-org-mode-hook)

  )
(add-hook 'text-mode-hook 'txt-mode-hook)
(add-hook 'org-mode-hook 'txt-mode-hook)
(add-hook 'novel-mode-hook 'txt-mode-hook)


(defun show-todo-list-after-init(&optional frame)
  (require 'org-agenda)
  (dolist (f org-agenda-files)
    (unless (file-exists-p f)
      (setq org-agenda-files (delete f org-agenda-files))))
  (when org-agenda-files
    (call-interactively 'org-todo-list)
    (switch-to-buffer "*Org Agenda*")))

(run-with-idle-timer 300 t 'show-todo-list-after-init) ;idle 300=5*60s,show todo list

(provide 'conf-org)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-org.el ends here.
