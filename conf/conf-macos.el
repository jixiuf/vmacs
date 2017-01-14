;; (setq exec-path (delete-dups  (cons "/usr/local/bin" exec-path)))
;; (setenv "PATH" (concat  "/usr/local/bin:" (getenv "PATH") ))
;; 从shell 中获取环境变量
(setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOROOT" "GOPATH" "EDITOR" "PYTHONPATH"))
(setq exec-path-from-shell-check-startup-files nil)
(setq exec-path-from-shell-arguments '("-l" "-i")) ;remove -i read form .zshenv
(exec-path-from-shell-initialize)


(setenv "LANG" "zh_CN.UTF-8")

;; (setq-default server-auth-dir (expand-file-name "~/.emacs.d/cache/"))
;; (setq-default server-socket-dir (expand-file-name "~/.emacs.d/cache/"))
;; (setq-default server-name "emacs-server-file")
;; (require 'server)
;; (when (not (server-running-p)) (server-start))


;; (setq-default server-auth-dir (expand-file-name "~/.emacs.d/cache/"))
;; (setq-default server-socket-dir  (expand-file-name "~/.emacs.d/cache/"))
;; (setq-default server-name "emacs-server-file")
;; (require 'server)
;; (when (not (server-running-p)) (server-start))

;; 允许emacs 直接编辑 OSX下的 .plist文件
;; Allow editing of binary .plist files.
(add-to-list 'jka-compr-compression-info-list
             ["\\.plist$"
              "converting text XML to binary plist"
              "plutil"
              ("-convert" "binary1" "-o" "-" "-")
              "converting binary plist to text XML"
              "plutil"
              ("-convert" "xml1" "-o" "-" "-")
              nil nil "bplist"])

;; # mac 上 emacs 直接编辑二进制applescript
(add-to-list 'jka-compr-compression-info-list
             `["\\.scpt\\'"
               "converting text applescript to binary applescprit " ,(expand-file-name "applescript-helper.sh" "~/.emacs.d/bin/") nil
               "converting binary applescript to text applescprit " ,(expand-file-name "applescript-helper.sh" "~/.emacs.d/bin/") ("-d")
               nil t "FasdUAS"])
;;It is necessary to perform an update!
(jka-compr-update)

(global-set-key (kbd "s-m") 'toggle-frame-maximized) ;cmd-m
(global-set-key  (kbd "s-a") 'evil-mark-whole-buffer) ;mac Cmd+a
(global-set-key  (kbd "s-t") 'shell-toggle-cd) ;mac Cmd+a

;; f11 (toggle-frame-fullscreen) default
(setq ns-pop-up-frames nil)


(defun term-enable-mouse-scroll(&optional f) ;
  (with-selected-frame f
    (when (and (equal system-type 'darwin) (not (display-graphic-p)))
      (require 'mouse) ;; needed for iterm2 compatibility
      (xterm-mouse-mode t)
      (global-set-key [mouse-4] '(lambda ()
                                   (interactive)
                                   (scroll-down 1)))
      (global-set-key [mouse-5] '(lambda ()
                                   (interactive)
                                   (scroll-up 1))))))

(add-hook 'after-make-frame-functions 'term-enable-mouse-scroll)

(provide 'conf-macos)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-macos.el ends here.
