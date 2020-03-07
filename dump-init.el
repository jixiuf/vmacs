(setq vmacs-dumping-state 'dumping)
(load (concat (file-name-directory load-file-name) "init.el"))
(require 'package)
(require 'saveplace)
(require 'savehist)
(require 'dired)
(require 'dired-x)
(require 'dired-aux)
(require 'wdired)

(require 'elec-pair)
(require 'recentf)

(require 'ido)
(require 'ibuffer)
(require 'ibuf-ext)
(require 'ibuf-macs)
(require 'hippie-exp)
(require 'ffap)
(require 'autoinsert)
(require 'ansi-color)



(require 'tramp)
(require 'thingatpt)
(require 'frame)
(require 'uniquify)
(require 'scroll-bar)

(require 'abbrev)
(require 'align)
(require 'css-mode)
(require 'winner)
(require 'widget)
(require 'cc-mode)
(require 'flymake)
(require 'hideshow)
(require 'grep)
(require 'etags)
(require 'python)
(require 'outline)
(require 'json)
(require 'js)
(require 'isearch)
(require 'compile-dwim)
(require 'lazy-buffer)
(require 'lazy-camelize)
(require 'lazy-dired-sort)
(require 'lazy-dired)
(require 'lazy-json)
;; (require 'lazy-minibuffer)
(require 'lazy-novel-mode)
(require 'lazy-open-in-file-manager)
(require 'lazy-org)
(require 'lazy-sudo)
(require 'lazy-window)
(require 'vmacs-dired-single)
(require 'sqlparser-mysql-complete)
(require 'vmacs-dired-history)


(when (vmacs-dumping-p)
  ;; disable undo-tree to prevent from segfaulting when loading the dump
  (message "dumping vmacs")
  (global-undo-tree-mode -1)
  (setq load-path-backup load-path)
  (setq vmacs-dumping-state 'dumped)
  (garbage-collect))


;; ;; (require 'applescript-mode)





(provide 'dump-init)

;; Local Variables:
;; coding: utf-8
;; End:

;;; .emacs.d/dump-init.el ends here.
