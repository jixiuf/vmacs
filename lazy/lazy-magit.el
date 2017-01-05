
;;;###autoload
(defun toggle-diff-whitespace-eol()
  (interactive)
  (if  (equal vc-git-diff-switches t)
      (progn
        (setq vc-git-diff-switches '("--ignore-space-at-eol" "--ignore-blank-lines"))
        (setq magit-diff-section-arguments '("--ignore-space-at-eol" "--ignore-blank-lines" "--no-ext-diff")))
    (setq vc-git-diff-switches t)
    (setq magit-diff-section-arguments '("--no-ext-diff")))
  (if  (and (boundp 'vc-svn-diff-switches)(equal vc-svn-diff-switches t))
      (setq-default vc-svn-diff-switches '("-x --ignore-eol-style"  ))
    (setq-default vc-svn-diff-switches t))
  (cond
   ((equal major-mode 'diff-mode)
    (revert-buffer))
   ((equal major-mode 'magit-status-mode)
    (magit-refresh)))
  (message "toggle diff show whitespace at end of line"))
;; (setq-default magit-log-format-graph-function 'magit-log-format-unicode-graph)

(provide 'lazy-magit)
