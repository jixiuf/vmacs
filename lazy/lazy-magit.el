(eval-when-compile
  (require  'ediff)
  (require 'magit))
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

;;;###autoload
(defun vmacs-update-repo-revision()
  (when (string-prefix-p "magit-commit" (symbol-name last-command) )
    (let* (( repos (magit-toplevel))
           (config-file (expand-file-name "config.online.toml" repos)))
      (when (and repos (file-exists-p config-file))
        (with-current-buffer (find-file-noselect config-file)
          (goto-char (point-min))
          (when (search-forward-regexp "git=\"\\(.*\\)\"" nil t)
            (replace-match (magit-rev-parse "--short" "HEAD") t t nil 1)
            ))))
    )
  )


(provide 'lazy-magit)
