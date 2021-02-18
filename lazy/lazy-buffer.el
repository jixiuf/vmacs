;;; -*- lexical-binding: t; -*-
(eval-when-compile (require 'cl-macs) (require 'cl-seq))
(require 'recentf)
(require 'consult nil t)
(require 'magit)

(defvar git-repos-files-cache (make-hash-table :test 'equal))
(defun vmacs--git-files (&optional n dir )
  "Append git files as virtual buffer"
  (let (result-list
        (default-directory (or dir default-directory))
        (magit-repos (mapcar 'car magit-repository-directories))
        list git-dir)
    (unless (file-remote-p default-directory)
      (setq git-dir (magit-toplevel))
      (when git-dir
        (setq git-dir (abbreviate-file-name (file-truename (directory-file-name git-dir))))
        (setq default-directory git-dir)
        (setq list (gethash git-dir  git-repos-files-cache))
        (when (or (not list) current-prefix-arg) ;prefix则会刷新缓存
          (setq list (split-string (shell-command-to-string (format "git ls-files --full-name --|sed \"s|^|%s/|g\"" default-directory)) "\n" t))
          (puthash git-dir list git-repos-files-cache))

        (setq result-list (append result-list list))))
    (dotimes (idx (or n 3) )
      (let ((magit-repo (nth idx magit-repos)))
        (when (and magit-repo (file-exists-p magit-repo))
          (setq magit-repo (abbreviate-file-name (directory-file-name (file-truename magit-repo))))
          (unless (string-equal magit-repo git-dir)
            (setq default-directory magit-repo)
            (setq list (gethash magit-repo  git-repos-files-cache))
            (when (or (not list) current-prefix-arg)
              (setq list (split-string (shell-command-to-string (format "git ls-files --full-name --|sed \"s|^|%s/|g\"" default-directory)) "\n" t))
              (puthash magit-repo list git-repos-files-cache))
            (setq result-list (append result-list list))))))
    result-list))


;;;###autoload
(defvar vmacs-consult--source-git
  `(:name     "GitFile"
              :narrow   ?g
              :category file
              :face     consult-file
              :history  file-name-history
              :action   ,#'consult--file-action
              :items
              ,(lambda ()
                 (require 'lazy-buffer)
                 (append (vmacs--git-files 0 "~/repos/vmacs")
                         (vmacs--git-files 0 "~/repos/dotfiles")
                         (vmacs--git-files 1 nil))))
  "Recent file candidate source for `consult-buffer'.")

;;;###autoload
(defvar vmacs-consult--source-dired
  `(:name     "Dired"
              :narrow   ?d
              :category file
              :face     consult-file
              :history  vmacs-dired-history
              :action   ,#'consult--file-action
              :items
              ,(lambda()(require 'vmacs-dired-history) vmacs-dired-history))
  "Recent dired candidate source for `consult-buffer'.")

;;
(setq vmacs-ignore-buffers
      (list
       "\\` " "\*Helm" "\*helm"
       "*Launch"
       "\*vc-diff\*" "\*magit-" "\*vc-" "\*vc*"
       "*Backtrace*" "*Package-Lint*" "\*Completions\*" "\*Compile-Log\*"
       "\*vc-change-log\*" "\*VC-log\*"
       "\*Async Shell Command\*" "\*Shell Command Output\*"
       "\*lsp" "\*ccls" "\*gopls" "\*bingo" "\*mspyls" "\*EGLOT"
       "\*sdcv\*" "\*tramp"  "\*Gofmt Errors\*"
       "\*Ido Completions\*" "\*Flycheck " "\*Flymake"
       "magit-process" "magit-diff" "magit-stash"))

(defvar boring-window-modes
  '(help-mode compilation-mode log-view-mode log-edit-mode
              org-agenda-mode magit-revision-mode ibuffer-mode))


(defun vmacs-filter(buf &optional ignore-buffers)
  (cl-find-if
   (lambda (f-or-r)
     (string-match-p f-or-r buf))
   (or ignore-buffers vmacs-ignore-buffers)))



(defun bury-boring-windows(&optional bury-cur-win-if-boring)
  "close boring *Help* windows with `C-g'"
  (let ((opened-windows (window-list))
        (cur-buf-win (get-buffer-window)))
    (dolist (win opened-windows)
      (with-current-buffer (window-buffer win)
        (when (or (memq  major-mode boring-window-modes)
                  (vmacs-filter (buffer-name)))
          (when (and (>  (length (window-list)) 1)
                     (not (minibufferp))
                     (or bury-cur-win-if-boring
                         (not (equal cur-buf-win win)))
                     (delete-window win))))))))

;;;###autoload
(defun vmacs-prev-buffer()
  (interactive)
  "switch to prev buffer ,but skip boring buffer."
  (let ((buf-name (buffer-name))
        (found  nil))
    (cl-loop until found do
             (previous-buffer)
             (unless (or (memq  major-mode boring-window-modes)
                         (vmacs-filter (buffer-name)))
               (setq found t))
             (when (string= (buffer-name) buf-name)
               (previous-buffer)
               (setq found t)))))

;;;###autoload
(defun vmacs-next-buffer()
  (interactive)
  "switch to next buffer ,but skip boring buffer."
  (let ((buf-name (buffer-name))
        (found  nil))
    (cl-loop until found do
             (next-buffer)
             (unless (or (memq  major-mode boring-window-modes)
                         (vmacs-filter (buffer-name) ))
               (setq found t))
             (when (string= (buffer-name) buf-name)
               (next-buffer)
               (setq found t)))))


(provide 'lazy-buffer)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-buffer.el ends here.
