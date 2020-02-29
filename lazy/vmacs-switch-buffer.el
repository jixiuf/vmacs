(require 'recentf)
(require 'magit)
;;;###autoload
(defun vmacs-switch-buffer ()
  "Open `recent-list' item in a new buffer.
The user's $HOME directory is abbreviated as a tilde."
  (interactive)
  (let* ((completion-styles '(flex))
         (icomplete-compute-delay 0)    ;do not delay
         (icomplete-delay-completions-threshold 1000000)
         (icomplete-separator "\n")
         (files (vmacs-switch-buffer--cands))
         (buf-or-file (completing-read "Switch to:" files nil t)))
    (if-let ((buf (get-buffer buf-or-file)))
        (pop-to-buffer-same-window buf)
      (find-file buf-or-file))))

(defun vmacs-switch-buffer--cands()
  (let ((bufs (vmacs-buffers))
        (recentf (vmacs-recentf))
        (gitfiles (vmacs-git-files)))
    (append bufs recentf  gitfiles)))

(setq vmacs-ignore-buffers
      (list
       "\\` " "\*Helm" "\*helm"
       "\*vc-diff\*" "\*magit-" "\*vc-" "\*vc*"
       "*Backtrace*" "*Package-Lint*" "\*Completions\*" "\*Compile-Log\*"
       "\*vc-change-log\*" "\*VC-log\*"
       "\*Async Shell Command\*" "\*Shell Command Output\*"
       "\*lsp" "\*ccls" "\*gopls" "\*bingo" "\*mspyls" "\*EGLOT"
       "\*sdcv\*" "\*tramp"  "\*Gofmt Errors\*"
       "\*Ido Completions\*" "\*Flycheck " "\*Flymake"
       "magit-process" "magit-diff" "magit-stash"))

(defun vmacs-buffers()
  (cl-remove-if
   (lambda (buf)
     (cl-find-if
      (lambda (f-or-r) (string-match-p f-or-r buf))
      vmacs-ignore-buffers))
   (mapcar (lambda(buf) (propertize (buffer-name buf) 'face 'shadow) )
           (buffer-list))))

(defun vmacs-recentf ()
  (mapcar #'abbreviate-file-name recentf-list ))

(defvar git-repos-files-cache (make-hash-table :test 'equal))
(defun vmacs-git-files ()
  "Append git files as virtual buffer"
  (let (result-list
        (default-directory default-directory)
        (magit-repos (mapcar 'car magit-repository-directories))
        list git-dir)
    (unless (file-remote-p default-directory)
      (setq git-dir (magit-toplevel))
      (when git-dir
        (setq git-dir (abbreviate-file-name (directory-file-name (file-truename git-dir))))
        (setq default-directory git-dir)
        (setq list (gethash git-dir  git-repos-files-cache))
        (when (or (not list) current-prefix-arg) ;prefix则会刷新缓存
          (setq list (split-string (shell-command-to-string (format "git ls-files --full-name --|sed \"s|^|%s/|g\"" default-directory)) "\n" t))
          (puthash git-dir list git-repos-files-cache))

        (setq result-list (append result-list list))))
    (dotimes (n 5 magit-repos)
      (let ((magit-repo (nth  n magit-repos)))
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
