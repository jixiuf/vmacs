(defvar  vmacs-switch-buffer-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-k") 'ivy-switch-buffer-kill)
    (define-key map (kbd "s-o") 'vmacs-switch-buffer-other-window)
    (define-key map (kbd "C-M-s-o") 'vmacs-switch-buffer-other-window)
    (define-key map (kbd "C-d") 'vmacs-switch-buffer-dired)
    (define-key map (kbd "C-M-s-i") 'vmacs-ivy-dropto-counsel-git)
    (define-key map (kbd "s-n") 'ivy-next-line)
    (define-key map (kbd "C-M-s-n") 'ivy-next-line)
    (define-key map (kbd "C-M-s-p") 'ivy-previous-line)
    (define-key map (kbd "s-p") 'ivy-previous-line)
    map))


;;;###autoload
(defun vmacs-switch-buffer()
  "Switch to another buffer."
  (interactive)
  (let ((this-command 'vmacs-switch-buffer))
    (ivy-read "Switch to Buffer or File: " (vmacs-switch-buffer--cands)
              :keymap vmacs-switch-buffer-map
              :action #'vmacs-switch-buffer-action
              :caller #'vmacs-switch-buffer
              )))

(defun vmacs-switch-buffer--cands()
  (let ((bufs (vmacs-switch-buffer--list))
        (recentf (vmacs-recentf))
        (views (ivy-source-views))
        (gitfiles (vmacs-git-files))
        )
    (append bufs recentf views gitfiles)))

(defun vmacs-switch-buffer--list()
  "Return list of buffer-related lines in Ibuffer as strings."
  (let ((oldbuf (get-buffer "*Ibuffer*")))
    (unless oldbuf
      ;; Avoid messing with the user's precious window/frame configuration.
      (save-window-excursion
        (let ((display-buffer-overriding-action
               '(display-buffer-same-window (inhibit-same-window . nil))))
          (ibuffer nil "*Ibuffer*" nil t))))
    (with-current-buffer "*Ibuffer*"
      (when oldbuf
        ;; Forcibly update possibly stale existing buffer.
        (ibuffer-update nil t))
      (goto-char (point-min))
      (let ((ibuffer-movement-cycle nil)
            entries)
        (while (not (eobp))
          (ibuffer-forward-line 1 t)
          (let ((buf (ibuffer-current-buffer)))
            ;; We are only interested in buffers we can actually visit.
            ;; This filters out headings and other unusable entries.
            (when (buffer-live-p buf)
              (unless (vmacs-filter (buffer-name buf) ivy-ignore-buffers)
                (push (cons (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))
                            buf)
                      entries)))))
        (nreverse entries)))))

(defun vmacs-switch-buffer-action(x)
  (let ((view (assoc x ivy-views)))
    (cond
     (view
      (delete-other-windows)
      (let (;; silence "Directory has changed on disk"
            (inhibit-message t))
        (ivy-set-view-recur (cadr view))))
     ((bufferp x)
      (switch-to-buffer x))
     ((and (stringp x) (file-exists-p x))
      (find-file x))
     ((listp x)
      (let ((buf-of-file (cdr x)))
        (cond
         ((bufferp buf-of-file)
          (switch-to-buffer buf-of-file))
         ((and (stringp buf-of-file) (file-exists-p buf-of-file))
          (find-file buf-of-file))))))))


(defun vmacs-recentf ()
  (mapcar #'abbreviate-file-name recentf-list ))

(defvar git-repos-files-cache (make-hash-table :test 'equal))
(defun vmacs-git-files ()
  "Append git files as virtual buffer"
  (let (result-list
        (default-directory default-directory)
        (magit-repos (mapcar 'car magit-repository-directories))
        list counsel--git-dir)
    (unless (file-remote-p default-directory)
      (setq counsel--git-dir (counsel--git-root))
      (when counsel--git-dir
        (setq counsel--git-dir (abbreviate-file-name (directory-file-name (file-truename counsel--git-dir))))
        (setq default-directory counsel--git-dir)
        (setq list (gethash counsel--git-dir  git-repos-files-cache))
        (when (or (not list) current-prefix-arg) ;prefix则会刷新缓存
          (setq list (split-string (shell-command-to-string (format "git ls-files --full-name --|sed \"s|^|%s/|g\"" default-directory)) "\n" t))
          (puthash counsel--git-dir list git-repos-files-cache))

        (setq result-list (append result-list list))))
    (dotimes (n 5 magit-repos)
      (let ((magit-repo (nth  n magit-repos)))
        (when magit-repo
          (setq magit-repo (abbreviate-file-name (directory-file-name (file-truename magit-repo))))
          (unless (string-equal magit-repo counsel--git-dir)
            (setq default-directory magit-repo)
            (setq list (gethash magit-repo  git-repos-files-cache))
            (when (or (not list) current-prefix-arg)
              (setq list (split-string (shell-command-to-string (format "git ls-files --full-name --|sed \"s|^|%s/|g\"" default-directory)) "\n" t))
              (puthash magit-repo list git-repos-files-cache))
            (setq result-list (append result-list list))))))
    result-list))


;;;###autoload
(defun vmacs-switch-buffer-dired(&optional arg)
  (interactive "P")
  (if (eolp)
      (ivy-quit-and-run
        (let ((x (nth ivy--index (ivy-state-collection ivy-last) )))
          (cond
           ((bufferp x)
            (with-current-buffer x (dired default-directory)))
           ((and (stringp x) (file-exists-p x))
            (dired (file-name-directory x)))
           ((listp x)
            (let ((buf-of-file (cdr x)))
              (cond
               ((bufferp buf-of-file)
                (with-current-buffer buf-of-file (dired default-directory)))
               ((and (stringp buf-of-file) (file-exists-p buf-of-file))
                (dired (file-name-directory buf-of-file)))))))))
    (call-interactively 'delete-char)))


(defun vmacs-switch-buffer-other-window()
  (interactive)
  (ivy-quit-and-run
    (let ((x (nth ivy--index (ivy-state-collection ivy-last) )))
      (cond
       ((bufferp x)
        (switch-to-buffer-other-window x))
       ((and (stringp x) (file-exists-p x))
        (find-file-other-window x))
       ((listp x)
        (let ((buf-of-file (cdr x)))
          (cond
           ((bufferp buf-of-file)
            (switch-to-buffer-other-window buf-of-file))
           ((and (stringp buf-of-file) (file-exists-p buf-of-file))
            (find-file-other-window buf-of-file)))))))
    )
  )
;; (ivy-set-sources 'vmacs-switch-buffer
;;                  '((original-source) (ivy-source-views)
;;                    (vmacs-recentf) (vmacs-git-files)))
