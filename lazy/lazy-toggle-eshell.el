(eval-when-compile
  (require 'em-term)
  (require 'em-hist)
  (require 'eshell))

;; 在当前目录打开eshell
;; ctrl-u 可打开多个eshell
;;;###autoload
(defun toggle-eshell-cd(&optional arg dir)
  (interactive "P")
  (let ((dest-dir-cd (or dir default-directory))
        (shell-buffer-name (toggle-shell-completing-read-buffer-name
                            arg (generate-new-buffer-name (format "*eshell*  (%s)"  default-directory)))))
    (toggle-eshell-internal shell-buffer-name)
    (with-current-buffer shell-buffer-name
      (goto-char (point-max))
      ;; (eshell-next-prompt 1)            ;
      (eshell-bol)
      (delete-region (point) (line-end-position))
      (cd dest-dir-cd)
      (insert (concat "cd \"" dest-dir-cd "\""))
      (eshell-send-input))))

;;;###autoload
(defun toggle-eshell(&optional arg dir)
  (interactive "P")
  (toggle-eshell-internal  (toggle-shell-completing-read-buffer-name
                            arg (generate-new-buffer-name (format "*eshell*  (%s)"  default-directory)))))

;;;###autoload
(defun toggle-eshell-new(&optional dir)
  (interactive )
  (let ((buffer-name (generate-new-buffer-name (format "*eshell*  (%s)"  default-directory))))
    (toggle-eshell-internal  buffer-name)))

(defun vmscs-eshell-next-index ()
  (let* ((current-buffer-index (cl-position (current-buffer) shell-buffer-hist))
         (switch-index (if current-buffer-index
                           (if (>= current-buffer-index (- (length shell-buffer-hist) 1))
                               0
                             (+ 1 current-buffer-index))
                         nil)))
    (when (equal current-buffer-index switch-index)
      (message "no next eshell buffer")
      )
    switch-index))

;;;###autoload
(defun vmscs-eshell-next ()
  "Select next eshell buffer.
Create new one if no eshell buffer exists."
  (interactive)
  (let ((switch-index (vmscs-eshell-next-index)))
    (when switch-index
      (switch-to-buffer  (nth switch-index shell-buffer-hist))
      (vmacs-eshell-update-hist))))

(defun vmscs-eshell-prev ()
  "Select previous eshell buffer.
Create new one if no eshell buffer exists."
  (interactive)
  (let* ((current-buffer-index (cl-position (current-buffer) shell-buffer-hist))
         (switch-index (if current-buffer-index
                           (if (<= current-buffer-index 0)
                               (- (length shell-buffer-hist) 1)
                             (- current-buffer-index 1))
                         (- (length shell-buffer-hist) 1))))
    (switch-to-buffer (nth switch-index shell-buffer-hist))
    (vmacs-eshell-update-hist)))


(defvar eshll-toggle-commands '(toggle-eshell-cd toggle-eshell  toggle-shell))
(defvar vmacs-window-configration nil)

(defun toggle-eshell-internal (&optional  shell-buffer-name)
  (interactive "sshell name:\nsshell buffer name:")
  (if (and (get-buffer shell-buffer-name)
           (buffer-live-p (get-buffer shell-buffer-name)))
      (cond
       ( (not (string= (buffer-name) shell-buffer-name))
         (setq vmacs-window-configration (current-window-configuration))
         (pop-to-buffer shell-buffer-name)
         (delete-other-windows)
         (vmacs-eshell-update-hist)
         )
       ;; ((and (string= (buffer-name) shell-buffer-name)
       ;;       (> (length (window-list)) 1)
       ;;       (member last-command eshll-toggle-commands))
       ;;  (delete-other-windows)
       ;;  )
       ((string= (buffer-name) shell-buffer-name)
        (bury-buffer)
        (set-window-configuration vmacs-window-configration)
        ))
    (setq vmacs-window-configration (current-window-configuration))
    (setq eshell-buffer-name shell-buffer-name)
    (eshell)
    (goto-char (point-max))
    ;; (insert (concat "cd " (concat "\""default-directory "\""))) ;;make sure current directory is default-directory
    ;; (eshell-send-input)
    (delete-other-windows)
    (pop-to-buffer shell-buffer-name)
    (vmacs-eshell-update-hist)

    )


  )
(defun vmacs-eshell-update-hist(&optional buf)
  (add-to-list 'shell-buffer-hist (or buf (current-buffer)) t)
  (setq  last-shell-buffer (or buf (current-buffer))))

(defvar shell-buffer-hist nil)
(defvar last-shell-buffer nil)

(defun toggle-shell-completing-read-buffer-name(arg &optional default-buffer-name-when-no-hist )
  (let* ((default-shell-buffer
           (if (and last-shell-buffer (buffer-live-p last-shell-buffer))
               (buffer-name last-shell-buffer) default-buffer-name-when-no-hist ))
         (buffer-name default-shell-buffer))
    (when arg
      (setq buffer-name (completing-read (concat "shell buffer name(default:"
                                                 (if (string-match "^\\*" default-shell-buffer)
                                                     default-shell-buffer
                                                   (concat "*"  default-shell-buffer "*"))
                                                 "):")
                                         (mapcar 'buffer-name shell-buffer-hist) nil nil nil nil default-shell-buffer ))
      (unless (string-match "^\\*" buffer-name)
        (setq buffer-name (concat "*eshell* "  buffer-name ))) )
    buffer-name))


(defadvice eshell-send-input (around change-buffer-name activate)
  "change-buffer-name."
  (let ((input (eshell-get-old-input))
        (eshell-buffer)
        )
    ad-do-it
    (setq eshell-buffer (generate-new-buffer-name (format "*eshell* %s (%s)"  input default-directory)))
    (rename-buffer eshell-buffer)))

(defun vmacs-eshell-remove-from-shell-buffer-hist()
  (let ((current-buffer-index (cl-position (current-buffer) shell-buffer-hist))
        (next-index (vmscs-eshell-next-index)))
    (when (eq (current-buffer) last-shell-buffer)
      (if (or (not next-index)
              (equal current-buffer-index next-index))
          (setq last-shell-buffer nil)
        (setq last-shell-buffer (nth next-index shell-buffer-hist))))
    (setq shell-buffer-hist (delq (current-buffer) shell-buffer-hist))
    ))

(add-hook 'eshell-exit-hook #'vmacs-eshell-remove-from-shell-buffer-hist)

(defun eshell-insert-last-cmd-argument()
  "like Alt-. in bash"
  (interactive)
  (let* ((last-hist (eshell-get-history 0))
        (last-argv (last (split-string last-hist "[ \t]+"))))
    (when last-argv (insert (car last-argv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shared history.
(defvar eshell-history-global-ring nil
  "The history ring shared across Eshell sessions.")

;;;###autoload
(defun eshell-hist-use-global-history ()
  "Make Eshell history shared across different sessions."
  (unless eshell-history-global-ring
    (let (eshell-history-ring)
      (when eshell-history-file-name
        (eshell-read-history nil t))
      (setq eshell-history-global-ring eshell-history-ring))
    (unless eshell-history-ring (setq eshell-history-global-ring (make-ring eshell-history-size))))
  (setq eshell-history-ring eshell-history-global-ring))


(provide 'lazy-toggle-eshell)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-toggle-eshell.el ends here.
