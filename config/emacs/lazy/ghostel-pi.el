;;; ghostel-pi.el --- Toggle pi coding agent in Ghostel -*- lexical-binding: t; -*-

(require 'ghostel)
(require 'project)

(defun ghostel-pi--find-buffer (root buf-name)
  "Return the live ghostel buffer running pi for project ROOT.
Buffers are matched by `ghostel-identity' (stable across renames and
`cd'), falling back to `get-buffer' by BUF-NAME."
  (or (seq-find
       (lambda (b)
         (and (buffer-live-p b)
              (with-current-buffer b
                (and (derived-mode-p 'ghostel-mode)
                     (ghostel-identity-match-p
                      `((kind . exec)
                        (project-root . ,(ghostel--normalize-root root)))
                      ghostel-identity)))))
       (buffer-list))
      (get-buffer buf-name)))

;;;###autoload
(defun ghostel-pi-project-toggle-chat ()
  "Toggle the pi coding agent chat buffer for the current project.

If a pi buffer for the current project exists:
  - If it is the current buffer, bury it.
  - Otherwise, switch to it.
If no such buffer exists, create a new Ghostel terminal running `pi'
in the project root directory."
  (interactive)
  (let* ((proj (project-current t))
         (root (project-root proj))
         (proj-name (project-name proj))
         (buf-name (format "*pi-%s*" proj-name))
         (buf (ghostel-pi--find-buffer root buf-name)))
    (cond
     ;; Buffer exists and is current → bury it
     ((and buf (eq buf (current-buffer)))
      (bury-buffer))
     ;; Buffer exists but is not current → switch to it
     (buf
      (pop-to-buffer buf (append display-buffer--same-window-action
                                  '((category . comint)))))
     ;; Buffer doesn't exist → create it and run pi
     (t
      (let ((default-directory root)
            (buffer (get-buffer-create buf-name)))
        (pop-to-buffer buffer (append display-buffer--same-window-action
                                       '((category . comint))))
        ;; Store the project-scoped identity so `ghostel-pi--find-buffer'
        ;; can match by `ghostel-identity' instead of buffer name.
        (ghostel-exec buffer "pi" nil
                      `((kind . exec)
                        (command . ("pi"))
                        (project-root . ,(ghostel--normalize-root root))))
        ;; Keep the fixed name after ghostel-exec (which may reset
        ;; buffer-local state when initializing the terminal).
        (with-current-buffer buffer
          (setq-local ghostel-buffer-name-function nil)))))))

(provide 'ghostel-pi)
;;; ghostel-pi.el ends here
