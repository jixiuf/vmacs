;;; ghostel-pi.el --- Toggle pi coding agent in Ghostel -*- lexical-binding: t; -*-

(require 'ghostel)
(require 'project)

(defun ghostel-pi--find-buffer (identity)
  "Return the live ghostel-mode buffer whose buffer-identity equals IDENTITY.
Falls back to `get-buffer' by IDENTITY (the buffer name)."
  (or (seq-find (lambda (b)
                  (and (buffer-live-p b)
                       (with-current-buffer b
                         (derived-mode-p 'ghostel-mode))
                       (equal (buffer-local-value 'ghostel--buffer-identity b)
                              identity)))
                (buffer-list))
      (get-buffer identity)))

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
         (buf (ghostel-pi--find-buffer buf-name)))
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
        (ghostel-exec buffer "pi")
        ;; Set after ghostel-exec (which calls ghostel--init-buffer →
        ;; ghostel-mode → kill-all-local-variables), so our local
        ;; settings survive.
        (with-current-buffer buffer
          (setq-local ghostel-buffer-name-function nil)
          (setq ghostel--buffer-identity buf-name)))))))

(provide 'ghostel-pi)
;;; ghostel-pi.el ends here
