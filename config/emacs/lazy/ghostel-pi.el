;;; ghostel-pi.el --- Toggle pi coding agent in Ghostel -*- lexical-binding: t; -*-

(require 'ghostel)
(require 'ghostel-project)
(require 'project)

(defun ghostel-pi--find-buffer (root)
  "Return the pi ghostel buffer for project ROOT by `ghostel-identity'.
Identity matching is stable across buffer renames and shell `cd'."
  (seq-find
   (lambda (b)
     (with-current-buffer b
       (and (derived-mode-p 'ghostel-mode)
            (ghostel-identity-match-p
             `((kind . exec)
               (command . ("pi"))
               (project-root . ,(ghostel--normalize-root root)))
             ghostel-identity))))
   (buffer-list)))

;;;###autoload
(defun ghostel-pi-project-toggle-chat ()
  "Toggle the pi coding agent chat buffer for the current project.

If a pi buffer for the current project exists:
  - If it is the current buffer, bury it.
  - Otherwise, switch to it.
If no such buffer exists, create a new Ghostel terminal running `pi'
in the project root directory.

Buffers follow the `ghostel-project' conventions (project-root
identity key and project-prefixed naming), so they also show up in
`ghostel-project-next' and `ghostel-project-list-buffers'."
  (interactive)
  (let* ((proj (project-current t))
         (root (project-root proj))
         (buf-name (format "*%s-pi*" (project-name proj)))
         (buf (ghostel-pi--find-buffer root)))
    (cond
     ;; Buffer exists and is current → bury the whole ghostel group and
     ;; land on the main group
     ((and buf (eq buf (current-buffer)))
      (vmacs-bury-tab-group 'ghostel))
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
        (ghostel-exec buffer "pi" nil
                      `((kind . exec)
                        (command . ("pi"))
                        (project-root . ,(ghostel--normalize-root root)))))))))

(provide 'ghostel-pi)
;;; ghostel-pi.el ends here
