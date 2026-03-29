;;; conf-wayland.el --- Description -*- lexical-binding: t; -*-
(require 'reka)
(setopt reka-intercept-prefixes '("C-x" "C-u" "C-h" "M-x" "M-:" "C-<tab>"))
(global-set-key (kbd "C-<tab>") #'consult-buffer)


(setq wayland-compositor 'reka)
(require 'lazy-wayland)

(defun reka-get-window-info (&optional id)
  "Return window info alist (id app title pid) for surface ID.
 When ID is nil, use the current buffer's surface or compositor focus.
 Returns nil when no surface is found."
  (when-let* ((surface-id (or id  (when (reka--is-reka-buffer (or id (current-buffer)))(buffer-name) )))
              (buf (get-buffer surface-id))
              (bufname (buffer-name buf))
              (app (buffer-local-value 'reka-app-id buf))
              (title (substring bufname 0 (- (length bufname) (length app) 3)))
              )
    (list (cons 'id surface-id)
          (cons 'app app)
          (cons 'title title)
          (cons 'pid nil))))

(defun reka-get-window-info-json (&optional id)
  "Return window info as a JSON string.
 Calls `ewm-get-window-info' and serializes the result.
 When ID is nil, use the current buffer's surface or compositor focus.
 Returns fallback Emacs frame info when no surface is found.

 For use with `emacsclient -e \\='(ewm-get-window-info-json)'."

 (when (string-equal (buffer-name) " *server*")
    (select-window (car (window-list)) t))
  (json-encode (or (reka-get-window-info id)
                   `((id . 0)
                     (app . "emacs")
                     (title . ,(format "GNU/Emacs<%s>" default-directory))
                     (pid . ,(emacs-pid))))))


(defalias 'ewm-get-window-info-json 'reka-get-window-info-json)


(provide 'conf-wayland)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-wayland.el ends here.
