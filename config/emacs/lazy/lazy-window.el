;;; -*- lexical-binding: t; -*-
;;; C-x2 ,C-x3 更好的分隔窗口
;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defvar split-window-status nil)
;;;###autoload
(defun vmacs-split-window-horizontally()
  (interactive)
  (split-window-horizontally)
  (setq split-window-status 'horizontally)
  (set-window-buffer (next-window) (other-buffer))
  )
;;;###autoload
(defun vmacs-split-window-vertically()
  (interactive)
  (split-window-vertically)
  (setq split-window-status 'vertically)
  (set-window-buffer (next-window) (other-buffer))
  )

;;;###autoload
(defun vmacs-split-frame-vertically()
  "for sway"
  (interactive)
  (when (eq system-type 'gnu/linux)
    (if (string-equal (getenv "XDG_SESSION_DESKTOP") "Hyprland")
        (call-process "hyprctl" nil nil nil "dispatch" "layoutmsg" "preselect" "d")
      (call-process "swaymsg" nil nil nil "splitv")))
  (make-frame))
;;;###autoload
(defun vmacs-split-frame-horizontally()
  "for sway"
  (interactive)
  (when (eq system-type 'gnu/linux)
    (if (string-equal (getenv "XDG_SESSION_DESKTOP") "Hyprland")
        (call-process "hyprctl" nil nil nil "dispatch" "layoutmsg" "preselect" "r")
    (call-process "swaymsg" nil nil nil "splith")))
  (make-frame))

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
;;;###autoload
(defun toggle-split-window()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (if (equal split-window-status 'horizontally)
        (vmacs-split-window-vertically)
      (vmacs-split-window-horizontally)
      )
    ))

;;;###autoload
(defun gui-frame-cnt()
  (let ((gui-frame-cnt 0))
    (dolist (frame (visible-frame-list))
      (with-selected-frame frame
        (when (frame-parameter frame 'window-id)
          (setq gui-frame-cnt (1+ gui-frame-cnt)))))
    gui-frame-cnt))
;;;###autoload
(defun vmacs-split-window-or-other-window()
  (interactive)
  (cond
   (  (equal 1 (count-windows))
      (vmacs-split-window-horizontally))
   (t
    (other-window 1)))
  )
;;;###autoload
(defun vmacs-split-window-or-prev-window()
  (interactive)
  (cond
   (  (equal 1 (count-windows))
      (vmacs-split-window-horizontally))
   (t
    (other-window -1)))
  )

;;;###autoload
(defun vmacs-window-rotate()
  "Rotates the windows according to the currenty cyclic ordering."
  (interactive)
  (let* ((wlist (window-list))
         (cur-win (car wlist))
         (blist (mapcar #'(lambda (w) (window-buffer w))
                        (window-list))))
    (setq blist (append (cdr blist) (list (car blist))))
    (while (and wlist blist)
      (set-window-buffer (car wlist) (car blist))
      (setq wlist (cdr wlist)
            blist (cdr blist)))
    (select-window cur-win)))

(defun win-xor (b1 b2)
  (or (and b1 b2)
      (and (not b1) (not b2))))

(defun move-border-left-or-right (arg dir)
  "General function covering move-border-left and move-border-right. If DIR is
     t, then move left, otherwise move right."
  (interactive)
  (if (null arg) (setq arg 3))
  (let ((left-edge (nth 0 (window-edges))))
    (if (win-xor (= left-edge 0) dir)
        (shrink-window arg t)
      (enlarge-window arg t))))

(defun move-border-up-or-down (arg dir)
  "General function covering move-border-up and move-border-down. If DIR is
     t, then move up, otherwise move down."
  (interactive)
  (if (null arg) (setq arg 2))
  (let ((top-edge (nth 1 (window-edges))))
    (if (win-xor (= top-edge 0) dir)
        (shrink-window arg nil)
      (enlarge-window arg nil))))

;;;###autoload
(defun move-border-left (arg)
  (interactive "P")
  (if (> (length (window-list)) 1)
      (move-border-left-or-right arg t)
    (call-process "resize-window" nil nil nil "left" "skip-emacs")))

;;;###autoload
(defun move-border-right (arg)
  (interactive "P")
  (if (> (length (window-list)) 1)
      (move-border-left-or-right arg nil)
    (call-process "resize-window" nil nil nil "right" "skip-emacs")))

;;;###autoload
(defun move-border-up (arg)
  (interactive "P")
  (if (> (length (window-list)) 1)
      (move-border-up-or-down arg t)
    (call-process "resize-window" nil nil nil "up" "skip-emacs")))

;;;###autoload
(defun move-border-down (arg)
  (interactive "P")
  (if (> (length (window-list)) 1)
      (move-border-up-or-down arg nil)
    (call-process "resize-window" nil nil nil "down" "skip-emacs")))

(provide 'lazy-window)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-window.el ends here.
