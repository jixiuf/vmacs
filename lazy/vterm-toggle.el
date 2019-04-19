;;; vterm-toggle.el --- Toggle to and from the vterm buffer

;; Author: jixiuf  jixiuf@qq.com
;; Keywords: vterm
;; URL: https://github.com/jixiuf/vterm-toggle

;; Copyright (C) 2019, jixiuf, all rights reserved.

;; This file is NOT part of GNU Emacs.
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; Description:
;;
;; Provides the command vterm-toggle which toggles between the
;; vterm buffer and whatever buffer you are editing.
;;
;; This is done in an "intelligent" way.  Features are:
;; o Starts a vterm if none is existent.
;; o Minimum distortion of your window configuration.
;; o When done in the vterm-buffer you are returned to the same window
;;   configuration you had before you toggled to the shell.
;; o If you desire, you automagically get a "cd" command in the shell to the
;;   directory where your current buffers file exists( even in a ssh session); just call
;;   vterm-toggle-cd instead of vterm-toggle.
;;

;;

;;; Code:
(require 'tramp)
(require 'vterm)
(require 'evil nil  t)


(defvar vterm-toggle-window-configration nil)

(defcustom vterm-toggle-evil-state-when-enter 'insert
  "Default evil state for vterm buffer."
  :group 'vterm-toggle
  :type 'symbolp)
(defcustom vterm-toggle-evil-state-when-leave 'normal
  "Default evil state for vterm buffer."
  :group 'vterm-toggle
  :type 'symbolp)

(defcustom vterm-toggle-prompt-regexp
  (concat "\\(?:^\\|\r\\)"
	      "[^]#$%>\n]*#?[]#$%➜⇒»☞\[@λ] *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*")
  "vterm prompt regexp. "
  :group 'vterm-toggle
  :type 'string)
(defcustom vterm-toggle-fullscreen-p t
  "vterm prompt regexp. "
  :group 'vterm-toggle
  :type 'boolean)

(defcustom vterm-toggle-use-dedicated-buffer nil
  "only toggle to or from dedicated vterm buffer."
  :group 'vterm-toggle
  :type 'boolean)

(defcustom vterm-toggle-after-ssh-login-function nil
  "those functions are called one by one after open a ssh session with 4 arguments.
`vterm-toggle-after-ssh-login-function' should be a symbol, a hook variable.
The value of HOOK may be nil, a function, or a list of functions.
for example
(defun vterm-toggle-after-ssh-login (user host port localdir)
    (when (equal host \"my-host\")
        (vterm-send-string \"zsh\" t)
        (vterm-send-key \"<return>\" nil nil nil))) "
  :group 'vterm-toggle
  :type 'hook)

(defvar vterm-toggle--vterm-dedicated-buffer nil)
(defvar vterm-toggle--vterm-buffer-p-function 'vterm-toggle--default-vterm-mode-p
  "Function to check whether a buffer is vterm-buffer mode. ")
(defvar vterm-toggle--buffer-list nil
  "The list of non-dedicated terminal buffers managed by `vterm-toggle'.")

(defun vterm-toggle--default-vterm-mode-p(&optional args)
  (derived-mode-p 'vterm-mode))


(defun vterm-toggle--switch-evil-state (state)
  (when (featurep 'evil)
    (funcall (intern (format "evil-%S-state" state)))))

;;;###autoload
(defun vterm-toggle(&optional args)
  (interactive "P")
  (cond
   ((funcall vterm-toggle--vterm-buffer-p-function args)
    (vterm-toggle-hide))
   (t
    (vterm-toggle-show nil args))))

;;;###autoload
(defun vterm-toggle-cd(&optional args)
  (interactive "P")
  (cond
   ((funcall vterm-toggle--vterm-buffer-p-function args)
    (vterm-toggle-hide args))
   (t
    (vterm-toggle-show t args))))

(defun vterm-toggle-hide(&optional args)
  (interactive "P")
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (funcall vterm-toggle--vterm-buffer-p-function args)
        (vterm-toggle--switch-evil-state vterm-toggle-evil-state-when-leave)
        (bury-buffer))))
  (when vterm-toggle-window-configration
    (set-window-configuration vterm-toggle-window-configration))
  (when (funcall vterm-toggle--vterm-buffer-p-function args)
    (switch-to-buffer (vterm-toggle--recent-other-buffer))))

(defun vterm-toggle-show(&optional make-cd args)
  (interactive)
  (let* ((shell-buffer (vterm-toggle--get-buffer make-cd args))
         (dir (and make-cd
                   (expand-file-name (or list-buffers-directory default-directory))))
         cd-cmd cur-host vterm-dir vterm-host cur-user cur-port remote-p)
    (when make-cd
      (if (ignore-errors (file-remote-p (or list-buffers-directory default-directory)))
          (with-parsed-tramp-file-name (or list-buffers-directory default-directory) nil
            (setq remote-p t)
            (setq cur-host host)
            (setq cur-user user)
            (setq cur-port (if port (concat ":" port) ""))
            (setq dir localname))
        (setq cur-host (system-name)))
      (setq cd-cmd (concat " cd " (shell-quote-argument dir))))
    (if shell-buffer
        (progn
          (unless (funcall vterm-toggle--vterm-buffer-p-function args)
            (setq vterm-toggle-window-configration (current-window-configuration)))
          (pop-to-buffer shell-buffer)
          (with-current-buffer shell-buffer
            (when (derived-mode-p 'vterm-mode)
              (if (ignore-errors (file-remote-p default-directory))
                  (with-parsed-tramp-file-name default-directory nil
                    (setq vterm-dir localname)
                    (setq vterm-host host))
                (setq vterm-dir default-directory)
                (setq vterm-host (system-name)))
              (when (and (not (equal vterm-dir dir))
                         (equal vterm-host cur-host))
                (vterm-send-key "u" nil nil t)
                (vterm-send-string cd-cmd t)
                (vterm-send-key "<return>" nil nil nil)))
            (vterm-toggle--switch-evil-state vterm-toggle-evil-state-when-enter))
          (when vterm-toggle-fullscreen-p
            (delete-other-windows)))
      (setq vterm-toggle-window-configration (current-window-configuration))
      (with-current-buffer (vterm-toggle--new)
        (when remote-p
          (if cur-user
              (vterm-send-string (format "ssh %s@%s%s" cur-user cur-host cur-port) t)
            (vterm-send-string (format "ssh %s%s"  cur-host cur-port) t))
          (vterm-send-key "<return>" nil nil nil)
          (run-hook-with-args 'vterm-toggle-after-ssh-login-function
                              cur-user cur-host cur-port dir)
          (vterm-send-string cd-cmd t)
          (vterm-send-key "<return>" nil nil nil))
        (when vterm-toggle-fullscreen-p
          (delete-other-windows))
        (vterm-toggle--switch-evil-state vterm-toggle-evil-state-when-enter)))))

(defun vterm-toggle--new()
  (if vterm-toggle-fullscreen-p
    (vterm)
    (vterm-other-window)))

(defun vterm-toggle--skip-prompt ()
  "Skip past the text matching regexp `vterm-toggle-prompt-regexp'.
If this takes us past the end of the current line, don't skip at all."
  (let ((eol (line-end-position)))
    (when (and (looking-at vterm-toggle-prompt-regexp)
	           (<= (match-end 0) eol))
      (goto-char (match-end 0)))))

(defun vterm-toggle--accept-cmd-p ()
  (save-excursion
    (goto-char (point-at-bol))
    (vterm-toggle--skip-prompt)))


(defun vterm-toggle--get-buffer(&optional make-cd args)
  (if vterm-toggle-use-dedicated-buffer
      (vterm-toggle--get-dedicated-buffer)
    (vterm-toggle--recent-vterm-buffer make-cd args)
      )
  )
(defun vterm-toggle--get-dedicated-buffer()
  (if (buffer-live-p vterm-toggle--vterm-dedicated-buffer)
      vterm-toggle--vterm-dedicated-buffer
    (setq vterm-toggle--vterm-dedicated-buffer (vterm-toggle--new))))

(defun vterm-toggle--recent-vterm-buffer(&optional make-cd args)
  (let ((shell-buffer)
        buffer-host
        vterm-host)
    (if (ignore-errors (file-remote-p default-directory))
        (with-parsed-tramp-file-name default-directory nil
          (setq buffer-host host))
      (setq buffer-host (system-name)))

    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (funcall vterm-toggle--vterm-buffer-p-function args)
          (cond
           ((and (derived-mode-p 'vterm-mode)
                 make-cd)
            (if (ignore-errors (file-remote-p default-directory))
                (with-parsed-tramp-file-name default-directory nil
                  (setq vterm-host host))
              (setq vterm-host (system-name)))
            (when (and (vterm-toggle--accept-cmd-p)
                       (equal buffer-host vterm-host))
              (unless shell-buffer
                (setq shell-buffer buf))))
           (t
            (unless shell-buffer
              (setq shell-buffer buf)))))))
    shell-buffer))

(defun vterm-toggle--recent-other-buffer(&optional args)
  (let ((list (buffer-list))
        (index 0)
        shell-buffer buf )
    (cl-loop until shell-buffer do
             (setq buf (nth index list))
             (with-current-buffer buf
               (when (and (not (funcall vterm-toggle--vterm-buffer-p-function args))
                          (not (char-equal ?\  (aref (buffer-name) 0))))
                 (setq shell-buffer buf)))
             (setq index (1+ index)))
    shell-buffer))

(defun vterm-toggle--exit-hook()
  (when (derived-mode-p 'vterm-mode)
    (setq vterm-toggle--buffer-list
	      (delq (current-buffer) vterm-toggle--buffer-list))
    (when vterm-toggle-window-configration
      (set-window-configuration vterm-toggle-window-configration))))

(add-hook 'kill-buffer-hook 'vterm-toggle--exit-hook)
;; (add-hook 'vterm-exit-functions #'vterm-toggle--exit-hook)

(defun vterm-toggle--mode-hook()
    (add-to-list 'vterm-toggle--buffer-list (current-buffer)))
(add-hook 'vterm-mode-hook 'vterm-toggle--mode-hook)

(defun vterm-toggle--switch (direction offset)
  "Internal `vterm-toggle' buffers switch function.
If DIRECTION is `forward', switch to the next term.
If DIRECTION `backward', switch to the previous term.
Option OFFSET for skip OFFSET number term buffer."
  (if vterm-toggle--buffer-list
      (let ((buffer-list-len (length vterm-toggle--buffer-list))
	        (index (position (current-buffer) vterm-toggle--buffer-list)))
	    (if index
	        (let ((target-index (if (eq direction 'forward)
				                    (mod (+ index offset) buffer-list-len)
				                  (mod (- index offset) buffer-list-len))))
	          (switch-to-buffer (nth target-index vterm-toggle--buffer-list)))
	      (switch-to-buffer (car vterm-toggle--buffer-list))))
    nil))

;;;###autoload
(defun vterm-toggle-forward (&optional offset)
  "Go to the next term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
  (interactive "P")
  (vterm-toggle--switch 'forward (or offset 1)))

;;;###autoload
(defun vterm-toggle-backward (&optional offset)
  "Go to the previous term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
  (interactive "P")
  (vterm-toggle--switch 'backward (or offset 1)))

(provide 'vterm-toggle)

;; Local Variables:
;; coding: utf-8
;; End:

;;; vterm-toggle.el ends here.
