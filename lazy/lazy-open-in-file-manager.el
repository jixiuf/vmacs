;;; -*- lexical-binding: t; coding:utf-8 -*-
;;用资源管理器打开当前文件所处目录 (open-in-filemanager)

;; 你可能需要调整你的文件管理器， 这只是个demo for linux
 ;; linux下文件管理器太多
(defsubst open-directory-with-pcmanfm()
  "a pcmanfm file manager for linux"
  (start-process "pcmanfm"  nil "pcmanfm" (expand-file-name  default-directory)))

;;;###autoload
(defun open-in-filemanager()
  (interactive)
  (when (eq system-type 'darwin) (reveal-in-osx-finder))
  (when (eq system-type 'windows-nt) (explorer-open))
  (when (eq system-type 'gnu/linux) (open-directory-with-pcmanfm)))


(defun w32explore (file)
  "Open Windows Explorer to FILE (a file or a folder)."
  (interactive "fFile: ")
  (let ((w32file (convert-standard-filename file)))
    (if (file-directory-p w32file)
        (w32-shell-execute "explore" w32file "/e,/select,")
      (w32-shell-execute "open" "explorer" (concat "/e,/select," w32file)))))

(defun explorer-open()
  "用windows 上的explorer.exe打开此文件夹."
  (interactive)
  (if (equal major-mode 'dired-mode)
      (w32explore (expand-file-name (dired-get-filename)))
    (w32explore (expand-file-name  (or (buffer-file-name) "~")))))



(defun reveal-in-osx-finder ()
  "Reveal the file associated with the current buffer in the OS X Finder.
In a dired buffer, it will open the current directory."
  (interactive)
  (let* ((path (buffer-file-name)) ; The full file path associated with the buffer.
	 (filename-at-point (dired-file-name-at-point)) ; effective in dired only
	 ;; Create a full path if filename-at-point is non-nil
	 (filename-at-point (if filename-at-point
				(expand-file-name filename-at-point) ; full path
			      nil)) ; if nil, return nil
	 dir file)		   ; let* definition part ends here.

    ;; Conditionals: The first one that is non-nil is executed.
    (cond (path
	   ;; If path is non-nil,
	   (setq dir  (file-name-directory    path))
	   (setq file (file-name-nondirectory path)))

	  (filename-at-point
	   ;; If filename-at-point is available from dired,
	   (setq dir  (file-name-directory    filename-at-point))
	   (setq file (file-name-nondirectory filename-at-point)))

	  (t
	   ;; Otherwise,
	   (setq dir  (expand-file-name default-directory))))

    ;; Pass dir and file to the helper function.
    ;; (message (concat "dir:" dir " ; file:" file " ; path:" path " ; fap:" filename-at-point)) ; for debugging
    (reveal-in-osx-finder-as dir file) ; These variables are  passed to the helper function.
    ))


;; AppleScript helper function. Thanks milkeypostman for suggestions.
;; Use let* to reuse revealpath in defining script.
(defun reveal-in-osx-finder-as (dir file)
  "A helper function for reveal-in-osx-finder.
This function runs the actual AppleScript."
  (let* ((revealpath (if file		   ; Define revealpath local variable.
			 (concat dir file) ; dir/file if file name available.
		       dir))		   ; dir only if not.
	 (script			   ; Define script variable using revealpath and text.
	  (concat
	   "set thePath to POSIX file \"" revealpath "\"\n"
	   "tell application \"Finder\"\n"
	   " set frontmost to true\n"
	   " reveal thePath \n"
	   "end tell\n")))		   ; let* definition part ends here.
    ;; (message script)			   ; Check the text output.
    (start-process "osascript-getinfo" nil "osascript" "-e" script) ; Run AppleScript.
    ))




(provide 'lazy-open-in-file-manager)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-openwith.el ends here.
