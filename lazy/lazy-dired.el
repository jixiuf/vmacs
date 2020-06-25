;;; -*- lexical-binding: t; coding:utf-8 -*-
(eval-when-compile
  (require 'dired)
  (require 'dired-aux)
  (require 'dired-x)
  (require 'wdired))

;; use dired-narrow install
;; ;;;###autoload
;; (defun dired-name-filter-only-show-matched-lines(filter-regexp)
;;   (interactive "s(只显示匹配的文件):")
;;   (let ((dired-marker-char 16)
;;         (files (directory-files default-directory t)))
;;     ;;(dired-unmark-all-files dired-marker-char)
;;     (save-excursion
;;       (dolist (file files)
;;         (when (and (dired-goto-file  (expand-file-name file))
;;                    (not (string= "" filter-regexp))
;;                    (string-match filter-regexp (file-name-nondirectory file)))
;;           (dired-mark 1)
;;           )))
;;     (dired-toggle-marks)
;;     (dired-do-kill-lines nil (concat "Filter:'" filter-regexp "' omitted %d line%s"))
;;     (when (eobp) (forward-char -1))
;;     (dired-move-to-filename)))


;;;###autoload
(defun dired-beginning-of-buffer()
  (interactive)
  (if (= 3 (line-number-at-pos))
      (goto-char (point-min))
    (goto-char (point-min))
    (dired-next-line 2)))

;;;###autoload
(defun dired-end-of-buffer()
  (interactive)
  (goto-char (point-max))
  (dired-previous-line 1)
  )

;; C-a is nicer in dired if it moves back to start of files
;;;###autoload
(defun dired-smart-beginning-of-line ()
  (interactive)
  (let ((oldpos (point)))
    (dired-move-to-filename)
    (and (= oldpos (point))
         (beginning-of-line))))

;;;###autoload
(defun dired-add-to-load-path-or-load-it()
  "on `dired-mode',if thing under point is directory add it to `load-path'
if it is a el-file ,then `load' it"
  (interactive)
  (let ((dir-or-file  (dired-get-filename)))
    (if (file-directory-p dir-or-file)
        (progn
          (add-to-list 'load-path dir-or-file)
          (message (concat dir-or-file " added to load-path" ))
          )
      (if (string-equal  (file-name-extension  dir-or-file ) "el")
          (load  dir-or-file)
        (message (concat dir-or-file "is loaded"))
        ))))

;; ;; "在dired buffer中,如果mark了两个文件,则对此二文件进行diff
;; ;; ,如果仅mark了一个文件,则将其作为其中之一,另一个文件名则让用户选择,默认是光标下的文件
;; ;; ,如果没有mark任何文件,以当前文件作其一,另一,让用户选择.
;; ;; 默认使用ediff进行比较,`C-u'则使用diff"
;; ;;;###autoload
;; (defun dired-ediff(&optional arg)
;;   (interactive "P")
;;   (let ((marked-file-or-cur-file (dired-get-marked-files nil nil nil t))
;;         first  second)
;;     (cond
;;      ((and (= 2  (length marked-file-or-cur-file))
;;            (not (eq t (car marked-file-or-cur-file))));;mark 2 files
;;       (setq first (car marked-file-or-cur-file ))
;;       (setq second (nth 1 marked-file-or-cur-file )))
;;      ((and (= 2  (length marked-file-or-cur-file))
;;            (eq t (car marked-file-or-cur-file)));;mark 1 file
;;       (setq first (nth 1 marked-file-or-cur-file ))
;;       (setq second (read-file-name "Diff Marked file with(default:current file):"
;;                                    default-directory (dired-get-filename))))
;;      ((and (= 1 (length marked-file-or-cur-file))
;;            (not (file-directory-p (car marked-file-or-cur-file ))))
;;       (setq first (car marked-file-or-cur-file ));;no mark file
;;       (setq second (read-file-name "Diff current file with:" default-directory ))
;;       ))
;;     (if arg (progn (switch-to-buffer (diff first second))
;;                    (delete-other-windows))
;;       (ediff first second))))

(provide 'lazy-dired)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-dired.el ends here.
