;;; -*- lexical-binding: t; coding:utf-8 -*-
(eval-when-compile (require 'evil)
                   (require 'pulse))
(autoload 'pulse-momentary-highlight-region "pulse")

(defvar evil-mark-funs-marker nil)

(defun go-back-after-mark-region()
  (when (and evil-mark-funs-marker
             (not mark-active))
    (evil-jump-backward)
    (setq evil-mark-funs-marker nil)))

  (add-hook 'post-command-hook 'go-back-after-mark-region)

;;;###autoload
(defun evil-mark-defun(&optional _arg)
  "call function binding to `C-M-h'"
  (interactive)
  (setq evil-mark-funs-marker (point-marker))
  (evil-set-jump)
  (call-interactively (key-binding (kbd "C-M-h")))
  (message (concat "call function: "
                   (symbol-name (key-binding (kbd "C-M-h"))))))
;;;###autoload
(defun evil-M-h(&optional _arg)
  "call function binding to `M-h'"
  (interactive)
  (setq evil-mark-funs-marker (point-marker))
  (evil-set-jump)
  (call-interactively (key-binding (kbd "M-h")))
  (message (concat "call function: "
                   (symbol-name (key-binding (kbd "M-h"))))))
;;;###autoload
(defun evil-mark-whole-buffer(&optional _arg)
  "call function binding to `C-xh'"
  (interactive)
  (setq evil-mark-funs-marker (point-marker))
  (evil-set-jump)
  (call-interactively (key-binding (kbd "C-x h")))
  (message (concat "call function: "
                   (symbol-name (key-binding (kbd "C-x h"))))))
;;;###autoload
(defun evil-begin-of-defun(&optional _arg)
  "call function binding to `C-M-a'"
  (interactive)
  (call-interactively (key-binding (kbd "C-M-a")))
  (message (concat "call function: "
                   (symbol-name (key-binding (kbd "C-M-a"))))))
;;;###autoload
(defun evil-end-of-defun(&optional _arg)
  "call function binding to `C-M-e'"
  (interactive)
  (call-interactively (key-binding (kbd "C-M-e")))
  (message (concat "call function: "
                   (symbol-name (key-binding (kbd "C-M-e"))))))
;;;###autoload
(defun evil-M-e(&optional _arg)
  "call function binding to `M-e'"
  (interactive)
  (call-interactively (key-binding (kbd "M-e")))
  (message (concat "call function: "
                   (symbol-name (key-binding (kbd "M-e"))))))
;;;###autoload
(defun evil-M-a(&optional _arg)
  "call function binding to `M-a'"
  (interactive)
  (call-interactively (key-binding (kbd "M-a")))
  (message (concat "call function: "
                   (symbol-name (key-binding (kbd "M-a"))))))

;;;###autoload
(defun evil-C-M-f(&optional _arg)
  "call function binding to `C-M-f'"
  (interactive)
  (call-interactively (key-binding (kbd "C-M-f")))
  (message (concat "call function: "
                   (symbol-name (key-binding (kbd "C-M-f"))))))
;;;###autoload
(defun evil-C-M-b(&optional _arg)
  "call function binding to `C-M-b'"
  (interactive)
  (call-interactively (key-binding (kbd "C-M-b")))
  (message (concat "call function: "
                   (symbol-name (key-binding (kbd "C-M-b"))))))
;;;###autoload
(defun evil-C-M-k(&optional _arg)
  "call function binding to `C-M-k'"
  (interactive)
  (call-interactively (key-binding (kbd "C-M-k")))
  (message (concat "call function: "
                   (symbol-name (key-binding (kbd "C-M-k"))))))

;;;###autoload
(defun evil-copy-sexp-at-point(&optional _arg)
  "call function binding to `C-M-kC-/'"
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'sexp))
         start end)
    (when bounds
      (setq start (car bounds))
      (setq end (cdr bounds))
      (kill-ring-save start end)
      (pulse-momentary-highlight-region start end)
      (if (> (- end start) 30)
          (message "`sexp' at point copied")
        (message "\"%s\" are copied"
                 (buffer-substring-no-properties start end))))))

;; 将针对register；的操作等同于register+的操作,以方便操作clipboard
;; 即";p == "+p ";yy =="+yy
;; 而我将；绑定为evil-use-register 故;;p =="+p
(defadvice evil-use-register(around easy-clipboard activate)
  "if reguster=; treat is as + "
  ad-do-it
  (when (equal ad-return-value ?\;)
    (setq ad-return-value ?+)
    (setq evil-this-register ad-return-value)))

;;;###autoload
(defun evil-repeat-find-char-or-evil-use-register(&optional register-or-count)
  "default evil `f' find char ,and `;' repeat it ,now I bound `to' this cmd
so that if you call `f' first, then `;' will repeat it ,
if not,it will call `evil-use-register' which default bind on `\"' "
  (interactive)
  (if (member last-command '(evil-find-char
                             evil-find-char-backward
                             evil-repeat-find-char-reverse
                             evil-repeat-find-char))
      (progn
        (call-interactively 'evil-repeat-find-char)
        (setq this-command 'evil-repeat-find-char))
    (call-interactively 'evil-use-register)
    (setq this-command 'evil-use-register)))

;;;###autoload
(defun vmacs-smart-double-ctrl-c()
  (interactive)
  (cond
   ((string-match-p "*Org Src" (buffer-name))
    (require 'org-src)
    (call-interactively 'org-edit-src-exit)
    )
   ((string-match-p "*Org Agenda" (buffer-name))
    (call-interactively 'org-agenda-set-tags)
    )
   ((derived-mode-p  'org-mode)
    (call-interactively 'org-ctrl-c-ctrl-c)
    )
   (magit-blob-mode
    (vmacs-magit-blob-save))))


;; (defvar evil-ex-search-move-back-one-char nil)

;; (defadvice evil-ex-start-search (around  go-back-one-char activate)
;;   (let ((p (point))
;;         (p2 ))
;;     (unless(bobp)
;;       (forward-char -1))
;;     (setq p2 (point))
;;     (setq-default evil-ex-search-move-back-one-char (not (= p p2)))
;;     (message ".....%s%d %d %s" evil-ex-search-move-back-one-char p p2 evil-ex-search-move-back-one-char)

;;     ad-do-it
;;     (mesage "ooooooo")
;;     (when (and (equal p2 evil-ex-search-start-point) evil-ex-search-move-back-one-char )
;;       (forward-char 1))))



;; ;;;###autoload
;; (defun evil-repeat-find-char-or-evil-backward-symbol-begin()
;;   "default evil `f' find char ,and `;' repeat it ,now I bound `to' this cmd
;; so that if you call `f' first, then `;' will repeat it ,
;; if not,it will call `evil-backward-symbol-begin' "
;;   (interactive)
;;   (if (member last-command '(evil-find-char
;;                              evil-find-char-backward
;;                              evil-repeat-find-char-reverse
;;                              evil-repeat-find-char))
;;       (progn
;;         (call-interactively 'evil-repeat-find-char)
;;         (setq this-command 'evil-repeat-find-char))
;;     (call-interactively 'evil-backward-symbol-begin)
;;     (setq this-command 'evil-backward-symbol-begin)))

;; ;;bug  http://article.gmane.org/gmane.emacs.vim-emulation/1894/match=clipboard
;; ;; 临时解决方案
;; ;;;###autoload
;; (defun my-evil-paste-after (count)
;;   ""
;;   (interactive "p")
;;   (let (interprogram-paste-function)
;;     (evil-paste-after count)))



;; (evil-define-command evil-jk-to-normal-mode ()
;;   "Allows to get into 'normal' mode using 'jk'."
;;   :repeat change
;;   (let ((modified (buffer-modified-p)))
;;     (insert "j")
;;     (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
;;                            nil 0.5)))
;;       (cond
;;        ((null evt)
;;         (message ""))
;;        ((and (integerp evt) (char-equal evt ?k))
;;         (delete-char -1)
;;         (set-buffer-modified-p modified)
;;         (push 'escape unread-command-events))
;;        ((and (integerp evt) (char-equal evt ?l))
;;         (delete-char -1)
;;         (set-buffer-modified-p modified)
;;         (evil-emacs-state))
;;        (t ; otherwise
;;         (setq unread-command-events (append unread-command-events
;;                                             (list evt))))))))

(provide 'lazy-evil)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-evil.el ends here.
