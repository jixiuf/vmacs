 (when (file-directory-p "~/.emacs.d/submodule/prescient")
   (add-to-list 'load-path "~/.emacs.d/submodule/prescient"))
 (when (file-directory-p "~/.emacs.d/submodule/selectrum")
   (add-to-list 'load-path "~/.emacs.d/submodule/selectrum"))

(require 'selectrum)
(require 'selectrum-prescient)
(setq selectrum-num-candidates-displayed (1- max-mini-window-height))
(add-to-list 'selectrum-minibuffer-bindings '("C-e" . selectrum-insert-current-candidate) )

(setq prescient-filter-method  '(literal fuzzy initialism))
(selectrum-mode 1)
(selectrum-prescient-mode 1)       ; to make sorting and filtering more intelligent
;; to save your command history on disk, so the sorting gets more
;; intelligent over time
(prescient-persist-mode 1)


;; yank-pop icomplete 支持， selectrum-mode 有问题，故临时关selectrum-mode雇用icomplete
(defun selectrum-mode-yank-pop ()
  (let* ((selectrum-should-sort-p nil))
    (insert
     (completing-read "Yank from kill ring: " kill-ring nil t))))

(defadvice yank-pop (around kill-ring-browse-maybe (arg) activate)
  "If last action was not a yank, run `browse-kill-ring' instead."
  ;; yank-pop has an (interactive "*p") form which does not allow
  ;; it to run in a read-only buffer. We want browse-kill-ring to
  ;; be allowed to run in a read only buffer, so we change the
  ;; interactive form here. In that case, we need to
  ;; barf-if-buffer-read-only if we're going to call yank-pop with
  ;; ad-do-it
  (interactive "p")
  (if (not (eq last-command 'yank))
      (selectrum-mode-yank-pop)         ;icomplete-mode-yank-pop
    ad-do-it))


(provide 'conf-selectrum)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-selectrum.el ends here.
