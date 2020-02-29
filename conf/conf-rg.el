;; https://github.com/dajva/rg.el
;; doc https://rgel.readthedocs.io
;;; Code:
(require 'rg)
(setq rg-show-header nil)
(vmacs-leader "g" rg-global-map)
(define-key rg-global-map (kbd "C-.") #'rg-dwim-current-dir)
(define-key rg-global-map (kbd ".") #'rg-dwim-current-dir)
(define-key rg-global-map (kbd ",") #'rg-dwim-project-dir)
(define-key rg-global-map "g" #'vmacs-rg-word-current-dir)
(define-key rg-global-map "p" #'vmacs-rg-word-root-dir)
(define-key rg-global-map "m" #'rg-menu)

(rg-define-search vmacs-rg-word-current-dir
  :format literal  ;; :menu ("Custom" "g" "dwim current dir")
  :files current :dir current)
(rg-define-search vmacs-rg-word-root-dir
  :format literal :files current :dir project)

;; c toggle case
(defun vmacs-rg-hook()
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)
  (define-key rg-mode-map "g" nil)
  (define-key rg-mode-map "e" nil)
  (define-key rg-mode-map "i" nil)
  (define-key rg-mode-map "I" #'rg-rerun-toggle-ignore)
  (define-key rg-mode-map (kbd "z") 'rg-occur-hide-lines-matching)
  (define-key rg-mode-map (kbd "/") 'rg-occur-hide-lines-not-matching)

  (evil-define-key 'normal 'local "gr" 'rg-recompile))

(add-hook 'rg-mode-hook #'vmacs-rg-hook)

;;;###autoload
(defun rg-occur-hide-lines-not-matching (search-text)
  "Hide lines that don't match the specified regexp."
  (interactive "MHide lines not matched by regexp: ")
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (save-excursion
    (goto-char (point-min))
    (forward-line 5)
    (let ((inhibit-read-only t)
          line)
      (while (not (looking-at-p "^rg finished "))
        (when (looking-at-p wgrep-rg-grouped-result-file-regexp)
          (save-excursion (backward-char)
                          (when (looking-back wgrep-rg-grouped-result-file-regexp)
                            (delete-region (match-beginning 0) (match-end 0))))
          (forward-line))
        (setq line (buffer-substring-no-properties (point) (point-at-eol)))
        (if (string-match-p search-text line)
            (forward-line)
          (when (not (looking-at-p "^rg finished "))
            (delete-region (point) (1+ (point-at-eol)))))))))

;;;###autoload
(defun rg-occur-hide-lines-matching  (search-text)
  "Hide lines matching the specified regexp."
  (interactive "MHide lines matching regexp: ")
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (save-excursion
    (goto-char (point-min))
    (forward-line 5)
    (let ((inhibit-read-only t)
          line)
      (while (not (looking-at-p "^rg finished "))
        (when (looking-at-p wgrep-rg-grouped-result-file-regexp)
          (save-excursion (backward-char 2)
                          (when (looking-back wgrep-rg-grouped-result-file-regexp)
                            (delete-region (match-beginning 0) (+ 2 (match-end 0)))))
          (forward-line))
        (setq line (buffer-substring-no-properties (point) (point-at-eol)))
        (if (not (string-match-p search-text line))
            (forward-line)
          (when (not (looking-at-p "^rg finished "))
            (delete-region (point) (1+ (point-at-eol)))))))))

(provide 'conf-rg)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-rg.el ends here.
