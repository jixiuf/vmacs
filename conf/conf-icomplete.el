;;; Code:
(require 'icomplete)

;; (setq orderless-component-separator " +")
;; (setq orderless-matching-styles '(orderless-regexp orderless-literal))

;; (setq icomplete-delay-completions-threshold 0)
;; (setq icomplete-max-delay-chars 3)
(setq icomplete-delay-completions-threshold 2000)
(setq icomplete-compute-delay 0)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-hide-common-prefix nil)
(setq icomplete-in-buffer t)
(setq icomplete-tidy-shadowed-file-names t)

(setq icomplete-prospects-height 15)
(setq icomplete-separator "\n")
;; (setq icomplete-separator (propertize " ⚫ " 'face  '(foreground-color . "SlateBlue1")))
;; 让第一个candidate不要显示在光标同一行，而是下一行

(if (fboundp 'fido-mode)
    (progn
      (fido-mode 1)
      (define-key icomplete-fido-mode-map (kbd "C-n") #'icomplete-forward-completions)
      (define-key icomplete-fido-mode-map (kbd "C-p") #'icomplete-backward-completions)
      (define-key icomplete-fido-mode-map (kbd "M-j") #'icomplete-force-complete-and-exit)
      (define-key icomplete-fido-mode-map (kbd "C-l") #'icomplete-fido-backward-updir)
      )
  (icomplete-mode 1))

(defun vmacs-fido-setup ()
  (if (require 'orderless nil t)
      (setq-local completion-styles '(orderless basic substring initials flex))
    (setq-local completion-styles '(basic substring initials flex))))

(add-hook 'minibuffer-setup-hook #'vmacs-fido-setup 99)

(define-key icomplete-minibuffer-map (kbd "C-j") #'icomplete-fido-exit) ;minibuffer-complete-and-exit
;; (define-key icomplete-fido-mode-map (kbd "SPC") #'self-insert-command)

(defun icomplete-mode-yank-pop ()
  (interactive)
  (let* ((icomplete-separator (concat "\n" (propertize (make-string 60 ?— ) 'face 'vertical-border) "\n "))
         ;;disable sorting https://emacs.stackexchange.com/questions/41801/how-to-stop-completing-read-ivy-completing-read-from-sorting
         (completion-table
          (lambda (string pred action)
            (if (eq action 'metadata)
                '(metadata (display-sort-function . identity)
                           (cycle-sort-function . identity))
              (complete-with-action
               action kill-ring string pred))))
         (selected (completing-read "Yank from kill ring: " completion-table nil t)))
    (if (eq major-mode 'vterm-mode)
        (vterm-send-string selected t)
      (insert selected))))


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
      (icomplete-mode-yank-pop)
    ad-do-it))



(provide 'conf-icomplete)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-icomplete.el ends here.
