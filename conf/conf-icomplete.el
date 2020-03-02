;;; Code:
(require 'icomplete)
;; (vmacs-leader "fh" #'(lambda()(interactive)(let ((default-directory "~/"))(call-interactively 'find-file))))
;; (vmacs-leader "ft" #'(lambda()(interactive)(let ((default-directory "/tmp/"))(call-interactively 'find-file))))
;; (vmacs-leader "ff" 'find-file)

(setq icomplete-prospects-height 20)
(setq icomplete-delay-completions-threshold 0)
(setq icomplete-max-delay-chars 0)
(setq icomplete-delay-completions-threshold 2000)
(setq icomplete-compute-delay 0)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-hide-common-prefix nil)
(setq icomplete-separator " . ")
(setq icomplete-with-completion-tables t)
(setq icomplete-in-buffer t)
(setq icomplete-tidy-shadowed-file-names t)
;; (icomplete-mode 1)
(define-key icomplete-minibuffer-map (kbd "C-n") #'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-p") #'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-s") #'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-r") #'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-k") #'icomplete-fido-kill)
(define-key icomplete-minibuffer-map (kbd "C-m") #'icomplete-fido-ret)
(define-key icomplete-minibuffer-map (kbd "RET") #'icomplete-fido-ret)
(define-key icomplete-minibuffer-map (kbd "C-l") #'icomplete-fido-backward-updir)


(defun icomplete-mode-yank-pop ()
  (let* ((icomplete-separator (concat "\n" (propertize "......" 'face 'shadow) "\n "))
         ;; (minibuffer-local-map minibuffer-local-map)
         ;;disable sorting https://emacs.stackexchange.com/questions/41801/how-to-stop-completing-read-ivy-completing-read-from-sorting
         (completion-table
          (lambda (string pred action)
            (if (eq action 'metadata)
                '(metadata (display-sort-function . identity)
                           (cycle-sort-function . identity))
              (complete-with-action
               action kill-ring string pred)))))
    ;; 默认的C-g 会导致 with-mode-off with-mode-on后续的代码无法执行，无法恢复
    ;; icomplete-mode  selectrum-mode mini-frame-mode到原值
    ;; (define-key minibuffer-local-map (kbd "C-g") 'exit-minibuffer)
    (insert
     (completing-read "Yank from kill ring: " completion-table nil t))))

;; this macro works
;; (macroexpand '(with-mode-on icomplete-mode (message "ss")))
(defmacro with-mode-on (mode &rest body)
  (declare (indent defun)
           (doc-string 3))
  (macroexp-let2 nil mode-p mode
    `(progn
       (unless ,mode-p (,mode 1))
       ,@body
       (unless ,mode-p (,mode -1)))))

;; (macroexpand '(with-mode-off icomplete-mode (message "ss")))
(defmacro with-mode-off (mode &rest body)
  (declare (indent defun)
           (doc-string 3))
  (macroexp-let2 nil mode-p mode
    `(progn
       (when ,mode-p (,mode -1))
       ,@body
       (when ,mode-p (,mode 1)))))


(provide 'conf-icomplete)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-icomplete.el ends here.
