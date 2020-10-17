(defmacro vmacs-define-key (mode-map key cmd  &optional  feature state)
  "define-key in `eval-after-load' block. `feature' is the file name where defined `mode-map'"
  (if state
      (if feature
          `(with-eval-after-load ,feature
             (with-eval-after-load 'evil  (evil-define-key ,state ,mode-map ,key ,cmd)))
        `(with-eval-after-load 'evil  (evil-define-key ,state ,mode-map ,key ,cmd)))
    `(with-eval-after-load ,feature (define-key ,mode-map ,key ,cmd))))

(defvar vmacs-leader-map (make-sparse-keymap))

(with-eval-after-load 'evil (evil-define-key '(normal visual operator motion emacs) 'global (kbd "SPC") vmacs-leader-map))

(defmacro vmacs-leader (key cmd)
  `(define-key vmacs-leader-map ,key ,cmd))

(provide 'conf-macro)

;; Local Variables:
;; coding: utf-8
;; End:

;;; init-macro.el ends here.
