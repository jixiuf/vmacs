(defmacro vmacs-define-key (mode-map key cmd  &optional  feature state)
  "define-key in `eval-after-load' block. `feature' is the file name where defined `mode-map'"
  (if state
      (if feature
          `(with-eval-after-load ,feature
             (with-eval-after-load 'evil  (evil-define-key ,state ,mode-map ,key ,cmd)))
        `(with-eval-after-load 'evil  (evil-define-key ,state ,mode-map ,key ,cmd)))
    `(with-eval-after-load ,feature (define-key ,mode-map ,key ,cmd))))

(defvar vmacs-leader-mode-map (make-sparse-keymap) "High precedence keymap.")
(defvar vmacs-space-leader-mode-map (make-sparse-keymap) "High precedence keymap.")

(define-minor-mode vmacs-leader-mode "Global minor mode for higher precedence evil keybindings." :global t)
(vmacs-leader-mode)

(with-eval-after-load 'evil
  (dolist (state '(normal visual insert))
    (evil-make-intercept-map
     ;; NOTE: This requires an evil version from 2018-03-20 or later
     (evil-get-auxiliary-keymap vmacs-leader-mode-map state t t)
     state))
  (evil-define-key '(normal visual operator motion emacs) vmacs-leader-mode-map " " vmacs-space-leader-mode-map))

(defmacro vmacs-leader (key cmd)
  `(define-key vmacs-space-leader-mode-map ,key ,cmd))

(provide 'conf-macro)

;; Local Variables:
;; coding: utf-8
;; End:

;;; init-macro.el ends here.
