(eval-and-compile (require 'evil-macros))
;;;###autoload (autoload 'evil-forward-symbol-begin "lazy-evil-symbol" nil t)
(evil-define-motion evil-forward-symbol-begin(count)
  "Move to the end of the COUNT-th next symbol."
  ;; :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob count)
  (evil-forward-beginning 'evil-symbol count)
  (let ((sym (thing-at-point 'evil-symbol)))
    (while (and sym (not (string-match "\\<" sym)))
      (evil-signal-at-bob-or-eob count)
      (evil-forward-beginning 'evil-symbol 1)
      (setq sym (thing-at-point 'evil-symbol))
      )
    )
  )

;;;###autoload (autoload 'evil-backward-symbol-begin "lazy-evil-symbol" nil t)
(evil-define-motion evil-backward-symbol-begin(count)
  "Move to the end of the COUNT-th next symbol."
  ;; :jump t
  :type exclusive
  ;; (forward-evil-symbol count)
  (evil-backward-beginning 'evil-symbol count)
  (evil-signal-at-bob-or-eob count)
  (let ((sym (thing-at-point 'evil-symbol))
        (pos (point))
        break)
    (while (and sym
                (not (string-match "\\<" sym))
                (not break))
      (evil-backward-beginning 'evil-symbol 1)
      (when (equal pos (point)) (setq break t))
      (setq pos (point))
      (evil-signal-at-bob-or-eob count)
      (setq sym (thing-at-point 'evil-symbol))))
  )


;;;###autoload (autoload 'evil-forward-symbol-end "lazy-evil-symbol" nil t)
(evil-define-motion evil-forward-symbol-end(count)
  "Move to the end of the COUNT-th next symbol."
  ;; :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob count)
  (forward-evil-symbol count)

  ;; (let ((sym (thing-at-point 'evil-symbol)))
  ;;   (while (and sym (not (string-match "^\\<" sym)))
  ;;     (evil-forward-end 'evil-symbol 1)
  ;;     (setq sym (thing-at-point 'evil-symbol))
  ;;     )
  ;;   )
  )

;;;###autoload (autoload 'evil-forward-symbol-end "lazy-evil-symbol" nil t)
(evil-define-motion evil-backward-symbol-end(count)
  "Move to the end of the COUNT-th next symbol."
  ;; :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob count)
  (evil-backward-end 'symbol count)
  (let ((sym (thing-at-point 'evil-symbol)))
    (while (and sym (not (string-match "\\<" sym)))
      (evil-backward-end 'evil-symbol 1)
      (setq sym (thing-at-point 'evil-symbol))
      )
    )
)


(provide 'lazy-evil-symbol)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-evil-symbol.el ends here
