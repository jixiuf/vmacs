;;; lazy-isearch.el --- Description -*- lexical-binding: t; -*-
;; ---------------------------------------------------------------------------
;; ISEARCH Wrapper
;;
;; Support searching in both directions as well as
;; searching based on the active region.
(defgroup lazy-isearch nil
  "Lightweight modal editing."
  :group 'isearch)
(defcustom lazy-isearch-activate-mark t
  "ISEARCH activates the mark (transient).
So motion drops the selection.

Useful for pasting while stepping over search results."
  
  :type 'boolean)


(defun lazy-isearch--done-hook ()
  "Temporary local hook, when starting ISEARCH, remove itself afterwards."
  ;; Assume the `has-region' argument is false,
  ;; this is OK as starting a new search typically doesn't continue
  ;; to use the prior selection.
  (remove-hook 'isearch-mode-end-hook #'lazy-isearch--done-hook t)
  (lazy-isearch--handle-done nil))


(defun lazy-isearch--handle-done (had-region)
  "Handle the result of ISEARCH being done.

This is only to be called from within ISEARCH functions.
When HAD-REGION is non-nil, mark the region."
  ;; When repeating a search with no previous search data,
  ;; it's possible for `isearch-success' to be set without the other end.
  ;; Use the other-end to detect success as well.
  (when (and isearch-success isearch-other-end)
    ;; Select.
    (set-marker (mark-marker) isearch-other-end)
    (cond
     ((or had-region lazy-isearch-activate-mark)
      (activate-mark)
      (setq transient-mark-mode (cons 'only t)))
     (t
      (setq deactivate-mark t)))))

;;;###autoload
(defun lazy-isearch-regexp-next ()
  "Search forward for a regexp."
  (interactive)
  (add-hook 'isearch-mode-end-hook #'lazy-isearch--done-hook 0 t)
  (call-interactively #'isearch-forward-regexp))

;;;###autoload
(defun lazy-isearch-regexp-prev ()
  "Search backward for a regexp."
  (interactive)
  (add-hook 'isearch-mode-end-hook #'lazy-isearch--done-hook 0 t)
  (call-interactively #'isearch-backward-regexp))

(defun lazy-isearch--repeat (dir)
  "Repeat search in direction DIR."
  ;; Re-display can flicker.
  (let ((inhibit-redisplay t)
        ;; Opinionated, but ISEARCH is not that usable without these.
        (isearch-wrap-pause 'no-ding)
        (isearch-repeat-on-direction-change t)
        (had-region (region-active-p)))

    (cond
     ((< dir 0)
      (isearch-repeat-backward (- dir)))
     (t
      (isearch-repeat-forward dir)))

    (lazy-isearch--handle-done had-region)))

;;;###autoload
(defun lazy-isearch-repeat-next (arg)
  "Repeat ISEARCH forwards ARG times.
 If the region content doesn't match `isearch-string', it is used to search the region content using
`lazy-isearch-at-point-next' or `lazy-isearch-at-point-prev'."
  (interactive "p")
  (if (and (region-active-p)
           (= (line-number-at-pos (region-end))
              (line-number-at-pos (region-beginning))))
      (if (< (point) (mark))
          (if (lazy--search-match (buffer-substring (region-beginning)(region-end)))
              (call-interactively #'lazy-isearch-repeat-prev)
            (call-interactively #'lazy-isearch-at-point-prev))
        (if (lazy--search-match (buffer-substring (region-beginning)(region-end)))
            (lazy-isearch--repeat arg)
          (call-interactively #'lazy-isearch-at-point-next)))
    (lazy-isearch--repeat arg)))


;;;###autoload
(defun lazy-isearch-repeat-prev (arg)
  "Repeat ISEARCH backwards ARG times."
  (interactive "p")
  (if (and (region-active-p)
           (= (line-number-at-pos (region-end))
              (line-number-at-pos (region-beginning))))
      (if (lazy--search-match (buffer-substring (region-beginning)(region-end)))
          (lazy-isearch--repeat (- arg))
        (call-interactively #'lazy-isearch-at-point-prev))
    (lazy-isearch--repeat (- arg))))

(defun lazy-isearch--bounds-at-point-impl ()
  "Return the region for `lazy-isearch-next-at-point' to use."
  (cond
   ((region-active-p)
    ;; TODO: don't attempt multi-line.
    (cons (region-beginning) (region-end)))
   (t
    (bounds-of-thing-at-point 'symbol))))


(defun lazy-isearch--extract-regex-from-bounds (text-bounds)
  "Extract a regexp from TEXT-BOUNDS for the purpose of searching."

  (let ((text (buffer-substring-no-properties (car text-bounds) (cdr text-bounds)))
        (beg nil)
        (end nil)
        (beg-test-list (list "\\_<" "\\<" "\\b"))
        (end-test-list (list "\\_>" "\\>" "\\b")))

    ;; NOTE: exactly how to do this isn't clear, looking-at commands work well enough.
    (save-excursion
      (goto-char (car text-bounds))
      (while beg-test-list
        (let ((beg-test (pop beg-test-list)))
          (when (looking-at-p beg-test)
            (setq beg-test-list nil) ; Break.
            (setq beg beg-test))))

      (goto-char (cdr text-bounds))
      (while end-test-list
        (let ((end-test (pop end-test-list)))
          (when (looking-at-p end-test)
            (setq end-test-list nil) ; Break.
            (setq end end-test)))))

    (concat (or beg "") (regexp-quote text) (or end ""))))

(defun lazy-isearch--at-point (dir)
  "Perform ISEARCH at point along DIR."
  (let ((had-region (region-active-p))
        (text-bounds (lazy-isearch--bounds-at-point-impl))
        ;; Re-display can flicker.
        (inhibit-redisplay t)
        ;; Always wrap.
        (isearch-wrap-pause 'no-ding))
    (unless text-bounds
      (user-error "No symbol at cursor"))
    (cond
     ((< dir 0)
      ;; Unlike searching forward the point needs to be moved *before* the symbol.
      ;; Skip before this instance, -1 or wrap.
      (goto-char
       (cond
        ((eq (point-min) (car text-bounds))
         (point-max))
        (t
         (1- (car text-bounds)))))
      (call-interactively #'isearch-backward-regexp))
     (t
      ;; Skip past this instance.
      (goto-char (cdr text-bounds))
      (call-interactively #'isearch-forward-regexp)))

    (let ((text (lazy-isearch--extract-regex-from-bounds text-bounds)))
      (push text regexp-search-ring)
      ;; This function defines the search as being "regex",
      ;; so it's important ISEARCH's variable is set accordingly.
      (setq isearch-regexp t)

      ;; Inline `isearch-yank-string' because it expects non regex text,
      ;; however this text is already quoted.
      (progn
        (setq isearch-yank-flag t)
        (isearch-process-search-string text (mapconcat #'isearch-text-char-description text ""))))

    (isearch-exit)

    (lazy-isearch--handle-done had-region)))

;;;###autoload
(defun lazy-isearch-at-point-next (arg)
  "Search forwards for the symbol or region at point.
Repeat the search ARG times."
  (interactive "p")
  (lazy-isearch--at-point arg))

;;;###autoload
(defun lazy-isearch-at-point-prev (arg)
  "Search backwards for the symbol or region at point.
Repeat the search ARG times."
  (interactive "p")
  (lazy-isearch--at-point (- arg)))

(defun lazy--search-match (match)
  "Check if MATCH can be matched by the current `isearch-string'."
  (when (and match
             (not (string-empty-p match))
             (not (string-empty-p isearch-string)))
    (save-match-data
      (save-mark-and-excursion
        (let ((case-fold-search isearch-case-fold-search)
              (search-invisible isearch-invisible))
          (with-temp-buffer
            (insert match)
            (when isearch-forward
              (goto-char (point-min)))
            (and (isearch-search-string isearch-string nil t)
                 (string-equal (match-string 0) match))))))))



(provide 'lazy-isearch)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-isearch.el ends here.
