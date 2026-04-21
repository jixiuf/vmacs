;;; lazy-isearch.el --- Description -*- lexical-binding: t; -*-

;; Author: jixiuf  jixiuf@qq.com
;; Keywords:
;; URL:

;; Copyright (C) 2026, jixiuf, all rights reserved.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:
;;
;; Support searching in both directions
;; as well as searching based on the active region.
(defgroup lazy-isearch nil
  "Search in both directions by isearch."
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
    (set-marker (mark-marker) isearch-other-end)
    (cond
     ((or had-region lazy-isearch-activate-mark)
      (activate-mark)
      (setq transient-mark-mode (cons 'only t)))
     (t
      (setq deactivate-mark t)))))


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

(defun lazy-isearch--bounds-at-point ()
  "Return the region for `lazy-isearch-next-at-point' to use."
  (cond
   ((and (region-active-p)
         (= (line-number-at-pos (region-end))
            (line-number-at-pos (region-beginning))))
    (cons (region-beginning) (region-end)))
   (t
    (bounds-of-thing-at-point 'symbol))))

(defun lazy-isearch--extract-regex (text-bounds)
  "Extract a regexp from TEXT-BOUNDS for the purpose of searching."

  (let ((text (buffer-substring-no-properties
               (car text-bounds) (cdr text-bounds)))
        (beg nil)
        (end nil)
        (beg-test-list (list "\\_<" "\\<" "\\b"))
        (end-test-list (list "\\_>" "\\>" "\\b")))
    ;; NOTE: exactly how to do this isn't clear,
    ;; looking-at commands work well enough.
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
        (text-bounds (lazy-isearch--bounds-at-point))
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

    (let ((text (lazy-isearch--extract-regex text-bounds)))
      (push text regexp-search-ring)
      ;; This function defines the search as being "regex",
      ;; so it's important ISEARCH's variable is set accordingly.
      (setq isearch-regexp t)

      ;; Inline `isearch-yank-string' because it expects non regex text,
      ;; however this text is already quoted.
      (progn
        (setq isearch-yank-flag t)
        (isearch-process-search-string
         text (mapconcat #'isearch-text-char-description text ""))))
    (isearch-exit)
    (lazy-isearch--handle-done had-region)))

(defun lazy-isearch--match (match)
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

(defun lazy-isearch--show-indicator (msg)
  (setq msg (string-trim-right msg))
  (message "%s" msg))
;; end of isearch (from meow)

(defun lazy-isearch--unhighlight ()
  (isearch-dehighlight)
  (lazy-highlight-cleanup t))

(defun lazy-isearch--count-hook ()
  (save-mark-and-excursion
    (when isearch-lazy-count-current
      (let ((search-term (if isearch-regexp
                             (format "/%s/"
                                     (propertize isearch-string 'face 'font-lock-variable-name-face))
                           (propertize isearch-string 'face 'font-lock-variable-name-face)))
            (count-info (isearch-lazy-count-format)))
        (lazy-isearch--show-indicator 
         (format "%s %s"
                 search-term
                 (propertize count-info 'face 'font-lock-function-name-face)))))))
;;;###autoload
(define-minor-mode lazy-isearch-mode
  "Enable Lazy isearch mode."
  :interactive nil
  (if lazy-isearch-mode
      (progn
        (setq isearch-lazy-count t)
        (advice-add 'keyboard-quit :before #'lazy-isearch--unhighlight)
        (add-hook 'lazy-count-update-hook #'lazy-isearch--count-hook))
    (advice-remove 'keyboard-quit  #'lazy-isearch--unhighlight)
    (remove-hook 'lazy-count-update-hook #'lazy-isearch--count-hook)))

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


;;;###autoload
(defun lazy-isearch-repeat-next (arg)
  "Repeat ISEARCH forwards ARG times.
for single-line active regions. If the region
content doesn't match `isearch-string', it is used to search the region content using
`lazy-isearch-at-point-next' or `lazy-isearch-at-point-prev'.
 Otherwise it just repeat last search."
  (interactive "p")
  (if (and (region-active-p)
           (= (line-number-at-pos (region-end))
              (line-number-at-pos (region-beginning))))
      (if (< (point) (mark))
          (if (lazy-isearch--match (buffer-substring (region-beginning)(region-end)))
              (call-interactively #'lazy-isearch-repeat-prev)
            (lazy-isearch-at-point-prev arg))
        (if (lazy-isearch--match (buffer-substring (region-beginning)(region-end)))
            (lazy-isearch--repeat arg)
          (lazy-isearch-at-point-prev arg)))
    (lazy-isearch--repeat arg)))

;;;###autoload
(defun lazy-isearch-repeat-prev (arg)
  "Repeat ISEARCH backwards ARG times.
for single-line active regions. If the region
content doesn't match `isearch-string', it is used to search the region content using
`lazy-isearch-at-point-next' or `lazy-isearch-at-point-prev'.
 Otherwise it just repeat last search."
  (interactive "p")
  (if (and (region-active-p)
           (= (line-number-at-pos (region-end))
              (line-number-at-pos (region-beginning))))
      (if (lazy-isearch--match (buffer-substring (region-beginning)(region-end)))
          (lazy-isearch--repeat (- arg))
        (call-interactively #'lazy-isearch-at-point-prev))
    (lazy-isearch--repeat (- arg))))

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

(provide 'lazy-isearch)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-isearch.el ends here.
