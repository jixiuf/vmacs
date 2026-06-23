;;; vcgit.el --- VC Git extensions for vc-dir  -*- lexical-binding: t; -*-

;; This package enhances `vc-dir' for Git repositories with Magit-like features:
;; - Unpulled commit log shown inline (inserted into buffer header)
;; - Recent branch commit log shown inline (inserted into buffer header)
;; - Uses Emacs 31's built-in outgoing (unpushed) count
;; - TODO/FIXME search displayed in the footer
;; - outline-minor-mode for folding sections

;; Requirements: Emacs 31+
;;
;; Usage:
;;   (with-eval-after-load 'vc-dir (vcgit-global-minor-mode 1))

;;; Code:

(eval-when-compile
  (require 'vc)
  (require 'vc-git)
  (require 'vc-dir)
  (require 'log-view)
  (require 'outline))

;;; Customization

(defgroup vcgit nil
  "VC Git extension."
  :group 'vc)

(defcustom vcgit-log-commit-count 12
  "How many recent commits to show in Unpulled/Recent headers."
  :group 'vcgit
  :type 'number)

(defcustom vcgit-outline-regexp
  "\\(^ +.+/$\\)\\|\\(^[^* \t\n].*:$\\)\\|\\(Stash\\)"
  "Regexp for `outline-regexp' in `vc-dir-mode' buffers."
  :group 'vcgit
  :type 'string)

(defcustom vcgit-outline-level2-regexp
  "\\(?:^ +.+?/.+/\\)\\|\\(?:^[^:]+::\\)"
  "Regexp for level-2 outline headings in `vc-dir-mode'."
  :group 'vcgit
  :type 'string)

(defface vcgit-todo-face
  '((t (:foreground "Red" :box (:line-width 2 :color "grey75"
                                :style released-button)
        :height 1.2 :inherit default)))
  "Face used to highlight TODO/FIXME in the vc-dir footer."
  :group 'vcgit)


;;; Git utility functions

(defun vcgit--tracking-branch (&optional branch)
  "Return the upstream tracking branch (e.g. \"origin/main\"), or nil.
Uses the built-in `vc-git--branch-remotes'."
  (cdr (assq 'upstream (vc-git--branch-remotes branch))))


;;; Outline

(defun vcgit--outline-level ()
  "Return outline level (1 or 2) for the current match."
  (if (string-match-p vcgit-outline-level2-regexp (match-string 0))
      2 1))

(defun vcgit--goto-stage ()
  "Move point to the staging area header."
  (goto-char (point-min))
  (when (re-search-forward "^[ ]+./$" nil t)
    (goto-char (point-at-bol))))


;;; Header log insertion
;;
;; Unlike overlay `after-string' (which is virtual text the cursor
;; can't reach), we insert commit logs as actual buffer text by
;; appending to the ewoc header string.  This way the cursor can move
;; into the log area and keyboard bindings (RET, etc.) work.

(defun vcgit--append-to-header (text)
  "Append TEXT after the vc-dir ewoc header text.
Inserts directly into the buffer without using `ewoc-set-hf',
so existing overlays on the header (like the Outgoing count) are preserved."
  (save-excursion
    (goto-char (point-min))
    (let* ((header-len (length (car (ewoc-get-hf vc-ewoc))))
           (pos (+ (point) header-len))
           (inhibit-read-only t))
      (goto-char pos)
      (insert text))))

(defun vcgit--format-log-section (title count body keymap)
  "Format a log section with TITLE, COUNT, BODY, and KEYMAP.
The BODY string already has font-lock faces from the temp buffer;
we add keymap and mouse-face on top without overwriting them."
  ;; (add-text-properties 0 (length body)
  ;;                      `(keymap ,keymap mouse-face highlight)
  ;;                      body)
  (concat
   (propertize (if count
                   (format "%s(%d):" title count)
                 (format "%s:" title))
               'face 'vc-dir-header)
   "\n"
   body
   "\n"))

(defun vcgit--run-git-log (buf args vcdir-buf section-title count-p)
  "Run `git log' with ARGS into BUF, then insert result into VCDIR-BUF."
  (let ((default-directory (with-current-buffer vcdir-buf
                             default-directory))
        (process-connection-type nil))   ; pipe, not pty (no pager)
    (with-current-buffer buf
      (let ((proc (apply #'start-process "vcgit-log" buf
                         "git" "--no-pager" "log" "--no-color"
                         "--graph"
                         "--decorate" "--decorate-refs-exclude=refs/remotes/"
                         "--date=short"
                         (format "--pretty=tformat:%s"
                                 (car vc-git-root-log-format))
                         "--abbrev-commit"
                         "-n" (number-to-string vcgit-log-commit-count)
                         args)))
        (set-process-sentinel
         proc
         (lambda (_process _event)
           (unwind-protect
               (with-demoted-errors "vcgit: %S"
                 (when (buffer-live-p vcdir-buf)
                   (with-current-buffer buf
                     ;; Apply faces manually using the regex and specs from
                     ;; `vc-git-root-log-format'. Each spec is (GROUP FACE ...).
                     (let ((re (nth 1 vc-git-root-log-format))
                           (specs (nth 2 vc-git-root-log-format)))
                       (goto-char (point-min))
                       (while (re-search-forward re nil t)
                         (dolist (spec specs)
                           (let ((grp (car spec))
                                 (face (eval (nth 1 spec) t)))
                             (when (and grp face (facep face)
                                        (match-beginning grp))
                               (put-text-property
                                (match-beginning grp)
                                (match-end grp)
                                'face face))))))
                     (let ((total (count-lines (point-min) (point-max))))
                       (unless (zerop total)
                         (let* ((shown (min vcgit-log-commit-count total))
                                (body (buffer-substring
                                       (point-min)
                                       (save-excursion
                                         (goto-char (point-min))
                                         (line-end-position shown)))))
                           (with-current-buffer vcdir-buf
                             (vcgit--append-to-header
                              (vcgit--format-log-section
                               section-title
                               (and count-p shown)
                               body
                               log-view-mode-map)))))))))
             (when (buffer-live-p buf)
               (kill-buffer buf)))))))))

(defun vcgit--async-unpulled ()
  "Start async computation of the unpulled commit log."
  (when-let* ((tracking (vcgit--tracking-branch)))
    (vcgit--run-git-log
     (generate-new-buffer " *vcgit-unpulled*" t)
     (list (format "HEAD..%s" tracking))
     (current-buffer)
     "Unpulled" t)))

(defun vcgit--async-recent ()
  "Start async computation of the recent branch commit log."
  (when-let* ((branch (vc-git-working-branch)))
    (vcgit--run-git-log
     (generate-new-buffer " *vcgit-recent*" t)
     (list branch)
     (current-buffer)
     (format "Recent(%s)" branch) nil)))


;;; TODO footer

(defvar-keymap vc-todo-map
  "RET" #'vcgit-todo-open-file-at-line)

(defun vcgit-todo-open-file-at-line ()
  "Open the TODO file at the reported line number."
  (interactive)
  (let (filepath linenum)
    (save-excursion
      (goto-char (point-at-eol))
      (while (and (not filepath)
                  (re-search-backward "^.*::$" nil t))
        (let ((face (get-text-property (point) 'face)))
          (when (and (listp face) (memq 'compilation-info face))
            (setq filepath (string-trim-right
                            (thing-at-point 'line t) "::\n"))))))
    (save-excursion
      (goto-char (point-at-bol))
      (let ((face (get-text-property (point) 'face)))
        (when (and (listp face)
                   (memq 'compilation-line-number face))
          (setq linenum (string-to-number
                         (thing-at-point 'line t))))))
    (when filepath
      (find-file filepath)
      (when linenum
        (goto-char (point-min))
        (forward-line (1- linenum))))))

(defun vcgit-dir--todo ()
  "Search for TODO/FIXME items and display them in the vc-dir footer."
  (let* ((default-directory (expand-file-name default-directory))
         (buf (format "*vc-todo : %s*" default-directory))
         (curbuf (current-buffer)))
    (condition-case nil
        (let ((proc (start-process "vc-todo" buf
                                   "rg" "--line-number"
                                   "--max-filesize=1M"
                                   "-g" "!*.git*"
                                   "TODO:|FIXME:" ".")))
          (set-process-sentinel
           proc
           (lambda (proc _ev)
             (vcgit--todo-finish (process-buffer proc) curbuf)
             (kill-buffer (process-buffer proc)))))
      (error (message "vcgit: todo start-process failed")))))

(defun vcgit--todo-finish (buf curbuf)
  "Process TODO rg output in BUF and insert into CURBUF footer."
  (when (buffer-live-p curbuf)
    (with-current-buffer buf
      (condition-case nil
          (progn
            (require 'compile)
            (font-lock-add-keywords
             nil
             '(("\\<\\([0-9]+\\):" 1 'compilation-line-number prepend)
               ("^\\([^:]+\\)::$" 1 'compilation-info prepend)
               ("\\<\\(FIXME\\|TODO\\|Todo\\|HACK\\|todo\\):" 1
                'vcgit-todo-face prepend)))
            (goto-char (point-min))
            (while (re-search-forward "^\\([^:]+?\\)$" nil t)
              (replace-match "\\&::"))
            (font-lock-ensure)
            (let* ((total (count-lines (point-min) (point-max)))
                   (lines (min 500 total))
                   (results (if (> total 0)
                                (buffer-substring
                                 (point-min)
                                 (save-excursion
                                   (goto-char (point-min))
                                   (line-end-position lines)))
                              "")))
              (when (> (length results) 0)
                (setq results (propertize results 'keymap vc-todo-map))
                (with-current-buffer curbuf
                  ;; Insert TODO footer at end of buffer, before ewoc footer.
                  (goto-char (point-max))
                  (let ((footer-len (length (cdr (ewoc-get-hf vc-ewoc))))
                        (inhibit-read-only t))
                    (when (> footer-len 0)
                      (backward-char footer-len))
                    (insert "\n"
                            (propertize "TODOs:" 'face 'vc-dir-header)
                            "\n" results))
                  (vcgit--goto-stage)))))
        (error (message "vcgit: todo output processing error"))))))


;;; Outline setup

(defun vcgit--setup-outline ()
  "Configure outline-minor-mode for `vc-dir-mode'."
  (setq-local outline-regexp vcgit-outline-regexp)
  (setq-local outline-level #'vcgit--outline-level)
  (setq outline-minor-mode-cycle t)
  (setq outline-minor-mode-cycle-filter 'bolp)
  (setq outline-minor-mode-use-buttons 'in-margins)
  (outline-minor-mode))

(defun vcgit--setup-log-view ()
  "Set up log-view variables so keybindings work on header log text.
When `log-view-mode-map' commands (like \[log-view-diff]) run
in the vc-dir buffer, they need these variables to locate the
VC backend and fileset."
  (require 'log-view)
  (setq-local log-view-vc-backend vc-dir-backend)
  (setq-local log-view-vc-fileset (list default-directory))
  (setq-local log-view-file-re regexp-unmatchable)
  (setq-local log-view-per-file-logs nil)
  ;; Match short-format commit lines (e.g., "abc123f Author: msg").
  (setq-local log-view-message-re (cadr vc-git-root-log-format))
  ;; Enable expanding/collapsing short log entries via RET.
  (require 'vc-git)
  (setq-local log-view-expanded-log-entry-function
              #'vc-git-expanded-log-entry))


;;; Refresh hook

(defun vcgit--dir-refresh ()
  "Run after each vc-dir refresh to insert async log sections."
  (when (eq vc-dir-backend 'Git)
    (condition-case nil
        (progn
          (vcgit--async-unpulled)
          (vcgit--async-recent)
          (vcgit-dir--todo))
      (error (message "vcgit: refresh hook failed")))))


;;; RET dispatch for log sections
;;
;; The log text has `log-view-mode-map' as a `keymap' text property,
;; which normally gives it higher priority than `vc-dir-mode-map'.
;; However, for robustness we also install a minor-mode binding that
;; explicitly checks whether point is on a commit line.

(defun vcgit--on-log-line-p ()
  "Return non-nil if point is on a log-entry line.
Checks whether the current line matches `log-view-message-re'
or has the `log-view-comment' text property (expanded entry)."
  (or (and log-view-message-re
           (save-excursion
             (forward-line 0)
             (looking-at log-view-message-re)))
      (eq (get-text-property (point) 'log-view-comment) t)))

(defun vcgit-ret ()
  "Handle RET in vc-dir: expand/collapse log entries, or visit file.
On a commit log line, toggles the expanded entry display.
Otherwise calls `vc-dir-find-file'."
  (interactive)
  (if (vcgit--on-log-line-p)
      (log-view-toggle-entry-display)
    (vc-dir-find-file)))

(defvar-keymap vcgit-minor-mode-map
  "RET" #'vcgit-ret
  "<return>" #'vcgit-ret)

(define-minor-mode vcgit-minor-mode
  "Minor mode for vcgit enhancements in `vc-dir-mode'.
Adds RET dispatch so `log-view-toggle-entry-display' works on
commit log lines inserted in the vc-dir buffer."
  :keymap vcgit-minor-mode-map)

;;; Minor mode

(defun vcgit--vc-dir-setup ()
  "Setup vcgit enhancements in the current `vc-dir' buffer."
  (when (eq vc-dir-backend 'Git)
    (condition-case nil
        (progn
          (vcgit--setup-outline)
          (vcgit--setup-log-view)
          (vcgit-minor-mode 1)
          (add-hook 'vc-dir-refresh-hook #'vcgit--dir-refresh nil t))
      (error (message "vcgit: setup error")))))

;;;###autoload
(define-minor-mode vcgit-global-minor-mode
  "Global minor mode for Git-specific enhancements in `vc-dir' buffers.

Adds unpulled/recent commit logs, TODO search, and outline-minor-mode.
Uses Emacs 31's built-in outgoing revision count.

Enable once in your config:
  (with-eval-after-load 'vc-dir (vcgit-global-minor-mode 1))"
  :global t
  :lighter nil
  (if vcgit-global-minor-mode
      (progn
        (with-eval-after-load 'vc-dir
          (define-key vc-dir-mode-map (kbd "M-n")
                      #'outline-next-visible-heading)
          (define-key vc-dir-mode-map (kbd "M-p")
                      #'outline-previous-visible-heading)
          (define-key vc-dir-mode-map (kbd "C-i") #'vc-diff))
        (with-eval-after-load 'log-view
          (define-key log-view-mode-map (kbd "C-i") #'log-view-diff))
        (add-hook 'vc-dir-mode-hook #'vcgit--vc-dir-setup))
    (remove-hook 'vc-dir-mode-hook #'vcgit--vc-dir-setup)
    ;; Clean up refresh hooks and minor mode from existing vc-dir buffers.
    (dolist (buf (buffer-list))
      (when (buffer-local-value 'vc-dir-backend buf)
        (with-current-buffer buf
          (remove-hook 'vc-dir-refresh-hook #'vcgit--dir-refresh t)
          (vcgit-minor-mode -1))))))

(provide 'vcgit)

;; Local Variables:
;; coding: utf-8
;; End:

;;; vcgit.el ends here.
