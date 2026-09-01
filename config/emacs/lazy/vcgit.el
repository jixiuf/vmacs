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

;; Declare functions to avoid byte-compiler warnings, without
;; triggering eager macro-expansion cycles.  vc-dir is guaranteed
;; to be loaded before vcgit.el (via `with-eval-after-load').
(eval-when-compile
  (declare-function vc-git--branch-remotes "vc-git")
  (declare-function vc-git-working-branch "vc-git")
  (declare-function vc-git-expanded-log-entry "vc-git")
  (declare-function log-view-toggle-entry-display "log-view")
  (declare-function outline-next-visible-heading "outline")
  (declare-function outline-previous-visible-heading "outline"))

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

(defun vcgit--branch-remote (branch)
  "Return the remote name for BRANCH (e.g. \"origin\"), or nil.
Looks up the upstream via `vc-git--branch-remotes' and extracts
the remote portion."
  (when-let* ((upstream (vcgit--tracking-branch branch)))
    (car (split-string upstream "/"))))


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
  (add-text-properties 0 (length body)
                       `(keymap ,keymap)
                       body)
  (concat
   (propertize (if count
                   (format "%s(%d):" title count)
                 (format "%s:" title))
               'face 'vc-dir-header)
   "\n"
   body
   "\n"))

(defun vcgit--run-git-log (buf args vcdir-buf section-title count-p)
  "Run `git log' with ARGS into BUF, then insert result into VCDIR-BUF.
Uses `start-file-process' so that for remote (Tramp) vc-dir buffers
`git' runs on the remote host.  The sentinel tolerates `quit' (e.g.
C-g interrupting a Tramp operation), which `with-demoted-errors'
does not catch."
  (let ((default-directory (with-current-buffer vcdir-buf
                             default-directory))
        (process-connection-type nil))   ; pipe, not pty (no pager)
    (with-current-buffer buf
      (let ((proc (apply #'start-file-process "vcgit-log" buf
                         "git" "--no-pager" "log" "--no-color"
                         "--graph"
                         "--decorate"
                         "--date=short"
                         (format "--pretty=tformat:%s"
                                 (car vc-git-root-log-format))
                         "--abbrev-commit"
                         "-n" (number-to-string vcgit-log-commit-count)
                         args)))
        (set-process-sentinel
         proc
         (lambda (_process _event)
           (condition-case err
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
                   (kill-buffer buf)))
             (quit (message "vcgit: git log interrupted: %S" err)))))))))

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

(defvar-keymap vcgit-todo-map
  "RET" #'vcgit-todo-open-file-at-line)

(defun vcgit-todo-open-file-at-line ()
  "Open the TODO file at the reported line number.
The buffer format mirrors `rg --line-number' output after
`vcgit--todo-finish' processing:
  - File header lines end with `::' (e.g., \"src/foo.el::\")
  - Match lines have the form \"LINENUM:TODO: message\"
We extract the line number from the current line and search
backward for the nearest `::' header to get the file path."
  (interactive)
  (let ((line (string-trim (thing-at-point 'line t))))
    ;; Skip file/dir header lines (they end with ::).
    (unless (string-suffix-p "::" line)
      (let (filepath linenum)
        ;; Extract line number from current line: "42:TODO: message" -> 42.
        (when (string-match "\\`\\([0-9]+\\):" line)
          (setq linenum (string-to-number (match-string 1 line))))
        ;; Walk backward to find the nearest file header line.
        (save-excursion
          (goto-char (point-at-bol))
          (when (re-search-backward "^\\(.+\\)::$" nil t)
            (setq filepath (match-string 1))))
        (when (and filepath linenum (> linenum 0))
          (find-file (expand-file-name filepath default-directory))
          (goto-char (point-min))
          (forward-line (1- linenum)))))))

(defun vcgit-dir--todo ()
  "Search for TODO/FIXME items and display them in the vc-dir footer.
Uses `start-file-process' so remote (Tramp) directories run `rg'
on the remote host.  Skips when `rg' is unavailable there."
  (if (executable-find "rg" t)
      (let* ((default-directory (expand-file-name default-directory))
             (buf (format "*vc-todo : %s*" default-directory))
             (curbuf (current-buffer)))
        (condition-case nil
            (let ((proc (start-file-process "vc-todo" buf
                                            "rg" "--line-number"
                                            "--max-filesize=1M"
                                            "-g" "!*.git*"
                                            "TODO:|FIXME:" ".")))
              (set-process-sentinel
               proc
               (lambda (proc _ev)
                 (vcgit--todo-finish (process-buffer proc) curbuf)
                 (kill-buffer (process-buffer proc)))))
          (error (message "vcgit: todo start-process failed"))))
    (message "vcgit: skip TODO search (no rg available)")))

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
                (setq results (propertize results 'keymap vcgit-todo-map))
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
  "Run after each vc-dir refresh to insert async log sections.

`vc-dir-refresh-hook' runs inside the vc-git dir-status process
sentinel (`vc-exec-after').  For remote (Tramp) vc-dir buffers that
sentinel executes while the Tramp connection is locked, so any
synchronous Tramp call from here (`vc-git-working-branch',
`vc-git--branch-remotes', `executable-find') would signal
`Forbidden reentrant call of Tramp' and corrupt the connection.
Defer the actual work with a timer so it runs after the Tramp
callback has unwound and the lock is released."
  (when (eq vc-dir-backend 'Git)
    (run-at-time 0 nil #'vcgit--dir-refresh-deferred (current-buffer))))

(defun vcgit--dir-refresh-deferred (buf)
  "Insert async log/todo sections for vc-dir buffer BUF.
Runs from a timer, outside any Tramp callback; see
`vcgit--dir-refresh'.  No-ops when BUF is dead, no longer a Git
vc-dir buffer, or vcgit has been disabled in the meantime."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and (eq vc-dir-backend 'Git)
                 (bound-and-true-p vcgit-minor-mode))
        (condition-case err
            (progn
              (vcgit--async-unpulled)
              (vcgit--async-recent)
              (vcgit-dir--todo))
          (error (message "vcgit: refresh hook failed: %S" err))
          (quit (message "vcgit: refresh interrupted")))))))


;;; RET dispatch for log and TODO sections
;;
;; The log text has `log-view-mode-map' as a `keymap' text property and
;; the TODO text has `vcgit-todo-map'.  We also install an :around advice
;; on `vc-dir-find-file' as a belt-and-suspenders: if the text-property
;; keymaps work (they should per `keymap' char-property priority in the
;; key-lookup order), they handle RET directly.  If not, the advice
;; inspects point and dispatches to the right command.  Everything else
;; (stash, file entries, directories) falls through to the original
;; `vc-dir-find-file'.

(defun vcgit--on-log-line-p ()
  "Return non-nil if point is on a log-entry line inserted by vcgit.
Checks for the `log-view-mode-map' keymap text property, which is
set only on the Unpulled/Recent commit log sections."
  (or (eq (get-char-property (point) 'keymap) log-view-mode-map)
      (eq (get-text-property (point) 'log-view-comment) t)))

(defun vcgit--on-todo-line-p ()
  "Return non-nil if point is on a TODO/FIXME result line.
Checks for the `vcgit-todo-map' keymap text property, which is
set only on the TODO search results."
  (eq (get-char-property (point) 'keymap) vcgit-todo-map))

(defvar-keymap vcgit-minor-mode-map)

(defun vcgit--find-file-advice (orig-fun &rest args)
  "Around-advice for `vc-dir-find-file'.
When `vcgit-minor-mode' is active in a Git vc-dir buffer, intercepts
RET on log and TODO lines.  Otherwise delegates to ORIG-FUN (which
handles stash, file entries, etc. correctly)."
  (if (and (bound-and-true-p vcgit-minor-mode)
           (eq vc-dir-backend 'Git))
      (cond
       ((vcgit--on-log-line-p) (log-view-toggle-entry-display))
       ((vcgit--on-todo-line-p) (vcgit-todo-open-file-at-line))
       (t (apply orig-fun args)))
    (apply orig-fun args)))

(define-minor-mode vcgit-minor-mode
  "Minor mode for vcgit enhancements in `vc-dir-mode'.
Managed by `vcgit-global-minor-mode' — not intended for direct use."
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
        (advice-add 'vc-dir-find-file :around #'vcgit--find-file-advice)
        (add-hook 'vc-dir-mode-hook #'vcgit--vc-dir-setup))
    (advice-remove 'vc-dir-find-file #'vcgit--find-file-advice)
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
