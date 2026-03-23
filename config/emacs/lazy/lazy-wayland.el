;;; lazy-wayland.el --- Wayland run-or-raise functionality -*- lexical-binding: t; -*-

(require 'seq)
(require 'cl-lib)

;;;; Configuration

(defvar wayland-compositor 'ewm
  "Current wayland compositor. Valid values: `ewm`, `reka`.")

(defvar wayland-run-or-raise--last-buffer nil
  "Last buffer matched by `wayland-run-or-raise'.")

(defvar wayland-run-or-raise--prev-buffer nil
  "Buffer visited before the last matched buffer.")

;;;; Internal Helpers

(defun wayland-run-or-raise--get-buffers ()
  "Get list of wayland-managed buffers based on `wayland-compositor'."
  (pcase wayland-compositor
    ('ewm (cl-loop for buf being the hash-values of ewm--surfaces
                  when (buffer-live-p buf) collect buf))
    ('reka (seq-filter #'reka--is-reka-buffer (buffer-list)))
    (_ (user-error "Unknown compositor: %s" wayland-compositor))))

(defun wayland-run-or-raise--get-app-id (buf)
  "Get app-id for buffer BUF based on `wayland-compositor'."
  (pcase wayland-compositor
    ('ewm (buffer-local-value 'ewm-surface-app buf))
    ('reka (buffer-local-value 'reka-app-id buf))
    (_ nil)))

(defun wayland-run-or-raise--get-title (buf)
  "Get title for buffer BUF based on `wayland-compositor'."
  (pcase wayland-compositor
    ('ewm (buffer-local-value 'ewm-surface-title buf))
    ('reka (buffer-name buf))
    (_ nil)))

(defun wayland-run-or-raise--match-p (buf app-id-pattern title-pattern)
  "Check if BUF matches APP-ID-PATTERN or TITLE-PATTERN (OR logic)."
  (or (and app-id-pattern
           (let* ((app-id (wayland-run-or-raise--get-app-id buf)))
             (and app-id (string-match-p app-id-pattern app-id))))
      (and title-pattern
           (let* ((title (wayland-run-or-raise--get-title buf)))
             (and title (string-match-p title-pattern title))))))

(defun wayland-run-or-raise--switch-to (buf)
  "Switch to buffer BUF and sync focus."
  (switch-to-buffer buf)
  (when (fboundp 'ewm--sync-focus)
    (ewm--sync-focus)))

(defun wayland-run-or-raise--run (command args)
  "Run COMMAND with ARGS using `start-process-shell-command'."
  (let ((full-command (if args
                          (format "%s %s" command (string-join args " "))
                        command)))
    (start-process-shell-command command nil full-command)))

(defun wayland-run-or-raise--cycle (matched-buffers)
  "Cycle through MATCHED-BUFFERS, switching to next after current.
If current buffer is already a matched buffer, switch to the next one.
If only one match and already in it, switch to the previous buffer."
  (let* ((current (current-buffer))
         (current-pos (cl-position current matched-buffers :test #'eq))
         (use-last-p (and wayland-run-or-raise--last-buffer
                          (not (eq current wayland-run-or-raise--last-buffer))
                          (cl-position wayland-run-or-raise--last-buffer matched-buffers :test #'eq))))
    (cond
     ;; Currently in a matched buffer - go to next
     (current-pos
      (let* ((next-pos (mod (1+ current-pos) (length matched-buffers)))
             (next-buffer (nth next-pos matched-buffers)))
        (setq wayland-run-or-raise--prev-buffer current)
        (setq wayland-run-or-raise--last-buffer next-buffer)
        (wayland-run-or-raise--switch-to next-buffer)))
     ;; Use last-buffer position
     (use-last-p
      (let* ((next-pos (mod (1+ use-last-p) (length matched-buffers)))
             (next-buffer (nth next-pos matched-buffers)))
        (setq wayland-run-or-raise--prev-buffer current)
        (setq wayland-run-or-raise--last-buffer next-buffer)
        (wayland-run-or-raise--switch-to next-buffer)))
     ;; Default: cycle from start
     (t
      (let* ((next-buffer (car matched-buffers))
             (next-pos (if next-buffer 0 0)))
        (setq wayland-run-or-raise--prev-buffer current)
        (setq wayland-run-or-raise--last-buffer next-buffer)
         (wayland-run-or-raise--switch-to next-buffer))))))

(defun wayland-run-or-raise--exec (title app-id command args)
  "Execute run-or-raise logic with parsed arguments."
  (let* ((buffers (wayland-run-or-raise--get-buffers))
         (matched-buffers (cl-loop for buf in buffers
                                  when (wayland-run-or-raise--match-p buf app-id title)
                                  collect buf)))
    (cond
     ((null matched-buffers)
      (wayland-run-or-raise--run command args))
     ((= (length matched-buffers) 1)
      (setq wayland-run-or-raise--prev-buffer (current-buffer))
      (setq wayland-run-or-raise--last-buffer (car matched-buffers))
      (wayland-run-or-raise--switch-to (car matched-buffers)))
     (t
      (wayland-run-or-raise--cycle matched-buffers)))))

;;;; Main API

;;;###autoload
(defmacro wayland-run-or-raise (&rest form)
  "Match wayland windows by TITLE or APP-ID and switch to them, or run COMMAND if none found.

Keywords:
  :title  - regexp pattern to match window title (string or nil)
  :app-id - regexp pattern to match app-id (string or nil)
  :command - the command to run if no match found
  :name   - the name for the generated command (required for keybindings)

Additional arguments after keywords are passed as args to the command.

If no buffer matches, run COMMAND with remaining args.
If one buffer matches, switch to it.
If multiple buffers match, switch to the next one after the current (cycling).

Example:
  (wayland-run-or-raise :title \"emacs\" :command \"emacsclient\" \"-c\")
  (wayland-run-or-raise :app-id \"firefox\" :command \"firefox\")
  (global-set-key (kbd \"s-C-<tab>\") (wayland-run-or-raise :name \"firefox\" :app-id \"firefox\" :command \"firefox\"))"
  (let* ((name (plist-get form :name))
         (title (plist-get form :title))
         (app-id (plist-get form :app-id))
         (command (plist-get form :command))
         (args (cl-loop for (key val) on form by #'cddr
                       unless (memq key '(:title :app-id :command :name))
                       collect val))
         (command-val (or command (car args)))
         (args-val (if command args (cdr args))))
    (unless name
      (user-error "wayland-run-or-raise: :name is required for keybinding"))
    (let ((func-sym (intern (format "wayland-run-or-raise-%s" name))))
      `(progn
         (defun ,func-sym ()
           (interactive)
           (wayland-run-or-raise--exec ,title ,app-id ,command-val ',args-val))
         #',func-sym))))


(provide 'lazy-wayland)
;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-wayland.el ends here
