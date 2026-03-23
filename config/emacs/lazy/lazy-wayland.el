;;; lazy-wayland.el --- Wayland run-or-raise functionality -*- lexical-binding: t; -*-

(require 'seq)
(require 'cl-lib)

;;;; Configuration

(defvar wayland-compositor 'ewm
  "Current wayland compositor. Valid values: `ewm`, `reka`.")

;;;; Internal Helpers

(defun wayland-run-or-raise--get-buffers (emacs)
  "Get list of wayland-managed buffers based on `wayland-compositor'."
  (let ((buffers (pcase wayland-compositor
                   ('ewm (cl-loop for buf being the hash-values of ewm--surfaces
                                  when (buffer-live-p buf) collect buf))
                   ('reka (seq-filter #'reka--is-reka-buffer (buffer-list)))
                   (_ (user-error "Unknown compositor: %s" wayland-compositor)))))
    (when emacs
      (setq buffers (seq-filter (lambda (buf)
                                  (and
                                   (not (memq (buffer-name buf) '("ewm-events")))
                                   (not (string-match-p "^ .*" (buffer-name buf)))
                                   (not (memq buf buffers))))
                                (tab-line-tabs-window-buffers)))
      (setq buffers (list (car buffers))))
    buffers))

(defun wayland-run-or-raise--get-app-id (&optional buf)
  "Get app-id for buffer BUF based on `wayland-compositor'."
  (pcase wayland-compositor
    ('ewm (or (buffer-local-value 'ewm-surface-app
                                  (or buf (current-buffer)))
              "emacs"))
    ('reka (or (buffer-local-value 'reka-app-id
                                   (or buf (current-buffer)))
               "emacs"))
    (_ nil)))

(defun wayland-run-or-raise--get-title (&optional buf)
  "Get title for buffer BUF based on `wayland-compositor'."
  (pcase wayland-compositor
    ('ewm (buffer-local-value 'ewm-surface-title (or buf (current-buffer))))
    ('reka (buffer-name (or buf (current-buffer))))
    (_ nil)))

(defun wayland-run-or-raise--match-p (buf app-id-pattern title-pattern)
  "Check if BUF matches APP-ID-PATTERN or TITLE-PATTERN (OR logic)."
  (or (and app-id-pattern
           (let* ((app-id (wayland-run-or-raise--get-app-id buf)))
             (and app-id (string-match-p app-id-pattern app-id))
             ))
      (and title-pattern
           (let* ((title (wayland-run-or-raise--get-title buf)))
             (and title (string-match-p title-pattern title))))))

(defun wayland-run-or-raise--switch-to (buf)
  "Switch to buffer BUF and sync focus."
  (switch-to-buffer buf)
  (when (fboundp 'ewm--sync-focus)
    (ewm--sync-focus)))

(defun wayland-run-or-raise--run (command args)
  "Run COMMAND with ARGS using `start-process'."
  (when command
    (if args
        (apply #'start-process command nil command args)
      (start-process command nil command))))

(defun wayland-run-or-raise--cycle (matched-buffers)
  "Cycle through MATCHED-BUFFERS.
If current buffer is in matched-buffers, switch to the next one.
Otherwise switch to the most recent matched buffer (first in buffer-list)."
  (let* ((current (current-buffer))
         (in-matched-p (memq current matched-buffers)))
    (cond
     ;; Currently in a matched buffer - go to next matched
     (in-matched-p
      (let* ((current-pos (cl-position current matched-buffers :test #'eq))
             (next-pos (mod (1+ current-pos) (length matched-buffers)))
             (next-buffer (nth next-pos matched-buffers)))
        (wayland-run-or-raise--switch-to next-buffer)))
     ;; Not in matched buffer - go to most recent matched (first in buffer-list)
     (t
      (wayland-run-or-raise--switch-to (car matched-buffers))))))

(defun wayland-run-or-raise--exec (title app-id command args)
  "Execute run-or-raise logic with parsed arguments."
  (let* ((buffers (wayland-run-or-raise--get-buffers (string= app-id "emacs")))
         (matched-buffers (cl-loop for buf in buffers
                                  when (wayland-run-or-raise--match-p buf app-id title)
                                  collect buf)))
    (cond
     ((null matched-buffers)
      (wayland-run-or-raise--run command args))
      ((= (length matched-buffers) 1)
       (if (eq (current-buffer) (car matched-buffers))
          (bury-buffer)
        (wayland-run-or-raise--switch-to (car matched-buffers))))
     (t
      (wayland-run-or-raise--cycle matched-buffers)))))

;;;; Main API

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
                        append (list key val)))
         (args (if (and args (null (car (last args))))
                   (butlast args)
                 args))
         (command-val (or command (car args)))
         (args-val (if command args (cdr args))))
    (let* ((name (or name app-id title command (user-error "wayland-run-or-raise: :name, :app-id or :title required")))
           (func-sym (intern (format "wayland-run-or-raise-%s" name))))
      `(progn
         (defun ,func-sym ()
           (interactive)
           (when (string-equal (buffer-name) " *server*")
             (select-window (car (window-list)) t))
           (wayland-run-or-raise--exec ,title ,app-id ,command-val ',args-val))
         #',func-sym))))


(provide 'lazy-wayland)
;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-wayland.el ends here
