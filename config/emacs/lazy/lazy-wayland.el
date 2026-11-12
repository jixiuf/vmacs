;;; lazy-wayland.el --- Wayland run-or-raise functionality -*- lexical-binding: t; -*-

(require 'seq)
(require 'cl-lib)

;;;; Configuration

(defvar wayland-compositor 'ewm
  "Current wayland compositor. Valid values: `ewm`, `reka`.")

(defvar wayland-run-or-raise--prev-group-buffer nil
  "Last buffer outside the matched group.
Recorded on each run-or-raise while the current buffer does not match;
used by hide-all to switch back to the previous tab-line group.")

;;;; Internal Helpers

;; TEMP DEBUG: remove after investigation
(defun wayland-run-or-raise--debug-log (format-string &rest args)
  "Append a debug line to /tmp/vmacs-tabs.log."
  (ignore-errors
    (let ((line (concat (format-time-string "[%H:%M:%S] [ror] ")
                        (apply #'format format-string args) "\n"))
          (old (if (file-exists-p "/tmp/vmacs-tabs.log")
                   (with-temp-buffer
                     (insert-file-contents "/tmp/vmacs-tabs.log")
                     (buffer-string))
                 "")))
      (with-temp-file "/tmp/vmacs-tabs.log"
        (insert old line)))))

(defun wayland-run-or-raise--get-buffers (emacs)
  "Get list of wayland-managed buffers based on `wayland-compositor'."
  (let ((buffers (pcase wayland-compositor
                   ('ewm
                    (let ((bl (buffer-list)))
                      ;; ewm--surfaces is a hash table, whose iteration order
                      ;; is arbitrary.  Sort live surface buffers by MRU
                      ;; (buffer-list) order so run-or-raise/cycle focus the
                      ;; most recently used window first.
                      (sort (cl-loop for buf being the hash-values of ewm--surfaces
                                     when (buffer-live-p buf) collect buf)
                            (lambda (a b)
                              (< (or (cl-position a bl) most-positive-fixnum)
                                 (or (cl-position b bl) most-positive-fixnum))))))
                   ('reka (seq-filter #'reka--is-reka-buffer (buffer-list)))
                   (_ (user-error "Unknown compositor: %s" wayland-compositor)))))
    (when emacs
      (setq buffers (seq-filter (lambda (buf)
                                  (and
                                   (not (memq (buffer-name buf) '("ewm-events")))
                                   (not (string-match-p "^ .*" (buffer-name buf)))
                                   (not (memq buf buffers))))
                                (tab-line-tabs-window-buffers)))
      (setq buffers (last  buffers 1)))
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

(defun wayland-run-or-raise--hide-matched (matched-buffers)
  "Bury all MATCHED-BUFFERS so ewm hides (minimizes) every one of them."
  (dolist (buf matched-buffers)
    (when (buffer-live-p buf)
      (bury-buffer buf))))

(defun wayland-run-or-raise--switch-to-prev-group (matched-buffers)
  "Switch back to the buffer recorded before entering the matched group.
Fall back to the most recent ordinary buffer outside MATCHED-BUFFERS."
  (let* ((prev wayland-run-or-raise--prev-group-buffer)
         (prev (and (buffer-live-p prev)
                    (not (memq prev matched-buffers))
                    prev))
         (rest (seq-remove
                (lambda (buf)
                  (or (memq buf matched-buffers)
                      (string-prefix-p " " (buffer-name buf))
                      (member (buffer-name buf) '("ewm-events"))))
                (buffer-list)))
         ;; Prefer non-surface buffers, so we land in the regular tab group.
         (target (or prev
                     (seq-find (lambda (buf)
                                 (not (buffer-local-value 'ewm-surface-app buf)))
                               rest)
                     (car rest))))
    (if target
        (wayland-run-or-raise--switch-to target)
      (bury-buffer))))

(defun wayland-run-or-raise--exec (title app-id command args after-exec)
  "Execute run-or-raise logic with parsed arguments.
AFTER-EXEC is a function to call after running the command (only when no matching buffer found).
When the current buffer is itself matched, hide ALL matched buffers and
switch back to the previous tab-line group instead of cycling."
  (let* ((buffers (wayland-run-or-raise--get-buffers (string= app-id "emacs")))
         (matched-buffers (cl-loop for buf in buffers
                                   when (wayland-run-or-raise--match-p buf app-id title)
                                   collect buf)))
    ;; Remember where we came from, so hide-all can switch back.
    (unless (memq (current-buffer) matched-buffers)
      (setq wayland-run-or-raise--prev-group-buffer (current-buffer)))
    (wayland-run-or-raise--debug-log
     "exec title=%S app-id=%S cur=%s(%S) matched=%s"
     title app-id (buffer-name)
     (and (fboundp 'vmacs-tab-line-buffer-group)
          (vmacs-tab-line-buffer-group))
     (mapcar #'buffer-name matched-buffers))
    (cond
     ((null matched-buffers)
      (when-let* ((p (wayland-run-or-raise--run command args))
                  (run (process-live-p p)))
        (when after-exec
          (funcall after-exec))))
     ((memq (current-buffer) matched-buffers)
      ;; Already inside the group: hide every matched window and go back.
      (wayland-run-or-raise--debug-log "exec -> hide-all %s"
                                       (mapcar #'buffer-name matched-buffers))
      (wayland-run-or-raise--hide-matched matched-buffers)
      (wayland-run-or-raise--switch-to-prev-group matched-buffers))
     (t
      (wayland-run-or-raise--debug-log "exec -> cycle to %s"
                                       (buffer-name (car matched-buffers)))
      (wayland-run-or-raise--cycle matched-buffers)))))

;;;; Main API

(defmacro wayland-run-or-raise (&rest form)
  "Match wayland windows by TITLE or APP-ID and switch to them, or run COMMAND if none found.

Keywords:
  :title      - regexp pattern to match window title (string or nil)
  :app-id     - regexp pattern to match app-id (string or nil)
  :command    - the command to run if no match found
  :name       - the name for the generated command (required for keybindings)
  :after-exec - function to call after executing command or switching to buffer

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
         (after-exec (plist-get form :after-exec))
         (args (cl-loop for (key val) on form by #'cddr
                        unless (memq key '(:title :app-id :command :name :after-exec))
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
           (wayland-run-or-raise--exec ,title ,app-id ,command-val ',args-val ,after-exec))
         #',func-sym))))


(wayland-run-or-raise :name "emacs" :app-id "emacs")
(wayland-run-or-raise :name "firefox" :app-id (rx (or "firefox" "firefox-bin" "firefox-esr" "google-chrome")) :command "firefox-bin")
(wayland-run-or-raise :name "term" :app-id (rx (or "foot" "alacritty" "foot-ws")) :command "alacritty")
(wayland-run-or-raise :app-id "apifox-pdv" :command "/opt/Apifox/apifox-pdv")
(wayland-run-or-raise :app-id "DBeaver" :command "dbeaver")
(wayland-run-or-raise :name "mongodb" :app-id "MongoDB Compass" :command "mongodb-compass" "--ignore-additional-command-line-flags" "--password-store=gnome-libsecret")
(wayland-run-or-raise :name "mitp" :app-id "mitp" :command "sh" "-c" "EDITOR=ec term.sh  --title=mimtproxy --class=mitp  -- mitmproxy")
(wayland-run-or-raise :name "apmssh" :app-id "APMSSH" :command "sh" "-c" "term.sh --termenv=tmux-direct --class=APMSSH --working-directory '/admin@bj-vc-client-apm-01:~'  -- tmux new-session -A -s vc")
(wayland-run-or-raise :app-id "wechat" :command "/opt/bin/wechat")
(defun kill-feishu-icon()
  (sleep-for 0.5)
  (dolist (buf (buffer-list))
    (when (string-match-p "\\*ewm:feishu:" (buffer-name buf))
      (kill-buffer buf))))


(wayland-run-or-raise :name "Bytedance-feishu"  :title "飞书" :after-exec 'kill-feishu-icon :command "bytedance-feishu-stable" "--enable-features=UseOzonePlatform" "--ozone-platform=wayland" "--enable-wayland-ime" "--wayland-text-input-version=3" )
(wayland-run-or-raise :name "keepassxc" :app-id "org.keepassxc.KeePassXC" :command "keepassxc")

(provide 'lazy-wayland)
;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-wayland.el ends here
