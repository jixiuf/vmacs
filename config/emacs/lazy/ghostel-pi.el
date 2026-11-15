;;; ghostel-pi.el --- Run the coding agent in Ghostel tmux windows -*- lexical-binding: t; -*-

;;; Commentary:

;; `ghostel-pi-project-toggle-chat' runs `ghostel-pi-cmd' (octet) in one
;; tmux session, a window per project, and shows its panes through
;; `ghostel-tmux', so the agents outlive Emacs: attach from a terminal
;; with `tmux attach -t octet' and every project's agent window is
;; there, and toggling a project again re-attaches to its window
;; instead of starting a second agent.

;;; Code:

(require 'ghostel)
(require 'ghostel-tmux)
(require 'project)
(require 'seq)

(defvar ghostel-pi-cmd "octet"
  "Command that starts the coding agent in its tmux window.
It runs as a shell command, so arguments are allowed too.")

(defvar ghostel-pi-tmux-session-prefix "octet"
  "Name of the tmux session that holds every project's agent window.
`ghostel-tmux--read-shell-session' leaves that session out of the ones
it offers, as its panes run the agent and not a shell.")

(defconst ghostel-pi--window-root-option "@ghostel-pi-root"
  "tmux window option that keeps the project root of an agent window.
tmux renames a window after the command it runs, and a window can be
renamed by hand, so the root is what finds a project's window again.")

(defun ghostel-pi--session-name ()
  "Return the tmux session every project's agent window lives in."
  ghostel-pi-tmux-session-prefix)

(defun ghostel-pi--window-name (proj)
  "Return the tmux window name for project PROJ.
tmux rewrites `:' and `.' in a window name, so every character outside
`[:alnum:]', `_' and `-' is mapped to `-' to keep the name readable."
  (replace-regexp-in-string "[^[:alnum:]_-]" "-" (project-name proj) t t))

(defun ghostel-pi--window-id (root)
  "Return the id of the tmux window that runs the agent of ROOT, or nil.
Nil also when tmux has no agent session."
  (let* ((result (ghostel-tmux--process-file
                  "list-windows" "-t" (ghostel-tmux--exact-session-target
                                       (ghostel-pi--session-name))
                  "-F" (concat "#{" ghostel-pi--window-root-option
                               "} #{window_id}")))
         (prefix (concat root " ")))
    (when (eql 0 (car result))
      (seq-some (lambda (line)
                  (when (string-prefix-p prefix line)
                    (string-to-number
                     (string-remove-prefix
                      "@" (string-trim (substring line (length prefix)))))))
                (split-string (cdr result) "\n" t)))))

(defun ghostel-pi--create-window (root name)
  "Start the agent of ROOT in a new tmux window called NAME.
Return the id of the window.  The session is created too, detached,
when tmux does not have it yet; the controller attaches to it."
  (let* ((session (ghostel-pi--session-name))
         (root (file-local-name (expand-file-name root)))
         (result (apply #'ghostel-tmux--process-file
                        (append
                         (if (ghostel-tmux--session-exists-p session)
                             (list "new-window" "-t"
                                   (ghostel-tmux--exact-session-target session))
                           (list "new-session" "-d" "-s" session))
                         (list "-P" "-F" "#{window_id}"
                               "-n" name "-c" root ghostel-pi-cmd)))))
    (when (eql 0 (car result))
      (let ((id (string-to-number
                 (string-remove-prefix "@" (string-trim (cdr result))))))
        (ghostel-tmux--process-file "set-option" "-w" "-t" (format "@%d" id)
                                    ghostel-pi--window-root-option root)
        id))))

(defun ghostel-pi--select-window (id)
  "Make tmux window ID the current window of the agent session."
  (ghostel-tmux--process-file
   "select-window" "-t"
   (format "%s@%d" (ghostel-tmux--exact-session-target
                    (ghostel-pi--session-name))
           id)))

(defun ghostel-pi--window-pane (controller id)
  "Return the pane buffer of tmux window ID under CONTROLLER, or nil."
  (seq-find (lambda (buf)
              (eql (buffer-local-value 'ghostel-tmux--window-id buf) id))
            (ghostel-tmux--controller-panes controller)))

(defun ghostel-pi--show (root name id)
  "Show the agent window of ROOT in the selected window.
ID is that window when tmux has it already.  A window that is missing is
started in ROOT, with the session and the agent when tmux does not have
them; the controller of the session then fills the selected window with
the pane of that window, as it does for an attach."
  (ghostel-tmux--check-spawn)
  (unless id
    (unless (setq id (ghostel-pi--create-window root name))
      (user-error "Ghostel-pi: tmux did not start a window for %s" root)))
  (ghostel-pi--select-window id)
  (let ((session (ghostel-pi--session-name)))
    (or (ghostel-tmux--reuse-controller session)
        (ghostel-tmux--spawn-controller
         session (ghostel-tmux--attach-args session)))))

;;;###autoload
(defun ghostel-pi-project-toggle-chat ()
  "Toggle the coding agent window of the current project.

Every project gets a window of the tmux session
`ghostel-pi-tmux-session-prefix', named after the project and running
`ghostel-pi-cmd' in its root, shown through `ghostel-tmux'.  From any
buffer of the ghostel tab group, that group is buried whole, whichever
terminal or agent pane is current.  From elsewhere, the project's pane
is switched to when it stands in a window already; otherwise its window
is selected in tmux, attaching the session and starting the agent when
tmux does not have them yet."
  (interactive)
  (if (eq (vmacs-tab-line-buffer-group) 'ghostel)
      ;; Inside the group -> bury it whole and land on the main group,
      ;; whichever of its terminals or panes is shown
      (vmacs-bury-tab-group 'ghostel)
    (let* ((proj (project-current t))
           (root (expand-file-name (project-root proj)))
           (id (ghostel-pi--window-id root))
           (controller (and id (ghostel-tmux--reuse-controller
                                (ghostel-pi--session-name) t)))
           (pane (and controller (ghostel-pi--window-pane controller id))))
      (if pane
          ;; Pane stands in a window -> switch to it, and to its window
          (ghostel-tmux--select-pane-buffer pane)
        (ghostel-pi--show root (ghostel-pi--window-name proj) id)))))

(provide 'ghostel-pi)
;;; ghostel-pi.el ends here
