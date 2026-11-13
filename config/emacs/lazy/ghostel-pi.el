;;; ghostel-pi.el --- Run the coding agent in a Ghostel tmux session -*- lexical-binding: t; -*-

;;; Commentary:

;; `ghostel-pi-project-toggle-chat' runs `ghostel-pi-cmd' (octet) in a
;; per-project tmux session and shows its panes through `ghostel-tmux',
;; so the agent outlives Emacs: attach from a terminal with
;; `tmux attach -t octet-<project>', and toggling again re-attaches to
;; the same session instead of starting a second agent.

;;; Code:

(require 'ghostel)
(require 'ghostel-tmux)
(require 'project)

(defvar ghostel-pi-cmd "octet"
  "Command that starts the coding agent in its tmux session.
It runs as a shell command, so arguments are allowed too.")

(defvar ghostel-pi-tmux-session-prefix "octet"
  "Prefix of the per-project tmux session name.
The session is named PREFIX-PROJECT and is what
`tmux attach -t' takes.")

(defun ghostel-pi--session-name (proj)
  "Return the tmux session name for project PROJ.
tmux rewrites `:' and `.' in a session name, so everything outside
`[:alnum:]', `_' and `-' is mapped to `-' to keep the name addressable
as a target."
  (concat ghostel-pi-tmux-session-prefix
          "-"
          (replace-regexp-in-string "[^[:alnum:]_-]" "-"
                                    (project-name proj) t t)))

(defun ghostel-pi--pane-session (buffer)
  "Return the tmux session name of agent pane BUFFER, or nil.
A pane whose session ended or was detached keeps its buffer as a
read-only snapshot, without a controller, so it has no session then."
  (with-current-buffer buffer
    (let ((controller (and (bound-and-true-p ghostel-tmux-pane-mode)
                           (derived-mode-p 'ghostel-mode)
                           ghostel-tmux--controller-buffer)))
      (and (buffer-live-p controller)
           (with-current-buffer controller
             (and (ghostel-tmux--controller-live-p)
                  (string-prefix-p (concat ghostel-pi-tmux-session-prefix "-")
                                   (or ghostel-tmux--session-name ""))
                  ghostel-tmux--session-name))))))

(defun ghostel-pi--session-panes (session)
  "Return the live pane buffers of tmux SESSION attached in this Emacs."
  (seq-filter (lambda (buf)
                (equal (ghostel-pi--pane-session buf) session))
              (buffer-list)))

(defun ghostel-pi--start (session root)
  "Show tmux SESSION running `ghostel-pi-cmd' in ROOT.
The session is created when tmux does not have it yet and attached
otherwise, so an agent left running in tmux comes back with its state."
  (let* ((root (expand-file-name root))
         (default-directory root))
    (ghostel-tmux--check-spawn)
    (let ((ghostel-tmux--server (ghostel-tmux--resolve-server)))
      (ghostel-tmux--wait-for-pane
       (ghostel-tmux--spawn-controller
        session
        (if (ghostel-tmux--session-exists-p session)
            (ghostel-tmux--attach-args session)
          ;; tmux does not expand `~' in the directory it starts in.
          (list "new-session" "-As" (ghostel-tmux--literal-format session)
                "-n" "octet" "-c" (file-local-name root) ghostel-pi-cmd)))
       5.0))))

;;;###autoload
(defun ghostel-pi-project-toggle-chat ()
  "Toggle the coding agent session of the current project.

The agent runs in the tmux session `ghostel-pi-tmux-session-prefix'
plus the project name, shown through `ghostel-tmux'.  If a pane of
that session is shown in this Emacs:
  - if it is the current buffer, bury the whole ghostel tab group;
  - otherwise switch to it.
If it is not, attach to the session, creating it in the project root
when tmux does not have it yet."
  (interactive)
  (let* ((proj (project-current t))
         (session (ghostel-pi--session-name proj))
         (pane (car (ghostel-pi--session-panes session))))
    (cond
     ;; Pane is current -> bury the whole ghostel group and land on the
     ;; main group
     ((and pane (eq pane (current-buffer)))
      (vmacs-bury-tab-group 'ghostel))
     ;; Pane exists but is not current -> switch to it
     (pane
      (pop-to-buffer pane (append display-buffer--same-window-action
                                  '((category . comint)))))
     (t
      (ghostel-pi--start session (project-root proj))))))

(provide 'ghostel-pi)
;;; ghostel-pi.el ends here
