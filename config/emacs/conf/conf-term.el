;;; conf-term.el  -*- lexical-binding: t; -*-

;;; Code:

;; (package-vc-install '(ghostel :url "https://github.com/dakra/ghostel.git" :branch "main"))
;; (package-vc-install '(ghostel-tmux :url "https://github.com/dakra/ghostel-tmux.git" :branch "main"))

(setq-default term-prompt-regexp "^[^#$%>\n]*[#$%>] *") ;默认 regex 相当于没定义，term-bol 无法正常中转到开头处
(setq ghostel-enable-osc52 t)
(setq ghostel-ignore-cursor-change t)
(setq ghostel-copy-mode-auto-load-scrollback t)
(setq ghostel-tmux-keep-detached-panes nil)
(defun ghostel-bury-buffer (arg)
  "Bury the whole ghostel tab group and land on the main group.
With a prefix arg, create a new ghostel terminal instead."
  (interactive "P")
  (if current-prefix-arg
      (call-interactively #'ghostel)
      (call-interactively #'ghostel-tmux-detach)
    (vmacs-bury-tab-group 'ghostel)))

;;; Auto cd for `ghostel-latest'

(defcustom vmacs-ghostel-latest-cd t
  "Non-nil means `ghostel-latest' cds its terminal to the calling directory.
The `cd' is sent only when the terminal's foreground program is a shell
sitting at an empty prompt, see `vmacs-ghostel-shell-programs': an agent
such as octet, or any other program, keeps its own directory."
  :type 'boolean
  :group 'ghostel)

(defcustom vmacs-ghostel-shell-programs
  '("sh" "bash" "zsh" "fish" "nu" "nushell" "dash" "ash" "ksh" "mksh"
    "pdksh" "oksh" "yash" "tcsh" "csh" "elvish" "xonsh" "osh" "oil" "ysh")
  "Program names that count as a shell for `vmacs-ghostel-latest-cd'.
The foreground program of a terminal is compared against these, so a
REPL, a pager or an agent is never sent a `cd'."
  :type '(repeat string)
  :group 'ghostel)

(defcustom vmacs-ghostel-empty-prompt-regexp
  "\\`[^[:cntrl:]]*[$%#>❯➜→λ❱»][ \u00a0]*\\'"
  "Regexp matching a terminal row that holds a prompt and nothing typed after it.
Consulted when the shell marks no OSC 133 prompt, as a shell inside tmux
does; with the markers ghostel knows the input start itself."
  :type 'regexp
  :group 'ghostel)

(defun vmacs-ghostel--shell-p (program)
  "Return non-nil when PROGRAM names one of `vmacs-ghostel-shell-programs'."
  (and program
       (member (string-remove-prefix "-" (file-name-nondirectory program))
               vmacs-ghostel-shell-programs)))

(defun vmacs-ghostel--tmux-pane-state ()
  "Return a plist describing this buffer's tmux pane, or nil.
The keys are :foreground, what the pane runs now, :directory, its
working directory, and :shell, the program the pane itself runs - nil
when tmux is not on this machine, as a local pid would then be a guess.
A pane buffer sees none of this: what it is fed from control mode has no
PTY."
  (when (and (derived-mode-p 'ghostel-mode)
             (bound-and-true-p ghostel-tmux-pane-mode)
             ghostel-tmux--pane-id)
    (let ((pane ghostel-tmux--pane-id)
          (controller ghostel-tmux--controller-buffer))
      (when (and (buffer-live-p controller)
                 (with-current-buffer controller
                   (ghostel-tmux--controller-live-p)))
        (with-current-buffer controller
          (let* ((proc ghostel-tmux--controller-process)
                 (client (and (processp proc) (process-id proc)))
                 (answer
                  (ignore-errors
                    (ghostel-tmux--process-file
                     "display-message" "-p" "-t" (format "%%%d" pane)
                     (concat "#{pane_current_command}|#{pane_current_path}"
                             "|#{pane_pid}|#{client_pid}")))))
            (when (eql 0 (car answer))
              (let ((fields (split-string (string-trim (cdr answer)) "|")))
                (list :foreground (nth 0 fields)
                      :directory (nth 1 fields)
                      :shell (and client
                                  (equal (nth 3 fields)
                                         (number-to-string client))
                                  (alist-get 'comm
                                             (process-attributes
                                              (string-to-number (nth 2 fields))))))))))))))

(defun vmacs-ghostel--terminal-state ()
  "Return a plist describing this terminal's own and foreground program.
The keys are those of `vmacs-ghostel--tmux-pane-state'.  A real PTY
learns its foreground program from the kernel and its directory from
OSC 7 alone."
  (or (vmacs-ghostel--tmux-pane-state)
      (let ((pgid (and ghostel--term (ghostel-foreground-pid))))
        (list :foreground (and pgid (alist-get 'comm (process-attributes pgid)))
              :directory (and ghostel--last-directory default-directory)
              :shell (and ghostel--term ghostel--shell-program)))))

(defun vmacs-ghostel--prompt-p ()
  "Return non-nil when this terminal waits at a prompt with nothing typed.
ghostel's OSC 133 input start answers when the shell reports one; a row
that ends with `vmacs-ghostel-empty-prompt-regexp' stands in when not."
  (and ghostel--cursor-char-pos
       (or (eq ghostel--cursor-char-pos (ghostel-input-start-point))
           (let ((row (save-excursion
                        (goto-char ghostel--cursor-char-pos)
                        (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position)))))
             (string-match-p vmacs-ghostel-empty-prompt-regexp row)))))

(defun vmacs-ghostel-cd (buffer directory)
  "Send a `cd' to DIRECTORY to ghostel BUFFER, once it sits at a shell prompt.
Does nothing unless the terminal's foreground program is a shell from
`vmacs-ghostel-shell-programs', so an agent's or another program's
directory stays what it is.  A shell that is not promptable yet - still
printing, or busy finishing a command - takes no `cd'."
  (when (and vmacs-ghostel-latest-cd
             directory
             (not (file-remote-p directory))
             (buffer-live-p buffer))
    (vmacs-ghostel--cd-attempt buffer (expand-file-name directory))))

(defun vmacs-ghostel--cd-attempt (buffer directory)
  "`cd' to DIRECTORY in ghostel BUFFER, unless it cannot take one yet."
  (when (and (buffer-live-p buffer)
             (not (file-remote-p directory)))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'ghostel-mode)
                 ghostel--term
                 (not (file-remote-p default-directory)))
        (let* ((state (vmacs-ghostel--terminal-state))
               (foreground (plist-get state :foreground))
               (here (plist-get state :directory))
               ;; The pane's own program stays the shell while commands come
               ;; and go in the foreground; only a shell takes a `cd', and
               ;; only when its keys are its own again.
               (shell-pane (vmacs-ghostel--shell-p
                            (or (plist-get state :shell) foreground)))
               (promptable (and (vmacs-ghostel--shell-p foreground)
                                (not (ghostel-alt-screen-p))
                                (not (ghostel-command-running-p))
                                (vmacs-ghostel--prompt-p))))
          (cond
           ;; A snapshot of an ended session knows no program: leave it be.
           ((and (null foreground) (null (plist-get state :shell))))
           ((and here (file-equal-p here directory))) ;already there
           (promptable
            (when (memq ghostel--input-mode '(copy emacs))
              (ghostel-readonly-exit))
            (ghostel-send-string
             (concat "cd " (shell-quote-argument
                            (file-local-name directory))))
            (ghostel-send-key "return"))
           ;; Busy, still printing, or between commands: not a shell
           ;; says why, a busy shell pane just takes no `cd' this time.
           ((not shell-pane)
            (message "Ghostel: %s runs %s, not a shell"
                     (buffer-name) foreground))))))))

(defun vmacs-ghostel--shell-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is a terminal whose own program is a shell.
The pane keeps that program while a command from it runs, so the buffer
stays the shell pane of its window even while it is busy."
  (with-current-buffer (or buffer (current-buffer))
    (when (and (derived-mode-p 'ghostel-mode) ghostel--term)
      (let ((state (vmacs-ghostel--terminal-state)))
        (or (vmacs-ghostel--shell-p (plist-get state :shell))
            (vmacs-ghostel--shell-p (plist-get state :foreground)))))))

(defun vmacs-ghostel--origin-directory ()
  "Return the directory the current buffer stands for, to `cd' elsewhere.
In a ghostel terminal that is where its program is, not the buffer's
`default-directory': a tmux pane only knows its controller's."
  (if (and (derived-mode-p 'ghostel-mode) ghostel--term)
      (plist-get (vmacs-ghostel--terminal-state) :directory)
    default-directory))

(defvar vmacs-ghostel--pending-attach-cd nil
  "The session and directory `ghostel-latest' left for its attach, a cons.
No pane buffer of the session is in Emacs yet when it is set: the `cd'
cannot be sent before `ghostel-tmux-attach-hook' says the session's
panes are there, and `vmacs-ghostel--cd-after-attach' sends it then.
An attach to another session drops it.")

(defun vmacs-ghostel--cd-after-attach ()
  "Send a `cd' that `ghostel-latest' left to this session's active pane.
`ghostel-tmux-attach-hook' runs this in the controller buffer, once the
session's panes are there: the moment an attach to an already running
session gains them, where `ghostel-latest' dropped its `cd' before.  A
shell that is busy or still printing takes no `cd', see
`vmacs-ghostel-cd'."
  (when vmacs-ghostel--pending-attach-cd
    (if (equal (car vmacs-ghostel--pending-attach-cd)
               ghostel-tmux--session-name)
        (let ((directory (cdr vmacs-ghostel--pending-attach-cd)))
          (setq vmacs-ghostel--pending-attach-cd nil)
          (when-let* ((pane (ghostel-tmux-active-pane)))
            (vmacs-ghostel-cd pane directory)))
      ;; The user attached elsewhere; the `cd' was for a session that
      ;; may never be attached.
      (setq vmacs-ghostel--pending-attach-cd nil))))

(defun ghostel-latest ()
  "Switch to the next ghostel terminal buffer, or create one.
A terminal whose foreground is a shell is preferred over an agent's
pane: that pane has `ghostel-pi-project-toggle-chat', and it would not
take a `cd'.  With `vmacs-ghostel-latest-cd', the buffer is first sent
a `cd' to the directory this command ran in, unless it holds a command,
a full screen program or typed input.  When no terminal of the tmux
session is in Emacs yet, the `cd' is remembered and sent to the
session's active pane once `ghostel-tmux-attach-hook' reports its
panes, so an attach to an existing session takes the directory too."
  (interactive)
  (let* ((directory (vmacs-ghostel--origin-directory))
         (bufs (cl-remove-if-not
                (lambda (b)
                  (with-current-buffer b
                    (and
                     (derived-mode-p 'ghostel-mode)
                     (string-match-p "*ghostel-tmux" (buffer-name))
                     (not (string-match-p "-pi*" (buffer-name))))))
                (buffer-list)))
         (target (or (cl-find-if #'vmacs-ghostel--shell-buffer-p bufs)
                     (car bufs))))
    (if target
        (progn
          (vmacs-ghostel-cd target directory)
          (pop-to-buffer target (append display-buffer--same-window-action
                                        '((category . comint)))))
      ;; No pane of the session is in Emacs yet, whether the session
      ;; exists or is to be created: remember where to `cd', and let
      ;; `vmacs-ghostel--cd-after-attach' send it when the session's
      ;; panes are there.
      (let ((session (ghostel-tmux--read-shell-session)))
        (setq vmacs-ghostel--pending-attach-cd (cons session directory))
        (ghostel-tmux-attach session))
      )))

(defvar ghostel-pi-tmux-session-prefix "rpi"
  "Name of the tmux session `ghostel-pi' runs every project's agent in.
That session holds a window per project, see `ghostel-pi--session-name'.
`ghostel-pi' owns the value; this is its default for the time before
that file is loaded.")

(defun ghostel-tmux--agent-session-p (name)
  "Return non-nil when tmux session NAME holds a `ghostel-pi' agent.
That is the session `ghostel-pi-tmux-session-prefix' names, whose panes
run the agent and not a shell, and the PREFIX-PROJECT sessions an
earlier `ghostel-pi' made for the same reason."
  (or (equal name ghostel-pi-tmux-session-prefix)
      (string-prefix-p (concat ghostel-pi-tmux-session-prefix "-") name)))

(defun ghostel-tmux--read-shell-session (&optional ask)
  "Return the name of the shell session tmux would attach.
That is the first of `ghostel-tmux--parse-session-rows' that is no
`ghostel-pi' agent session, see `ghostel-tmux--agent-session-p'.  With
ASK non-nil, prompt for a session, with that one as the default.
A non-zero exit (no server running) counts as no sessions."
  (let* ((result (ghostel-tmux--process-file
                  "list-sessions" "-F" ghostel-tmux--session-rows-format))
         (sessions (and (eql (car result) 0)
                        (cl-remove-if #'ghostel-tmux--agent-session-p
                                      (mapcar #'car
                                              (ghostel-tmux--parse-session-rows
                                               (cdr result)))))))
    (cond
     ((null sessions)
      (user-error "Ghostel-tmux: No tmux shell sessions found%s"
                  (ghostel-tmux--server-label)))
     (ask (ghostel-tmux--completing-read-session "tmux session" sessions))
     (t (car sessions)))))

(defun ghostel-reset-cursor-point ()
  "Move Emacs point to the terminal cursor position.
`ghostel--cursor-pos' holds the viewport-relative (COL . ROW), so
the row must be offset by the scrollback line count."
  (when (and ghostel--term ghostel--term-rows)
    (let ((pos ghostel--cursor-pos))
      (when pos
        (let ((scrollback (max 0 (- (count-lines (point-min) (point-max))
                                    ghostel--term-rows))))
          (goto-char (point-min))
          (forward-line (+ scrollback (cdr pos)))
          (move-to-column (car pos)))))))


(defun vmacs-ghostel-disable-copy()
  (when (member major-mode '(ghostel-mode))
    (ghostel-readonly-exit)
    (ghostel-reset-cursor-point)))

(defun vmacs-ghostel-enable-copy()
  (when (member major-mode '(ghostel-mode))
    (setq display-line-numbers nil)
    (when (member this-command '(keyboard-quit
                                 ;; helixel-normal-escape
                                 helixel-insert-exit
                                 ghostel-latest
                                 ghostel
                                 ghostel-other))
      (ghostel-line-mode))))
(with-eval-after-load 'ghostel
  (add-hook 'ghostel-tmux-attach-hook #'vmacs-ghostel--cd-after-attach))

(with-eval-after-load 'ghostel
  (define-key  ghostel-mode-map (kbd "C-s-v") #'ghostel-yank)
  (define-key  ghostel-mode-map (kbd "C-c C-f") #'ghostel-char-mode)
  (define-key  ghostel-mode-map (kbd "C-s-<return>") #'ghostel-char-mode)
  (define-key  ghostel-char-mode-map (kbd "C-s-<return>") #'ghostel-semi-char-mode)
  (helixel-define-key 'insert (kbd "C-s-v") #'ghostel-yank 'ghostel-mode)
  (helixel-define-key 'normal (kbd "C-s-v") #'ghostel-yank 'ghostel-mode)
  (add-hook 'helixel-normal-state-hook 'vmacs-ghostel-enable-copy)
  (add-hook 'helixel-insert-state-hook 'vmacs-ghostel-disable-copy))

(provide 'conf-term)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-term.el ends here.
