;;; embark-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "embark" "embark.el" (0 0 0 0))
;;; Generated autoloads from embark.el

(defun embark--record-this-command nil "\
Record command which opened the minibuffer.
We record this because `embark-default-action' needs to know it.
This function is meant to be added to `minibuffer-setup-hook'." (setq-local embark--command this-command))

(add-hook 'minibuffer-setup-hook #'embark--record-this-command)

(autoload 'embark-act-noexit "embark" "\
Embark upon an action.
The target of the action is chosen by `embark-target-finders'.
By default, if called from a minibuffer the target is the top
completion candidate, if called from an Embark Occur or a
Completions buffer it is the candidate at point." t nil)

(autoload 'embark-act "embark" "\
Embark upon an action and exit from all minibuffers (if any).
The target of the action is chosen by `embark-target-finders'.
By default, if called from a minibuffer the target is the top
completion candidate, if called from an Embark Occur or a
Completions buffer it ixs the candidate at point." t nil)

(autoload 'embark-become "embark" "\
Make current command become a different command.
Take the current minibuffer input as initial input for new
command.  The new command can be run normally using keybindings or
\\[execute-extended-command], but if the current command is found in a keymap in
`embark-become-keymaps', that keymap is activated to provide
convenient access to the other commands in it." t nil)

(autoload 'embark-live-occur "embark" "\
Create a live-updating Embark Occur buffer.
Optionally start in INITIAL-VIEW (either `list' or `grid')
instead of what `embark-occur-initial-view-alist' specifies.
Interactively, \\[universal-argument] means grid view, a prefix
argument of 1 means list view.

To control the display, add an entry to `display-buffer-alist'
with key \"Embark Live Occur\".

\(fn &optional INITIAL-VIEW)" t nil)

(autoload 'embark-occur "embark" "\
Create an Embark Occur buffer and exit all minibuffers.
Optionally start in INITIAL-VIEW (either `list' or `grid')
instead of what `embark-occur-initial-view-alist' specifies.
Interactively, \\[universal-argument] means grid view, a prefix
argument of 1 means list view.

To control the display, add an entry to `display-buffer-alist'
with key \"Embark Occur\".

\(fn &optional INITIAL-VIEW)" t nil)

(autoload 'embark-switch-to-live-occur "embark" "\
Switch to the Embark Live Occur buffer, creating it if necessary." t nil)

(autoload 'embark-export "embark" "\
Create a type-specific buffer to manage current candidates.
The variable `embark-exporters-alist' controls how to make the
buffer for each type of completion." t nil)

(register-definition-prefixes "embark" '("embark-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; embark-autoloads.el ends here
