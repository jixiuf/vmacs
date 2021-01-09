;;; consult-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "consult" "consult.el" (0 0 0 0))
;;; Generated autoloads from consult.el

(autoload 'consult-multi-occur "consult" "\
Improved version of `multi-occur' based on `completing-read-multiple'.

See `multi-occur' for the meaning of the arguments BUFS, REGEXP and NLINES.

\(fn BUFS REGEXP &optional NLINES)" t nil)

(autoload 'consult-outline "consult" "\
Jump to an outline heading.

This command supports candidate preview." t nil)

(autoload 'consult-error "consult" "\
Jump to a compliation error in the current buffer.

This command works in compilation buffers and grep buffers.
The command supports preview of the currently selected error." t nil)

(autoload 'consult-mark "consult" "\
Jump to a marker in the buffer-local `mark-ring'.

The command supports preview of the currently selected marker position." t nil)

(autoload 'consult-global-mark "consult" "\
Jump to a marker in `global-mark-ring'.

The command supports preview of the currently selected marker position." t nil)

(autoload 'consult-line "consult" "\
Search for a matching line and jump to the line beginning.

The default candidate is a non-empty line closest to point.
This command obeys narrowing. Optionally INITIAL input can be provided.

\(fn &optional INITIAL)" t nil)

(autoload 'consult-goto-line "consult" "\
Read line number and jump to the line with preview.

The command respects narrowing and the settings
`consult-goto-line-numbers' and `consult-line-numbers-widen'." t nil)

(autoload 'consult-recent-file "consult" "\
Find recent using `completing-read'." t nil)

(autoload 'consult-recent-file-other-frame "consult" "\
Find recent using `completing-read'." t nil)

(autoload 'consult-recent-file-other-window "consult" "\
Find recent using `completing-read'." t nil)

(autoload 'consult-file-externally "consult" "\
Open FILE externally using the default application of the system.

\(fn FILE)" t nil)

(autoload 'consult-completion-in-region "consult" "\
Prompt for completion of region in the minibuffer if non-unique.

The function is called with 4 arguments: START END COLLECTION PREDICATE.
The arguments and expected return value are as specified for
`completion-in-region'. Use as a value for `completion-in-region-function'.

\(fn START END COLLECTION &optional PREDICATE)" nil nil)

(autoload 'consult-mode-command "consult" "\
Run a command from any of the given MODES.

If no MODES are specified, use currently active major and minor modes.

\(fn &rest MODES)" t nil)

(autoload 'consult-yank "consult" "\
Select text from the kill ring and insert it." t nil)

(autoload 'consult-yank-pop "consult" "\
If there is a recent yank act like `yank-pop'.

Otherwise select text from the kill ring and insert it.
See `yank-pop' for the meaning of ARG.

\(fn &optional ARG)" t nil)

(autoload 'consult-yank-replace "consult" "\
Select text from the kill ring.

If there was no recent yank, insert the text.
Otherwise replace the just-yanked text with the selected text." t nil)

(autoload 'consult-register "consult" "\
Use register REG and either jump to location or insert the stored text.

\(fn REG)" t nil)

(autoload 'consult-bookmark "consult" "\
If bookmark NAME exists, open it, otherwise create a new bookmark with NAME.

The command supports preview of file bookmarks and narrowing.

\(fn NAME)" t nil)

(autoload 'consult-apropos "consult" "\
Select pattern and call `apropos'." t nil)

(autoload 'consult-complex-command "consult" "\
Select and evaluate command from the command history.

This command can act as a drop-in replacement for `repeat-complex-command'." t nil)

(autoload 'consult-history "consult" "\
Insert string from HISTORY of current buffer.

In order to select from a specific HISTORY, pass the history variable as argument.

\(fn &optional HISTORY)" t nil)

(autoload 'consult-minor-mode-menu "consult" "\
Enable or disable minor mode.

This is an alternative to `minor-mode-menu-from-indicator'." t nil)

(autoload 'consult-theme "consult" "\
Disable current themes and enable THEME from `consult-themes'.

The command supports previewing the currently selected theme.

\(fn THEME)" t nil)

(autoload 'consult-buffer-other-frame "consult" "\
Enhanced `switch-to-buffer-other-frame' with support for virtual buffers.

See `consult-buffer'." t nil)

(autoload 'consult-buffer-other-window "consult" "\
Enhanced `switch-to-buffer-other-window' with support for virtual buffers.

See `consult-buffer'." t nil)

(autoload 'consult-buffer "consult" "\
Enhanced `switch-to-buffer' command with support for virtual buffers.

The command supports recent files, bookmarks, views and project files
as virtual buffers. Buffers are previewed. Furthermore narrowing
to buffers (b), files (f), bookmarks (m), views (v) and project files (p)
is supported via the corresponding keys." t nil)

(autoload 'consult-kmacro "consult" "\
Run a chosen keyboard macro.  With prefix ARG, run the macro that many times.

Macros containing mouse clicks aren't displayed.

\(fn ARG)" t nil)

(autoload 'consult-imenu "consult" "\
Choose item from flattened `imenu' using `completing-read' with preview.

See also `consult-project-imenu'." t nil)

(autoload 'consult-project-imenu "consult" "\
Choose item from the imenus of all buffers from the same project.

Only the buffers with the same major mode as the current buffer are
used. See also `consult-imenu'." t nil)

(autoload 'consult-grep "consult" "\
Search for regexp with grep in DIR with INITIAL input.

\(fn &optional DIR INITIAL)" t nil)

(autoload 'consult-git-grep "consult" "\
Search for regexp with grep in DIR with INITIAL input.

\(fn &optional DIR INITIAL)" t nil)

(autoload 'consult-ripgrep "consult" "\
Search for regexp with rg in DIR with INITIAL input.

\(fn &optional DIR INITIAL)" t nil)

(autoload 'consult-find "consult" "\
Search for regexp with find in DIR with INITIAL input.

\(fn &optional DIR INITIAL)" t nil)

(autoload 'consult-locate "consult" "\
Search for regexp with locate with INITIAL input.

\(fn &optional INITIAL)" t nil)

(register-definition-prefixes "consult" '("consult-"))

;;;***

;;;### (autoloads nil "consult-flymake" "consult-flymake.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from consult-flymake.el

(autoload 'consult-flymake "consult-flymake" "\
Jump to Flymake diagnostic." t nil)

(register-definition-prefixes "consult-flymake" '("consult-"))

;;;***

;;;### (autoloads nil nil ("consult-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; consult-autoloads.el ends here
