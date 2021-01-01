;;; consult-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "consult" "consult.el" (0 0 0 0))
;;; Generated autoloads from consult.el

(defvar consult-preview-mode nil "\
Non-nil if Consult-Preview mode is enabled.
See the `consult-preview-mode' command
for a description of this minor mode.")

(custom-autoload 'consult-preview-mode "consult" nil)

(autoload 'consult-preview-mode "consult" "\
Enable preview for consult commands.

If called interactively, toggle `Consult-Preview mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'consult-multi-occur "consult" "\
Improved version of `multi-occur' based on `completing-read-multiple'.
See `multi-occur' for the meaning of the arguments BUFS, REGEXP and NLINES.

\(fn BUFS REGEXP &optional NLINES)" t nil)

(autoload 'consult-outline "consult" "\
Jump to an outline heading." t nil)

(autoload 'consult-error "consult" "\
Jump to an error in the current buffer." t nil)

(autoload 'consult-mark "consult" "\
Jump to a marker in `mark-ring'." t nil)

(autoload 'consult-global-mark "consult" "\
Jump to a marker in `global-mark-ring'." t nil)

(autoload 'consult-line "consult" "\
Search for a matching line and jump to the line beginning.
The default candidate is a non-empty line closest to point.
This command obeys narrowing. Optionally INITIAL input can be provided.

\(fn &optional INITIAL)" t nil)

(autoload 'consult-goto-line "consult" "\
Read line number and jump to the line with preview.

Respects narrowing and the settings
`consult-goto-line-numbers' and `consult-line-numbers-widen'." t nil)

(autoload 'consult-recent-file "consult" "\
Find recent using `completing-read'." t nil)

(autoload 'consult-recent-file-other-frame "consult" "\
Find recent using `completing-read'." t nil)

(autoload 'consult-recent-file-other-window "consult" "\
Find recent using `completing-read'." t nil)

(autoload 'consult-file-externally "consult" "\
Open FILE using system's default application.

\(fn FILE)" t nil)

(autoload 'consult-completion-in-region "consult" "\
Prompt for completion of region in the minibuffer if non-unique.

The function is called with 4 arguments: START END COLLECTION PREDICATE.
The arguments and expected return value are as specified for
`completion-in-region'. Use as a value for `completion-in-region-function'.

\(fn START END COLLECTION &optional PREDICATE)" nil nil)

(autoload 'consult-mode-command "consult" "\
Run a command from the MODES.

If no modes are specified, use currently active major and minor modes.

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
Use register REG. Either jump to location or insert the stored text.

\(fn REG)" t nil)

(autoload 'consult-bookmark "consult" "\
If bookmark NAME exists, open it, otherwise set bookmark under the given NAME.

\(fn NAME)" t nil)

(autoload 'consult-apropos "consult" "\
Select pattern and call `apropos'." t nil)

(autoload 'consult-complex-command "consult" "\
Select and evaluate command from the command history." t nil)

(autoload 'consult-history "consult" "\
Insert string from buffer HISTORY.

\(fn &optional HISTORY)" t nil)

(autoload 'consult-minor-mode-menu "consult" "\
Enable or disable minor mode.
This is an alternative to `minor-mode-menu-from-indicator'." t nil)

(autoload 'consult-theme "consult" "\
Disable current themes and enable THEME from `consult-themes'.

\(fn THEME)" t nil)

(autoload 'consult-buffer-other-frame "consult" "\
Enhanced `switch-to-buffer-other-frame' command with support for virtual buffers." t nil)

(autoload 'consult-buffer-other-window "consult" "\
Enhanced `switch-to-buffer-other-window' command with support for virtual buffers." t nil)

(autoload 'consult-buffer "consult" "\
Enhanced `switch-to-buffer' command with support for virtual buffers." t nil)

(autoload 'consult-kmacro "consult" "\
Run a chosen keyboard macro.  With prefix ARG, run the macro that many times.

Macros containing mouse clicks aren't displayed.

\(fn ARG)" t nil)

(autoload 'consult-imenu "consult" "\
Choose from flattened `imenu' using `completing-read'." t nil)

(autoload 'consult-grep "consult" "\
Search for regexp with grep in DIR.

\(fn &optional DIR)" t nil)

(autoload 'consult-git-grep "consult" "\
Search for regexp with grep in DIR.

\(fn &optional DIR)" t nil)

(autoload 'consult-ripgrep "consult" "\
Search for regexp with rg in DIR.

\(fn &optional DIR)" t nil)

(autoload 'consult-find "consult" "\
Search for regexp with find in DIR.

\(fn &optional DIR)" t nil)

(autoload 'consult-fdfind "consult" "\
Search for regexp with fdfind in DIR.

\(fn &optional DIR)" t nil)

(autoload 'consult-locate "consult" "\
Search for regexp with locate." t nil)

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
