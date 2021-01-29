;;; verb-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ob-verb" "ob-verb.el" (0 0 0 0))
;;; Generated autoloads from ob-verb.el

(autoload 'ob-verb-response-mode "ob-verb" "\
Major mode for displaying HTTP responses with Babel.

\(fn)" t nil)

(register-definition-prefixes "ob-verb" '("ob-verb--" "org-babel-execute:verb"))

;;;***

;;;### (autoloads nil "verb" "verb.el" (0 0 0 0))
;;; Generated autoloads from verb.el

(defvar verb-command-map (let ((map (make-sparse-keymap))) (define-key map (kbd "C-s") #'verb-send-request-on-point-other-window) (define-key map (kbd "C-r") #'verb-send-request-on-point-other-window-stay) (define-key map (kbd "C-m") #'verb-send-request-on-point-no-window) (define-key map (kbd "C-f") #'verb-send-request-on-point) (define-key map (kbd "C-k") #'verb-kill-all-response-buffers) (define-key map (kbd "C-e") #'verb-export-request-on-point) (define-key map (kbd "C-u") #'verb-export-request-on-point-curl) (define-key map (kbd "C-b") #'verb-export-request-on-point-verb) (define-key map (kbd "C-v") #'verb-set-var) map) "\
Keymap for `verb-mode' commands.
Bind this to an easy-to-reach key in Org mode in order to use Verb
comfortably.  All commands listed in this keymap automatically enable
`verb-mode' in the current buffer when used.")

(autoload 'verb-mode "verb" "\
Minor mode for organizing and making HTTP requests from Emacs.
This mode acts as an extension to Org mode.  Make sure you enable it
on buffers using Org as their major mode.

If called interactively, toggle `Verb mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

See the documentation in URL `https://github.com/federicotdn/verb' for
more details on how to use it.

\(fn &optional ARG)" t nil)

(autoload 'verb-send-request-on-point-other-window "verb" "\
Send the request specified by the selected heading's text contents.
Show the results on another window and switch to it, using
`verb-send-request-on-point'.  See that function's documentation for a
description of prefix argument ARG.

\(fn &optional ARG)" t nil)

(autoload 'verb-send-request-on-point-other-window-stay "verb" "\
Send the request specified by the selected heading's text contents.
Show the results on another window but don't switch to it, using
`verb-send-request-on-point'.  See that function's documentation for a
description of prefix argument ARG.

\(fn &optional ARG)" t nil)

(autoload 'verb-send-request-on-point-no-window "verb" "\
Send the request specified by the selected heading's text contents.
Do this using `verb-send-request-on-point', but do not show the
results on any window.  See that function's documentation for a
description of prefix argument ARG.

This command is useful for cases where the request is only being sent
for its side effects.

\(fn &optional ARG)" t nil)

(autoload 'verb-send-request-on-point "verb" "\
Send the request specified by the selected heading's text contents.
After the request has been sent, return the response buffer (the
buffer where the response will be loaded into).

Note that the contents of all parent headings are considered as well;
see `verb--request-spec-from-hierarchy' to see how this is done.

The buffer containing the response is shown (or not shown) in
different ways, depending on the value of WHERE:

- `other-window': Show the response buffer on another window and
  select it.
- `stay-window': Show the response buffer on another window, but
  keep the current one selected.
- `this-window': Show the response buffer in the current window.
- `minibuffer': Show the response status on the minibuffer, but don't
  show the response buffer anywhere.
- nil: Send the request but do not show the response buffer nor the
  response status anywhere.

The response buffer won't have any contents until the HTTP response
has been received.  For all valid values of WHERE except nil, the
response status will be shown on the minibuffer when the response is
received.

If prefix argument ARG is non-nil, allow the user to quickly edit the
request before it is sent.  The changes made will not affect the
contents of the current buffer and will be discarded after the request
is sent.

The `verb-post-response-hook' hook is called after a response has been
received.

\(fn WHERE &optional ARG)" t nil)

(autoload 'verb-kill-all-response-buffers "verb" "\
Kill all response buffers, and delete their windows.
If KEEP-WINDOWS is non-nil, do not delete their respective windows.

\(fn &optional KEEP-WINDOWS)" t nil)

(autoload 'verb-export-request-on-point "verb" "\
Export the request specification on point.
Interactively, prompt the user for an export function, and call that
function with the request specification object.  See the
`verb-export-functions' variable for more details.  If called from
Lisp, use the export function under NAME.  If NAME is nil, prompt the
user anyways.

No HTTP request is sent, unless the export function does this
explicitly.  Lisp code tags are evaluated when exporting.

\(fn &optional NAME)" t nil)

(autoload 'verb-export-request-on-point-verb "verb" "\
Export request on point to verb format.
See `verb--export-to-verb' for more information." t nil)

(autoload 'verb-export-request-on-point-curl "verb" "\
Export request on point to curl format.
See `verb--export-to-curl' for more information." t nil)

(register-definition-prefixes "verb" '("verb-"))

;;;***

;;;### (autoloads nil nil ("verb-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; verb-autoloads.el ends here
