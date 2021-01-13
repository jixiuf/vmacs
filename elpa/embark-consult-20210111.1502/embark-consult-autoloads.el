;;; embark-consult-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "embark-consult" "embark-consult.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from embark-consult.el

(autoload 'embark-consult-preview-at-point "embark-consult" "\
Trigger Consult preview for Embark Collect entry at point.
Must be run from an auto-updating Embark Collect buffer that is
associated to an active minibuffer for a Consult command." t nil)

(autoload 'embark-consult-preview-minor-mode "embark-consult" "\
Minor mode to use Consult preview as you move around.
Must be used in an auto-updating Embark Collect buffer that is
associated to an active minibuffer for a Consult command.

If called interactively, toggle `Embark-Consult-Preview minor
mode'.  If the prefix argument is positive, enable the mode, and
if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "embark-consult" '("embark-consult-preview--"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; embark-consult-autoloads.el ends here
