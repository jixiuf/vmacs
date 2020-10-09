;;; mini-frame-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mini-frame" "mini-frame.el" (0 0 0 0))
;;; Generated autoloads from mini-frame.el

(defvar mini-frame-mode nil "\
Non-nil if Mini-Frame mode is enabled.
See the `mini-frame-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `mini-frame-mode'.")

(custom-autoload 'mini-frame-mode "mini-frame" nil)

(autoload 'mini-frame-mode "mini-frame" "\
Show minibuffer in child frame on read-from-minibuffer.

If called interactively, enable Mini-Frame mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "mini-frame" '("mini-frame-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mini-frame-autoloads.el ends here
