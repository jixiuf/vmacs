;;; ethan-wspace-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ethan-wspace" "ethan-wspace.el" (0 0 0 0))
;;; Generated autoloads from ethan-wspace.el

(autoload 'ethan-wspace-mode "ethan-wspace" "\
Minor mode for coping with whitespace.

This just activates each whitespace type in this buffer.

\(fn &optional ARG)" t nil)

(defvar global-ethan-wspace-mode t "\
Non-nil if Global Ethan-Wspace mode is enabled.
See the `global-ethan-wspace-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-ethan-wspace-mode'.")

(custom-autoload 'global-ethan-wspace-mode "ethan-wspace" nil)

(autoload 'global-ethan-wspace-mode "ethan-wspace" "\
Toggle Ethan-Wspace mode in all buffers.
With prefix ARG, enable Global Ethan-Wspace mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Ethan-Wspace mode is enabled in all buffers where
`ethan-wspace-is-buffer-appropriate' would do it.
See `ethan-wspace-mode' for more information on Ethan-Wspace mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ethan-wspace" '("ethan-wspace-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ethan-wspace-autoloads.el ends here
