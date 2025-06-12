;;; -*- lexical-binding: t; coding:utf-8 -*-
;; golang 生成getter setter
;;;###autoload
(defun golang-setter-getter(beg end)
  "generate sets and gets for golang."
  (interactive "r")
  (require 'lazy-camelize)
  (let ((region-string (buffer-substring-no-properties beg end))
        (struct-name)
        (pairs)
        (buf-str))
    (with-temp-buffer
      (modify-syntax-entry ?< "w" )
      (modify-syntax-entry ?> "w" )
      (insert region-string) (insert "\n")
      (goto-char (point-min))
      (when (re-search-forward "type \\(.*\\) struct" (point-max) t)
        (setq struct-name (match-string 1))
        (message struct-name)
        (forward-line)(beginning-of-line)
        (while (re-search-forward "\\([a-zA-Z0-9_]+\\)[ \t]+\\([a-zA-z0-9_.]+\\)\\b" (point-at-eol) t)
          (setq pairs (append pairs (list (list (match-string 1)(match-string 2)))))
          (forward-line) (beginning-of-line)
          )
        (erase-buffer)
        (dolist (pair pairs)
          (insert (format "func (this *%s) Set%s(value %s) *%s{\n" struct-name (upcase-first-char(car pair)) (nth 1 pair) struct-name) )
          (insert (format "    this.%s = value\n" (car pair)) )
          (insert (format "    return this\n"))
          (insert (format "}\n") )
          (insert (format "func (this %s) Get%s() %s {\n" struct-name (upcase-first-char(car pair)) (nth 1 pair)) )
          (insert (format "    return this.%s\n" (car pair)) )
          (insert (format "}\n") )
          )
        (setq buf-str (buffer-string))))
    (goto-char end)
    (insert "\n")
    (insert buf-str)))

;;;###autoload
(defun go-get-package-path ()
  "Get the package path of the Go file currently being edited and copy it to the clipboard.
   This function needs `go` command installed and a `go.mod` file in the project root directory."
  (interactive)
  (let* ((filename (buffer-file-name))
         (root (substring filename 0 (string-match-p "\\(?:/src/\\)" filename)))
         (pkg-path-cmd (concat "go list -f '{{ .Dir }}' " (file-name-directory filename))) ; Generate command to get the package path
         (mod-path-cmd (concat "go list -m")) ; Generate command to get the mod path
         (pkg-path-output (shell-command-to-string pkg-path-cmd))
         (mod-path-output (shell-command-to-string mod-path-cmd))
         )
    (setq pkg-path (substring pkg-path-output 0 -1)) ; Remove the trailing newline from pkg-path
    (setq mod-path (substring mod-path-output 0 -1)) ; Remove the trailing newline from pkg-path
    (setq pkg-path (substring pkg-path (+ 5 (length root ))))
    (setq pkg-path (format"\"%s%s\"" (file-name-parent-directory mod-path) pkg-path) )
    (when (not (string= pkg-path ""))
      (kill-new pkg-path)
      (message pkg-path))))


;;;###autoload
(defun go-get()
  (interactive)
  (let* ((pkg (meow--parse-range-of-thing 'go-package t))
         (master "")
         ( display-buffer-alist
           '((t (display-buffer-no-window ))))
         cmd)
    (when pkg
      (if (= (prefix-numeric-value current-prefix-arg) 16)
          (setq master "@develop")
        (if (= (prefix-numeric-value current-prefix-arg) 4)
            (setq master "@master")))
      (setq cmd (format "go get %s%s;go mod tidy"
                        (buffer-substring (car pkg) (cdr pkg))
                        master))
      (message "%s" cmd)
      (async-shell-command cmd (messages-buffer)(messages-buffer)))))

(provide 'lazy-golang)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-golang.el ends here.
