;; emacs -Q -l ~/vmacs/init-straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq vterm-always-compile-module t)
(use-package vterm
  :straight t
  :bind (("C-c v" . vterm)
         :map vterm-mode-map
         ("C-g" . vterm--self-insert)))

(use-package vterm-toggle
  :straight (vterm-toggle :type git :host github :repo "jixiuf/vterm-toggle" :branch "master"))

(provide 'init-str)

;; Local Variables:
;; coding: utf-8
;; End:

;;; init-str.el ends here.
