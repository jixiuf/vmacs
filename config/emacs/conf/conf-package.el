;;; -*- lexical-binding: t -*-
(require 'package)
(setq package-archives
      '(("melpa-cn" .  "https://melpa.org/packages/")
        ("nognu-cn" .  "https://elpa.nongnu.org/nongnu/")
        ("gnu-cn"   .  "https://elpa.gnu.org/packages/")))

(or (file-exists-p package-user-dir) (package-refresh-contents))
;; (package-initialize)
(add-hook 'after-init-hook (lambda()
                             (package-install-selected-packages t)))
;; (defun vmacs-package-hook()
;;   (meep-local-set-key "/" #'consult-focus-lines)
;;   (meep-local-set-key "z" #'consult-hide-lines))

;; (add-hook 'package-menu-mode-hook #'vmacs-package-hook)
(setq treesit-language-source-alist
      '((go . ("https://github.com/tree-sitter/tree-sitter-go.git" "v0.25.0"))
	    (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod.git" "v1.1.0"))
        (java . ("https://github.com/tree-sitter/tree-sitter-java.git" "v0.23.5"))
	    (rust . ("https://github.com/tree-sitter/tree-sitter-rust.git" "v0.23.2"))
	    (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile.git" "v0.2.0"))
	    (yaml . ("https://github.com/ikatyang/tree-sitter-yaml.git" "v0.5.0"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json.git" "v0.24.8"))
	    (c . ("https://github.com/tree-sitter/tree-sitter-c.git" "v0.23.5"))
	    (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp.git" "v0.23.0"))))

;; Install missing grammars.
(mapc #'(lambda (lang)
	      (unless (treesit-language-available-p lang)
	        (treesit-install-language-grammar lang)))
      (mapcar #'car treesit-language-source-alist))

(provide 'conf-package)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-common.el ends here.
