(eval-when-compile
  (require 'em-term)
  (require 'em-hist)
  (require 'eshell))

;; zsh 的alias  转变成eshell
;; alias | awk '{print "alias "$0}' | sed -E "s/^alias ([^=]+)='(.*)'$/alias \1 \2 \$*/g; s/'\\\''/'/g;" > ~/.emacs.d/eshell/alias
(setq-default eshell-directory-name (concat user-emacs-directory "eshell"))
(with-eval-after-load 'em-term
  (add-to-list 'eshell-visual-commands  "tmux")
  (when (boundp 'eshell-visual-subcommands) ;from emacs24.4
    (add-to-list 'eshell-visual-subcommands  '("tmux"))
    (add-to-list 'eshell-visual-subcommands  '("git" "l" "log" "diff" "show"))))

(autoload 'helm-eshell-history "helm-eshell" "" t nil)
(autoload 'helm-esh-pcomplete "helm-eshell" "" t nil)
;;using helm.el as the complete engine
(add-hook 'eshell-mode-hook
          #'(lambda ()
              ;; (define-key eshell-mode-map [M-right] 'copy-above-while-same);
              (define-key eshell-mode-map (kbd "M-.") 'eshell-insert-last-cmd-argument)
              (define-key eshell-mode-map [remap eshell-previous-matching-input] 'helm-eshell-history ) ;M-r
              (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete) ;tab
              (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete))) ;Tab

(defalias 'vi 'find-file)
(defalias 'o 'find-file-other-window)

(provide 'conf-eshell)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-eshell.el ends here.
