;; config for yasnippet
(require 'yasnippet)
;; batch 模式下不启用yas
(if (and noninteractive (not (vmacs-dumping-p)))
  (yas-global-mode -1)
  (yas-global-mode 1))

(setq-default yas-prompt-functions '(yas-completing-prompt))
;; (define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)

(defun vmacs-autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

;;; auto-insert
(setq-default auto-insert-directory (concat user-emacs-directory "auto-insert-template/"))
  ;;; Adds hook to find-files-hook
(if (and noninteractive (not (vmacs-dumping-p)))
    (auto-insert-mode -1)
  (auto-insert-mode 1))


(setq-default auto-insert-query nil) ;;; If you don't want to be prompted before insertion
(define-auto-insert "\\.el$" ["el-auto-insert" vmacs-autoinsert-yas-expand])
(define-auto-insert "\\.erl$" ["erl-auto-insert" vmacs-autoinsert-yas-expand])
(define-auto-insert "\\.hrl$" ["hrl-auto-insert" vmacs-autoinsert-yas-expand])
(define-auto-insert "\\.c$"  ["c-auto-insert" vmacs-autoinsert-yas-expand])
(define-auto-insert "\\.cpp$" ["c++-auto-insert" vmacs-autoinsert-yas-expand])
(define-auto-insert "\\.cc$" ["c++-auto-insert" vmacs-autoinsert-yas-expand])
(define-auto-insert "\\.cs$" ["csharp-auto-insert" vmacs-autoinsert-yas-expand])
(define-auto-insert "\\.org$" ["org-auto-insert" vmacs-autoinsert-yas-expand])
(define-auto-insert "\\.tex$" ["tex-auto-insert" vmacs-autoinsert-yas-expand])
(define-auto-insert "\\.py$" ["py-auto-insert" vmacs-autoinsert-yas-expand])
(define-auto-insert "\\.go$" ["go-auto-insert" vmacs-autoinsert-yas-expand])

(provide 'conf-yasnippet)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-yasnippet.el ends here.
