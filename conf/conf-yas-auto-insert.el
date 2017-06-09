;; 利用yasnipet模版,在创新新文件时,自动在文件中插入相应的模版

(autoload 'yas-expand-snippet "yasnippet")
(defun my-autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

;;; auto-insert
(setq-default auto-insert-directory (concat user-emacs-directory "auto-insert-template/"))
(unless noninteractive (auto-insert-mode 1))  ;;; Adds hook to find-files-hook

(setq-default auto-insert-query nil) ;;; If you don't want to be prompted before insertion
(define-auto-insert "\\.el$" ["el-auto-insert" my-autoinsert-yas-expand])
(define-auto-insert "\\.erl$" ["erl-auto-insert" my-autoinsert-yas-expand])
(define-auto-insert "\\.hrl$" ["hrl-auto-insert" my-autoinsert-yas-expand])
(define-auto-insert "\\.c$"  ["c-auto-insert" my-autoinsert-yas-expand])
(define-auto-insert "\\.cpp$" ["c++-auto-insert" my-autoinsert-yas-expand])
(define-auto-insert "\\.cc$" ["c++-auto-insert" my-autoinsert-yas-expand])
(define-auto-insert "\\.cs$" ["csharp-auto-insert" my-autoinsert-yas-expand])
(define-auto-insert "\\.org$" ["org-auto-insert" my-autoinsert-yas-expand])
(define-auto-insert "\\.tex$" ["tex-auto-insert" my-autoinsert-yas-expand])
(define-auto-insert "\\.py$" ["py-auto-insert" my-autoinsert-yas-expand])
(define-auto-insert "\\.go$" ["go-auto-insert" my-autoinsert-yas-expand])
;; (define-auto-insert "\\.org$" "org-auto-insert")
(provide 'conf-yas-auto-insert)
