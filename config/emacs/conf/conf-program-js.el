;; -*- lexical-binding: t; -*-

(with-eval-after-load 'json-ts-mode
  (helix-define-key 'normal (kbd "C-c C-p") #'json-ts-jq-path-at-point 'json-ts-mode)
  (helix-define-key 'normal "z" #'hs-toggle-hiding 'json-ts-mode)
  (helix-define-key 'normal (kbd "<tab>") #'(lambda()
                                                           (interactive)
                                                           (if (region-active-p)
                                                               (call-interactively #'json-pretty-print)
                                                             (call-interactively #'json-pretty-print-buffer) )) 'json-ts-mode))
;; (add-hook 'js-mode-hook 'vmacs-js-mode-hook)
(add-hook 'json-ts-mode-hook 'vmacs-js-mode-hook)
;; (add-hook 'js-json-mode-hook 'vmacs-js-mode-hook)
(require 'hideshow)

;; evil 的za用来toggle hiddle
(defun vmacs-js-mode-hook()
  (modify-syntax-entry ?_ "_" (syntax-table))  ;还是让 _ 作为symbol，
  (hs-minor-mode 1))
;; (add-to-list 'hs-special-modes-alist
;;              '(js-mode
;;                "\\[\\|{" "\\]\\|}" "/[*/]" nil nil))
(add-to-list 'hs-special-modes-alist
             '(json-ts-mode
               "\\[\\|{" "\\]\\|}" "/[*/]" nil nil))


;; web-mode setup
;; https://github.com/joaotavora/eglot/discussions/1184

;; (defun vue-eglot-init-options ()
;;   (let ((tsdk-path (expand-file-name
;;                     "lib"
;;                     ;; (string-trim-right (shell-command-to-string "npm list --global --parseable typescript | head -n1"))
;;                     (shell-command-to-string "npm list --global --parseable typescript | head -n1 | tr -d \"\n\""))))
;;     `(:typescript (:tsdk ,tsdk-path
;;                          :languageFeatures (:completion
;;                                             (:defaultTagNameCase "both"
;;                                                                  :defaultAttrNameCase "kebabCase"
;;                                                                  :getDocumentNameCasesRequest nil
;;                                                                  :getDocumentSelectionRequest nil)
;;                                             :diagnostics
;;                                             (:getDocumentVersionRequest nil))
;;                          :documentFeatures (:documentFormatting
;;                                             (:defaultPrintWidth 100
;;                                                                 :getDocumentPrintWidthRequest nil)
;;                                             :documentSymbol t
;;                                             :documentColor t)))))

;; ;; Volar
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                `(vue-mode . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))
;; (add-to-list 'eglot-server-programs
;;                `(typescript-ts-mode . ("typescript-language-server" "--stdio")))  
;; )

;; eglot-workspace-configuration 
;; ;;; Directory Local Variables
;; ;;; For more information see (info "(emacs) Directory Variables")
;; ((typescript-ts-mode . ((eglot-server-programs . ((typescript-ts-mode . ("/home/jixiuf/.nvm/versions/node/v16.20.2/bin/typescript-language-server" "--stdio"))))))
;;  (vue-mode . ((eglot-server-programs . ((vue-mode . ("/home/jixiuf/.nvm/versions/node/v16.20.2/bin/vls"))))
;;               (eglot-workspace-configuration . (:vetur (:useWorkspaceDependencies t
;;                                                                                   :validation (:script t)))))))
(dolist (mod '(vue-mode-hook typescript-ts-mode-hook ))
  (add-hook mod #'editorconfig-mode))

(provide 'conf-program-js)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-jslang.el ends here.
