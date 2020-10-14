
;; (declare-function helm-gtags-set-GTAGSLIBPATH-alist "helm-gtags")

(add-hook 'objc-mode-hook 'vmacs-objc-mode-hook)

(defun vmacs-objc-mode-hook()
  (company-mode 1)
  (local-set-key "]" 'objc-surround)
  (setq-local company-backends
              '( company-clang company-xcode company-semantic  company-files  company-keywords company-dabbrev))
  (when (equal system-type 'darwin)
    ;; Forgot what this was for..think some os x issues.
    (setenv "LC_CTYPE" "UTF-8"))

  ;; (setq-local
  ;;  ;; flycheck-make-executable "/usr/local/bin/make"
  ;;  company-clang-executable
  ;;  "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang++")

  ;; (setq-local  company-clang-arguments
  ;;              '(
  ;;                ;; "-std=c++11"

  ;;                ;; If coding for OS X
  ;;                "-isysroot"
  ;;                "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk"

  ;;                ;; If coding for iOS
  ;;                ;; "-isysroot"
  ;;                ;; "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk"
  ;;                ))

  ;; (let ((xcode-proj-root (find-xcode-proj-root))
  ;;       project-name)
  ;;   (when xcode-proj-root
  ;;     (require 'helm-gtags)
  ;;     (setq project-name
  ;;           (file-relative-name
  ;;            (directory-file-name xcode-proj-root)
  ;;            (file-name-directory(directory-file-name xcode-proj-root))))
  ;;     (helm-gtags-set-GTAGSLIBPATH-alist (concat xcode-proj-root project-name)
  ;;                                        `(
  ;;                                          ;; ,(concat xcode-proj-root project-name)
  ;;                                          ;; 在这两个目录运行sudo gtags,;;之后gd 可以跳转到函数定义处
  ;;                                          "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk"
  ;;                                          "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk"))
  ;;     )
  ;;   )
  )
(defun find-xcode-proj-root()
  (locate-dominating-file
   default-directory
   (lambda(dir) (directory-files dir t "\.xcodeproj$" t))))


(provide 'conf-program-objc)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-program-objc.el ends here.
