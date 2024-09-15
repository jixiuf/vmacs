;; https://github.com/yveszoundi/eglot-java
;; https://emacs-china.org/t/dape/27728

;; mvn  org.apache.maven.plugins:maven-dependency-plugin:3.7.1:get  -Dartifact=com.microsoft.java:com.microsoft.java.debug.plugin:0.53.0
;; mvn  org.apache.maven.plugins:maven-dependency-plugin:3.7.1:get  -Dartifact=org.projectlombok:lombok:1.18.34
;; https://github.com/apache/maven-dependency-plugin
;; ~/.m2/repository/com/microsoft/java/com.microsoft.java.debug.plugin/0.51.1/com.microsoft.java.debug.plugin-0.51.1.jar

(autoload 'eglot-java-mode "eglot-java" "" t)
(add-hook 'java-mode-hook #'eglot-java-mode)

(with-eval-after-load 'eglot-java
  (setq eglot-java-user-init-opts-fn 'custom-eglot-java-init-opts)
  ;; (add-to-list 'eglot-java-eclipse-jdt-args (concat "-javaagent:" (expand-file-name (download-lombok))))
  )

(with-eval-after-load 'dape
  (add-to-list 'dape-configs
               `(junit
                 modes (java-mode java-ts-mode)
                 ensure (lambda (config)
                          (save-excursion
                            (unless (eglot-current-server)
                              (user-error "No eglot instance active in buffer %s" (current-buffer)))
                            (when (equal ':json-false
                                         (eglot-execute-command
                                          (eglot-java--find-server)
                                          "java.project.isTestFile"
                                          (vector (eglot--path-to-uri (buffer-file-name)))))
                              (user-error "Not in a java test file"))
                            t))
                 fn (lambda (config)
                      (let ((file (expand-file-name (plist-get config :program)
                                                    (project-root (project-current)))))
                        (with-current-buffer (find-file-noselect file)
                          (save-excursion (eglot-java-run-test t))
                          (thread-first
                            config
                            (plist-put 'hostname "localhost")
                            (plist-put 'port (eglot-execute-command (eglot-current-server)
                                                                    "vscode.java.startDebugSession" nil))
                            (plist-put :projectName (project-name (project-current)))))))
                 :program dape-buffer-default
                 :request "attach"
                 :hostname "localhost"
                 :port 8000)))

(defun custom-eglot-java-init-opts ( &optional server eglot-java-eclipse-jdt)
  "Custom options that will be merged with any default settings."
  ;; :bundles ["/home/me/.emacs.d/lsp-bundles/com.microsoft.java.debug.plugin-0.50.0.jar"]
  `(:bundles [,(download-java-debug-plugin)]))

(defun download-java-debug-plugin ()
  (let ((cache-dir (expand-file-name "~/.cache/emacs/"))
        (url "https://repo1.maven.org/maven2/com/microsoft/java/com.microsoft.java.debug.plugin/maven-metadata.xml")
        (version nil)
        (jar-file-name nil)
        (jar-file nil))
    (unless (file-directory-p cache-dir)
      (make-directory cache-dir t))
    ;; 检查现有文件
    (setq jar-file (car (directory-files cache-dir nil "com\\.microsoft\\.java\\.debug\\.plugin-\\([0-9]+\\.[0-9]+\\.[0-9]+\\)\\.jar" t)))
    (if jar-file
        (expand-file-name jar-file cache-dir)  ; 返回已存在的文件路径
      (with-temp-buffer
        (url-insert-file-contents url)  ; 下载元数据
        (when (re-search-forward "<latest>\\(.*?\\)</latest>" nil t)
          (setq version (match-string 1))
          (setq jar-file-name (format "com.microsoft.java.debug.plugin-%s.jar" version))
          (setq jar-file (expand-file-name jar-file-name cache-dir))
          (unless (file-exists-p jar-file)
            (setq url (format "https://repo1.maven.org/maven2/com/microsoft/java/com.microsoft.java.debug.plugin/%s/%s"
                              version jar-file-name))
            (message url)
            (url-copy-file url jar-file))  ; 下载新的 jar 文件
          jar-file)))))  ; 返回新下载的文件路径



;; (defun download-lombok ()
;;   (let ((cache-dir (expand-file-name "~/.cache/emacs/"))
;;         (url "https://repo1.maven.org/maven2/org/projectlombok/lombok/maven-metadata.xml")
;;         (version nil)
;;         (jar-file-name nil)
;;         (jar-file nil))
;;     (unless (file-directory-p cache-dir)
;;       (make-directory cache-dir t))

;;     ;; 检查现有文件
;;     (setq jar-file (car (directory-files cache-dir nil "lombok-\\([0-9]+\\.[0-9]+\\.[0-9]+\\)\\.jar" t)))
;;     (if jar-file
;;         (expand-file-name jar-file cache-dir)  ; 返回已存在的文件路径
;;       (with-temp-buffer
;;         (url-insert-file-contents url)  ; 下载元数据
;;         (when (re-search-forward "<latest>\\(.*?\\)</latest>" nil t)
;;           (setq version (match-string 1))
;;           (setq jar-file-name (format "lombok-%s.jar" version))
;;           (setq jar-file (expand-file-name jar-file-name cache-dir))
;;           (unless (file-exists-p jar-file)
;;             (setq url (format "https://repo1.maven.org/maven2/org/projectlombok/lombok/%s/%s"
;;                               version jar-file-name))
;;             (message url)
;;             (url-copy-file url jar-file))  ; 下载新的 jar 文件
;;           jar-file)))))  ; 返回新下载的文件路径


(provide 'conf-program-java)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-program-java.el ends here.
