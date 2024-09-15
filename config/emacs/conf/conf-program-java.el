;; https://github.com/yveszoundi/eglot-java
(autoload 'eglot-java-mode "eglot-java" "" t)
(add-hook 'java-mode-hook #'eglot-java-mode)

(setq eglot-java-user-init-opts-fn 'custom-eglot-java-init-opts)
(defun custom-eglot-java-init-opts ( &optional server eglot-java-eclipse-jdt)
  "Custom options that will be merged with any default settings."
  ;; :bundles: ["/home/me/.emacs.d/lsp-bundles/com.microsoft.java.debug.plugin-0.50.0.jar"]
  `(:bundles: [,(download-java-debug-plugin)]
              ))

;; mvn  org.apache.maven.plugins:maven-dependency-plugin:2.1:get  -Dartifact=com.microsoft.java:com.microsoft.java.debug.plugin:0.53.0
;; ~/.m2/repository/com/microsoft/java/com.microsoft.java.debug.plugin/0.51.1/com.microsoft.java.debug.plugin-0.51.1.jar
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
            (print (format "https://repo1.maven.org/maven2/com/microsoft/java/com.microsoft.java.debug.plugin/%s/%s"
                           version jar-file))
            (url-copy-file (format "https://repo1.maven.org/maven2/com/microsoft/java/com.microsoft.java.debug.plugin/%s/%s"
                                   version jar-file-name)
                           jar-file))  ; 下载新的 jar 文件
          jar-file)))))  ; 返回新下载的文件路径

(provide 'conf-program-java)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-program-java.el ends here.
