;;; -*- lexical-binding: t; -*-
;; Make vc-dir behave like Magit.
;; Insert Unpulled/Unpushed header in vc-dir-mode

;; lazy enable vcgit-global-minor-mode with:
;; (add-hook 'vc-dir-mode-hook #'vcgit-global-minor-mode)
;; Tab: only when cursor not at the beginning of outline header
;; see outline-minor-mode-cycle-filter


(eval-when-compile
  (require  'vc)
  (require  'vc-git)
  (require  'vc-dir))

(defgroup vcgit nil
  "VC Git extension"
  :group 'vc)

(defcustom vcgit-log-commit-count 6
  "How many recent commits to show in Unpulled/Unpushed header."
  :group 'vcgit
  :type 'number)
(defcustom vcgit-outline-regexp "\\(^ +.+/$\\)\\|\\(^[^* \t\n].*:$\\)\\|\\(Stash\\)"
  "Regexp for outline-regexp"
  :group 'vcgit
  :type 'string)
(defcustom vcgit-outline-level2-regexp "\\(?:^ +.+?/.+/\\)\\|\\(?:^[^:]+::\\)"
  "Regexp for outline-regexp of level 2"
  :group 'vcgit
  :type 'string)

(defface vcgit-todo-face
  '((t (:foreground "Red" :box (:line-width 2 :color "grey75" :style released-button) :height 1.2
        :inherit default)))
  "Font Lock mode face used to highlight TODO."
  :group 'vcgit)


;; functions from emacs31
(unless (fboundp 'vc-git--current-branch)
  (defun vc-git--out-match (args regexp group)
    "Run `git ARGS...' and return match for group number GROUP of REGEXP.
Return nil if the output does not match.  The exit status is ignored."
    (let ((out (apply #'vc-git--out-str args)))
      (when (string-match regexp out)
        (match-string group out))))
  (defun vc-git--current-branch ()
    (vc-git--out-match '("symbolic-ref" "HEAD")
                       "^\\(refs/heads/\\)?\\(.+\\)$" 2)))

(defun vcgit--branch-remote (&optional branch)
  "Return the remote name that the given BRANCH is tracking.
If BRANCH is not provided, use the current branch.
If the branch is tracking a local branch, return `.'."
  (let ((br (or branch (vc-git--current-branch))))
    (vc-git--out-match
     `("config" ,(concat "branch." br ".remote"))
     "\\([^\n]+\\)" 1)))

(defun vcgit--branch-merge (&optional branch)
  "Return the remote branch name that the given BRANCH is merging into.
If BRANCH is not provided, use the current branch."
  (let ((br (or branch (vc-git--current-branch))))
    (vc-git--out-match
     `("config" ,(concat "branch." br ".merge"))
     "^\\(refs/heads/\\)?\\(.+\\)$" 2)))

(defun vcgit--tracking-branch (&optional branch remote-name)
  "Return the full tracking branch name for the given BRANCH.
If BRANCH is not provided, use the current branch.
If REMOTE-NAME is provided, use it instead of fetching the remote name.
If the branch is not tracking a remote branch, return nil."
  (when-let* ((br (or branch (vc-git--current-branch)))
              (branch-merge (vcgit--branch-merge br))
              (branch-remote (or remote-name (vcgit--branch-remote br))))
    (unless (string= branch-remote ".")
      (concat branch-remote "/" branch-merge))))

(defun vcgit-format-header (header limit lines)
  (concat (propertize  (format"%s(%d):\n" header lines)
                       'face 'vc-dir-header)
          (propertize (buffer-substring (point-min)
                                        (save-excursion
                                          (goto-char (point-min))
                                          (if limit
                                              (forward-line limit)
                                            (goto-char (point-max)))
                                          (point)))
                      'keymap vc-git-log-view-mode-map)
          "\n"))

(defun vcgit-append-header(header)
  (let* ((hf (ewoc-get-hf vc-ewoc))
         (tail (cdr hf))
         (oldh (car hf)))
    (setq header (concat oldh header))
    (ewoc-set-hf vc-ewoc header tail)))

(defun vcgit-append-footer(f)
  (let* ((hf (ewoc-get-hf vc-ewoc))
         (footer (cdr hf))
         (header (car hf)))
    (setq footer (concat footer f))
    (ewoc-set-hf vc-ewoc header footer)))

(defun vcgit--log (type header-name limit &optional code)
  (let ((header "")
        (branch (vc-git--current-branch))
        (vcdir-buf (current-buffer))
        (inhibit-redisplay t)
        (buffer  (cond ((eq type 'incoming)
                        "*vc-incoming*")
                       ((eq type 'recent)
                        "*vc-change-log*")
                       ((eq type 'outgoing)
                        "*vc-outgoing*"))))
    (when (or (and (eq type 'recent) branch)
              (vcgit--tracking-branch))
      (save-window-excursion
        (save-excursion
          (cond
           ((eq type 'recent)
            (let ((vc-log-show-limit vcgit-log-commit-count))
              (vc-print-branch-log (vc-git--current-branch))))
           ((eq type 'incoming)
            (vc-log-incoming))
           ((eq type 'outgoing)
            (vc-log-outgoing)))
          (vc-run-delayed
            (let ((lines (count-lines (point-min) (point-max))))
              (unless (zerop lines)
                (font-lock-ensure)
                (setq header (vcgit-format-header header-name limit lines)))
              (kill-buffer buffer)
              (with-current-buffer vcdir-buf
                (vcgit-append-header header)
                (if (functionp code) (funcall code  lines) (eval code t))))))))))

(defun vcgit--log-incoming (buffer &optional remote-location)
  "Run git fetch async."
  (vc-setup-buffer buffer)
  (when (vcgit--tracking-branch)
    (when (eq this-command 'vc-log-incoming)
      (vc-git-command nil 0 nil "fetch"
                      (unless (string= remote-location "")
                        ;; `remote-location' is in format "repository/branch",
                        ;; so remove everything except a repository name.
                        (replace-regexp-in-string
                         "/.*" "" remote-location))))
    (apply #'vc-git-command buffer 'async nil
           `("log"
             "--no-color" "--graph" "--decorate" "--date=short"
             ,(format "--pretty=tformat:%s" (car vc-git-root-log-format))
             "--abbrev-commit"
             ,@(ensure-list vc-git-shortlog-switches)
             ,(concat "HEAD.." (if (string= remote-location "")
			                       "@{upstream}"
		                         remote-location))))))
(defun vcgit--outline-level ()
  (if (string-match-p vcgit-outline-level2-regexp
                      (match-string 0))
      2 1))
(defun vcgit-todo-open-file-at-line ()
  "Locate the filename using the `compilation-info' face and
 the line number using the `compilation-line-number' face,
 then open the file and jump to the specified line."
  (interactive)
  (let (filepath linenum)
    (save-excursion
      (goto-char (point-at-eol))
      (while (and (not filepath)
                  (re-search-backward "^.*::$" nil t))
        (let ((face (get-text-property (point) 'face)))
          (when (and (listp face) (memq 'compilation-info face))
            (setq filepath (string-trim-right (thing-at-point 'line t) "::\n"))))))
    (save-excursion
      (goto-char (point-at-bol))
      (let ((face (get-text-property (point) 'face)))
        (when (and (listp face) (memq 'compilation-line-number face))
          (setq linenum (string-to-number (thing-at-point 'line t))))))
    (when filepath
      (find-file filepath)
      (when linenum
        (goto-char (point-min))
        (forward-line (1- linenum))))))

(defvar-keymap  vc-todo-map
  "RET" #'vcgit-todo-open-file-at-line)

(defun vcgit-dir--todo ()
  (let* ((buffer (format "*vc-todo : %s*"
                         (expand-file-name default-directory)))
            (curbuf (current-buffer))
            (process (start-process "vc-todo" buffer
                                    "rg"  "--line-number"
                                    "TODO:|FIXME:" ".")))
           (set-process-sentinel
            process
            (lambda (process event)
              (when (string= event "finished\n")
                (with-current-buffer (process-buffer process)
                  (require 'compile)
                  (font-lock-add-keywords nil '(("\\<\\([0-9]+\\):" 1
                                                 'compilation-line-number prepend)
                                                ("^\\([^:]+\\)::$" 1
                                                 'compilation-info prepend)
                                                ("\\<\\(FIXME\\|TODO\\|Todo\\|HACK\\|todo\\):" 1
                                                 'vcgit-todo-face prepend)))
                  (goto-char (point-min))
                  (while (re-search-forward "^\\([^:]+?\\)$" nil t)
                     ;append :: after filename for match outline-regexp
                    (replace-match "\\&::"))
                  (font-lock-ensure)
                  (let ((results (buffer-string)))
                    (setq results
                          (propertize results 'keymap vc-todo-map))
                    (with-current-buffer curbuf
                      (vcgit-append-footer
                       (concat "\n" (propertize  "TODOs:" 'face 'vc-dir-header)
                               (concat "\n" results)))
                      (goto-char (point-min))
                      (outline-next-heading))
                    (kill-buffer))))))))


(defun vcgit--dir-unpulled (&optional code)
  (vcgit--log 'incoming "Unpulled" vcgit-log-commit-count code))
(defun vcgit--dir-unpushed (&optional code)
  (vcgit--log 'outgoing "Unpushed" vcgit-log-commit-count code))

(defun vcgit--dir-recent (unpushed-lines)
  (when (zerop unpushed-lines)
    (vcgit--log 'recent "Recent" vcgit-log-commit-count)))

(defun vcgit--dir-refresh ()
  (when (eq vc-dir-backend 'Git)
    (setq-local log-view-vc-backend vc-dir-backend)
    (setq-local log-view-vc-fileset `(,default-directory))
    (require 'log-view)
    ;;copy from (vc-git-log-view-mode)
    (setq-local log-view-file-re regexp-unmatchable)
    (setq-local log-view-per-file-logs nil)
    (setq-local log-view-message-re
                (if (not (memq vc-log-view-type '(long log-search with-diff)))
                    (cadr vc-git-root-log-format)
                  "^commit +\\([0-9a-z]+\\)"))
    (require 'outline)
    (setq-local outline-regexp vcgit-outline-regexp)
    (setq-local outline-level #'vcgit--outline-level)
    (setq outline-minor-mode-cycle t) ;tab
    (setq outline-minor-mode-cycle-filter 'bolp) ; tab only work when at bol
    (setq outline-minor-mode-use-buttons 'in-margins)

    (outline-minor-mode)
    ;; insert Unpulled/Unpushed to vc-dir
    (vcgit--dir-unpulled
     (vcgit--dir-unpushed
      'vcgit--dir-recent
      ))
    (vcgit-dir--todo)
    ;; move to the first outline header
    (outline-next-heading)))

;;;###autoload
(define-minor-mode vcgit-global-minor-mode
  "A minor mode for git-specific commands in `vc-dir-mode' buffers."
  :global t
  ;; The indicator for the mode line.
  :lighter nil
  (if vcgit-global-minor-mode
      (progn
        (with-eval-after-load 'vc-dir
          (define-key vc-dir-mode-map (kbd "M-n") #'outline-next-visible-heading)
          (define-key vc-dir-mode-map (kbd "M-p") #'outline-previous-visible-heading)
          (define-key vc-dir-mode-map (kbd "C-i") #'vc-diff))
        (with-eval-after-load 'log-view
          (define-key log-view-mode-map (kbd "C-i") #'log-view-diff))
        (advice-add 'vc-git-log-incoming :override 'vcgit--log-incoming)
        (add-hook 'vc-dir-refresh-hook #'vcgit--dir-refresh))
    (advice-remove 'vc-git-log-incoming  'vcgit--log-incoming)
    (remove-hook 'vc-dir-refresh-hook #'vcgit--dir-refresh)))


(provide 'vcgit)

;; Local Variables:
;; coding: utf-8
;; End:

;;; vcgit.el ends here.
