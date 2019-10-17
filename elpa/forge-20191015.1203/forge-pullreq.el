;;; forge-pullreq.el --- Pullreq support          -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Forge is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Forge is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Forge.  If not, see http://www.gnu.org/licenses.

;;; Code:

(require 'forge)
(require 'forge-post)
(require 'forge-topic)

;;; Classes

(defclass forge-pullreq (forge-topic)
  ((closql-table         :initform pullreq)
   (closql-primary-key   :initform id)
   (closql-order-by      :initform [(desc number)])
   (closql-foreign-key   :initform repository)
   (closql-class-prefix  :initform "forge-")
   (id                   :initarg :id)
   (repository           :initarg :repository)
   (number               :initarg :number)
   (state                :initarg :state)
   (author               :initarg :author)
   (title                :initarg :title)
   (created              :initarg :created)
   (updated              :initarg :updated)
   (closed               :initarg :closed)
   (merged               :initarg :merged)
   (unread-p             :initarg :unread-p :initform nil)
   (locked-p             :initarg :locked-p)
   (editable-p           :initarg :editable-p)
   (cross-repo-p         :initarg :cross-repo-p)
   (base-ref             :initarg :base-ref)
   (base-repo            :initarg :base-repo)
   (head-ref             :initarg :head-ref)
   (head-user            :initarg :head-user)
   (head-repo            :initarg :head-repo)
   (milestone            :initarg :milestone)
   (body                 :initarg :body)
   (assignees            :closql-table (pullreq-assignee assignee))
   (project-cards) ; projectsCards
   (commits)
   (edits) ; userContentEdits
   (labels               :closql-table (pullreq-label label))
   (participants)
   (posts                :closql-class forge-pullreq-post)
   (reactions)
   (review-requests      :closql-table (pullreq-review-request assignee))
   (reviews)
   (timeline)
   (marks                :closql-table (pullreq-mark mark))
   ;; We don't use these fields:
   ;; includesCreatedEdit (huh?),
   ;; lastEditedAt (same as updatedAt?),
   ;; publishedAt (same as createdAt?),
   ;; activeLockReason, additions, authorAssociation, (baseRefName), baseRefOid,
   ;; bodyHTML, bodyText, canBeRebased, changedFiles, closed, createdViaEmail,
   ;; databaseId, deletions, editor, (headRefName), headRefOid, mergeCommit,
   ;; mergeStateStatus, mergeable, merged, mergedBy, permalink,
   ;; potentialMergeCommit,, reactionGroups, resourcePath, revertResourcePath,
   ;; revertUrl, url, viewer{*}
   ))

(defclass forge-pullreq-post (forge-post)
  ((closql-table         :initform pullreq-post)
   (closql-primary-key   :initform id)
   (closql-order-by      :initform [(asc number)])
   (closql-foreign-key   :initform pullreq)
   (closql-class-prefix  :initform "forge-pullreq-")
   (id                   :initarg :id)
   (pullreq              :initarg :pullreq)
   (number               :initarg :number)
   (author               :initarg :author)
   (created              :initarg :created)
   (updated              :initarg :updated)
   (body                 :initarg :body)
   (edits)
   (reactions)
   ;; We don't use these fields:
   ;; includesCreatedEdit (huh?),
   ;; lastEditedAt (same as updatedAt?),
   ;; publishedAt (same as createdAt?),
   ;; pullRequest (same as issue),
   ;; repository (use .pullreq.project),
   ;; authorAssociation, bodyHTML, bodyText, createdViaEmail,
   ;; editor, id, reactionGroups, resourcePath, url, viewer{*}
   ))

;;; Query

(cl-defmethod forge-get-repository ((post forge-pullreq-post))
  (forge-get-repository (forge-get-pullreq post)))

(cl-defmethod forge-get-topic ((post forge-pullreq-post))
  (forge-get-pullreq post))

(cl-defmethod forge-get-pullreq ((repo forge-repository) number-or-id)
  (closql-get (forge-db)
              (if (numberp number-or-id)
                  (forge--object-id 'forge-pullreq repo number-or-id)
                number-or-id)
              'forge-pullreq))

(cl-defmethod forge-get-pullreq ((number integer))
  (when-let ((repo (forge-get-repository t)))
    (forge-get-pullreq repo number)))

(cl-defmethod forge-get-pullreq ((id string))
  (when-let ((repo (forge-get-repository t)))
    (forge-get-pullreq repo id)))

(cl-defmethod forge-get-pullreq ((post forge-pullreq-post))
  (closql-get (forge-db)
              (oref post pullreq)
              'forge-pullreq))

(cl-defmethod forge-ls-pullreqs ((repo forge-repository) &optional type)
  (forge-ls-topics repo 'forge-pullreq type))

;;; Utilities

(defun forge-read-pullreq (prompt &optional type)
  (when (eq type t)
    (setq type (if current-prefix-arg nil 'open)))
  (let* ((default (forge-pullreq-at-point))
         (repo    (forge-get-repository (or default t)))
         (format  (lambda (topic)
                    (format "%s  %s"
                            (oref topic number)
                            (oref topic title))))
         (choices (forge-ls-pullreqs repo type))
         (choice  (magit-completing-read
                   prompt
                   (mapcar format choices)
                   nil nil nil nil
                   (and default
                        (funcall format default)))))
    (and (string-match "\\`\\([0-9]+\\)" choice)
         (string-to-number (match-string 1 choice)))))

(defun forge--pullreq-branch (pullreq &optional confirm-reset)
  (with-slots (head-ref number cross-repo-p editable-p) pullreq
    (let ((branch head-ref)
          (branch-n (format "pr-%s" number)))
      (when (or (and cross-repo-p (not editable-p))
                ;; Such a branch name would be invalid.  If we encounter
                ;; this, then it means that we are dealing with a Gitlab
                ;; pull-request whose source branch has been deleted.
                (string-match-p ":" branch)
                ;; These are usually the target, not soruce, of a pr.
                (member branch '("master" "next" "maint")))
        (setq branch branch-n))
      (when (and confirm-reset (magit-branch-p branch))
        (when (magit-branch-p branch)
          (if (string-prefix-p "pr-" branch)
              (unless (y-or-n-p
                       (format "Branch %S already exists.  Reset it? " branch))
                (user-error "Abort"))
            (pcase (read-char-choice
                    (format "A branch named %S already exists.

This could be because you checked out this pull-request before,
in which case resetting might be the appropriate thing to do.

Or the contributor worked directly on their version of a branch
that also exists on the upstream, in which case you probably
should not reset because you would end up resetting your version.

Or you are trying to checkout a pull-request that you created
yourself, in which case you probably should not reset either.

  [r]eset existing %S branch
  [c]reate new \"pr-%s\" branch instead
  [a]bort" branch branch number) '(?r ?c ?a))
              (?r)
              (?c (setq branch branch-n)
                  (when (magit-branch-p branch)
                    (error "Oh no!  %S already exists too" branch)))
              (?a (user-error "Abort"))))
          (message "")))
      branch)))

(defun forge--pullreq-ref (pullreq)
  (let ((ref (format "refs/pullreqs/%s" (oref pullreq number))))
    (and (magit-rev-verify ref) ref)))

(cl-defmethod forge-get-url ((pullreq forge-pullreq))
  (forge--format pullreq 'pullreq-url-format))

;;; Sections

(defun forge-pullreq-at-point ()
  (or (magit-section-value-if 'pullreq)
      (when-let ((post (magit-section-value-if 'post)))
        (if (forge-pullreq-p post)
            post
          (forge-get-pullreq post)))))

(defvar forge-pullreqs-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-browse-thing] 'forge-browse-pullreqs)
    (define-key map [remap magit-visit-thing]  'forge-list-pullreqs)
    (define-key map (kbd "C-c C-n")            'forge-create-pullreq)
    map))

(defvar forge-pullreq-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-browse-thing] 'forge-browse-pullreq)
    (define-key map [remap magit-visit-thing]  'forge-visit-pullreq)
    map))

(defun forge-insert-pullreqs ()
  "Insert a list of mostly recent and/or open pull-requests.
Also see option `forge-topic-list-limit'."
  (when-let ((repo (forge-get-repository nil)))
    (unless (oref repo sparse-p)
      (forge-insert-topics "Pull requests"
                           (forge-ls-recent-topics repo 'pullreq)
                           (forge--topic-type-prefix repo 'pullreq)))))

(defun forge--insert-pullreq-commits (pullreq)
  (when-let ((ref (forge--pullreq-ref pullreq)))
    (magit-insert-section-body
      (cl-letf (((symbol-function #'magit-cancel-section) (lambda ())))
        (magit-insert-log (format "%s..%s" (oref pullreq base-ref) ref)
                          magit-buffer-log-args)
        (magit-make-margin-overlay nil t)))))

(cl-defmethod forge--insert-topic-contents :after ((pullreq forge-pullreq)
                                                   _width _prefix)
  (unless (oref pullreq merged)
    (magit-insert-heading)
    (forge--insert-pullreq-commits pullreq)))

(cl-defmethod forge--format-topic-id ((pullreq forge-pullreq) &optional prefix)
  (propertize (format "%s%s"
                      (or prefix (forge--topic-type-prefix pullreq))
                      (oref pullreq number))
              'font-lock-face (if (oref pullreq merged)
                                  'forge-topic-merged
                                'forge-topic-unmerged)))

(cl-defmethod forge--topic-type-prefix ((pullreq forge-pullreq))
  (if (forge--childp (forge-get-repository pullreq) 'forge-gitlab-repository)
      "!"
    "#"))

(defun forge-insert-assigned-pullreqs ()
  "Insert a list of open pull-requests that are assigned to you."
  (when-let ((repo (forge-get-repository nil)))
    (unless (oref repo sparse-p)
      (forge-insert-topics "Assigned pull requests"
                           (forge--ls-assigned-pullreqs repo)
                           (forge--topic-type-prefix repo 'pullreq)))))

(defun forge--ls-assigned-pullreqs (repo)
  (mapcar (lambda (row)
            (closql--remake-instance 'forge-pullreq (forge-db) row))
          (forge-sql
           [:select $i1 :from pullreq
            :join pullreq_assignee :on (= pullreq_assignee:pullreq pullreq:id)
            :join assignee         :on (= pullreq_assignee:id      assignee:id)
            :where (and (= pullreq:repository $s2)
                        (= assignee:login     $s3)
                        (isnull pullreq:closed))
            :order-by [(desc updated)]]
           (vconcat (closql--table-columns (forge-db) 'pullreq t))
           (oref repo id)
           (ghub--username repo))))

;;; _
(provide 'forge-pullreq)
;;; forge-pullreq.el ends here
