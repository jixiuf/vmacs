;;; lazy-dired-sort.el --- in Dired: press s then s, x, t or n to sort by Size, eXtension, Time or Name -*- lexical-binding: t; coding:utf-8 -*-

;; Copyright (C) 2002 -> Free Software Foundation, Inc.

;; Inspired by Francis J. Wright's dired-sort-menu.el
;; Author: Patrick Anderson
;; Version: 2.1a

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Install:
;; Copy this file to a directory in your load path.
;; Execute: M-x eval-buffer :so you don't have to restart.
;; Add the line: (require 'lazy-dired-sort) : to your .emacs


;;; Code:
(require 'dired)
(defvar lazy-dired-sort (make-sparse-keymap))

(define-key dired-mode-map "s" lazy-dired-sort)

(define-key lazy-dired-sort "s" (lambda () "sort by Size"
                                 (interactive) (dired-sort-other (concat dired-listing-switches "S"))
                                 (message "s-->size x-->ext t-->time n-->name d-->dir first")
                                 ))
(define-key lazy-dired-sort "x" (lambda () "sort by eXtension"
                                 (interactive) (dired-sort-other (concat dired-listing-switches "X"))
                                 (message "s-->size x-->ext t-->time n-->name d-->dir first")
                                 ))
(define-key lazy-dired-sort "t" (lambda () "sort by Time"
                                 (interactive) (dired-sort-other (concat dired-listing-switches "t"))
                                 (message "s-->size x-->ext t-->time n-->name d-->dir first")
                                 ))
(define-key lazy-dired-sort "n" (lambda () "sort by Name"
                                 (interactive) (dired-sort-other dired-listing-switches)
                                 (message "s-->size x-->ext t-->time n-->name d-->dir first")
                                 ))
(define-key lazy-dired-sort "d" (lambda () "sort by name grouping Dirs"
                                 (interactive) (dired-sort-other (concat dired-listing-switches " --group-directories-first"))
                                 (message "s-->size x-->ext t-->time n-->name d-->dir first")
                                 ))

(provide 'lazy-dired-sort)

;;; lazy-dired-sort.el ends here
