;;; orderless.el --- Completion style for matching regexps in any order  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: extensions
;; Package-Version: 20200905.2113
;; Package-Commit: e56eeef6e11909ccd62aa7250867dce803706d2c
;; Version: 0.5
;; Homepage: https://github.com/oantolin/orderless
;; Package-Requires: ((emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an `orderless' completion style that divides
;; the pattern into components (space-separated by default), and
;; matches candidates that match all of the components in any order.

;; Completion styles are used as entries in the variables
;; `completion-styles' and `completion-category-overrides', see their
;; documentation.

;; To use this completion style you can use the following minimal
;; configuration:

;; (setq completion-styles '(orderless))

;; You can customize the `orderless-component-separator' to decide how
;; the input pattern is split into component regexps.  The default
;; splits on spaces.  You might want to add hyphens and slashes, for
;; example, to ease completion of symbols and file paths,
;; respectively.

;; Each component can match in any one of several matching styles:
;; literally, as a regexp, as an initialism, in the flex style, or as
;; word prefixes.  It is easy to add new styles: they are functions
;; from strings to strings that map a component to a regexp to match
;; against.  The variable `orderless-matching-styles' lists the
;; matching styles to be used for components, by default it allows
;; regexp and initialism matching.

;;; Code:

(require 'cl-lib)

(defgroup orderless nil
  "Completion method that matches space-separated regexps in any order."
  :group 'completion)

(defface orderless-match-face-0
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#72a4ff")
    (((class color) (min-colors 88) (background light)) :foreground "#223fbf")
    (t :foreground "blue"))
  "Face for matches of components numbered 0 mod 4."
  :group 'orderless)

(defface orderless-match-face-1
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#ed92f8")
    (((class color) (min-colors 88) (background light)) :foreground "#8f0075")
    (t :foreground "magenta"))
  "Face for matches of components numbered 1 mod 4."
  :group 'orderless)

(defface orderless-match-face-2
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#90d800")
    (((class color) (min-colors 88) (background light)) :foreground "#145a00")
    (t :foreground "green"))
  "Face for matches of components numbered 2 mod 4."
  :group 'orderless)

(defface orderless-match-face-3
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#f0ce43")
    (((class color) (min-colors 88) (background light)) :foreground "#804000")
    (t :foreground "yellow"))
  "Face for matches of components numbered 3 mod 4."
  :group 'orderless)

(defcustom orderless-component-separator " +"
  "Component separators for orderless completion.
This can either be a string, which is passed to `split-string',
or a function of a single string argument."
  :type '(choice (const :tag "Spaces" " +")
                 (const :tag "Spaces, hyphen or slash" " +\\|[-/]")
                 (const :tag "Escapable space"
                        orderless-escapable-split-on-space)
                 (const :tag "Quotable spaces" split-string-and-unquote)
                 (regexp :tag "Custom regexp")
                 (function : tag "Custom function"))
  :group 'orderless)

(defvar-local orderless-transient-component-separator nil
  "Component separator regexp override.
This variabel, if non-nil, overrides `orderless-component-separator'.
It is meant to be set by commands that interactively change the
separator.  No such commands are provided with this package, but
this variable is meant to make writing them simple.  If you do
use this variable you are likely to want to reset it to nil after
every completion session, which can be achieved by adding the
function `orderless-remove-transient-configuration' to the
`minibuffer-exit-hook'.")

(defcustom orderless-match-faces
  [orderless-match-face-0
   orderless-match-face-1
   orderless-match-face-2
   orderless-match-face-3]
  "Vector of faces used (cyclically) for component matches."
  :type '(vector face)
  :group 'orderless)

(defcustom orderless-matching-styles
  '(orderless-regexp orderless-initialism)
  "List of component matching styles.
If this variable is nil, regexp matching is assumed.

A matching style is simply a function from strings to strings
that takes a component to a regexp to match against.  If the
resulting regexp has no capturing groups, the entire match is
highlighted, otherwise just the captured groups are.  Several are
provided with this package: try customizing this variable to see
a list of them."
  :type 'hook
  :options '(orderless-regexp
             orderless-literal
             orderless-initialism
             orderless-strict-initialism
             orderless-strict-leading-initialism
             orderless-strict-full-initialism
             orderless-prefixes
             orderless-flex)
  :group 'orderless)

(defcustom orderless-style-dispatchers nil
  "List of style dispatchers.
Style dispatchers are used to override the matching styles
based on the actual component and its place in the list of
components.  A style dispatcher is a function that takes a string
and two integers as arguments, it gets called with a component,
the 0-based index of the component and the total number of
components.  It can decide what matching styles to use for the
component and optionally replace the component with a different
string, or it can decline to handle the component leaving it for
future dispatchers.  For details see `orderless-dispatch'.

For example, a style dispatcher could arrange for the first
component to match as an initialism and subsequent components to
match as literals.  As another example, a style dispatcher could
arrange for a component starting with `?' to match the rest of
the component in the `orderless-flex' style.  For more
information on how this variable is used, see
`orderless-default-pattern-compiler'."
  :type 'hook
  :group 'orderless)

(defvar-local orderless-transient-matching-styles nil
  "Component matching styles override.
This variable, if non-nil, overrides `orderless-matching-styles'.
It is meant to be set by commands that interactively change the
matching style configuration.  No such commands are provided with
this package, but this variable is meant to make writing them
simple.  If you do use this variable you are likely to want to
reset it to nil after every completion session, which can be
achieved by adding the function
`orderless-remove-transient-configuration' to the
`minibuffer-exit-hook'.")

(defvar-local orderless-transient-style-dispatchers nil
  "Component style dispatchers override.
This variable, if non-nil, overrides `orderless-style-dispatchers'.
It is meant to be set by commands that interactively change the
matching style configuration.  No such commands are provided with
this package, but this variable is meant to make writing them
simple.  If you do use this variable you are likely to want to
reset it to nil after every completion session, which can be
achieved by adding the function
`orderless-remove-transient-configuration' to the
`minibuffer-exit-hook'.")

(defcustom orderless-pattern-compiler #'orderless-default-pattern-compiler
  "The `orderless' pattern compiler.
This should be a function that takes an input pattern and returns
a list of regexps that must all match a candidate in order for
the candidate to be considered a completion of the pattern.

The default pattern compiler is probably flexible enough for most
users.  See `orderless-default-pattern-compiler' for details.

The documentation for `orderless-matching-styles' is written
assuming the default pattern compiler is used, if you change the
pattern compiler it can, of course, do anything and need not
consult this variable at all."
  :type 'function
  :group 'orderless)

(defcustom orderless-smart-case t
  "Whether to use smart case.
If this variable is t, then case-sensitivity is decided as
follows: if any component contains upper case letters, the
matches are case sensitive; otherwise case-insensitive.  This
like the behavior of `isearch' when `search-upper-case' is
non-nil.

On the other hand, if this variable is nil, then case-sensitivity
is determined by the values of `completion-ignore-case',
`read-file-name-completion-ignore-case' and
`read-buffer-completion-ignore-case', as usual for completion."
  :type 'boolean
  :group 'orderless)

;;; Matching styles

(defalias 'orderless-regexp #'identity
  "Match a component as a regexp.
This is simply the identity function.")

(defalias 'orderless-literal #'regexp-quote
  "Match a component as a literal string.
This is simply `regexp-quote'.")

(defun orderless--separated-by (sep rxs &optional before after)
  "Return a regexp to match the rx-regexps RXS with SEP in between.
If BEFORE is specified, add it to the beginning of the rx
sequence.  If AFTER is specified, add it to the end of the rx
sequence."
  (rx-to-string
   `(seq
     ,(or before "")
     ,@(cl-loop for (sexp . more) on rxs
                collect `(group ,sexp)
                when more collect sep)
     ,(or after ""))))

(defun orderless-flex (component)
  "Match a component in flex style.
This means the characters in COMPONENT must occur in the
candidate in that order, but not necessarily consecutively."
  (orderless--separated-by '(zero-or-more nonl)
   (cl-loop for char across component collect char)))

(defun orderless-initialism (component)
  "Match a component as an initialism.
This means the characters in COMPONENT must occur in the
candidate, in that order, at the beginning of words."
  (orderless--separated-by '(zero-or-more nonl)
   (cl-loop for char across component collect `(seq word-start ,char))))

(defun orderless--strict-*-initialism (component &optional anchored)
  "Match a COMPONENT as a strict initialism, optionally ANCHORED.
The characters in COMPONENT must occur in the candidate in that
order at the beginning of subsequent words comprised of letters.
Only non-letters can be in between the words that start with the
initials.

If ANCHORED is `start' require that the first initial appear in
the first word of the candidate.  If ANCHORED is `both' require
that the first and last initials appear in the first and last
words of the candidate, respectively."
  (orderless--separated-by
   '(seq (zero-or-more word) word-end (zero-or-more (not alpha)))
   (cl-loop for char across component collect `(seq word-start ,char))
   (when anchored '(seq buffer-start (zero-or-more (not alpha))))
   (when (eq anchored 'both)
     '(seq (zero-or-more word) word-end (zero-or-more (not alpha)) eol))))

(defun orderless-strict-initialism (component)
  "Match a COMPONENT as a strict initialism.
This means the characters in COMPONENT must occur in the
candidate in that order at the beginning of subsequent words
comprised of letters.  Only non-letters can be in between the
words that start with the initials."
  (orderless--strict-*-initialism component))

(defun orderless-strict-leading-initialism (component)
  "Match a COMPONENT as a strict initialism, anchored at start.
See `orderless-strict-initialism'.  Additionally require that the
first initial appear in the first word of the candidate."
  (orderless--strict-*-initialism component 'start))

(defun orderless-strict-full-initialism (component)
  "Match a COMPONENT as a strict initialism, anchored at both ends.
See `orderless-strict-initialism'.  Additionally require that the
first and last initials appear in the first and last words of the
candidate, respectively."
  (orderless--strict-*-initialism component 'both))

(defun orderless-prefixes (component)
  "Match a component as multiple word prefixes.
The COMPONENT is split at word endings, and each piece must match
at a word boundary in the candidate.  This is similar to the
`partial-completion' completion style."
  (orderless--separated-by '(zero-or-more nonl)
   (cl-loop for prefix in (split-string component "\\>")
            collect `(seq word-boundary ,prefix))))

;;; Highlighting matches

(defun orderless--highlight (regexps string)
  "Propertize STRING to highlight a match of each of the REGEXPS."
  (cl-loop with n = (length orderless-match-faces)
           for regexp in regexps and i from 0
           when (string-match regexp string) do
           (cl-loop
            for (x y) on (or (cddr (match-data)) (match-data)) by #'cddr
            when x do
            (font-lock-prepend-text-property
             x y
             'face (aref orderless-match-faces (mod i n))
             string)))
  string)

(defun orderless-highlight-matches (regexps strings)
    "Highlight a match of each of the REGEXPS in each of the STRINGS.
Warning: only use this if you know all REGEXPs match all STRINGS!
For the user's convenience, if REGEXPS is a string, it is
converted to a list of regexps according to the value of
`orderless-matching-styles'."
    (when (stringp regexps)
      (setq regexps (funcall orderless-pattern-compiler regexps)))
    (cl-loop for original in strings
             for string = (copy-sequence original)
             collect (orderless--highlight regexps string)))

;;; Compiling patterns to lists of regexps

(defun orderless-escapable-split-on-space (string)
  "Split STRING on spaces, which can be escaped with backslash."
  (mapcar
   (lambda (piece) (replace-regexp-in-string (string 0) " " piece))
   (split-string (replace-regexp-in-string "\\\\ " (string 0) string) " ")))

(defun orderless-remove-transient-configuration ()
  "Remove all transient orderless configuration.
Meant to be added to `exit-minibuffer-hook'."
  (setq orderless-transient-matching-styles nil
        orderless-transient-component-separator nil))

(defun orderless-dispatch (dispatchers default string &rest args)
  "Run DISPATCHERS to compute matching styles for STRING.

A style dispatcher is a function that takes a string and possibly
some extra arguments.  It should either return (a) nil to
indicate the dispatcher will not handle the string, (b) a new
string to replace the current string and continue dispatch,
or (c) the matching styles to use and, if needed, a new string to
use in place of the current one (for example, a dispatcher can
decide which style to use based on a suffix of the string and
then it must also return the component stripped of the suffix).

More precisely, the return value of a style dispatcher can be of
one of the following forms:

- nil (to continue dispatching)

- a string (to replace the component and continue dispatching),

- a matching style or non-empty list of matching styles to
  return,

- a `cons' whose `car' is either as in the previous case or
  nil (to request returning the DEFAULT matching styles), and
  whose `cdr' is a string (to replace the current one).

This function tries all DISPATCHERS in sequence until one returns
a list of styles (passing any extra ARGS to every style
dispatcher).  When that happens it returns a `cons' of the list
of styles and the possibly updated STRING.  If none of the
DISPATCHERS returns a list of styles, the return value will use
DEFAULT as the list of styles."
  (cl-loop for dispatcher in dispatchers
           for result = (apply dispatcher string args)
           if (stringp result)
           do (setq string result result nil)
           else if (and (consp result) (null (car result)))
           do (setf (car result) default)
           else if (and (consp result) (stringp (cdr result)))
           do (setq string (cdr result) result (car result))
           when result return (cons result string)
           finally (return (cons default string))))

(defun orderless-default-pattern-compiler (pattern &optional styles dispatchers)
  "Build regexps to match the components of PATTERN.
Split PATTERN on `orderless-component-separator' and compute
matching styles for each component.  For each component the style
DISPATCHERS are run to determine the matching styles to be used;
they are called with arguments the component, the 0-based index
of the component and the total number of components.  If the
DISPATCHERS decline to handle the component, then the list of
matching STYLES is used.  See `orderless-dispatch' for details on
dispatchers.

The STYLES default to `orderless-matching-styles', and the
DISPATCHERS default to `orderless-dipatchers'.  Since nil gets you
the default, if want to no dispatchers to be run, use '(ignore)
as the value of DISPATCHERS.

The `orderless-transient-*' variables, when non-nil, override the
corresponding value among `orderless-component-separator', STYLES
and DISPATCHERS.

This function is the default for `orderless-pattern-compiler' and
might come in handy as a subroutine to implement other pattern
compilers."
  (unless styles (setq styles orderless-matching-styles))
  (setq styles (or orderless-transient-matching-styles styles))
  (unless dispatchers (setq dispatchers orderless-style-dispatchers))
  (setq dispatchers (or orderless-transient-style-dispatchers dispatchers))
  (cl-loop
   with splitter = (or orderless-transient-component-separator
                       orderless-component-separator)
   with components = (if (functionp splitter)
                         (funcall splitter pattern)
                       (split-string pattern splitter))
   with total = (length components)
   for component in components and index from 0
   for (newstyles . newcomp) = (orderless-dispatch
                                dispatchers styles component index total)
   collect
   (if (functionp newstyles)
       (funcall newstyles newcomp)
     (rx-to-string
      `(or
        ,@(cl-loop for style in newstyles
                   collect `(regexp ,(funcall style newcomp))))))))

;;; Completion style implementation

(defun orderless--prefix+pattern (string table pred)
  "Split STRING into prefix and pattern according to TABLE.
The predicate PRED is used to constrain the entries in TABLE."
  (let ((limit (car (completion-boundaries string table pred ""))))
    (cons (substring string 0 limit) (substring string limit))))

;;;###autoload
(defun orderless-filter (string table &optional pred)
  "Split STRING into components and find entries TABLE matching all.
The predicate PRED is used to constrain the entries in TABLE."
  (condition-case nil
      (save-match-data
        (pcase-let* ((`(,prefix . ,pattern)
                      (orderless--prefix+pattern string table pred))
                     (completion-regexp-list
                      (funcall orderless-pattern-compiler pattern))
                     (completion-ignore-case
                      (if orderless-smart-case
                          (cl-loop for regexp in completion-regexp-list
                                   always (isearch-no-upper-case-p regexp t))
                        completion-ignore-case)))
          (all-completions prefix table pred)))
    (invalid-regexp nil)))

;;;###autoload
(defun orderless-all-completions (string table pred _point)
  "Split STRING into components and find entries TABLE matching all.
The predicate PRED is used to constrain the entries in TABLE.  The
matching portions of each candidate are highlighted.
This function is part of the `orderless' completion style."
  (let ((completions (orderless-filter string table pred)))
    (when completions
      (pcase-let ((`(,prefix . ,pattern)
                   (orderless--prefix+pattern string table pred)))
        (nconc
         (orderless-highlight-matches pattern completions)
         (length prefix))))))

;;;###autoload
(defun orderless-try-completion (string table pred point &optional _metadata)
  "Complete STRING to unique matching entry in TABLE.
This uses `orderless-all-completions' to find matches for STRING
in TABLE among entries satisfying PRED.  If there is only one
match, it completes to that match.  If there are no matches, it
returns nil.  In any other case it \"completes\" STRING to
itself, without moving POINT.
This function is part of the `orderless' completion style."
  (let ((all (orderless-filter string table pred)))
    (cond
     ((null all) nil)
     ((null (cdr all))
      (let ((full (concat
                   (car (orderless--prefix+pattern string table pred))
                   (car all))))
        (cons full (length full))))
     (t (cons string point)))))

;;;###autoload
(add-to-list 'completion-styles-alist
             '(orderless
               orderless-try-completion orderless-all-completions
               "Completion of multiple components, in any order."))

;;; Ivy integration

(defvar ivy-regex)
(defvar ivy-highlight-functions-alist)

;;;###autoload
(defun orderless-ivy-re-builder (str)
  "Convert STR into regexps for use with ivy.
This function is for integration of orderless with ivy, use it as
a value in `ivy-re-builders-alist'."
  (or (mapcar (lambda (x) (cons x t))
              (funcall orderless-pattern-compiler str))
      ""))

(defun orderless-ivy-highlight (str)
  "Highlight a match in STR of each regexp in `ivy-regex'.
This function is for integration of orderless with ivy."
  (orderless--highlight (mapcar #'car ivy-regex) str) str)

;;;###autoload
(with-eval-after-load 'ivy
  (add-to-list 'ivy-highlight-functions-alist
               '(orderless-ivy-re-builder . orderless-ivy-highlight)))

(provide 'orderless)
;;; orderless.el ends here
