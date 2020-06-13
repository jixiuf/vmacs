;;; lsp-mode.el --- LSP mode                              -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: Vibhav Pant, Fangrui Song, Ivan Yonchovski
;; Keywords: languages
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (dash-functional "2.14.1") (f "0.20.0") (ht "2.0") (spinner "1.7.3") (markdown-mode "2.3") (lv "0"))
;; Version: 6.3.1

;; URL: https://github.com/emacs-lsp/lsp-mode
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

;; Emacs client/library for the Language Server Protocol

;;; Code:

(require 'bindat)
(require 'cl-generic)
(require 'cl-lib)
(require 'compile)
(require 'dash)
(require 'dash-functional)
(require 'em-glob)
(require 'ewoc)
(require 'f)
(require 'filenotify)
(require 'files)
(require 'ht)
(require 'imenu)
(require 'inline)
(require 'json)
(require 'lv)
(require 'markdown-mode)
(require 'network-stream)
(require 'pcase)
(require 'rx)
(require 's)
(require 'seq)
(require 'spinner)
(require 'subr-x)
(require 'tree-widget)
(require 'url-parse)
(require 'url-util)
(require 'widget)
(require 'xref)
(require 'minibuffer)
(require 'yasnippet nil t)
(require 'lsp-protocol)

(declare-function company-mode "ext:company")
(declare-function company-doc-buffer "ext:company")
(declare-function evil-set-command-property "ext:evil-common")
(declare-function projectile-project-root "ext:projectile")
(declare-function yas-expand-snippet "ext:yasnippet")

(defvar company-backends)
(defvar c-basic-offset)
(defvar yas-inhibit-overlay-modification-protection)
(defvar yas-indent-line)

(defconst lsp--message-type-face
  `((1 . ,compilation-error-face)
    (2 . ,compilation-warning-face)
    (3 . ,compilation-message-face)
    (4 . ,compilation-info-face)))

(defconst lsp--errors
  '((-32700 "Parse Error")
    (-32600 "Invalid Request")
    (-32601 "Method not Found")
    (-32602 "Invalid Parameters")
    (-32603 "Internal Error")
    (-32099 "Server Start Error")
    (-32000 "Server End Error")
    (-32002 "Server Not Initialized")
    (-32001 "Unknown Error Code")
    (-32800 "Request Cancelled"))
  "Alist of error codes to user friendly strings.")

(defconst lsp--completion-item-kind
  [nil
   "Text"
   "Method"
   "Function"
   "Constructor"
   "Field"
   "Variable"
   "Class"
   "Interface"
   "Module"
   "Property"
   "Unit"
   "Value"
   "Enum"
   "Keyword"
   "Snippet"
   "Color"
   "File"
   "Reference"
   "Folder"
   "EnumMember"
   "Constant"
   "Struct"
   "Event"
   "Operator"
   "TypeParameter"])

(defconst lsp--empty-ht (make-hash-table))

(define-obsolete-variable-alias 'lsp-print-io 'lsp-log-io "lsp-mode 6.1")

(defcustom lsp-log-io nil
  "If non-nil, log all messages to and from the language server to a *lsp-log* buffer."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-print-performance nil
  "If non-nil, print performance info in the logs."
  :group 'lsp-mode
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-log-max message-log-max
  "Maximum number of lines to keep in the log buffer.
If nil, disable message logging.  If t, log messages but don’t truncate
the buffer when it becomes large."
  :group 'lsp-mode
  :type '(choice (const :tag "Disable" nil)
                 (integer :tag "lines")
                 (const :tag "Unlimited" t))
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-io-messages-max t
  "Maximum number of messages that can be locked in a `lsp-io' buffer."
  :group 'lsp-mode
  :type '(choice (const :tag "Unlimited" t)
                 (integer :tag "Messages"))
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-keep-workspace-alive t
  "If non nil keep workspace alive when the last workspace buffer is closed."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-enable-snippet t
  "Enable/disable snippet completion support."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-enable-folding t
  "Enable/disable code folding support."
  :group 'lsp-mode
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-enable-semantic-highlighting nil
  "Enable/disable support for semantic highlighting as defined by
the Language Server Protocol 3.16."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-semantic-highlighting-warn-on-missing-face nil
  "When non-nil, this option will emit a warning any time a token
or modifier type returned by a language server has no face associated with it."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-semantic-tokens-apply-modifiers nil
  "Determines whether semantic highlighting should take token
modifiers into account."
  :group 'lsp-mode
  :type 'boolean)

(defface lsp-face-semhl-constant
  '((t :inherit font-lock-constant-face))
  "Face used for semantic highlighting scopes matching constant scopes."
  :group 'lsp-faces)

(defface lsp-face-semhl-variable
  '((t :inherit font-lock-variable-name-face))
  "Face used for semantic highlighting scopes matching variable.*,
unless overridden by a more specific face association."
  :group 'lsp-faces)

(defface lsp-face-semhl-function
  '((t :inherit font-lock-function-name-face))
  "Face used for semantic highlighting scopes matching entity.name.function.*,
unless overridden by a more specific face association."
  :group 'lsp-faces)

(defface lsp-face-semhl-method
  '((t :inherit lsp-face-semhl-function))
  "Face used for semantic highlighting scopes matching entity.name.function.method.*,
unless overridden by a more specific face association."
  :group 'lsp-faces)

(defface lsp-face-semhl-namespace
  '((t :inherit font-lock-type-face :weight bold))
  "Face used for semantic highlighting scopes matching entity.name.namespace.*,
unless overridden by a more specific face association."
  :group 'lsp-faces)

(defun lsp--semhl-scope-matchp (matchspec scopes)
  "Returns t iff there is an element M in MATCHSPEC s.t. every N in M
 is a prefix of, or identical to, one of the scopes contained in SCOPES"
  (-any? (lambda (or-matchspec)
           (-all? (lambda (and-matchspec)
                    (let ((re (format "^%s\\(\\..*\\)?$" (regexp-quote and-matchspec))))
                      (seq-some (lambda (s) (s-matches-p re s)) scopes)))
                  or-matchspec))
         matchspec))

(defface lsp-face-semhl-comment
  '((t (:inherit font-lock-comment-face)))
  "Face used for comments."
  :group 'lsp-faces)

(defface lsp-face-semhl-keyword
  '((t (:inherit font-lock-keyword-face)))
  "Face used for keywords."
  :group 'lsp-faces)

(defface lsp-face-semhl-string
  '((t (:inherit font-lock-string-face)))
  "Face used for keywords."
  :group 'lsp-faces)

(defface lsp-face-semhl-number
  '((t (:inherit font-lock-constant-face)))
  "Face used for numbers."
  :group 'lsp-faces)

(defface lsp-face-semhl-regexp
  '((t (:inherit font-lock-string-face :slant italic)))
  "Face used for regexps."
  :group 'lsp-faces)

(defface lsp-face-semhl-operator
  '((t (:inherit font-lock-function-name-face)))
  "Face used for operators."
  :group 'lsp-faces)

(defface lsp-face-semhl-namespace
  '((t (:inherit font-lock-keyword-face)))
  "Face used for namespaces."
  :group 'lsp-faces)

(defface lsp-face-semhl-type
  '((t (:inherit font-lock-type-face)))
  "Face used for types."
  :group 'lsp-faces)

(defface lsp-face-semhl-struct
  '((t (:inherit font-lock-type-face)))
  "Face used for structs."
  :group 'lsp-faces)

(defface lsp-face-semhl-class
  '((t (:inherit font-lock-type-face)))
  "Face used for classes."
  :group 'lsp-faces)

(defface lsp-face-semhl-interface
  '((t (:inherit font-lock-type-face)))
  "Face used for interfaces."
  :group 'lsp-faces)

(defface lsp-face-semhl-enum
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for enums."
  :group 'lsp-faces)

(defface lsp-face-semhl-type-parameter
  '((t (:inherit font-lock-type-face)))
  "Face used for type parameters."
  :group 'lsp-faces)

;; function face already defined, move here when support
;; for theia highlighting gets removed
(defface lsp-face-semhl-member
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for members."
  :group 'lsp-faces)

(defface lsp-face-semhl-property
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for properties."
  :group 'lsp-faces)

(defface lsp-face-semhl-macro
  '((t (:inherit font-lock-preprocessor-face)))
  "Face used for macros."
  :group 'lsp-faces)

(defface lsp-face-semhl-variable
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for variables."
  :group 'lsp-faces)

(defface lsp-face-semhl-parameter
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for parameters."
  :group 'lsp-faces)

(defface lsp-face-semhl-label
  '((t (:inherit font-lock-comment-face)))
  "Face used for labels."
  :group 'lsp-faces)

(defvar lsp-semantic-token-faces
  '(("comment" . lsp-face-semhl-comment)
    ("keyword" . lsp-face-semhl-keyword)
    ("string" . lsp-face-semhl-string)
    ("number" . lsp-face-semhl-number)
    ("regexp" . lsp-face-semhl-regexp)
    ("operator" . lsp-face-semhl-operator)
    ("namespace" . lsp-face-semhl-namespace)
    ("type" . lsp-face-semhl-type)
    ("struct" . lsp-face-semhl-struct)
    ("class" . lsp-face-semhl-class)
    ("interface" . lsp-face-semhl-interface)
    ("enum" . lsp-face-semhl-enum)
    ("typeParameter" . lsp-face-semhl-type-parameter)
    ("function" . lsp-face-semhl-function)
    ("member" . lsp-face-semhl-member)
    ("property" . lsp-face-semhl-property)
    ("macro" . lsp-face-semhl-macro)
    ("variable" . lsp-face-semhl-variable)
    ("parameter" . lsp-face-semhl-parameter)
    ("label" . lsp-face-semhl-label)
    ("enumConstant" . lsp-face-semhl-constant)
    ("dependent" . lsp-face-semhl-type)
    ("concept" . lsp-face-semhl-interface))
  "Faces to use for semantic highlighting.")

(defvar lsp-semantic-token-modifier-faces
  ;; TODO: add default definitions
  '(("declaration" . lsp-face-semhl-interface)
    ("readonly" . lsp-face-semhl-constant))
  "Faces to use for semantic token modifiers if
`lsp-semantic-tokens-apply-modifiers' is non-nil.")

(defvar lsp--semantic-tokens-idle-timer nil)

(defvar-local lsp--semantic-tokens-cache nil)

(defvar-local lsp--semantic-tokens-teardown nil)

(defvar-local lsp--semantic-tokens-use-ranged-requests nil)

(defcustom lsp-folding-range-limit nil
  "The maximum number of folding ranges to receive from the language server."
  :group 'lsp-mode
  :type '(choice (const :tag "No limit." nil)
                 (integer :tag "Number of lines."))
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-folding-line-folding-only nil
  "If non-nil, only fold complete lines."
  :group 'lsp-mode
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-client-packages
  '(ccls lsp-clients lsp-clojure lsp-csharp lsp-css lsp-dart lsp-elm
         lsp-erlang lsp-eslint lsp-fsharp lsp-gdscript lsp-go lsp-haskell lsp-haxe
         lsp-intelephense lsp-java lsp-json lsp-metals lsp-perl lsp-pwsh lsp-pyls
         lsp-python-ms lsp-rust lsp-serenata lsp-solargraph lsp-terraform lsp-verilog lsp-vetur
         lsp-vhdl lsp-xml lsp-yaml)
  "List of the clients to be automatically required."
  :group 'lsp-mode
  :type '(repeat symbol))

(defcustom lsp-progress-via-spinner t
  "If non-nil, display LSP $/progress reports via a spinner in the modeline."
  :group 'lsp-mode
  :type 'boolean)

(defvar-local lsp--cur-workspace nil)

(defvar-local lsp--cur-version 0)
(defvar-local lsp--virtual-buffer-connections nil)
(defvar-local lsp--virtual-buffer nil)
(defvar lsp--virtual-buffer-mappings (ht))

(defvar lsp--uri-file-prefix (pcase system-type
                               (`windows-nt "file:///")
                               (_ "file://"))
  "Prefix for a file-uri.")

(defvar-local lsp-buffer-uri nil
  "If set, return it instead of calculating it using `buffer-file-name'.")

(define-error 'lsp-error "Unknown lsp-mode error")
(define-error 'lsp-empty-response-error
  "Empty response from the language server" 'lsp-error)
(define-error 'lsp-timed-out-error
  "Timed out while waiting for a response from the language server" 'lsp-error)
(define-error 'lsp-capability-not-supported
  "Capability not supported by the language server" 'lsp-error)
(define-error 'lsp-file-scheme-not-supported
  "Unsupported file scheme" 'lsp-error)
(define-error 'lsp-client-already-exists-error
  "A client with this server-id already exists" 'lsp-error)
(define-error 'lsp-no-code-actions
  "No code actions" 'lsp-error)

(defcustom lsp-auto-guess-root nil
  "Automatically guess the project root using projectile/project.
Do *not* use this setting unless you are familiar with `lsp-mode'
internals and you are sure that all of your projects are
following `projectile'/`project.el' conventions."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-restart 'interactive
  "Defines how server exited event must be handled."
  :group 'lsp-mode
  :type '(choice (const interactive)
                 (const auto-restart)
                 (const ignore)))

(defcustom lsp-session-file (expand-file-name (locate-user-emacs-file ".lsp-session-v1"))
  "File where session information is stored."
  :group 'lsp-mode
  :type 'file)

(defcustom lsp-auto-configure t
  "Auto configure `lsp-mode'.
When set to t `lsp-mode' will auto-configure `company',
`flycheck', `flymake', `imenu', symbol highlighting, lenses,
links, and so on. For finer granularity you may use `lsp-enable-*' properties."
  :group 'lsp-mode
  :type 'boolean
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-disabled-clients nil
  "A list of disabled/blacklisted clients.
Each entry in the list can be either:
a symbol, the server-id for the LSP client, or
a cons pair (MAJOR-MODE . CLIENTS), where MAJOR-MODE is the major-mode,
and CLIENTS is either a client or a list of clients.

This option can also be used as a file or directory local variable to
disable a language server for individual files or directories/projects
respectively."
  :group 'lsp-mode
  :type 'list
  :safe 'listp
  :package-version '(lsp-mode . "6.1"))

(defvar lsp-clients (make-hash-table :test 'eql)
  "Hash table server-id -> client.
It contains all of the clients that are currently registered.")

(defvar lsp-enabled-clients nil
  "List of clients allowed to be used for projects.
When nil, all registered clients are considered candidates.")

(defvar lsp-last-id 0
  "Last request id.")

(defcustom lsp-before-initialize-hook nil
  "List of functions to be called before a Language Server has been initialized for a new workspace."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-after-initialize-hook nil
  "List of functions to be called after a Language Server has been initialized for a new workspace."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-before-open-hook nil
  "List of functions to be called before a new file with LSP support is opened."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-after-open-hook nil
  "List of functions to be called after a new file with LSP support is opened."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-enable-file-watchers t
  "If non-nil lsp-mode will watch the files in the workspace if
the server has requested that."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.1"))
;;;###autoload(put 'lsp-enable-file-watchers 'safe-local-variable #'booleanp)

(defcustom lsp-file-watch-ignored '(; SCM tools
                                    "[/\\\\]\\.git$"
                                    "[/\\\\]\\.hg$"
                                    "[/\\\\]\\.bzr$"
                                    "[/\\\\]_darcs$"
                                    "[/\\\\]\\.svn$"
                                    "[/\\\\]_FOSSIL_$"
                                    ;; IDE tools
                                    "[/\\\\]\\.idea$"
                                    "[/\\\\]\\.ensime_cache$"
                                    "[/\\\\]\\.eunit$"
                                    "[/\\\\]node_modules$"
                                    "[/\\\\]\\.fslckout$"
                                    "[/\\\\]\\.tox$"
                                    "[/\\\\]\\.stack-work$"
                                    "[/\\\\]\\.bloop$"
                                    "[/\\\\]\\.metals$"
                                    "[/\\\\]target$"
                                    "[/\\\\]\\.ccls-cache$"
                                    ;; Autotools output
                                    "[/\\\\]\\.deps$"
                                    "[/\\\\]build-aux$"
                                    "[/\\\\]autom4te.cache$"
                                    "[/\\\\]\\.reference$")
  "List of regexps matching directory paths which won't be monitored when creating file watches."
  :group 'lsp-mode
  :type '(repeat string)
  :package-version '(lsp-mode . "6.1"))

(defun lsp-file-watch-ignored ()
  lsp-file-watch-ignored)

;; Allow lsp-file-watch-ignored as a file or directory-local variable
(put 'lsp-file-watch-ignored 'safe-local-variable 'lsp--string-listp)

(defcustom lsp-after-uninitialized-functions nil
  "List of functions to be called after a Language Server has been uninitialized."
  :type 'hook
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.3"))

(defconst lsp--sync-none 0)
(defconst lsp--sync-full 1)
(defconst lsp--sync-incremental 2)

(defcustom lsp-debounce-full-sync-notifications t
  "If non-nil debounce full sync events.
This flag affects only server which do not support incremental update."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-debounce-full-sync-notifications-interval 1.0
  "Time to wait before sending full sync synchronization after buffer modification."
  :type 'float
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.1"))

(defvar lsp--stderr-index 0)

(defvar lsp--delayed-requests nil)
(defvar lsp--delay-timer nil)

(defgroup lsp-mode nil
  "Language Server Protocol client."
  :group 'tools
  :tag "Language Server")

(defgroup lsp-faces nil
  "Faces."
  :group 'lsp-mode
  :tag "Faces")

(defcustom lsp-document-sync-method nil
  "How to sync the document with the language server."
  :type '(choice (const :tag "Documents should not be synced at all." nil)
                 (const :tag "Documents are synced by always sending the full content of the document." lsp--sync-full)
                 (const :tag "Documents are synced by always sending incremental changes to the document." lsp--sync-incremental)
                 (const :tag "Use the method recommended by the language server." nil))
  :group 'lsp-mode)

(defcustom lsp-auto-execute-action t
  "Auto-execute single action."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-links t
  "If non-nil, all references to links in a file will be made clickable, if supported by the language server."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.1"))

(defcustom lsp-enable-imenu t
  "If non-nil, automatically enable `imenu' integration when server provides `textDocument/documentSymbol'."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-links-check-internal 0.1
  "The interval for updating document links."
  :group 'lsp-mode
  :type 'float)

(defcustom lsp-eldoc-enable-hover t
  "If non-nil, eldoc will display hover info when it is present."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-eldoc-render-all nil
  "Display all of the info returned by document/onHover.
If this is set to nil, `eldoc' will show only the symbol information."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-completion-at-point t
  "Enable `completion-at-point' integration."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-symbol-highlighting t
  "Highlight references of the symbol at point."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-xref t
  "Enable xref integration."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-indentation t
  "Indent regions using the file formatting functionality provided by the language server."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-on-type-formatting t
  "Enable `textDocument/onTypeFormatting' integration."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-enable-text-document-color t
  "Enable `textDocument/documentColor' integration."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-before-save-edits t
  "If non-nil, `lsp-mode' will apply edits suggested by the language server before saving a document."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-after-diagnostics-hook nil
  "Hooks to run after diagnostics are received.
Note: it runs only if the receiving buffer is open. Use
`lsp-diagnostics-updated-hook'if you want to be notified when
diagnostics have changed."
  :type 'hook
  :group 'lsp-mode)

(define-obsolete-variable-alias 'lsp-after-diagnostics-hook
  'lsp-diagnostics-updated-hook  "lsp-mode 6.4")

(defcustom lsp-diagnostics-updated-hook nil
  "Hooks to run after diagnostics are received."
  :type 'hook
  :group 'lsp-mode)

(define-obsolete-variable-alias 'lsp-workspace-folders-changed-hook
  'lsp-workspace-folders-changed-functions "lsp-mode 6.3")

(defcustom lsp-workspace-folders-changed-functions nil
  "Hooks to run after the folders has changed.
The hook will receive two parameters list of added and removed folders."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-eldoc-hook '(lsp-hover)
  "Hooks to run for eldoc."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-before-apply-edits-hook nil
  "Hooks to run before applying edits."
  :type 'hook
  :group 'lsp-mode)

(defgroup lsp-imenu nil
  "Imenu."
  :group 'lsp-mode
  :tag "Imenu")

(defcustom lsp-imenu-show-container-name t
  "Display the symbol's container name in an imenu entry."
  :type 'boolean
  :group 'lsp-imenu)

(defcustom lsp-imenu-container-name-separator "/"
  "Separator string to use to separate the container name from the symbol while displaying imenu entries."
  :type 'string
  :group 'lsp-imenu)

(defcustom lsp-imenu-sort-methods '(kind name)
  "How to sort the imenu items.

The value is a list of `kind' `name' or `position'.  Priorities
are determined by the index of the element."
  :type '(repeat (choice (const name)
                         (const position)
                         (const kind))))

;; vibhavp: Should we use a lower value (5)?
(defcustom lsp-response-timeout 10
  "Number of seconds to wait for a response from the language server before timing out."
  :type 'number
  :group 'lsp-mode)

(defcustom lsp-tcp-connection-timeout 2
  "The timeout for tcp connection in seconds."
  :type 'number
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.2"))

(defconst lsp--imenu-compare-function-alist
  (list (cons 'name #'lsp--imenu-compare-name)
        (cons 'kind #'lsp--imenu-compare-kind)
        (cons 'position #'lsp--imenu-compare-line-col))
  "An alist of (METHOD . FUNCTION).
METHOD is one of the symbols accepted by
`lsp-imenu-sort-methods'.

FUNCTION takes two hash tables representing DocumentSymbol.  It
returns a negative number, 0, or a positive number indicating
whether the first parameter is less than, equal to, or greater
than the second parameter.")

(defcustom lsp-diagnostic-package :auto
  "`lsp-mode' diagnostics auto-configuration."
  :type
  '(choice
    (const :tag "Pick flycheck if present and fallback to flymake" :auto)
    (const :tag "Pick flycheck" :flycheck)
    (const :tag "Pick flymake" :flymake)
    (const :tag "Use neither flymake nor lsp" :none)
    (const :tag "Prefer flymake" t)
    (const :tag "Prefer flycheck" nil))
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.3"))

(make-obsolete-variable 'lsp-prefer-flymake 'lsp-diagnostic-package "lsp-mode 6.2")

(defcustom lsp-prefer-capf nil
  "Prefer capf."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-completion-enable-additional-text-edit t
  "Whether or not to apply additional text edit when performing completion.

If set to non-nil, `lsp-mode' will apply additional text edits
from the server.  Otherwise, the additional text edits are
ignored."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.3.2"))

(defcustom lsp-completion-show-detail t
  "Whether or not to show detail of completion candidates."
  :type 'boolean
  :group 'lsp-mode)

(defcustom lsp-server-trace nil
  "Request tracing on the server side.
The actual trace output at each level depends on the language server in use.
Changes take effect only when a new session is started."
  :type '(choice (const :tag "Disabled" "off")
                 (const :tag "Messages only" "messages")
                 (const :tag "Verbose" "verbose")
                 (const :tag "Default (disabled)" nil))
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.1"))

(defvar-local lsp--flymake-report-fn nil)

(defvar lsp-language-id-configuration '((".*\\.vue$" . "vue")
                                        (".*\\.tsx$" . "typescriptreact")
                                        (".*\\.ts$" . "typescript")
                                        (".*\\.jsx$" . "javascriptreact")
                                        (".*\\.xml$" . "xml")
                                        (".*\\.hx$" . "haxe")
                                        (".*\\.lua$" . "lua")
                                        (".*\\.sql$" . "sql")
                                        (".*\\.html$" . "html")
                                        (ada-mode . "ada")
                                        (sql-mode . "sql")
                                        (vimrc-mode . "vim")
                                        (sh-mode . "shellscript")
                                        (scala-mode . "scala")
                                        (julia-mode . "julia")
                                        (clojure-mode . "clojure")
                                        (clojurec-mode . "clojure")
                                        (clojurescript-mode . "clojurescript")
                                        (java-mode . "java")
                                        (groovy-mode . "groovy")
                                        (python-mode . "python")
                                        (lsp--render-markdown . "markdown")
                                        (rust-mode . "rust")
                                        (rustic-mode . "rust")
                                        (kotlin-mode . "kotlin")
                                        (css-mode . "css")
                                        (less-mode . "less")
                                        (less-css-mode . "less")
                                        (lua-mode . "lua")
                                        (sass-mode . "sass")
                                        (scss-mode . "scss")
                                        (xml-mode . "xml")
                                        (c-mode . "c")
                                        (c++-mode . "cpp")
                                        (objc-mode . "objective-c")
                                        (web-mode . "html")
                                        (html-mode . "html")
                                        (sgml-mode . "html")
                                        (mhtml-mode . "html")
                                        (go-dot-mod-mode . "go")
                                        (go-mode . "go")
                                        (haskell-mode . "haskell")
                                        (hack-mode . "hack")
                                        (php-mode . "php")
                                        (powershell-mode . "powershell")
                                        (json-mode . "json")
                                        (jsonc-mode . "jsonc")
                                        (rjsx-mode . "javascript")
                                        (js2-mode . "javascript")
                                        (js-mode . "javascript")
                                        (typescript-mode . "typescript")
                                        (fsharp-mode . "fsharp")
                                        (reason-mode . "reason")
                                        (caml-mode . "ocaml")
                                        (tuareg-mode . "ocaml")
                                        (swift-mode . "swift")
                                        (elixir-mode . "elixir")
                                        (conf-javaprop-mode . "spring-boot-properties")
                                        (yaml-mode . "spring-boot-properties-yaml")
                                        (ruby-mode . "ruby")
                                        (enh-ruby-mode . "ruby")
                                        (fortran-mode . "fortran")
                                        (f90-mode . "fortran")
                                        (elm-mode . "elm")
                                        (dart-mode . "dart")
                                        (erlang-mode . "erlang")
                                        (dockerfile-mode . "dockerfile")
                                        (csharp-mode . "csharp")
                                        (plain-tex-mode . "plaintex")
                                        (latex-mode . "latex")
                                        (vhdl-mode . "vhdl")
                                        (verilog-mode . "verilog")
                                        (terraform-mode . "terraform")
                                        (ess-julia-mode . "julia")
                                        (ess-r-mode . "r")
                                        (crystal-mode . "crystal")
                                        (nim-mode . "nim")
                                        (dhall-mode . "dhall")
                                        (cmake-mode . "cmake")
                                        (purescript-mode . "purescript")
                                        (gdscript-mode . "gdscript")
                                        (perl-mode . "perl")
                                        (robot-mode . "robot"))
  "Language id configuration.")

(defvar lsp--last-active-workspaces nil
  "Keep track of last active workspace.
We want to try the last workspace first when jumping into a library
directory")

(defvar lsp-method-requirements
  '(("textDocument/callHierarchy" :capability "callHierarchyProvider")
    ("textDocument/codeAction" :capability "codeActionProvider")
    ("textDocument/codeLens" :capability "codeLensProvider")
    ("textDocument/completion" :capability "completionProvider")
    ("textDocument/declaration" :capability "declarationProvider")
    ("textDocument/definition" :capability "definitionProvider")
    ("textDocument/documentColor" :capability "colorProvider")
    ("textDocument/documentLink" :capability "documentLinkProvider")
    ("textDocument/documentHighlight" :capability "documentHighlightProvider")
    ("textDocument/documentSymbol" :capability "documentSymbolProvider")
    ("textDocument/foldingRange" :capability "foldingRangeProvider")
    ("textDocument/formatting" :capability "documentFormattingProvider")
    ("textDocument/hover" :capability "hoverProvider")
    ("textDocument/implementation" :capability "implementationProvider")
    ("textDocument/onTypeFormatting" :capability "documentOnTypeFormattingProvider")
    ("textDocument/prepareRename"
     :check-command (lambda (workspace)
                      (with-lsp-workspace workspace
                        (let ((table (or (lsp--capability "renameProvider")
                                         (-some-> (lsp--registered-capability "textDocument/rename")
                                           (lsp--registered-capability-options)))))
                          (and (hash-table-p table)
                               (gethash "prepareProvider" table))))))
    ("textDocument/rename" :capability "renameProvider")
    ("textDocument/rangeFormatting" :capability "documentRangeFormattingProvider")
    ("textDocument/references" :capability "referencesProvider")
    ("textDocument/selectionRange" :capability "selectionRangeProvider")
    ("textDocument/semanticTokens" :capability "semanticTokensProvider")
    ("textDocument/semanticTokensRangeProvider"
     :check-command (lambda (workspace)
                      (with-lsp-workspace workspace
                        (let ((table (lsp--capability "semanticTokensProvider")))
                          (and (hash-table-p table)
                               (gethash "rangeProvider" table))))))
    ("textDocument/signatureHelp" :capability "signatureHelpProvider")
    ("textDocument/typeDefinition" :capability "typeDefinitionProvider")
    ("workspace/executeCommand" :capability "executeCommandProvider")
    ("workspace/symbol" :capability "workspaceSymbolProvider"))

  "Contain method to requirements mapping.
It is used by send request functions to determine which server
must be used for handling a particular message.")

(defconst lsp--file-change-type
  `((created . 1)
    (changed . 2)
    (deleted . 3)))

(defvar lsp-window-body-width 40
  "Window body width when rendering doc.")

(defface lsp-face-highlight-textual
  '((t :inherit highlight))
  "Face used for textual occurrences of symbols."
  :group 'lsp-faces)

(defface lsp-face-highlight-read
  '((t :inherit highlight :underline t))
  "Face used for highlighting symbols being read."
  :group 'lsp-faces)

(defface lsp-face-highlight-write
  '((t :inherit highlight :weight bold))
  "Face used for highlighting symbols being written to."
  :group 'lsp-faces)

(defcustom lsp-lens-auto-enable nil
  "Auto lenses if server there is server support."
  :group 'lsp-mode
  :type 'boolean
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-lens-debounce-interval 0.2
  "Debounce interval for loading lenses."
  :group 'lsp-mode
  :type 'number)

(defcustom lsp-symbol-highlighting-skip-current nil
  "If non-nil skip current symbol when setting symbol highlights."
  :group 'lsp-mode
  :type 'boolean)

(defcustom lsp-file-watch-threshold 1000
  "Show warning if the files to watch are more than.
Set to nil to disable the warning."
  :type 'number
  :group 'lsp-mode)
;;;###autoload(put 'lsp-file-watch-threshold 'safe-local-variable (lambda (i) (or (numberp i) (not i))))

(defvar lsp-custom-markup-modes
  '((rust-mode "no_run" "rust,no_run" "rust,ignore" "rust,should_panic"))
  "Mode to uses with markdown code blocks.
They are added to `markdown-code-lang-modes'")

(defface lsp-lens-mouse-face
  '((t :height 0.8 :inherit link))
  "The face used for code lens overlays."
  :group 'lsp-faces)

(defface lsp-lens-face
  '((t :height 0.8 :inherit shadow))
  "The face used for code lens overlays."
  :group 'lsp-faces)

(defcustom lsp-signature-render-documentation t
  "Display signature documentation in `eldoc'."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-signature-auto-activate t
  "Auto activate signature when trigger char is pressed."
  :type 'boolean
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-signature-doc-lines 20
  "If number, limit the number of lines to show in the docs."
  :type 'number
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-signature-function 'lsp-lv-message
  "The function used for displaying signature info.
It will be called with one param - the signature info. When
called with nil the signature info must be cleared."
  :type 'function
  :group 'lsp-mode
  :package-version '(lsp-mode . "6.3"))

(defvar-local lsp--lens-overlays nil
  "Current lenses.")

(defvar-local lsp--lens-page nil
  "Pair of points which holds the last window location the lenses were loaded.")

(defvar-local lsp--lens-last-count nil
  "The number of lenses the last time they were rendered.")

(defvar lsp-lens-backends '(lsp-lens-backend)
  "Backends providing lenses.")

(defvar-local lsp--lens-refresh-timer nil
  "Refresh timer for the lenses.")

(defvar-local lsp--lens-data nil
  "Pair of points which holds the last window location the lenses were loaded.")

(defvar-local lsp--lens-backend-cache nil)

(defvar-local lsp--buffer-workspaces ()
  "List of the buffer workspaces.")

(defvar lsp--session nil
  "Contain the `lsp-session' for the current Emacs instance.")

(defvar lsp--tcp-port 10000)

(defvar-local lsp--document-symbols nil
  "The latest document symbols.")

(defvar-local lsp--document-selection-range-cache nil
  "The document selection cache.")

(defvar-local lsp--document-symbols-request-async nil
  "If non-nil, request document symbols asynchronously.")

(defvar-local lsp--document-symbols-tick -1
  "The value of `buffer-chars-modified-tick' when document
  symbols were last retrieved.")

(defvar-local lsp--have-document-highlights nil
  "Set to `t' on symbol highlighting, cleared on
`lsp--cleanup-highlights-if-needed'. Checking a separately
defined flag is substantially faster than unconditionally
calling `remove-overlays'.")

;; Buffer local variable for storing number of lines.
(defvar lsp--log-lines)

(defvar-local lsp--eldoc-saved-message nil)

(defvar lsp--on-change-timer nil)
(defvar lsp--on-idle-timer nil)

(defvar-local lsp--signature-last nil)
(defvar-local lsp--signature-last-index nil)
(defvar lsp--signature-last-buffer nil)

(defvar-local lsp--virtual-buffer-point-max nil)

(cl-defgeneric lsp-execute-command (server command arguments)
  "Ask SERVER to execute COMMAND with ARGUMENTS.")

(defun lsp-elt (sequence n)
  "Return Nth element of SEQUENCE or nil if N is out of range."
  (cond
   ((listp sequence) (elt sequence n))
   ((arrayp sequence)
    (and (> (length sequence) n) (aref sequence n)))
   (t (and (> (length sequence) n) (elt sequence n)))))

;; define seq-first and seq-rest for older emacs
(defun lsp-seq-first (sequence)
  "Return the first element of SEQUENCE."
  (lsp-elt sequence 0))

(defun lsp-seq-rest (sequence)
  "Return a sequence of the elements of SEQUENCE except the first one."
  (seq-drop sequence 1))

(defun lsp--string-listp (sequence)
  "Return t if all elements of SEQUENCE are strings, else nil."
  (not (seq-find (lambda (x) (not (stringp x))) sequence)))

(defun lsp--string-vector-p (candidate)
  "Returns true if CANDIDATE is a vector data structure and
every element of it is of type string, else nil."
  (and
   (vectorp candidate)
   (seq-every-p #'stringp candidate)))

(define-widget 'lsp-string-vector 'lazy
  "A vector of zero or more elements, every element of which is a string.
Appropriate for any language-specific `defcustom' that needs to
serialize as a JSON array of strings."
  :offset 4
  :tag "Vector"
  :type '(restricted-sexp
          :match-alternatives (lsp--string-vector-p)))

(defun lsp--info (format &rest args)
  "Display lsp info message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "LSP" 'face 'success) (apply #'format format args)))

(defun lsp--warn (format &rest args)
  "Display lsp warn message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "LSP" 'face 'warning) (apply #'format format args)))

(defun lsp--error (format &rest args)
  "Display lsp error message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "LSP" 'face 'error) (apply #'format format args)))

(defun lsp--eldoc-message (&optional msg)
  "Show MSG in eldoc."
  (setq lsp--eldoc-saved-message msg)
  (run-with-idle-timer 0 nil (lambda () (eldoc-message msg))))

(defun lsp-log (format &rest args)
  "Log message to the ’*lsp-log*’ buffer.

FORMAT and ARGS i the same as for `message'."
  (when lsp-log-max
    (let ((log-buffer (get-buffer "*lsp-log*"))
          (inhibit-read-only t))
      (unless log-buffer
        (setq log-buffer (get-buffer-create "*lsp-log*"))
        (with-current-buffer log-buffer
          (view-mode 1)
          (set (make-local-variable 'lsp--log-lines) 0)))
      (with-current-buffer log-buffer
        (save-excursion
          (let* ((message (apply 'format format args))
                 ;; Count newlines in message.
                 (newlines (1+ (cl-loop with start = 0
                                        for count from 0
                                        while (string-match "\n" message start)
                                        do (setq start (match-end 0))
                                        finally return count))))
            (goto-char (point-max))

            ;; in case the buffer is not empty insert before last \n to preserve
            ;; the point position(in case it is in the end)
            (if (eq (point) (point-min))
                (progn
                  (insert "\n")
                  (backward-char))
              (backward-char)
              (insert "\n"))
            (insert message)

            (setq lsp--log-lines (+ lsp--log-lines newlines))

            (when (and (integerp lsp-log-max) (> lsp--log-lines lsp-log-max))
              (let ((to-delete (- lsp--log-lines lsp-log-max)))
                (goto-char (point-min))
                (forward-line to-delete)
                (delete-region (point-min) (point))
                (setq lsp--log-lines lsp-log-max)))))))))

(defalias 'lsp-message 'lsp-log)

(defalias 'lsp-ht 'ht)

;; `file-local-name' was added in Emacs 26.1.
(defalias 'lsp-file-local-name
  (if (fboundp 'file-local-name)
      'file-local-name
    (lambda (file)
      "Return the local name component of FILE."
      (or (file-remote-p file 'localname) file))))

(defun lsp-f-canonical (file-name)
  "Return the canonical, without trailing slash FILE-NAME."
  (directory-file-name (expand-file-name file-name)))

(defalias 'lsp-canonical-file-name 'lsp-f-canonical)

(defun lsp-f-same? (path-a path-b)
  "Return t if PATH-A and PATH-B are references to same file.
Symlinks are not followed."
  (when (and (f-exists? path-a)
             (f-exists? path-b))
    (equal
     (lsp-f-canonical (directory-file-name (f-expand path-a)))
     (lsp-f-canonical (directory-file-name (f-expand path-b))))))

(defun lsp-f-ancestor-of? (path-a path-b)
  "Return t if PATH-A is ancestor of PATH-B.
Symlinks are not followed."
  (unless (lsp-f-same? path-a path-b)
    (s-prefix? (lsp-f-canonical path-a)
               (lsp-f-canonical path-b))))

(defun lsp--merge-results (results method)
  "Merge RESULTS by filtering the empty hash-tables and merging the lists.
METHOD is the executed method so the results could be merged
depending on it."
  (pcase (--map (if (vectorp it) (append it nil) it) (-filter 'identity results))
    (`() ())
    ;; only one result - simply return it
    (`(,fst) fst)
    ;; multiple results merge it based on strategy
    (results
     (pcase method
       ("textDocument/hover" (let ((results (seq-filter
                                             (-compose #'not #'hash-table-empty-p)
                                             results)))
                               (if (not (cdr results))
                                   (car results)
                                 (let ((merged (make-hash-table :test 'equal)))
                                   (seq-each
                                    (lambda (it)
                                      (let ((to-add (gethash "contents" it)))
                                        (puthash "contents"
                                                 (append
                                                  (if (and (sequencep to-add)
                                                           (not (stringp to-add)))
                                                      to-add
                                                    (list to-add))
                                                  (gethash "contents" merged))
                                                 merged)))
                                    results)
                                   merged))))
       ("textDocument/completion"
        (ht
         ;; any incomplete
         ("isIncomplete" (seq-some
                          (-andfn #'ht? (-partial 'gethash "isIncomplete"))
                          results))
         ("items" (apply 'append (--map (append (if (ht? it)
                                                    (gethash "items" it)
                                                  it)
                                                nil)
                                        results)))))
       (_ (apply 'append (seq-map (lambda (it)
                                    (if (seqp it)
                                        it
                                      (list it)))
                                  results)))))))
(defun lsp--spinner-start ()
  "Start spinner indication."
  (condition-case _err (spinner-start 'progress-bar-filled) (error)))

(defun lsp--propertize (str type)
  "Propertize STR as per TYPE."
  (propertize str 'face (alist-get type lsp--message-type-face)))

(defun lsp-workspaces ()
  "Return the lsp workspaces associated with the current project."
  (if lsp--cur-workspace (list lsp--cur-workspace) lsp--buffer-workspaces))

(defun lsp--completing-read (prompt collection transform-fn &optional predicate
                                    require-match initial-input
                                    hist def inherit-input-method)
  "Wrap `completing-read' to provide transformation function.

TRANSFORM-FN will be used to transform each of the items before displaying.

PROMPT COLLECTION PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
INHERIT-INPUT-METHOD will be proxied to `completing-read' without changes."
  (let* ((col (--map (cons (funcall transform-fn it) it) collection))
         (completion (completing-read prompt col
                                      predicate require-match initial-input hist
                                      def inherit-input-method)))
    (cdr (assoc completion col))))

;; A ‘lsp--client’ object describes the client-side behavior of a language
;; server.  It is used to start individual server processes, each of which is
;; represented by a ‘lsp--workspace’ object.  Client objects are normally
;; created using ‘lsp-define-stdio-client’ or ‘lsp-define-tcp-client’.  Each
;; workspace refers to exactly one client, but there can be multiple workspaces
;; for a single client.
(cl-defstruct lsp--client
  ;; ‘language-id’ is a function that receives a buffer as a single argument
  ;; and should return the language identifier for that buffer.  See
  ;; https://microsoft.github.io/language-server-protocol/specification#textdocumentitem
  ;; for a list of language identifiers.  Also consult the documentation for
  ;; the language server represented by this client to find out what language
  ;; identifiers it supports or expects.
  (language-id nil)

  ;; ‘add-on?’ when set to t the server will be started no matter whether there
  ;; is another server handling the same mode.
  (add-on? nil)
  ;; ‘new-connection’ is a function that should start a language server process
  ;; and return a cons (COMMAND-PROCESS . COMMUNICATION-PROCESS).
  ;; COMMAND-PROCESS must be a process object representing the server process
  ;; just started.  COMMUNICATION-PROCESS must be a process (including pipe and
  ;; network processes) that ‘lsp-mode’ uses to communicate with the language
  ;; server using the language server protocol.  COMMAND-PROCESS and
  ;; COMMUNICATION-PROCESS may be the same process; in that case
  ;; ‘new-connection’ may also return that process as a single
  ;; object. ‘new-connection’ is called with two arguments, FILTER and
  ;; SENTINEL.  FILTER should be used as process filter for
  ;; COMMUNICATION-PROCESS, and SENTINEL should be used as process sentinel for
  ;; COMMAND-PROCESS.
  (new-connection nil)

  ;; ‘ignore-regexps’ is a list of regexps.  When a data packet from the
  ;; language server matches any of these regexps, it will be ignored.  This is
  ;; intended for dealing with language servers that output non-protocol data.
  (ignore-regexps nil)

  ;; ‘ignore-messages’ is a list of regexps.  When a message from the language
  ;; server matches any of these regexps, it will be ignored.  This is useful
  ;; for filtering out unwanted messages; such as servers that send nonstandard
  ;; message types, or extraneous log messages.
  (ignore-messages nil)

  ;; ‘notification-handlers’ is a hash table mapping notification method names
  ;; (strings) to functions handling the respective notifications.  Upon
  ;; receiving a notification, ‘lsp-mode’ will call the associated handler
  ;; function passing two arguments, the ‘lsp--workspace’ object and the
  ;; deserialized notification parameters.
  (notification-handlers (make-hash-table :test 'equal))

  ;; ‘request-handlers’ is a hash table mapping request method names
  ;; (strings) to functions handling the respective notifications.  Upon
  ;; receiving a request, ‘lsp-mode’ will call the associated handler function
  ;; passing two arguments, the ‘lsp--workspace’ object and the deserialized
  ;; request parameters.
  (request-handlers (make-hash-table :test 'equal))

  ;; ‘response-handlers’ is a hash table mapping integral JSON-RPC request
  ;; identifiers for pending asynchronous requests to functions handling the
  ;; respective responses.  Upon receiving a response from the language server,
  ;; ‘lsp-mode’ will call the associated response handler function with a
  ;; single argument, the deserialized response parameters.
  (response-handlers (make-hash-table :test 'eql))

  ;; ‘prefix-function’ is called for getting the prefix for completion.
  ;; The function takes no parameter and returns a cons (start . end) representing
  ;; the start and end bounds of the prefix. If it's not set, the client uses a
  ;; default prefix function."
  (prefix-function nil)

  ;; Contains mapping of scheme to the function that is going to be used to load
  ;; the file.
  (uri-handlers (make-hash-table :test #'equal))

  ;; ‘action-handlers’ is a hash table mapping action to a handler function. It
  ;; can be used in `lsp-execute-code-action' to determine whether the action
  ;; current client is interested in executing the action instead of sending it
  ;; to the server.
  (action-handlers (make-hash-table :test 'equal))

  ;; major modes supported by the client.
  major-modes
  ;; Function that will be called to decide if this language client
  ;; should manage a particular buffer. The function will be passed
  ;; the file name and major mode to inform the decision. Setting
  ;; `activation-fn' will override `major-modes' and `remote?', if
  ;; present.
  activation-fn
  ;; Break the tie when major-mode is supported by multiple clients.
  (priority 0)
  ;; Unique identifier for representing the client object.
  server-id
  ;; defines whether the client supports multi root workspaces.
  multi-root
  ;; Initialization options or a function that returns initialization options.
  initialization-options
  ;; Provides support for registering LSP Server specific capabilities.
  custom-capabilities
  ;; Function which returns the folders that are considered to be not projects but library files.
  ;; The function accepts one parameter currently active workspace.
  ;; See: https://github.com/emacs-lsp/lsp-mode/issues/225.
  library-folders-fn
  ;; function which will be called when opening file in the workspace to perform
  ;; client specific initialization. The function accepts one parameter
  ;; currently active workspace.
  before-file-open-fn
  ;; Function which will be called right after a workspace has been initialized.
  initialized-fn
  ;; ‘remote?’ indicate whether the client can be used for LSP server over TRAMP.
  (remote? nil)

  ;; ‘completion-in-comments?’ t if the client supports completion in comments.
  (completion-in-comments? nil)

  ;; ‘path->uri-fn’ the function to use for path->uri conversion for the client.
  (path->uri-fn nil)

  ;; ‘uri->path-fn’ the function to use for uri->path conversion for the client.
  (uri->path-fn nil)
  ;; Function that returns an environment structure that will be used
  ;; to set some environment variables when starting the language
  ;; server process. These environment variables enable some
  ;; additional features in the language server. The environment
  ;; structure is an alist of the form (KEY . VALUE), where KEY is a
  ;; string (regularly in all caps), and VALUE may be a string, a
  ;; boolean, or a sequence of strings.
  environment-fn

  ;; ‘after-open-fn’ workspace after open specific hooks.
  (after-open-fn nil)

  ;; ‘async-request-handlers’ is a hash table mapping request method names
  ;; (strings) to functions handling the respective requests that may take
  ;; time to finish.  Upon receiving a request, ‘lsp-mode’ will call the
  ;; associated handler function passing three arguments, the ‘lsp--workspace’
  ;; object, the deserialized request parameters and the callback which accept
  ;; result as its parameter.
  (async-request-handlers (make-hash-table :test 'equal))
  download-server-fn
  download-in-progress?
  buffers)

(defvar lsp--already-widened nil)

(defmacro lsp-save-restriction-and-excursion (&rest form)
  (declare (indent 0) (debug t))
  `(if lsp--already-widened
       (save-excursion ,@form)
     (-let [lsp--already-widened t]
       (save-restriction
         (widen)
         (save-excursion ,@form)))))

;; from http://emacs.stackexchange.com/questions/8082/how-to-get-buffer-position-given-line-number-and-column-number
(defun lsp--line-character-to-point (line character)
  "Return the point for character CHARACTER on line LINE."
  (or (lsp-virtual-buffer-call :line/character->point line character)
      (let ((inhibit-field-text-motion t))
        (lsp-save-restriction-and-excursion
          (goto-char (point-min))
          (forward-line line)
          ;; server may send character position beyond the current line and we
          ;; should fallback to line end.
          (-let [line-end (line-end-position)]
            (if (> character (- line-end (point)))
                line-end
              (forward-char character)
              (point)))))))

(lsp-defun lsp--position-to-point ((&Position :line :character))
  "Convert `Position' object in PARAMS to a point."
  (lsp--line-character-to-point line character))

(lsp-defun lsp--range-to-region ((&Range :start :end))
  (cons (lsp--position-to-point start) (lsp--position-to-point end)))

(lsp-defun lsp--find-wrapping-range ((&SelectionRange :parent? :range))
  (-let* (((start . end) (lsp--range-to-region range)))
    (cond
     ((and
       (region-active-p)
       (<= start (region-beginning) end)
       (<= start (region-end) end)
       (or (not (= start (region-beginning)))
           (not (= end (region-end)))))
      (cons start end))
     ((and (<= start (point) end)
           (not (region-active-p)))
      (cons start end))
     (parent? (lsp--find-wrapping-range parent?)))))

(defun lsp--get-selection-range ()
  (or
   (-when-let ((cache . cache-tick) lsp--document-selection-range-cache)
     (when (= cache-tick (buffer-modified-tick)) cache))
   (let ((response (cl-first
                    (lsp-request
                     "textDocument/selectionRange"
                     (list :textDocument (lsp--text-document-identifier)
                           :positions (vector (lsp--cur-position)))))))
     (setq lsp--document-selection-range-cache
           (cons response (buffer-modified-tick)))
     response)))

(defun lsp-extend-selection ()
  "Extend selection."
  (interactive)
  (unless (lsp--capability "selectionRangeProvider")
    (signal 'lsp-capability-not-supported (list "selectionRangeProvider")))
  (-when-let ((start . end) (lsp--find-wrapping-range (lsp--get-selection-range)))
    (goto-char start)
    (set-mark (point))
    (goto-char end)
    (exchange-point-and-mark)))

(defun lsp-warn (message &rest args)
  "Display a warning message made from (`format-message' MESSAGE ARGS...).
This is equivalent to `display-warning', using `lsp-mode' as the type and
`:warning' as the level."
  (display-warning 'lsp-mode (apply #'format-message message args)))

(defun lsp--get-uri-handler (scheme)
  "Get uri handler for SCHEME in the current workspace."
  (--some (gethash scheme (lsp--client-uri-handlers (lsp--workspace-client it)))
          (or (lsp-workspaces) (lsp--session-workspaces (lsp-session)))))

(defun lsp--fix-path-casing (path)
  "On windows, downcases path because the windows file system is
case-insensitive.

On other systems, returns path without change."
  (if (eq system-type 'windows-nt) (downcase path) path))

(defun lsp--uri-to-path (uri)
  "Convert URI to a file path."
  (if-let (fn (->> (lsp-workspaces)
                   (-keep (-compose #'lsp--client-uri->path-fn #'lsp--workspace-client))
                   (cl-first)))
      (funcall fn uri)
    (lsp--uri-to-path-1 uri)))

(defun lsp-remap-path-if-needed (file-name)
  (-if-let ((virtual-buffer &as &plist :buffer) (gethash file-name lsp--virtual-buffer-mappings))
      (propertize (buffer-local-value 'buffer-file-name buffer)
                  'lsp-virtual-buffer virtual-buffer)
    file-name))

(defun lsp--uri-to-path-1 (uri)
  "Convert URI to a file path."
  (let* ((url (url-generic-parse-url (url-unhex-string uri)))
         (type (url-type url))
         (file (decode-coding-string (url-filename url)
                                     (or locale-coding-system 'utf-8)))
         (file-name (if (and type (not (string= type "file")))
                        (if-let ((handler (lsp--get-uri-handler type)))
                            (funcall handler uri)
                          uri)
                      ;; `url-generic-parse-url' is buggy on windows:
                      ;; https://github.com/emacs-lsp/lsp-mode/pull/265
                      (or (and (eq system-type 'windows-nt)
                               (eq (elt file 0) ?\/)
                               (substring file 1))
                          file))))
    (->> file-name
         (concat (-some #'lsp--workspace-host-root (lsp-workspaces)))
         (lsp--fix-path-casing)
         (lsp-remap-path-if-needed))))

(defun lsp--buffer-uri ()
  "Return URI of the current buffer."
  (or lsp-buffer-uri
      (plist-get lsp--virtual-buffer :buffer-uri)
      (lsp--path-to-uri
       (or buffer-file-name (buffer-file-name (buffer-base-buffer))))))

(defun lsp-register-client-capabilities (&rest _args)
  "Implemented only to make `company-lsp' happy.
DELETE when `lsp-mode.el' is deleted.")

(defconst lsp--url-path-allowed-chars
  (url--allowed-chars (append '(?/) url-unreserved-chars))
  "`url-unreserved-chars' with additional delim ?/.
This set of allowed chars is enough for hexifying local file paths.")

(defun lsp--path-to-uri-1 (path)
  (concat lsp--uri-file-prefix
          (--> path
               (expand-file-name it)
               (or (file-remote-p it 'localname t) it)
               (url-hexify-string it lsp--url-path-allowed-chars))))

(defun lsp--path-to-uri (path)
  "Convert PATH to a uri."
  (if-let (uri-fn (->> (lsp-workspaces)
                       (-keep (-compose #'lsp--client-path->uri-fn #'lsp--workspace-client))
                       (cl-first)))
      (funcall uri-fn path)
    (lsp--path-to-uri-1 path)))

(defun lsp--string-match-any (regex-list str)
  "Given a list of REGEX-LIST and STR return the first matching regex if any."
  (--first (string-match it str) regex-list))

(cl-defstruct lsp-watch
  (descriptors (make-hash-table :test 'equal))
  root-directory)

(defun lsp--folder-watch-callback (event callback watch)
  (let ((file-name (cl-caddr event))
        (event-type (cadr event)))
    (cond
     ((and (file-directory-p file-name)
           (equal 'created event-type)
           (not (lsp--string-match-any (lsp-file-watch-ignored) file-name)))

      (lsp-watch-root-folder (file-truename file-name) callback watch)

      ;; process the files that are already present in
      ;; the directory.
      (->> (directory-files-recursively file-name ".*" t)
           (seq-do (lambda (f)
                     (unless (file-directory-p f)
                       (funcall callback (list nil 'created f)))))))
     ((and (not (file-directory-p file-name))
           (memq event-type '(created deleted changed)))
      (funcall callback event)))))

(defun lsp--directory-files-recursively (dir regexp &optional include-directories)
  "Copy of `directory-files-recursively' but it skips `lsp-file-watch-ignored'."
  (let* ((result nil)
         (files nil)
         (dir (directory-file-name dir))
         ;; When DIR is "/", remote file names like "/method:" could
         ;; also be offered.  We shall suppress them.
         (tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (sort (file-name-all-completions "" dir)
                        'string<))
      (unless (member file '("./" "../"))
        (if (and (directory-name-p file)
                 (not (lsp--string-match-any (lsp-file-watch-ignored) (f-join dir (f-filename file)))))
            (let* ((leaf (substring file 0 (1- (length file))))
                   (full-file (concat dir "/" leaf)))
              ;; Don't follow symlinks to other directories.
              (unless (file-symlink-p full-file)
                (setq result
                      (nconc result (lsp--directory-files-recursively
                                     full-file regexp include-directories))))
              (when (and include-directories
                         (string-match regexp leaf))
                (setq result (nconc result (list full-file)))))
          (when (string-match regexp file)
            (push (concat dir "/" file) files)))))
    (nconc result (nreverse files))))

(defun lsp--ask-about-watching-big-repo (number-of-files dir)
  "Ask the user if they want to watch NUMBER-OF-FILES from a repository DIR.
This is useful when there is a lot of files in a repository, as
that may slow Emacs down. Returns t if the user wants to watch
the entire repository, nil otherwise."
  (prog1
      (yes-or-no-p
       (format
        "There are %s files in folder %s so watching the repo may slow Emacs down.
Do you want to watch all files in %s? "
        number-of-files
        dir
        dir))
    (lsp--info
     (concat "You can configure this warning with the `lsp-enable-file-watchers' "
             "and `lsp-file-watch-threshold' variables"))))

(defun lsp-watch-root-folder (dir callback &optional watch warn-big-repo?)
  "Create recursive file notification watch in DIR.
CALLBACK will be called when there are changes in any of
the monitored files. WATCHES is a hash table directory->file
notification handle which contains all of the watch that
already have been created."
  (let* ((dir (if (f-symlink? dir)
                  (file-truename dir)
                dir))
         (watch (or watch (make-lsp-watch :root-directory dir))))
    (lsp-log "Creating watch for %s" dir)
    (when (or
           (not warn-big-repo?)
           (not lsp-file-watch-threshold)
           (let ((number-of-files (length (lsp--directory-files-recursively  dir ".*" t))))
             (or
              (< number-of-files lsp-file-watch-threshold)
              (condition-case _err
                  (lsp--ask-about-watching-big-repo number-of-files dir)
                ('quit)))))
      (condition-case err
          (progn
            (puthash
             dir
             (file-notify-add-watch dir
                                    '(change)
                                    (lambda (event)
                                      (lsp--folder-watch-callback event callback watch)))
             (lsp-watch-descriptors watch))
            (seq-do
             (-rpartial #'lsp-watch-root-folder callback watch)
             (seq-filter (lambda (f)
                           (and (file-directory-p f)
                                (not (gethash (if (f-symlink? f)
                                                  (file-truename f)
                                                f)
                                              (lsp-watch-descriptors watch)))
                                (not (lsp--string-match-any (lsp-file-watch-ignored) f))
                                (not (-contains? '("." "..") (f-filename f)))))
                         (directory-files dir t))))
        (error (lsp-log "Failed to create a watch for %s: message" (error-message-string err)))
        (file-missing (lsp-log "Failed to create a watch for %s: message" (error-message-string err)))))
    watch))

(defun lsp-kill-watch (watch)
  "Delete WATCH."
  (-> watch lsp-watch-descriptors hash-table-values (-each #'file-notify-rm-watch))
  (ht-clear! (lsp-watch-descriptors watch)))

(defun lsp-json-bool (val)
  "Convert VAL to JSON boolean."
  (if val t :json-false))

(defmacro with-lsp-workspace (workspace &rest body)
  "Helper macro for invoking BODY in WORKSPACE context."
  (declare (debug (form body))
           (indent 1))
  `(let ((lsp--cur-workspace ,workspace)) ,@body))

(defmacro with-lsp-workspaces (workspaces &rest body)
  "Helper macro for invoking BODY against multiple WORKSPACES."
  (declare (debug (form body))
           (indent 1))
  `(let ((lsp--buffer-workspaces ,workspaces)) ,@body))

(defmacro lsp-foreach-workspace (&rest body)
  "Execute BODY for each of the current workspaces."
  (declare (debug (form body)))
  `(--map (with-lsp-workspace it ,@body) (lsp-workspaces)))

(defmacro when-lsp-workspace (workspace &rest body)
  "Helper macro for invoking BODY in WORKSPACE context if present."
  (declare (debug (form body))
           (indent 1))
  `(when-let (lsp--cur-workspace ,workspace) ,@body))

(defun lsp--window-show-message (_workspace params)
  "Send the server's messages to log.
PARAMS - the data sent from _WORKSPACE."
  (-let (((&hash "message" "type") params))
    (funcall (cl-case type
               (1 'lsp--error)
               (2 'lsp--warn)
               (t 'lsp--info))
             "%s"
             message)))

(defun lsp--window-log-message (workspace params)
  "Send the server's messages to log.
PARAMS - the data sent from WORKSPACE."
  (ignore
   (let* ((message (gethash "message" params))
          (client (lsp--workspace-client workspace)))
     (when (or (not client)
               (cl-notany (-rpartial #'string-match-p message)
                          (lsp--client-ignore-messages client)))
       (lsp-log "%s" (lsp--propertize message (gethash "type" params)))))))

(defun lsp--window-log-message-request (params)
  "Display a message request to the user and send the user's selection back to the server."
  (let* ((type (gethash "type" params))
         (message (lsp--propertize (gethash "message" params) type))
         (choices (seq-map (-partial 'gethash "title")
                           (gethash "actions" params))))
    (if choices
        (completing-read (concat message " ") (seq-into choices 'list) nil t)
      (lsp-log message))))

(defun lsp--on-progress (workspace params)
  "Callback for $/progress.
type ProgressToken = number | string;
interface ProgressParams<T> {
    token: ProgressToken;
    value: T;
}
PARAMS contains the progress data.
WORKSPACE is the workspace that contains the progress token."
  (let* ((token (gethash "token" params))
         (value (gethash "value" params))
         (kind (gethash "kind" value)))
    (pcase kind

      ("begin"
       (let* ((message (gethash "title" value))
              (cur (gethash "percentage" value nil))
              (reporter
               (if lsp-progress-via-spinner
                   (let* ((spinner-strings (alist-get 'progress-bar spinner-types))
                          ;; Set message as a tooltip for the spinner strings
                          (propertized-strings
                           (seq-map (lambda (string) (propertize string 'help-echo message))
                                    spinner-strings))
                          (spinner-type (vconcat propertized-strings)))
                     ;; The progress relates to the server as a whole,
                     ;; display it on all buffers.
                     (mapcar (lambda (buffer)
                               (with-current-buffer buffer
                                 (spinner-start spinner-type))
                               buffer)
                             (lsp--workspace-buffers workspace)))
                 (if cur
                     (make-progress-reporter message 0 100 cur)
                   ;; No percentage, just progress
                   (make-progress-reporter message nil nil)))))
         (lsp-workspace-set-work-done-token token reporter workspace)))

      ("report"
       (when-let ((reporter (lsp-workspace-get-work-done-token token workspace)))
         (when (not lsp-progress-via-spinner)
           (progress-reporter-update reporter (gethash "percentage" value nil)))))

      ("end"
       (when-let ((reporter (lsp-workspace-get-work-done-token token workspace)))
         (if lsp-progress-via-spinner
             (mapc (lambda (buffer)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (spinner-stop))))
                   reporter)
           (progress-reporter-done reporter))
         (lsp-workspace-rem-work-done-token token workspace))))))

(defun lsp-diagnostics (&optional current-workspace?)
  "Return the diagnostics from all workspaces."
  (or (pcase (if current-workspace?
                 (lsp-workspaces)
               (lsp--session-workspaces (lsp-session)))
        (`() ())
        (`(,workspace) (lsp--workspace-diagnostics workspace))
        (`,workspaces (let ((result (make-hash-table :test 'equal)))
                        (mapc (lambda (workspace)
                                (->> workspace
                                     (lsp--workspace-diagnostics)
                                     (maphash (lambda (file-name diagnostics)
                                                (puthash file-name
                                                         (append (gethash file-name result) diagnostics)
                                                         result)))))
                              workspaces)
                        result)))
      (ht)))


;; diagnostic modeline
(defun lsp--severity-code->severity (severity-code)
  (cl-case severity-code
    (1 'error)
    (2 'warning)
    (3 'info)
    (4 'hint)))

(defcustom lsp-diagnostics-modeline-scope :workspace
  "The scope "
  :group 'lsp-mode
  :type '(choice (const :tag "File" :file)
                 (const :tag "Project" :workspace)
                 (const :tag "All Projects" :global))
  :package-version '(lsp-mode . "6.3"))

(defun lsp--diagnostics-modeline-statistics ()
  "Calculate diagnostics statistics based on `lsp-diagnostics-modeline-scope'"
  (let ((diagnostics (cond
                      ((equal :file lsp-diagnostics-modeline-scope)
                       (lsp--get-buffer-diagnostics))
                      (t (->> (eq :workspace lsp-diagnostics-modeline-scope)
                              (lsp-diagnostics)
                              (ht-values)
                              (-flatten))))))
    (->> diagnostics
         (-group-by #'lsp:diagnostic-severity?)
         (-sort (-lambda ((left) (right))
                  (> right left)))
         (-map (-juxt (-compose #'lsp--severity-code->severity #'cl-first)
                      (-compose #'length #'cl-rest)))
         (-map (-lambda ((code count))
                 (propertize (format "%s" count)
                             'face (cl-case code
                                     ('error 'error)
                                     ('warning 'warning)
                                     ('info 'success)
                                     ('hint 'success)))))
         (s-join "/")
         (format "%s"))))

(define-minor-mode lsp-diagnostics-modeline-mode
  "Toggle diagnostics modeline."
  :group 'lsp-mode
  :global nil
  :lighter ""
  (let ((status '(t (:eval (concat " " (lsp--diagnostics-modeline-statistics) " ")))))
    (setq-local global-mode-string
                (cond ((and lsp-diagnostics-modeline-mode
                            (not (-contains? global-mode-string status)))
                       (cons status global-mode-string))
                      (t (remove status global-mode-string))))))



(defalias 'lsp--buffer-for-file (if (eq system-type 'windows-nt)
                                    #'find-buffer-visiting
                                  #'get-file-buffer))

(defun lsp--on-diagnostics (workspace params)
  "Callback for textDocument/publishDiagnostics.
interface PublishDiagnosticsParams {
    uri: string;
    diagnostics: Diagnostic[];
}
PARAMS contains the diagnostics data.
WORKSPACE is the workspace that contains the diagnostics."
  (let* ((lsp--virtual-buffer-mappings (ht))
         (file (lsp--uri-to-path (gethash "uri" params)))
         (diagnostics (gethash "diagnostics" params))
         (workspace-diagnostics (lsp--workspace-diagnostics workspace)))

    (if (seq-empty-p diagnostics)
        (remhash file workspace-diagnostics)
      (puthash file (append diagnostics nil) workspace-diagnostics))

    (run-hooks 'lsp-diagnostics-updated-hook)
    (lsp--idle-reschedule (current-buffer))))

(with-no-warnings
  (unless (version< emacs-version "26")
    (defun lsp--flymake-setup()
      "Setup flymake."
      (setq lsp--flymake-report-fn nil)
      (flymake-mode 1)
      (add-hook 'flymake-diagnostic-functions 'lsp--flymake-backend nil t)
      (add-hook 'lsp-after-diagnostics-hook 'lsp--flymake-after-diagnostics nil t))

    (defun lsp--flymake-after-diagnostics ()
      "Handler for `lsp-after-diagnostics-hook'"
      (cond
       ((and lsp--flymake-report-fn flymake-mode)
        (lsp--flymake-update-diagnostics))
       ((not flymake-mode)
        (setq lsp--flymake-report-fn nil))))

    (defun lsp--flymake-backend (report-fn &rest _args)
      "Flymake backend."
      (let ((first-run (null lsp--flymake-report-fn)))
        (setq lsp--flymake-report-fn report-fn)
        (when first-run
          (lsp--flymake-update-diagnostics))))

    (defun lsp--flymake-update-diagnostics ()
      "Report new diagnostics to flymake."
      (funcall lsp--flymake-report-fn
               (-some->> (lsp-diagnostics t)
                 (gethash (lsp--fix-path-casing buffer-file-name))
                 (--map (-let* (((&hash "message" "severity" "range") it)
                                ((start . end) (lsp--range-to-region range)))
                          (when (= start end)
                            (-let (((&hash "line" start-line "character") (gethash "start" range)))
                              (if-let ((region (and (fboundp 'flymake-diag-region)
                                                    (flymake-diag-region (current-buffer)
                                                                         (1+ start-line)
                                                                         character))))
                                  (setq start (car region)
                                        end (cdr region))
                                (lsp-save-restriction-and-excursion
                                  (goto-char (point-min))
                                  (-let (((&hash "line" end-line) (gethash "end" range)))
                                    (setq start (point-at-bol (1+ start-line))
                                          end (point-at-eol (1+ end-line))))))))
                          (and (fboundp 'flymake-make-diagnostic)
                               (flymake-make-diagnostic (current-buffer)
                                                        start
                                                        end
                                                        (cl-case severity
                                                          (1 :error)
                                                          (2 :warning)
                                                          (t :note))
                                                        message)))))
               ;; This :region keyword forces flymake to delete old diagnostics in
               ;; case the buffer hasn't changed since the last call to the report
               ;; function. See https://github.com/joaotavora/eglot/issues/159
               :region (cons (point-min) (point-max))))))

(defun lsp--ht-get (tbl &rest keys)
  "Get nested KEYS in TBL."
  (let ((val tbl))
    (while (and keys val)
      (setq val (ht-get val (cl-first keys)))
      (setq keys (cl-rest keys)))
    val))



;; textDocument/foldingRange support

(cl-defstruct lsp--folding-range beg end kind children orig-folding-range)

(defvar-local lsp--cached-folding-ranges nil)
(defvar-local lsp--cached-nested-folding-ranges nil)

(defun lsp--folding-range-width (range)
  (- (lsp--folding-range-end range)
     (lsp--folding-range-beg range)))

(defun lsp--get-folding-ranges ()
  "Get the folding ranges for the current buffer."
  (unless (eq (buffer-chars-modified-tick) (car lsp--cached-folding-ranges))
    (let* ((ranges (lsp-request "textDocument/foldingRange"
                                `(:textDocument ,(lsp--text-document-identifier))))
           (sorted-line-col-pairs (->> ranges
                                       (cl-mapcan (lambda (range)
                                                    (-let [(&hash "startLine" start-line
                                                                  "startCharacter" start-character
                                                                  "endLine" end-line
                                                                  "endCharacter" end-character)
                                                           range]
                                                      (list (cons start-line start-character)
                                                            (cons end-line end-character)))))
                                       (-sort #'lsp--line-col-comparator)))
           (line-col-to-point-map (lsp--convert-line-col-to-points-batch
                                   sorted-line-col-pairs)))
      (setq lsp--cached-folding-ranges
            (cons (buffer-chars-modified-tick)
                  (--> ranges
                       (seq-map (lambda (range)
                                  (-let [(&hash "startLine" start-line
                                                "startCharacter" start-character
                                                "endLine" end-line
                                                "endCharacter" end-character
                                                "kind" kind)
                                         range]
                                    (make-lsp--folding-range
                                     :beg (ht-get line-col-to-point-map
                                                  (cons start-line start-character))
                                     :end (ht-get line-col-to-point-map
                                                  (cons end-line end-character))
                                     :kind kind
                                     :orig-folding-range range)))
                                it)
                       (seq-filter (lambda (folding-range)
                                     (< (lsp--folding-range-beg folding-range)
                                        (lsp--folding-range-end folding-range)))
                                   it)
                       (seq-into it 'list)
                       (delete-dups it))))))
  (cdr lsp--cached-folding-ranges))

(defun lsp--get-nested-folding-ranges ()
  "Get a list of nested folding ranges for the current buffer."
  (-let [(tick . _) lsp--cached-folding-ranges]
    (if (and (eq tick (buffer-chars-modified-tick))
             lsp--cached-nested-folding-ranges)
        lsp--cached-nested-folding-ranges
      (setq lsp--cached-nested-folding-ranges
            (lsp--folding-range-build-trees (lsp--get-folding-ranges))))))

(defun lsp--folding-range-build-trees (ranges)
  (setq ranges (seq-sort #'lsp--range-before-p ranges))
  (let* ((dummy-node (make-lsp--folding-range
                      :beg most-negative-fixnum
                      :end most-positive-fixnum))
         (stack (list dummy-node)))
    (dolist (range ranges)
      (while (not (lsp--range-inside-p range (car stack)))
        (pop stack))
      (push range (lsp--folding-range-children (car stack)))
      (push range stack))
    (lsp--folding-range-children dummy-node)))

(defun lsp--range-inside-p (r1 r2)
  "Return non-nil if folding range R1 lies inside R2"
  (and (>= (lsp--folding-range-beg r1) (lsp--folding-range-beg r2))
       (<= (lsp--folding-range-end r1) (lsp--folding-range-end r2))))

(defun lsp--range-before-p (r1 r2)
  "Return non-nil if folding range R1 ends before R2"
  ;; Ensure r1 comes before r2
  (or (< (lsp--folding-range-beg r1)
         (lsp--folding-range-beg r2))
      ;; If beg(r1) == beg(r2) make sure r2 ends first
      (and (= (lsp--folding-range-beg r1)
              (lsp--folding-range-beg r2))
           (< (lsp--folding-range-end r2)
              (lsp--folding-range-end r1)))))

(defun lsp--point-inside-range-p (point range)
  "Return non-nil if POINT lies inside folding range RANGE."
  (and (>= point (lsp--folding-range-beg range))
       (<= point (lsp--folding-range-end range))))

(cl-defun lsp--get-current-innermost-folding-range (&optional (point (point)))
  "Return the innermost folding range POINT lies in."
  (let (inner)
    (seq-doseq (range (lsp--get-folding-ranges))
      (when (lsp--point-inside-range-p point range)
        (if inner
            (when (lsp--range-inside-p inner range)
              (setq inner range))
          (setq inner range))))
    inner))

(cl-defun lsp--get-current-outermost-folding-range (&optional (point (point)))
  "Return the outermost folding range POINT lies in."
  (let (outer width)
    (seq-doseq (range (lsp--get-folding-ranges))
      (when (lsp--point-inside-range-p point range)
        (setq width (lsp--folding-range-width range))
        (if outer
            (when (> width (car outer))
              (setq outer (cons width range)))
          (setq outer (cons width range)))))
    (cdr outer)))

(defun lsp--folding-range-at-point-bounds ()
  (if (and (or (lsp--capability "foldingRangeProvider")
               (lsp--registered-capability "textDocument/foldingRange"))
           lsp-enable-folding)
      (if-let ((range (lsp--get-current-innermost-folding-range)))
          (cons (lsp--folding-range-beg range)
                (lsp--folding-range-end range)))
    nil))
(put 'lsp--folding-range 'bounds-of-thing-at-point
     #'lsp--folding-range-at-point-bounds)

(defun lsp--get-nearest-folding-range (&optional backward)
  (let ((point (point))
        (found nil))
    (while (not
            (or found
                (if backward
                    (<= point (point-min))
                  (>= point (point-max)))))
      (if backward (cl-decf point) (cl-incf point))
      (setq found (lsp--get-current-innermost-folding-range point)))
    found))

(defun lsp--folding-range-at-point-forward-op (n)
  (when (and (or (lsp--capability "foldingRangeProvider")
                 (lsp--registered-capability "textDocument/foldingRange"))
             lsp-enable-folding
             (not (zerop n)))
    (cl-block break
      (dotimes (_ (abs n))
        (if-let (range (lsp--get-nearest-folding-range (< n 0)))
            (goto-char (if (< n 0)
                           (lsp--folding-range-beg range)
                         (lsp--folding-range-end range)))
          (cl-return-from break))))))
(put 'lsp--folding-range 'forward-op
     #'lsp--folding-range-at-point-forward-op)

(defun lsp--folding-range-at-point-beginning-op ()
  (goto-char (car (lsp--folding-range-at-point-bounds))))
(put 'lsp--folding-range 'beginning-op
     #'lsp--folding-range-at-point-beginning-op)

(defun lsp--folding-range-at-point-end-op ()
  (goto-char (cdr (lsp--folding-range-at-point-bounds))))
(put 'lsp--folding-range 'end-op
     #'lsp--folding-range-at-point-end-op)

(defun lsp--range-at-point-bounds ()
  (or (lsp--folding-range-at-point-bounds)
      (if-let ((range (and
                       (lsp--capability "hoverProvider")
                       (->> (lsp--text-document-position-params)
                            (lsp-request "textDocument/hover")
                            (gethash "range")))))
          (cons (lsp--position-to-point (gethash "start" range))
                (lsp--position-to-point (gethash "end" range)))
        nil)))

;; A more general purpose "thing", useful for applications like focus.el
(put 'lsp--range 'bounds-of-thing-at-point
     #'lsp--range-at-point-bounds)


;; lenses support

(defvar-local lsp--lens-modified? nil)

(defun lsp--lens-text-width (from to)
  "Measure the width of the text between FROM and TO.
Results are meaningful only if FROM and TO are on the same line."
  ;; `current-column' takes prettification into account
  (- (save-excursion (goto-char to) (current-column))
     (save-excursion (goto-char from) (current-column))))

(defun lsp--lens-update (ov)
  "Redraw quick-peek overlay OV."
  (let* ((offset (lsp--lens-text-width (save-excursion
                                         (beginning-of-visual-line)
                                         (point))
                                       (save-excursion
                                         (beginning-of-line-text)
                                         (point))))
         (str (concat (make-string offset ?\s)
                      (overlay-get ov 'lsp--lens-contents)
                      "\n")))
    (save-excursion
      (goto-char (overlay-start ov))
      (overlay-put ov 'before-string str)
      (overlay-put ov 'lsp-original str))))

(defun lsp--lens-overlay-ensure-at (pos)
  "Find or create a lens for the line at POS."
  (or (when-let ((ov (-first (lambda (ov) (lsp--lens-overlay-matches-pos ov pos)) lsp--lens-overlays)))
        (save-excursion
          (goto-char pos)
          (move-overlay ov (point-at-bol) (1+ (point-at-eol))))
        ov)
      (let* ((ov (save-excursion
                   (goto-char pos)
                   (make-overlay (point-at-bol) (1+ (point-at-eol))))))
        (overlay-put ov 'lsp-lens t)
        ov)))

(defun lsp--lens-show (str pos metadata)
  "Show STR in an inline window at POS."
  (let ((ov (lsp--lens-overlay-ensure-at pos)))
    (save-excursion
      (goto-char pos)
      (setf (overlay-get ov 'lsp--lens-contents) str)
      (setf (overlay-get ov 'lsp--metadata) metadata)
      (lsp--lens-update ov)
      ov)))

(defun lsp--lens-idle-function (&optional buffer)
  "Create idle function for buffer BUFFER."
  (when (and (or (not buffer) (eq (current-buffer) buffer))
             (not (equal (cons (window-start) (window-end)) lsp--lens-page)))
    (lsp--lens-schedule-refresh nil)))

(defun lsp--lens-overlay-matches-pos (ov pos)
  "Check if OV is a lens covering POS."
  (and (overlay-get ov 'lsp-lens)
       (<= (overlay-start ov) pos)
       (< pos (overlay-end ov))))

(defun lsp--lens-after-save ()
  "Handler for `after-save-hook' for lens mode."
  (lsp--lens-schedule-refresh t))

(defun lsp--lens-schedule-refresh (buffer-modified?)
  "Call each of the backend.
BUFFER-MODIFIED? determines whether the buffer is modified or not."
  (-some-> lsp--lens-refresh-timer cancel-timer)

  (setq lsp--lens-page (cons (window-start) (window-end)))
  (setq lsp--lens-refresh-timer
        (run-with-timer lsp-lens-debounce-interval
                        nil
                        #'lsp-lens-refresh
                        (or lsp--lens-modified? buffer-modified?)
                        (current-buffer))))

(defun lsp--lens-keymap (command)
  (-doto (make-sparse-keymap)
    (define-key [mouse-1] (lsp--lens-create-interactive-command command))))

(defun lsp--lens-create-interactive-command (command)
  (let ((server-id (->> (lsp-workspaces)
                        (cl-first)
                        (or lsp--cur-workspace)
                        (lsp--workspace-client)
                        (lsp--client-server-id))))
    (if (functionp (gethash "command" command))
        (gethash "command" command)
      (lambda ()
        (interactive)
        (lsp-execute-command server-id
                             (intern (gethash "command" command))
                             (gethash "arguments" command))))))

(defun lsp--lens-display (lenses)
  "Show LENSES."
  ;; rerender only if there are lenses which are not processed or if their count
  ;; has changed(e. g. delete lens should trigger redisplay).
  (setq lsp--lens-modified? nil)
  (when (or (--any? (not (gethash "processed" it)) lenses)
            (eq (length lenses) lsp--lens-last-count)
            (not lenses))
    (setq lsp--lens-last-count (length lenses))
    (let ((overlays
           (->> lenses
                (--filter (gethash "command" it))
                (--map (prog1 it (puthash "processed" t it)))
                (--group-by (lsp--ht-get it "range" "start" "line"))
                (-map
                 (-lambda ((_ . lenses))
                   (let* ((sorted (--sort (< (lsp--ht-get it "range" "start" "character")
                                             (lsp--ht-get other "range" "start" "character"))
                                          lenses))
                          (data (-map
                                 (-lambda ((lens &as &hash "command" (command &as &hash "title" "face")))
                                   (propertize
                                    title
                                    'face (or face 'lsp-lens-face)
                                    'action (lsp--lens-create-interactive-command command)
                                    'mouse-face 'lsp-lens-mouse-face
                                    'local-map (lsp--lens-keymap command)))
                                 sorted)))
                     (lsp--lens-show
                      (s-join (propertize "|" 'face 'lsp-lens-face) data)
                      (lsp--position-to-point (lsp--ht-get (cl-first sorted) "range" "start"))
                      data)))))))
      (--each lsp--lens-overlays
        (unless (-contains? overlays it)
          (delete-overlay it)))
      (setq lsp--lens-overlays overlays))))

(defun lsp-lens-refresh (buffer-modified? &optional buffer)
  "Refresh lenses using lenses backend.
BUFFER-MODIFIED? determines whether the buffer is modified or not."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (dolist (backend lsp-lens-backends)
          (funcall backend buffer-modified?
                   (lambda (lenses version)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (lsp--process-lenses backend lenses version))))))))))

(defun lsp--process-lenses (backend lenses version)
  "Process LENSES originated from BACKEND.
VERSION is the version of the file. The lenses has to be
refreshed only when all backends have reported for the same
version."
  (setq lsp--lens-data (or lsp--lens-data (make-hash-table)))
  (puthash backend (cons version (append lenses nil)) lsp--lens-data)

  (-let [backend-data (->> lsp--lens-data ht-values (-filter #'cl-rest))]
    (when (and
           (= (length lsp-lens-backends) (ht-size lsp--lens-data))
           (seq-every-p (-lambda ((version))
                          (or (not version) (eq version lsp--cur-version)))
                        backend-data))
      ;; display the data only when the backends have reported data for the
      ;; current version of the file
      (lsp--lens-display (-flatten (-map 'cl-rest backend-data)))))
  version)

(defun lsp-lens-show ()
  "Display lenses in the buffer."
  (interactive)
  (->> (lsp-request "textDocument/codeLens"
                    `(:textDocument (:uri
                                     ,(lsp--path-to-uri buffer-file-name))))
       (seq-map (lambda (it)
                  (if (gethash "command" it)
                      it
                    (lsp-request "codeLens/resolve" it))))
       lsp--lens-display))

(defun lsp-lens-hide ()
  "Delete all lenses."
  (interactive)
  (let ((scroll-preserve-screen-position t))
    (seq-do #'delete-overlay lsp--lens-overlays)
    (setq lsp--lens-overlays nil)))

(defun lsp--lens-backend-not-loaded? (lens)
  "Return t if LENS has to be loaded."
  (-let (((&hash "range" (&hash "start") "command" "pending") lens))
    (and (< (window-start) (lsp--position-to-point start) (window-end))
         (not command)
         (not pending))))

(defun lsp--lens-backend-present? (lens)
  "Return t if LENS has to be loaded."
  (-let (((&hash "range" (&hash "start") "command") lens))
    (or command
        (not (< (window-start) (lsp--position-to-point start) (window-end))))))

(defun lsp--lens-backend-fetch-missing (lenses callback file-version)
  "Fetch LENSES without command in for the current window.

TICK is the buffer modified tick. If it does not match
`buffer-modified-tick' at the time of receiving the updates the
updates must be discarded..
CALLBACK - the callback for the lenses.
FILE-VERSION - the version of the file."
  (seq-each
   (lambda (it)
     (with-lsp-workspace (gethash "workspace" it)
       (puthash "pending" t it)
       (remhash "workspace" it)
       (lsp-request-async "codeLens/resolve" it
                          (lambda (lens)
                            (remhash "pending" it)
                            (puthash "command" (gethash "command" lens) it)
                            (when (seq-every-p #'lsp--lens-backend-present? lenses)
                              (funcall callback lenses file-version)))
                          :mode 'tick)))
   (seq-filter #'lsp--lens-backend-not-loaded? lenses)))

(defun lsp-lens-backend (modified? callback)
  "Lenses backend using `textDocument/codeLens'.
MODIFIED? - t when buffer is modified since the last invocation.
CALLBACK - callback for the lenses."
  (when (lsp--find-workspaces-for "textDocument/codeLens")
    (if modified?
        (progn
          (setq lsp--lens-backend-cache nil)
          (lsp-request-async "textDocument/codeLens"
                             `(:textDocument (:uri ,(lsp--buffer-uri)))
                             (lambda (lenses)
                               (setq lsp--lens-backend-cache
                                     (seq-mapcat
                                      (-lambda ((workspace . workspace-lenses))
                                        ;; preserve the original workspace so we can later use it to resolve the lens
                                        (seq-do (-partial 'puthash "workspace" workspace) workspace-lenses)
                                        workspace-lenses)
                                      lenses))
                               (if (--every? (gethash "command" it) lsp--lens-backend-cache)
                                   (funcall callback lsp--lens-backend-cache lsp--cur-version)
                                 (lsp--lens-backend-fetch-missing lsp--lens-backend-cache callback lsp--cur-version)))
                             :error-handler #'ignore
                             :mode 'tick
                             :no-merge t
                             :cancel-token (concat (buffer-name (current-buffer)) "-lenses")))
      (if (-all? #'lsp--lens-backend-present? lsp--lens-backend-cache)
          (funcall callback lsp--lens-backend-cache lsp--cur-version)
        (lsp--lens-backend-fetch-missing lsp--lens-backend-cache callback lsp--cur-version)))))

(define-minor-mode lsp-lens-mode
  "Toggle code-lens overlays."
  :group 'lsp-mode
  :global nil
  :init-value nil
  :lighter "Lens"
  (cond
   (lsp-lens-mode
    (add-hook 'lsp-on-idle-hook #'lsp--lens-idle-function nil t)
    (add-hook 'lsp-on-change-hook (lambda () (lsp--lens-schedule-refresh t)) nil t)
    (add-hook 'after-save-hook (lambda () (lsp--lens-schedule-refresh t)) nil t)
    (add-hook 'before-revert-hook #'lsp-lens-hide nil t)
    (lsp-lens-refresh t))
   (t
    (lsp-lens-hide)
    (remove-hook 'lsp-on-idle-hook #'lsp--lens-idle-function t)
    (remove-hook 'lsp-on-change-hook (lambda () (lsp--lens-schedule-refresh nil)) t)
    (remove-hook 'after-save-hook (lambda () (lsp--lens-schedule-refresh t)) t)
    (remove-hook 'before-revert-hook #'lsp-lens-hide t)
    (setq lsp--lens-last-count nil)
    (setq lsp--lens-backend-cache nil))))


;; toggles

(defun lsp-toggle-trace-io ()
  "Toggle client-server protocol logging."
  (interactive)
  (setq lsp-print-io (not lsp-print-io))
  (lsp--info "Server logging %s." (if lsp-print-io "enabled" "disabled")))

(defun lsp-toggle-signature-auto-activate ()
  "Toggle signature auto activate."
  (interactive)
  (setq lsp-signature-auto-activate (not lsp-signature-auto-activate))
  (lsp--info "Signature autoactivate %s." (if lsp-signature-auto-activate "enabled" "disabled"))
  (lsp--update-signature-help-hook))

(defun lsp-toggle-on-type-formatting ()
  "Toggle on type formatting."
  (interactive)
  (setq lsp-enable-on-type-formatting (not lsp-enable-on-type-formatting))
  (lsp--info "On type formatting is %s." (if lsp-enable-on-type-formatting "enabled" "disabled"))
  (lsp--update-on-type-formatting-hook))

(defun lsp-toggle-symbol-highlight ()
  "Toggle symbol highlighting."
  (interactive)
  (setq lsp-enable-symbol-highlighting (not lsp-enable-symbol-highlighting))

  (cond
   ((and lsp-enable-symbol-highlighting
         (lsp-feature? "textDocument/documentHighlight"))
    (add-hook 'lsp-on-idle-hook #'lsp--document-highlight nil t)
    (lsp--info "Symbol highlighting enabled in current buffer."))
   ((not lsp-enable-symbol-highlighting)
    (remove-hook 'lsp-on-idle-hook #'lsp--document-highlight t)
    (lsp--remove-overlays 'lsp-highlight)
    (lsp--info "Symbol highlighting disabled in current buffer."))))


;; keybindings

(defmacro lsp-define-conditional-key (keymap key def cond &rest bindings)
  "In KEYMAP, define key sequence KEY as DEF conditionally.
This is like `define-key', except the definition disappears
whenever COND evaluates to nil.
BINDINGS is a list of (key def cond)."
  (declare (indent defun)
           (debug (form form form form &rest sexp)))
  (->> (cl-list* key def cond bindings)
       (-partition 3)
       (-map (-lambda ((key def cond))
               `(define-key ,keymap ,key
                  '(menu-item
                    ,(format "maybe-%s" def)
                    ,def
                    :filter (lambda (item)
                              (when (with-current-buffer (or (when (buffer-live-p lsp--describe-buffer)
                                                               lsp--describe-buffer)
                                                             (current-buffer))
                                      ,cond)
                                item))))))
       macroexp-progn))

(defvar lsp--describe-buffer nil)

(defun lsp-describe-buffer-bindings-advice (buffer &optional _prefix _menus)
  (setq lsp--describe-buffer buffer))

(advice-add 'describe-buffer-bindings
            :before
            #'lsp-describe-buffer-bindings-advice)

(defcustom lsp-keymap-prefix "s-l"
  "lsp-mode keymap prefix."
  :group 'lsp-mode
  :type 'string
  :package-version '(lsp-mode . "6.3"))

(defun lsp--prepend-prefix (mappings)
  (->> mappings
       (-partition 2)
       (-mapcat (-lambda ((key description))
                  (list (concat lsp-keymap-prefix " " key)
                        description)))))

(defvar lsp-command-map
  (-doto (make-sparse-keymap)
    (lsp-define-conditional-key
      ;; sessions
      "sr" lsp-workspace-restart (lsp-workspaces)
      "ss" lsp t
      "sq" lsp-workspace-shutdown (lsp-workspaces)
      "sd" lsp-describe-session t
      "sD" lsp-disconnect (lsp-workspaces)

      ;; formatting
      "==" lsp-format-buffer (or (lsp-feature? "textDocument/rangeFormatting")
                                 (lsp-feature? "textDocument/formatting"))
      "=r" lsp-format-region (lsp-feature? "textDocument/rangeFormatting")

      ;; folders
      "Fa" lsp-workspace-folders-add t
      "Fr" lsp-workspace-folders-remove t
      "Fb" lsp-workspace-blacklist-remove t

      ;; toggles
      "Tl" lsp-lens-mode (lsp-feature? "textDocument/codeLens")
      "TL" lsp-toggle-trace-io t
      "Th" lsp-toggle-symbol-highlight (lsp-feature? "textDocument/documentHighlight")
      "TS" lsp-ui-sideline-mode (featurep 'lsp-ui-sideline)
      "Td" lsp-ui-doc-mode (featurep 'lsp-ui-doc)
      "Ts" lsp-toggle-signature-auto-activate (lsp-feature? "textDocument/signatureHelp")
      "Tf" lsp-toggle-on-type-formatting (lsp-feature? "textDocument/onTypeFormatting")
      "TT" lsp-treemacs-sync-mode (featurep 'lsp-treemacs)

      ;; goto
      "gg" lsp-find-definition (lsp-feature? "textDocument/definition")
      "gr" lsp-find-references (lsp-feature? "textDocument/references")
      "gi" lsp-find-implementation (lsp-feature? "textDocument/implementation")
      "gt" lsp-find-type-definition (lsp-feature? "textDocument/typeDefinition")
      "gd" lsp-find-declaration (lsp-feature? "textDocument/declaration")
      "gh" lsp-treemacs-call-hierarchy (and (lsp-feature? "callHierarchy/incomingCalls")
                                            (fboundp 'lsp-treemacs-call-hierarchy))
      "ga" xref-find-apropos (lsp-feature? "workspace/symbol")
      "ge" lsp-treemacs-errors-list (fboundp 'lsp-treemacs-errors-list)

      ;; help
      "hh" lsp-describe-thing-at-point (lsp-feature? "textDocument/hover")
      "hs" lsp-signature-activate (lsp-feature? "textDocument/signatureHelp")
      "hg" lsp-ui-doc-glance (and (featurep 'lsp-ui-doc)
                                  (lsp-feature? "textDocument/hover"))

      ;; refactoring
      "rr" lsp-rename (lsp-feature? "textDocument/rename")
      "ro" lsp-organize-imports (lsp-feature? "textDocument/rename")


      ;; actions
      "aa" lsp-execute-code-action (lsp-feature? "textDocument/codeAction")
      "al" lsp-avy-lens (and lsp-lens-mode (featurep 'avy))
      "ah" lsp-document-highlight (lsp-feature? "textDocument/documentHighlight")

      ;; peeks
      "Gg" lsp-ui-peek-find-definitions (and (lsp-feature? "textDocument/definition")
                                             (fboundp 'lsp-ui-peek-find-definitions))
      "Gr" lsp-ui-peek-find-references (and (fboundp 'lsp-ui-peek-find-references)
                                            (lsp-feature? "textDocument/references"))
      "Gi" lsp-ui-peek-find-implementation (and (fboundp 'lsp-ui-peek-find-implementation)
                                                (lsp-feature? "textDocument/implementation"))
      "Gs" lsp-ui-peek-find-workspace-symbol (and (fboundp 'lsp-ui-peek-find-workspace-symbol)
                                                  (lsp-feature? "workspace/symbol")))))


;; which-key integration

(declare-function which-key-add-major-mode-key-based-replacements "ext:which-key")
(declare-function which-key-add-key-based-replacements "ext:which-key")

(defun lsp-enable-which-key-integration (&optional all-modes)
  "Adds descriptions for `lsp-mode-map' to `which-key-mode' for the current
active `major-mode', or for all major modes when ALL-MODES is t."
  (cl-flet ((which-key-fn (if all-modes
                              'which-key-add-key-based-replacements
                            (apply-partially 'which-key-add-major-mode-key-based-replacements major-mode))))
    (apply
     #'which-key-fn
     (lsp--prepend-prefix
      (list
       ""    "lsp"
       "s"   "sessions"
       "s s" "start server"
       "s r" "restart server"
       "s q" "shutdown server"
       "s d" "describe session"
       "s D" "disconnect"

       "F"   "folders"
       "F a" "add folder"
       "F r" "remove folder"
       "F b" "un-blacklist folder"

       "="   "formatting"
       "= r" "format region"
       "= =" "format buffer"

       "T"   "toggle"
       "T l" "toggle lenses"
       "T h" "toggle highlighting"
       "T L" "toggle log io"
       "T s" "toggle signature"
       "T S" "toggle sideline"
       "T d" "toggle documentation popup"
       "T p" "toggle signature help"
       "T f" "toggle on type formatting"
       "T T" "toggle treemacs integration"

       "g"   "goto"
       "g g" "find definitions"
       "g r" "find references"
       "g i" "find implementations"
       "g d" "find declarations"
       "g t" "find type definition"
       "g h" "call hierarchy"
       "g a" "find symbol in workspace"
       "g A" "find symbol in all workspaces"
       "g e" "show errors"

       "h"   "help"
       "h h" "describe symbol at point"
       "h s" "signature help"

       "r"   "refactor"
       "r r" "rename"
       "r o" "organize imports"

       "a"   "code actions"
       "a a" "code actions"
       "a l" "lens"
       "a h" "highlight symbol"

       "G"   "peek"
       "G g" "peek definitions"
       "G r" "peek references"
       "G i" "peek implementations"
       "G s" "peek workspace symbol")))))



(defvar lsp-mode-menu)

(defun lsp-mouse-click (event)
  (interactive "e")
  (let* ((ec (event-start event))
         (choice (x-popup-menu event lsp-mode-menu))
         (action (lookup-key lsp-mode-menu (apply 'vector choice))))

    (select-window (posn-window ec))

    (unless (and (region-active-p) (eq action 'lsp-execute-code-action))
      (goto-char (posn-point ec)))
    (run-with-idle-timer
     0.001 nil
     (lambda ()
       (cl-labels ((check (value) (not (null value))))
         (when choice
           (call-interactively action)))))))

(defvar lsp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<down-mouse-1>") #'lsp-find-definition-mouse)
    (define-key map (kbd "C-<mouse-1>") #'ignore)
    (define-key map (kbd "<mouse-3>") #'lsp-mouse-click)
    (define-key map (kbd "C-S-SPC") #'lsp-signature-activate)
    (when lsp-keymap-prefix
      (define-key map (kbd lsp-keymap-prefix) lsp-command-map))
    map)
  "Keymap for `lsp-mode'.")

(define-minor-mode lsp-mode ""
  nil nil nil
  :keymap lsp-mode-map
  :lighter (:eval (lsp-mode-line))
  :group 'lsp-mode)

(defvar lsp-mode-menu
  (easy-menu-create-menu
   nil
   '(["Go to definition" lsp-find-definition
      :active (lsp--find-workspaces-for "textDocument/definition")]
     ["Find references" lsp-find-references
      :active (lsp--find-workspaces-for "textDocument/references")]
     ["Find implementations" lsp-find-implementation
      :active (lsp--find-workspaces-for "textDocument/implementation")]
     ["Find declarations" lsp-find-declaration
      :active (lsp--find-workspaces-for "textDocument/declaration")]
     ["Go to type declaration" lsp-find-type-definition
      :active (lsp--find-workspaces-for "textDocument/typeDefinition")]
     "--"
     ["Describe" lsp-describe-thing-at-point]
     ["Code action" lsp-execute-code-action]
     ["Format" lsp-format-buffer]
     ["Highlight references" lsp-document-highlight]
     ["Rename" lsp-rename
      :active (or (lsp--capability "renameProvider")
                  (lsp--registered-capability "textDocument/rename"))]
     "--"
     ("Session"
      ["View logs" lsp-workspace-show-log]
      ["Describe" lsp-describe-session]
      ["Shutdown" lsp-shutdown-workspace]
      ["Restart" lsp-restart-workspace])
     ("Workspace Folders"
      ["Add" lsp-workspace-folders-add]
      ["Remove" lsp-workspace-folders-remove]
      ["Open" lsp-workspace-folders-open])
     ["Toggle Lenses" lsp-lens-mode]))
  "Menu for lsp-mode.")

(defun lsp-mode-line ()
  "Construct the mode line text."
  (if-let (workspaces (lsp-workspaces))
      (concat " LSP" (string-join (--map (format "[%s]" (lsp--workspace-print it))
                                         workspaces)))
    (concat " LSP" (propertize "[Disconnected]" 'face 'warning))))

(defalias 'make-lsp-client 'make-lsp--client)

(cl-defstruct lsp--registered-capability
  (id "")
  (method " ")
  (options nil))

;; A ‘lsp--workspace’ object represents exactly one language server process.
(cl-defstruct lsp--workspace
  ;; the `ewoc' object for displaying I/O to and from the server
  (ewoc nil)

  ;; ‘server-capabilities’ is a hash table of the language server capabilities.
  ;; It is the hash table representation of a LSP ServerCapabilities structure;
  ;; cf. https://microsoft.github.io/language-server-protocol/specification#initialize.
  (server-capabilities nil)

  ;; ‘registered-server-capabilities’ is a list of hash tables that represent
  ;; dynamically-registered Registration objects.  See
  ;; https://microsoft.github.io/language-server-protocol/specification#client_registerCapability.
  (registered-server-capabilities nil)

  ;; ‘root’ is a directory name or a directory file name for the workspace
  ;; root.  ‘lsp-mode’ passes this directory to the ‘initialize’ method of the
  ;; language server; see
  ;; https://microsoft.github.io/language-server-protocol/specification#initialize.
  (root nil)

  ;; ‘client’ is the ‘lsp--client’ object associated with this workspace.
  (client nil)

  ;; ‘host-root’ contains the host root info as derived from `file-remote-p'. It
  ;; used to derive the file path in `lsp--uri-to-path' when using tramp
  ;; connection.
  (host-root nil)

  ;; ‘proc’ is a process object; it may represent a regular process, a pipe, or
  ;; a network connection.  ‘lsp-mode’ communicates with ‘proc’ using the
  ;; language server protocol.  ‘proc’ corresponds to the COMMUNICATION-PROCESS
  ;; element of the return value of the client’s ‘get-root’ field, which see.
  (proc nil)

  ;; ‘proc’ is a process object; it must represent a regular process, not a
  ;; pipe or network process.  It represents the actual server process that
  ;; corresponds to this workspace.  ‘cmd-proc’ corresponds to the
  ;; COMMAND-PROCESS element of the return value of the client’s ‘get-root’
  ;; field, which see.
  (cmd-proc nil)

  ;; ‘buffers’ is a list of buffers associated with this workspace.
  (buffers nil)

  ;; if semantic highlighting is enabled, `semantic-highlighting-faces' contains
  ;; one face (or nil) for each token type supported by the language server.
  (semantic-highlighting-faces nil)

  ;; If semantic highlighting is enabled, `semantic-highlighting-modifier-faces'
  ;; contains one face (or nil) for each modifier type supported by the language
  ;; server
  (semantic-highlighting-modifier-faces nil)

  ;; Extra client capabilities provided by third-party packages using
  ;; `lsp-register-client-capabilities'. It's value is an alist of (PACKAGE-NAME
  ;; . CAPS), where PACKAGE-NAME is a symbol of the third-party package name,
  ;; and CAPS is either a plist of the client capabilities, or a function that
  ;; takes no argument and returns a plist of the client capabilities or nil.")
  (extra-client-capabilities nil)

  ;; Workspace status
  (status nil)

  ;; ‘metadata’ is a generic storage for workspace specific data. It is
  ;; accessed via `lsp-workspace-set-metadata' and `lsp-workspace-set-metadata'
  (metadata (make-hash-table :test 'equal))

  ;; contains all the file notification watches that have been created for the
  ;; current workspace in format filePath->file notification handle.
  (watches (make-hash-table :test 'equal))

  ;; list of workspace folders
  (workspace-folders nil)

  ;; ‘last-id’ the last request id for the current workspace.
  (last-id 0)

  ;; ‘status-string’ allows extensions to specify custom status string based on
  ;; the Language Server specific messages.
  (status-string nil)

  ;; ‘shutdown-action’ flag used to mark that workspace should not be restarted (e.g. it
  ;; was stopped).
  shutdown-action

  ;; ‘diagnostics’ a hashmap with workspace diagnostics.
  (diagnostics (make-hash-table :test 'equal))

  ;; contains all the workDone progress tokens that have been created
  ;; for the current workspace.
  (work-done-tokens (make-hash-table :test 'equal)))


(cl-defstruct lsp-session
  ;; contains the folders that are part of the current session
  folders
  ;; contains the folders that must not be imported in the current workspace.
  folders-blacklist
  ;; contains the list of folders that must be imported in a project in case of
  ;; multi root LSP server.
  (server-id->folders (make-hash-table :test 'equal))
  ;; folder to list of the servers that are associated with the folder.
  (folder->servers (make-hash-table :test 'equal))
  ;; ‘metadata’ is a generic storage for workspace specific data. It is
  ;; accessed via `lsp-workspace-set-metadata' and `lsp-workspace-set-metadata'
  (metadata (make-hash-table :test 'equal)))

(defun lsp-workspace-status (status-string &optional workspace)
  "Set current workspace status to STATUS-STRING.
If WORKSPACE is not specified defaults to lsp--cur-workspace."
  (let ((status-string (when status-string (replace-regexp-in-string "%" "%%" status-string))))
    (setf (lsp--workspace-status-string (or workspace lsp--cur-workspace)) status-string)))

(defun lsp-session-set-metadata (key value &optional _workspace)
  "Associate KEY with VALUE in the WORKSPACE metadata.
If WORKSPACE is not provided current workspace will be used."
  (puthash key value (lsp-session-metadata (lsp-session))))

(defalias 'lsp-workspace-set-metadata 'lsp-session-set-metadata)

(defun lsp-session-get-metadata (key &optional _workspace)
  "Lookup KEY in WORKSPACE metadata.
If WORKSPACE is not provided current workspace will be used."
  (gethash key (lsp-session-metadata (lsp-session))))

(defalias 'lsp-workspace-get-metadata 'lsp-session-get-metadata)

(defun lsp-workspace-set-work-done-token (token value &optional workspace)
  "Associate TOKEN with VALUE in the WORKSPACE work-done-tokens.
If WORKSPACE is not provided current workspace will be used."
  (puthash token value
           (lsp--workspace-work-done-tokens (or workspace lsp--cur-workspace))))

(defun lsp-workspace-get-work-done-token (token &optional workspace)
  "Lookup TOKEN in the WORKSPACE work-done-tokens.
If WORKSPACE is not provided current workspace will be used."
  (gethash token
           (lsp--workspace-work-done-tokens (or workspace lsp--cur-workspace))))

(defun lsp-workspace-rem-work-done-token (token &optional workspace)
  "Remove TOKEN from the WORKSPACE work-done-tokens.
If WORKSPACE is not provided current workspace will be used."
  (remhash token
           (lsp--workspace-work-done-tokens (or workspace lsp--cur-workspace))))


(defun lsp--make-notification (method &optional params)
  "Create notification body for method METHOD and parameters PARAMS."
  (list :jsonrpc "2.0" :method method :params params))

(defalias 'lsp--make-request 'lsp--make-notification)
(defalias 'lsp-make-request 'lsp--make-notification)

(defun lsp--make-response (id result)
  "Create response for REQUEST with RESULT."
  `(:jsonrpc "2.0" :id ,id :result ,result))

(defun lsp-make-notification (method &optional params)
  "Create notification body for method METHOD and parameters PARAMS."
  (lsp--make-notification method params))

(defmacro lsp--json-serialize (params)
  (if (progn
        (require 'json)
        (fboundp 'json-serialize))
      `(json-serialize ,params
                       :null-object nil
                       :false-object :json-false)
    `(let ((json-false :json-false))
       (json-encode ,params))))

(defun lsp--make-message (params)
  "Create a LSP message from PARAMS, after encoding it to a JSON string."
  (let ((body (lsp--json-serialize params) ))
    (concat "Content-Length: "
            (number-to-string (1+ (string-bytes body)))
            "\r\n\r\n"
            body
            "\n")))

(cl-defstruct lsp--log-entry timestamp process-time type method id body)

(defun lsp--make-log-entry (method id body type &optional process-time)
  "Create an outgoing log object from BODY with method METHOD and id ID.
If ID is non-nil, then the body is assumed to be a notification.
TYPE can either be 'incoming or 'outgoing"
  (cl-assert (memq type '(incoming-req outgoing-req incoming-notif
                                       outgoing-notif incoming-resp
                                       outgoing-resp)))
  (make-lsp--log-entry
   :timestamp (format-time-string "%I:%M:%S %p")
   :process-time process-time
   :method method
   :id id
   :type type
   :body body))

(defun lsp--log-entry-pp (entry)
  (cl-assert (lsp--log-entry-p entry))
  (pcase-let (((cl-struct lsp--log-entry timestamp method id type process-time
                          body)
               entry)
              (json-false :json-false)
              (json-encoding-pretty-print t)
              (str nil))
    (setq str
          (concat (format "[Trace - %s] " timestamp)
                  (pcase type
                    ('incoming-req (format "Received request '%s - (%s)." method id))
                    ('outgoing-req (format "Sending request '%s - (%s)'." method id))

                    ('incoming-notif (format "Received notification '%s'." method))
                    ('outgoing-notif (format "Sending notification '%s'." method))

                    ('incoming-resp (format "Received response '%s - (%s)' in %dms."
                                            method id process-time))
                    ('outgoing-resp
                     (format
                      "Sending response '%s - (%s)'. Processing request took %dms"
                      method id process-time)))
                  "\n"
                  (if (memq type '(incoming-resp ougoing-resp))
                      "Result: "
                    "Params: ")
                  (json-encode body)
                  "\n\n\n"))
    (setq str (propertize str 'mouse-face 'highlight 'read-only t))
    (insert str)))

(defvar-local lsp--log-io-ewoc nil)

(defun lsp--get-create-io-ewoc (workspace)
  (if (and (lsp--workspace-ewoc workspace)
           (buffer-live-p (ewoc-buffer (lsp--workspace-ewoc workspace))))
      (lsp--workspace-ewoc workspace)
    (with-current-buffer (lsp--get-log-buffer-create workspace)
      (unless (eq 'lsp-log-io-mode major-mode) (lsp-log-io-mode))
      (setq-local window-point-insertion-type t)
      (setq lsp--log-io-ewoc (ewoc-create #'lsp--log-entry-pp nil nil t))
      (setf (lsp--workspace-ewoc workspace) lsp--log-io-ewoc))
    (lsp--workspace-ewoc workspace)))

(defun lsp--ewoc-count (ewoc)
  (let* ((count 0)
         (count-fn (lambda (_) (setq count (1+ count)))))
    (ewoc-map count-fn ewoc)
    count))

(defun lsp--log-entry-new (entry workspace)
  (let* ((ewoc (lsp--get-create-io-ewoc workspace))
         (count (and (not (eq lsp-io-messages-max t)) (lsp--ewoc-count ewoc)))
         (node (if (or (eq lsp-io-messages-max t)
                       (>= lsp-io-messages-max count))
                   nil
                 (ewoc-nth ewoc (1- lsp-io-messages-max))))
         (prev nil)
         (inhibit-read-only t))
    (while node
      (setq prev (ewoc-prev ewoc node))
      (ewoc-delete ewoc node)
      (setq node prev))
    (ewoc-enter-last ewoc entry)))

(defun lsp--send-notification (body)
  "Send BODY as a notification to the language server."
  (lsp-foreach-workspace
   (when lsp-print-io
     (lsp--log-entry-new (lsp--make-log-entry
                          (plist-get body :method)
                          nil (plist-get body :params) 'outgoing-notif)
                         lsp--cur-workspace))
   (lsp--send-no-wait (lsp--make-message body)
                      (lsp--workspace-proc lsp--cur-workspace))))

(defalias 'lsp-send-notification 'lsp--send-notification)

(defun lsp-notify (method params)
  "Send notification METHOD with PARAMS."
  (lsp--send-notification (lsp--make-notification method params)))

(defun lsp--cur-workspace-check ()
  "Check whether buffer lsp workspace(s) are set."
  (cl-assert (lsp-workspaces) nil
             "No language server(s) is associated with this buffer."))

(defun lsp--send-request (body &optional no-wait no-merge)
  "Send BODY as a request to the language server, get the response.
If NO-WAIT is non-nil, don't synchronously wait for a response.
If NO-MERGE is non-nil, don't merge the results but return alist workspace->result."
  (lsp-request (plist-get body :method)
               (plist-get body :params)
               :no-wait no-wait
               :no-merge no-merge))

(defalias 'lsp-send-request 'lsp--send-request
  "Send BODY as a request to the language server and return the response synchronously.
\n(fn BODY)")

(cl-defun lsp-request (method params &key no-wait no-merge)
  "Send request METHOD with PARAMS.
If NO-MERGE is non-nil, don't merge the results but return alist workspace->result.
If NO-WAIT is non-nil send the request as notification."
  (if no-wait
      (lsp-notify method params)
    (let* ((send-time (time-to-seconds (current-time)))
           ;; max time by which we must get a response
           (expected-time (+ send-time lsp-response-timeout))
           resp-result resp-error done?)
      (unwind-protect
          (progn
            (lsp-request-async method params (lambda (res) (setf resp-result (or res :finished)))
                               :error-handler (lambda (err) (setf resp-error err))
                               :no-merge no-merge
                               :mode 'detached
                               :cancel-token :sync-request)
            (while (not (or resp-error resp-result))
              (accept-process-output nil 0.001)
              (when (< expected-time (time-to-seconds (current-time)))
                (error "Timeout while waiting for response. Method: %s." method)))
            (setq done? t)
            (cond
             ((eq resp-result :finished) nil)
             (resp-result resp-result)
             ((ht? resp-error) (error (gethash "message" resp-error)))
             (t (error (gethash "message" (cl-first resp-error))))))
        (unless done?
          (lsp-cancel-request-by-token :sync-request))))))

(cl-defun lsp-request-while-no-input (method params)
  "Send request METHOD with PARAMS and waits until there is no input."
  (let* (resp-result resp-error done?)
    (unwind-protect
        (progn
          (lsp-request-async
           method
           params
           (lambda (res) (setf resp-result (or res :finished)))
           :error-handler (lambda (err) (setf resp-error err))
           :mode 'detached
           :cancel-token :sync-request)

          (while (and (not (or resp-error resp-result))
                      (not (input-pending-p)))
            (accept-process-output nil 0.001))
          (setq done? t)
          (cond
           ((eq resp-result :finished) nil)
           (resp-result resp-result)
           ((ht? resp-error) (error (gethash "message" resp-error)))
           ((input-pending-p) nil)
           (t (error (gethash "message" (cl-first resp-error))))))
      (unless done?
        (lsp-cancel-request-by-token :sync-request)))))

(defvar lsp--cancelable-requests (ht))

(cl-defun lsp-request-async (method params callback
                                    &key mode error-handler no-merge cancel-token)
  "Send request METHOD with PARAMS."
  (lsp--send-request-async `(:jsonrpc "2.0" :method ,method :params ,params)
                           callback mode error-handler no-merge cancel-token))

(defun lsp--create-request-cancel (id workspaces hook buf method)
  (lambda (&rest _)
    (unless (and (equal 'post-command-hook hook)
                 (equal (current-buffer) buf))
      (lsp--request-cleanup-hooks id)
      (with-lsp-workspaces workspaces
        (lsp--cancel-request id))
      (lsp-log "Cancelling %s(%s) in hook %s" method id hook))))

(defun lsp--create-async-callback
    (callback method no-merge workspaces)
  "Create async handler expecting COUNT results, merge them and call CALLBACK.
MODE determines when the callback will be called depending on the
condition of the original buffer. METHOD is the invoked method.
If NO-MERGE is non-nil, don't merge the results but return alist workspace->result.
ID is the request id. "
  (let (results errors)
    (lambda (result)
      (push (cons lsp--cur-workspace result)
            (if (eq result :error) errors results))
      (when (and (not (eq (length errors) (length workspaces)))
                 (eq (+ (length errors) (length results)) (length workspaces)))
        (funcall callback
                 (if no-merge
                     results
                   (lsp--merge-results (-map #'cl-rest results) method)))))))

(defun lsp--create-default-error-handler (method)
  "Default error handler.
METHOD is the executed method."
  (lambda (error)
    (lsp--warn "%s" (or (gethash "message" error)
                        (format "%s Request has failed" method)))))

(defvar lsp--request-cleanup-hooks (ht))

(defun lsp--request-cleanup-hooks (request-id)
  (when-let (cleanup-function (gethash request-id lsp--request-cleanup-hooks))
    (funcall cleanup-function)
    (remhash request-id lsp--request-cleanup-hooks)))

(defun lsp-cancel-request-by-token (cancel-token)
  "Cancel request using CANCEL-TOKEN."
  (-when-let ((request-id . workspaces) (gethash cancel-token lsp--cancelable-requests))
    (with-lsp-workspaces workspaces
      (lsp--cancel-request request-id))
    (remhash cancel-token lsp--cancelable-requests)
    (lsp--request-cleanup-hooks request-id)))

(defun lsp--send-request-async (body callback &optional mode error-callback no-merge cancel-token)
  "Send BODY as a request to the language server.
Call CALLBACK with the response received from the server
asynchronously. MODE determines when the callback will be called
depending on the condition of the original buffer. It could be:
`detached' which means that the callback will be executed no
matter what has happened to the buffer. `alive' - the callback
will be executed only if the buffer from which the call was
executed is still alive. `current' the callback will be executed
only if the original buffer is still selected. `tick' - the
callback will be executed only if the buffer was not modified.
`unchanged' - the callback will be executed only if the buffer
hasn't changed and if the buffer is not modified.

ERROR-CALLBACK will be called in case the request has failed.
If NO-MERGE is non-nil, don't merge the results but return alist workspace->result.
"
  (when cancel-token
    (lsp-cancel-request-by-token cancel-token))

  (if-let ((target-workspaces (lsp--find-workspaces-for body)))
      (let* ((start-time (current-time))
             (method (plist-get body :method))
             (id (cl-incf lsp-last-id))
             ;; calculate what are the (hook . local) pairs which will cancel
             ;; the request
             (hooks (pcase mode
                      ('alive     '((kill-buffer-hook . t)))
                      ('tick      '((kill-buffer-hook . t) (after-change-functions . t)))
                      ('unchanged '((after-change-functions . t) (post-command-hook . nil)))
                      ('current   '((post-command-hook . nil)))))
             (buf (current-buffer))
             ;; note: lambdas in emacs can be compared but we should make sure
             ;; that all of the captured arguments are the same - in our case
             ;; `lsp--create-request-cancel' will return the same lambda when
             ;; called with the same params.
             (cleanup-hooks (lambda ()
                              (when (buffer-live-p buf)
                                (with-current-buffer buf
                                  (mapc (-lambda ((hook . local))
                                          (remove-hook
                                           hook
                                           (lsp--create-request-cancel
                                            id target-workspaces hook buf method)
                                           local))
                                        hooks)))
                              (remhash cancel-token lsp--cancelable-requests)))
             (callback (pcase mode
                         ((or 'alive 'tick) (lambda (&rest args)
                                              (with-current-buffer buf
                                                (apply callback args))))
                         (_ callback)))
             (callback (lsp--create-async-callback callback
                                                   method
                                                   no-merge
                                                   target-workspaces))
             (callback (lambda (result)
                         (lsp--request-cleanup-hooks id)
                         (funcall callback result)))
             (error-callback (lsp--create-async-callback
                              (or error-callback
                                  (lsp--create-default-error-handler method))
                              method
                              nil
                              target-workspaces))
             (error-callback (lambda (error)
                               (funcall callback :error)
                               (lsp--request-cleanup-hooks id)
                               (funcall error-callback error)))
             (body (plist-put body :id id)))

        ;; cancel request in any of the hooks
        (when hooks
          (mapc (-lambda ((hook . local))
                  (add-hook hook
                            (lsp--create-request-cancel
                             id target-workspaces hook buf method)
                            nil local))
                hooks)
          (puthash id cleanup-hooks lsp--request-cleanup-hooks))

        (setq lsp--last-active-workspaces target-workspaces)

        (when cancel-token
          (puthash cancel-token (cons id target-workspaces) lsp--cancelable-requests))

        (seq-doseq (workspace target-workspaces)
          (when lsp-log-io
            (lsp--log-entry-new (lsp--make-log-entry method id
                                                     (plist-get body :params)
                                                     'outgoing-req)
                                workspace))
          (let ((message (lsp--make-message body)))
            (puthash id
                     (list callback error-callback method start-time (current-time))
                     (-> workspace
                         (lsp--workspace-client)
                         (lsp--client-response-handlers)))
            (lsp--send-no-wait message (lsp--workspace-proc workspace))))
        body)
    (error "The connected server(s) does not support method %s
To find out what capabilities support your server use `M-x lsp-describe-session' and expand the capabilities section."
           (plist-get body :method))))

;; deprecated, use lsp-request-async.
(defalias 'lsp-send-request-async 'lsp--send-request-async)

;; Clean up the entire state of lsp mode when Emacs is killed, to get rid of any
;; pending language servers.
(add-hook 'kill-emacs-hook #'lsp--global-teardown)

(defun lsp--global-teardown ()
  "Unload working workspaces."
  (lsp-foreach-workspace (lsp--shutdown-workspace)))

(defun lsp--shutdown-workspace (&optional restart)
  "Shut down the language server process for ‘lsp--cur-workspace’."
  (with-demoted-errors "LSP error: %S"
    (let ((lsp-response-timeout 0.5))
      (condition-case _err
          (lsp-request "shutdown" lsp--empty-ht)
        (error (lsp--error "Timeout while sending shutdown request"))))
    (lsp-notify "exit" nil))
  (setf (lsp--workspace-shutdown-action lsp--cur-workspace) (or (and restart 'restart) 'shutdown))
  (lsp--uninitialize-workspace))

(defun lsp--uninitialize-workspace ()
  "Cleanup buffer state.
When a workspace is shut down, by request or from just
disappearing, unset all the variables related to it."
  (let ((proc (lsp--workspace-cmd-proc lsp--cur-workspace)))
    (when (process-live-p proc)
      (kill-process proc))
    (unless lsp--buffer-workspaces
      (lsp-managed-mode -1))))

(defun lsp--client-capabilities (&optional custom-capabilities)
  "Return the client capabilities."
  (append
   `((workspace . ((workspaceEdit . ((documentChanges . t)
                                     (resourceOperations . ["create" "rename" "delete"])))
                   (applyEdit . t)
                   (symbol . ((symbolKind . ((valueSet . ,(apply 'vector (number-sequence 1 26)))))))
                   (executeCommand . ((dynamicRegistration . :json-false)))
                   ,@(when lsp-enable-file-watchers '((didChangeWatchedFiles . ((dynamicRegistration . t)))))
                   (workspaceFolders . t)
                   (configuration . t)))
     (textDocument . ((declaration . ((linkSupport . t)))
                      (definition . ((linkSupport . t)))
                      (implementation . ((linkSupport . t)))
                      (typeDefinition . ((linkSupport . t)))
                      (synchronization . ((willSave . t) (didSave . t) (willSaveWaitUntil . t)))
                      (documentSymbol . ((symbolKind . ((valueSet . ,(apply 'vector (number-sequence 1 26)))))
                                         (hierarchicalDocumentSymbolSupport . t)))
                      (formatting . ((dynamicRegistration . t)))
                      (rangeFormatting . ((dynamicRegistration . t)))
                      ,@(when lsp-enable-semantic-highlighting
                            `((semanticTokens
                               . ((tokenModifiers . ,(if lsp-semantic-tokens-apply-modifiers
                                                         (apply 'vector (mapcar #'car lsp-semantic-token-modifier-faces)) []))
                                  (tokenTypes . ,(apply 'vector (mapcar #'car lsp-semantic-token-faces)))))))
                      (rename . ((dynamicRegistration . t) (prepareSupport . t)))
                      (codeAction . ((dynamicRegistration . t)
                                     (isPreferredSupport . t)
                                     (codeActionLiteralSupport . ((codeActionKind . ((valueSet . [""
                                                                                                  "quickfix"
                                                                                                  "refactor"
                                                                                                  "refactor.extract"
                                                                                                  "refactor.inline"
                                                                                                  "refactor.rewrite"
                                                                                                  "source"
                                                                                                  "source.organizeImports"])))))))
                      (completion . ((completionItem . ((snippetSupport . ,(cond
                                                                            ((and lsp-enable-snippet (not (featurep 'yasnippet)) t)
                                                                             (lsp--warn (concat
                                                                                         "Yasnippet is not installed, but `lsp-enable-snippet' is set to `t'. "
                                                                                         "You must either install yasnippet, or disable snippet support."))
                                                                             :json-false)
                                                                            (lsp-enable-snippet t)
                                                                            (t :json-false)))
                                                        (documentationFormat . ["markdown"])))
                                     (contextSupport . t)))
                      (signatureHelp . ((signatureInformation . ((parameterInformation . ((labelOffsetSupport . t)))))))
                      (documentLink . ((dynamicRegistration . t)
                                       (tooltipSupport . t)))
                      (hover . ((contentFormat . ["markdown" "plaintext"])))
                      (foldingRange . ,(when lsp-enable-folding
                                         `((dynamicRegistration . t)
                                           ,@(when lsp-folding-range-limit
                                               `((rangeLimit . ,lsp-folding-range-limit)))
                                           ,@(when lsp-folding-line-folding-only
                                               `((lineFoldingOnly . t))))))
                      (callHierarchy . ((dynamicRegistration . :json-false)))
                      (publishDiagnostics . ((relatedInformation . t)
                                             (tagSupport . ((valueSet . [1 2])))
                                             (versionSupport . t)))))
     (window . ((workDoneProgress . t))))
   custom-capabilities))

(defun lsp-find-roots-for-workspace (workspace session)
  "Get all roots for the WORKSPACE."
  (-filter #'identity (ht-map (lambda (folder workspaces)
                                (when (-contains? workspaces workspace)
                                  folder))
                              (lsp-session-folder->servers session))))

(defun lsp-session-watches (&optional session)
  "Get watches created for SESSION."
  (or (gethash "__watches" (lsp-session-metadata (or session (lsp-session))))
      (-let [res (make-hash-table :test 'equal)]
        (puthash "__watches" res (lsp-session-metadata (or session (lsp-session))))
        res)))

(defun lsp--file-process-event (session root-folder event)
  "Process file event."
  (let ((changed-file (cl-caddr event)))
    (->>
     session
     lsp-session-folder->servers
     (gethash root-folder)
     (seq-do (lambda (workspace)
               (when (->>
                      workspace
                      lsp--workspace-registered-server-capabilities
                      (-any?
                       (lambda (capability)
                         (and
                          (equal (lsp--registered-capability-method capability)
                                 "workspace/didChangeWatchedFiles")
                          (->>
                           capability
                           lsp--registered-capability-options
                           (gethash "watchers")
                           (seq-find
                            (-lambda ((&hash "globPattern" glob-pattern))
                              (-let [glob-regex (eshell-glob-regexp glob-pattern)]
                                (or (string-match glob-regex changed-file)
                                    (string-match glob-regex (f-relative changed-file root-folder)))))))))))
                 (with-lsp-workspace workspace
                   (lsp-notify
                    "workspace/didChangeWatchedFiles"
                    `((changes . [((type . ,(alist-get (cadr event) lsp--file-change-type))
                                   (uri . ,(lsp--path-to-uri changed-file)))]))))))))))

(defun lsp--server-register-capability (reg)
  "Register capability REG."
  (-let (((&hash "method" "id" "registerOptions") reg)
         (session (lsp-session)))
    (when (and lsp-enable-file-watchers
               (equal method "workspace/didChangeWatchedFiles"))
      (-let* ((created-watches (lsp-session-watches session))
              (root-folders (cl-set-difference
                             (lsp-find-roots-for-workspace lsp--cur-workspace session)
                             (ht-keys created-watches))))
        ;; create watch for each root folder without such
        (dolist (folder root-folders)
          (let ((watch (make-lsp-watch :root-directory folder)))
            (puthash folder watch created-watches)
            (lsp-watch-root-folder (file-truename folder)
                                   (-partial #'lsp--file-process-event session folder)
                                   watch
                                   t)))))

    (push
     (make-lsp--registered-capability :id id :method method :options registerOptions)
     (lsp--workspace-registered-server-capabilities lsp--cur-workspace))))

(defun lsp--cleanup-hanging-watches ()
  "Cleanup watches in case there are no more workspaces that are interested
in that particular folder."
  (let* ((session (lsp-session))
         (watches (lsp-session-watches session)))
    (dolist (watched-folder (ht-keys watches))
      (when (-none? (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--registered-capability "workspace/didChangeWatchedFiles")))
                    (gethash watched-folder (lsp-session-folder->servers (lsp-session))))
        (lsp-log "Cleaning up watches for folder %s. There is no workspace watching this folder..." watched-folder)
        (lsp-kill-watch (gethash watched-folder watches))
        (remhash watched-folder watches)))))

(defun lsp--server-unregister-capability (unreg)
  "Unregister capability UNREG."
  (-let [(&hash "id" "method") unreg]
    (setf (lsp--workspace-registered-server-capabilities lsp--cur-workspace)
          (seq-remove (lambda (e) (equal (lsp--registered-capability-id e) id))
                      (lsp--workspace-registered-server-capabilities lsp--cur-workspace)))
    (when (equal method "workspace/didChangeWatchedFiles")
      (lsp--cleanup-hanging-watches))))

(defun lsp--server-capabilities ()
  "Return the capabilities of the language server associated with the buffer."
  (->> (lsp-workspaces)
       (-map #'lsp--workspace-server-capabilities)
       (-filter #'identity)
       (apply #'ht-merge)))

(defun lsp--send-open-close-p ()
  "Return whether open and close notifications should be sent to the server."
  (let ((sync (gethash "textDocumentSync" (lsp--server-capabilities))))
    (or (memq sync '(1 2))
        (and (hash-table-p sync) (gethash "openClose" sync)))))

(defun lsp--send-will-save-p ()
  "Return whether will save notifications should be sent to the server."
  (let ((sync (gethash "textDocumentSync" (lsp--server-capabilities))))
    (and (hash-table-p sync) (gethash "willSave" sync))))

(defun lsp--send-will-save-wait-until-p ()
  "Return whether will save wait until notifications should be sent to the server."
  (let ((sync (gethash "textDocumentSync" (lsp--server-capabilities))))
    (and (hash-table-p sync) (gethash "willSaveWaitUntil" sync))))

(defun lsp--send-did-save-p ()
  "Return whether did save notifications should be sent to the server."
  (let ((sync (gethash "textDocumentSync" (lsp--server-capabilities))))
    (or (memq sync '(1 2))
        (and (hash-table-p sync) (gethash "save" sync)))))

(defun lsp--save-include-text-p ()
  "Return whether save notifications should include the text document's contents."
  (let ((sync (gethash "textDocumentSync" (lsp--server-capabilities))))
    (and (hash-table-p sync)
         (hash-table-p (gethash "save" sync nil))
         (gethash "includeText" (gethash "save" sync)))))

(defun lsp--suggest-project-root ()
  "Get project root."
  (or
   (when (featurep 'projectile) (condition-case nil
                                    (projectile-project-root)
                                  (error nil)))
   (when (featurep 'project)
     (when-let ((project (project-current)))
       (car (project-roots project))))))

(defun lsp--read-from-file (file)
  "Read FILE content."
  (when (file-exists-p file)
    (cl-first (read-from-string (f-read-text file 'utf-8)))))

(defun lsp--persist (file-name to-persist)
  "Persist TO-PERSIST in FILE-NAME."
  (f-write-text (prin1-to-string to-persist) 'utf-8 file-name))

(defun lsp-workspace-folders-add (project-root)
  "Add PROJECT-ROOT to the list of workspace folders."
  (interactive
   (list (read-directory-name "Select folder to add: "
                              (or (lsp--suggest-project-root) default-directory) nil t)))
  (cl-pushnew (lsp-f-canonical project-root)
              (lsp-session-folders (lsp-session)) :test 'equal)
  (lsp--persist-session (lsp-session))

  (run-hook-with-args 'lsp-workspace-folders-changed-functions (list project-root) nil))

(defun lsp-workspace-folders-remove (project-root)
  "Remove PROJECT-ROOT from the list of workspace folders."
  (interactive (list (completing-read "Select folder to remove: "
                                      (lsp-session-folders (lsp-session)) nil t
                                      (lsp-find-session-folder (lsp-session) default-directory))))

  (setq project-root (lsp-f-canonical project-root))

  ;; send remove folder to each multiroot workspace associated with the folder
  (dolist (wks (->> (lsp-session)
                    (lsp-session-folder->servers)
                    (gethash project-root)
                    (--filter (lsp--client-multi-root (lsp--workspace-client it)))))
    (with-lsp-workspace wks
      (lsp-notify "workspace/didChangeWorkspaceFolders"
                  `(:event (:removed ,(vector (list :uri (lsp--path-to-uri project-root))))))))

  ;; turn off servers in the removed directory
  (let* ((session (lsp-session))
         (folder->servers (lsp-session-folder->servers session))
         (server-id->folders (lsp-session-server-id->folders session))
         (workspaces (gethash project-root folder->servers)))

    (remhash project-root folder->servers)

    ;; turn off the servers without root folders
    (dolist (workspace workspaces)
      (when (--none? (-contains? it workspace) (ht-values folder->servers))
        (lsp--info "Shutdown %s since folder %s is removed..."
                   (lsp--workspace-print workspace) project-root)
        (with-lsp-workspace workspace (lsp--shutdown-workspace))))

    (setf (lsp-session-folders session)
          (-remove-item project-root (lsp-session-folders session)))

    (ht-aeach (puthash key
                       (-remove-item project-root value)
                       server-id->folders)
              server-id->folders)
    (lsp--persist-session (lsp-session)))

  (run-hook-with-args 'lsp-workspace-folders-changed-functions nil (list project-root)))

(defun lsp-workspace-blacklist-remove (project-root)
  "Remove PROJECT-ROOT from the workspace blacklist."
  (interactive (list (completing-read "Select folder to remove:"
                                      (lsp-session-folders-blacklist (lsp-session))
                                      nil t)))
  (setf (lsp-session-folders-blacklist (lsp-session))
        (delete project-root
                (lsp-session-folders-blacklist (lsp-session))))
  (lsp--persist-session (lsp-session)))

(define-obsolete-function-alias 'lsp-workspace-folders-switch
  'lsp-workspace-folders-open "lsp-mode 6.1")

(defun lsp-workspace-folders-open (project-root)
  "Open the directory located at PROJECT-ROOT"
  (interactive (list (completing-read "Open folder: "
                                      (lsp-session-folders (lsp-session))
                                      nil t)))
  (find-file project-root))

(defun lsp--maybe-enable-signature-help (trigger-characters)
  (let ((ch last-command-event))
    (when (cl-find ch trigger-characters :key #'string-to-char)
      (lsp-signature-activate))))

(defun lsp--update-on-type-formatting-hook (&optional cleanup?)
  (let ((on-type-formatting-handler
         (when-let (provider (lsp--capability "documentOnTypeFormattingProvider"))
           (-let [(&hash "moreTriggerCharacter" more-trigger-characters
                         "firstTriggerCharacter" first-trigger-characters) provider]
             (lambda ()
               (lsp--on-type-formatting first-trigger-characters
                                        more-trigger-characters))))))
    (cond
     ((and lsp-enable-on-type-formatting on-type-formatting-handler)
      (add-hook 'post-self-insert-hook on-type-formatting-handler nil t))

     ((or cleanup?
          (not lsp-enable-on-type-formatting))
      (remove-hook 'post-self-insert-hook on-type-formatting-handler t)))))

(defun lsp--update-signature-help-hook (&optional cleanup?)
  (let ((signature-help-handler
         (-when-let ((&hash? "triggerCharacters" trigger-characters)
                     (lsp--capability "signatureHelpProvider"))
           (lambda ()
             (lsp--maybe-enable-signature-help trigger-characters)))))
    (cond
     ((and lsp-signature-auto-activate signature-help-handler)
      (add-hook 'post-self-insert-hook signature-help-handler nil t))

     ((or cleanup?
          (not lsp-signature-auto-activate))
      (remove-hook 'post-self-insert-hook signature-help-handler t)))))

(defun lsp--semantic-highlighting-warn-about-deprecated-setting ()
  (when (boundp 'lsp-semantic-highlighting)
    (pcase lsp-semantic-highlighting
      (:semantic-tokens
       (lsp-warn "It seems you wish to use semanticTokens-based
 highlighting. To do so, please remove any references to the
 deprecated variable `lsp-semantic-highlighting' from your
 configuration and set `lsp-enable-semantic-highlighting' to `t'
 instead.")
       (setq lsp-enable-semantic-highlighting t))
      ((or :immediate :deferred)
       (lsp-warn "It seems you wish to use Theia-based semantic
 highlighting. This protocol has been superseded by the
 semanticTokens protocol specified by LSP v3.16 and is no longer
 supported by lsp-mode. If your language server provides
 semanticToken support, please set
 `lsp-enable-semantic-highlighting' to `t' to use it.")))))

(define-minor-mode lsp-managed-mode
  "Mode for source buffers managed by lsp-mode."
  nil nil nil
  (let ((status '(t (:eval (-keep #'lsp--workspace-status-string (lsp-workspaces))))))
    (cond
     (lsp-managed-mode
      (when (lsp-feature? "textDocument/hover")
        (add-function :before-until (local 'eldoc-documentation-function) #'lsp-eldoc-function)
        (eldoc-mode 1))

      (add-hook 'after-change-functions #'lsp-on-change nil t)
      (add-hook 'after-revert-hook #'lsp-on-revert nil t)
      (add-hook 'after-save-hook #'lsp-on-save nil t)
      (add-hook 'auto-save-hook #'lsp--on-auto-save nil t)
      (add-hook 'before-change-functions #'lsp-before-change nil t)
      (add-hook 'before-save-hook #'lsp--before-save nil t)
      (when (and lsp-enable-completion-at-point
                 (lsp-feature? "textDocument/completion"))
        (setq-local completion-at-point-functions nil)
        (add-hook 'completion-at-point-functions #'lsp-completion-at-point nil t)
        (setq-local completion-category-defaults
                    (add-to-list 'completion-category-defaults '(lsp-capf (styles basic)))))
      (add-hook 'kill-buffer-hook #'lsp--text-document-did-close nil t)

      (lsp--update-on-type-formatting-hook)
      (lsp--update-signature-help-hook)

      (lsp--semantic-highlighting-warn-about-deprecated-setting)

      (when (and lsp-enable-semantic-highlighting
                 (lsp-feature? "textDocument/semanticTokens"))
        (lsp--semantic-tokens-initialize-buffer
         (lsp-feature? "textDocument/semanticTokensRangeProvider")))
      (add-hook 'post-command-hook #'lsp--post-command nil t)
      (when lsp-enable-xref
        (add-hook 'xref-backend-functions #'lsp--xref-backend nil t))
      (when (and lsp-enable-text-document-color
                 (lsp-feature? "textDocument/documentColor"))
        (add-hook 'lsp-on-change-hook #'lsp--document-color nil t))

      (when (and lsp-lens-auto-enable (lsp-feature? "textDocument/codeLens"))
        (lsp-lens-mode 1))

      (setq-local global-mode-string (if (-contains? global-mode-string status)
                                         global-mode-string
                                       (cons status global-mode-string)))
      (when (bound-and-true-p company-mode)
        (lsp--setup-company)))
     (t
      (setq-local indent-region-function nil)
      (remove-function (local 'eldoc-documentation-function) #'lsp-eldoc-function)

      (remove-hook 'post-command-hook #'lsp--post-command t)
      (remove-hook 'after-change-functions #'lsp-on-change t)
      (remove-hook 'after-revert-hook #'lsp-on-revert t)
      (remove-hook 'after-save-hook #'lsp-on-save t)
      (remove-hook 'auto-save-hook #'lsp--on-auto-save t)
      (remove-hook 'before-change-functions #'lsp-before-change t)
      (remove-hook 'before-save-hook #'lsp--before-save t)
      (remove-hook 'completion-at-point-functions #'lsp-completion-at-point t)
      (setq-local completion-category-defaults
                  (cl-remove 'lsp-capf completion-category-defaults :key #'car))
      (remove-hook 'kill-buffer-hook #'lsp--text-document-did-close t)

      (lsp--update-on-type-formatting-hook :cleanup)
      (lsp--update-signature-help-hook :cleanup)

      (when lsp--on-idle-timer
        (cancel-timer lsp--on-idle-timer)
        (setq lsp--on-idle-timer nil))

      (remove-hook 'lsp-on-idle-hook #'lsp--document-links t)
      (remove-hook 'lsp-on-idle-hook #'lsp--document-highlight t)

      (when lsp--semantic-tokens-teardown
        (funcall lsp--semantic-tokens-teardown)
        (setq lsp--semantic-tokens-teardown nil))

      (lsp--remove-overlays 'lsp-sem-highlight)
      (lsp--remove-overlays 'lsp-highlight)
      (lsp--remove-overlays 'lsp-links)
      (lsp--remove-overlays 'lsp-color)
      (lsp-lens-mode -1)

      (remove-hook 'xref-backend-functions #'lsp--xref-backend t)
      (remove-hook 'lsp-on-change-hook #'lsp--document-color t)
      (setq-local global-mode-string (remove status global-mode-string))))))

(defun lsp-configure-buffer ()
  (when lsp-auto-configure
    (when (and lsp-enable-imenu (lsp-feature? "textDocument/documentSymbol"))
      (lsp-enable-imenu))

    (when (and lsp-enable-indentation
               (lsp-feature? "textDocument/rangeFormatting"))
      (setq-local indent-region-function #'lsp-format-region))

    (when (and lsp-enable-symbol-highlighting
               (lsp-feature? "textDocument/documentHighlight"))
      (add-hook 'lsp-on-idle-hook #'lsp--document-highlight nil t))

    (when (and lsp-enable-links
               (lsp-feature? "textDocument/documentLink"))
      (add-hook 'lsp-on-idle-hook #'lsp--document-links nil t)))

  (let ((buffer (current-buffer)))
    (run-with-idle-timer
     0.0 nil
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (lsp--on-change-debounce buffer)
           (lsp--on-idle buffer)))))))

(defun lsp--buffer-content ()
  (lsp-save-restriction-and-excursion
    (or (lsp-virtual-buffer-call :buffer-string)
        (buffer-substring-no-properties (point-min)
                                        (point-max)))))

(defun lsp--text-document-did-open ()
  "'document/didOpen' event."
  (run-hooks 'lsp-before-open-hook)
  (setq lsp--cur-version (or lsp--cur-version 0))
  (cl-pushnew (lsp-current-buffer) (lsp--workspace-buffers lsp--cur-workspace))
  (lsp-notify
   "textDocument/didOpen"
   (list :textDocument
         (list :uri (lsp--buffer-uri)
               :languageId (lsp-buffer-language)
               :version lsp--cur-version
               :text (lsp--buffer-content))))

  (lsp-managed-mode 1)

  (lsp-configure-buffer)

  (run-hooks 'lsp-after-open-hook)
  (-some-> lsp--cur-workspace
    (lsp--workspace-client)
    (lsp--client-after-open-fn)
    (funcall)))

(defun lsp--text-document-identifier ()
  "Make TextDocumentIdentifier."
  (list :uri (lsp--buffer-uri)))

(defun lsp--versioned-text-document-identifier ()
  "Make VersionedTextDocumentIdentifier."
  (plist-put (lsp--text-document-identifier) :version lsp--cur-version))

(defun lsp--cur-line (&optional point)
  (1- (line-number-at-pos point)))

(defun lsp--cur-position ()
  "Make a Position object for the current point."
  (or (lsp-virtual-buffer-call :cur-position)
      (lsp-save-restriction-and-excursion
        (list :line (lsp--cur-line)
              :character (- (point) (line-beginning-position))))))

(defun lsp--point-to-position (point)
  "Convert POINT to Position."
  (lsp-save-restriction-and-excursion
    (goto-char point)
    (lsp--cur-position)))

(defun lsp--range (start end)
  "Make Range body from START and END."
  ;; make sure start and end are Position objects
  (list :start start :end end))

(defun lsp--region-to-range (start end)
  "Make Range object for the current region."
  (lsp--range (lsp--point-to-position start)
              (lsp--point-to-position end)))

(defun lsp--region-or-line ()
  "The active region or the current line."
  (if (use-region-p)
      (lsp--region-to-range (region-beginning) (region-end))
    (lsp--region-to-range (point-at-bol) (point-at-eol))))

(defun lsp--check-document-changes-version (document-changes)
  "Verify that DOCUMENT-CHANGES have the proper version."
  (unless (seq-every-p
           (lambda (it)
             (or
              (not (gethash "textDocument" it))
              (let* ((ident (gethash "textDocument" it))
                     (filename (lsp--uri-to-path (gethash "uri" ident)))
                     (version (gethash "version" ident)))
                (with-current-buffer (find-file-noselect filename)
                  (or (null version) (zerop version)
                      (equal version lsp--cur-version))))))
           document-changes)
    (error "Document changes cannot be applied")))

(defun lsp--apply-workspace-edit (edit)
  "Apply the WorkspaceEdit object EDIT."
  (if-let (document-changes (seq-reverse (gethash "documentChanges" edit)))
      (progn
        (lsp--check-document-changes-version document-changes)
        (->> document-changes
             (seq-filter (-lambda ((&hash "kind"))
                           (or (not kind) (equal kind "edit"))))
             (seq-do #'lsp--apply-text-document-edit))
        (->> document-changes
             (seq-filter (-lambda ((&hash "kind"))
                           (not (or (not kind) (equal kind "edit")))))
             (seq-do #'lsp--apply-text-document-edit)))
    (when-let (changes (gethash "changes" edit))
      (maphash
       (lambda (uri text-edits)
         (with-current-buffer (-> uri lsp--uri-to-path find-file-noselect)
           (lsp--apply-text-edits text-edits)))
       changes))))

(defmacro lsp-with-current-buffer (buffer-id &rest body)
  (declare (indent 1) (debug t))
  `(if-let (wcb (plist-get ,buffer-id :with-current-buffer))
       (with-lsp-workspaces (plist-get ,buffer-id :workspaces)
         (funcall wcb (lambda () ,@body)))
     (with-current-buffer ,buffer-id
       ,@body)))

(defmacro lsp-with-filename (file &rest body)
  "Execute BODY with FILE as a context.
Need to handle the case when FILE indicates virtual buffer."
  (declare (indent 1) (debug t))
  `(if-let (lsp--virtual-buffer (get-text-property 0 'lsp-virtual-buffer ,file))
       (lsp-with-current-buffer lsp--virtual-buffer
         ,@body)
     ,@body))

(defun lsp--apply-text-document-edit (edit)
  "Apply the TextDocumentEdit object EDIT.
If the file is not being visited by any buffer, it is opened with
`find-file-noselect'.
Because lsp-mode does not store previous document versions, the edit is only
applied if the version of the textDocument matches the version of the
corresponding file.

interface TextDocumentEdit {
  textDocument: VersionedTextDocumentIdentifier;
  edits: TextEdit[];
}"
  (pcase (gethash "kind" edit)
    ("create" (-let* (((&hash "uri" "options") edit)
                      (file-name (lsp--uri-to-path uri)))
                (f-touch file-name)
                (when (-some->> options (gethash "override"))
                  (f-write-text "" nil file-name))))
    ("delete" (-let* (((&hash "uri" "options") edit)
                      (file-name (lsp--uri-to-path uri))
                      (recursive (and options (gethash "recursive" options))))
                (f-delete file-name recursive)))
    ("rename" (-let* (((&hash "oldUri" "newUri" "options") edit)
                      (old-file-name (lsp--uri-to-path oldUri))
                      (new-file-name (lsp--uri-to-path newUri))
                      (buf (lsp--buffer-for-file old-file-name)))
                (when buf
                  (lsp-with-current-buffer buf
                    (save-buffer)
                    (lsp--text-document-did-close)))
                (rename-file old-file-name new-file-name
                             (and options (gethash "override" options)))
                (when buf
                  (lsp-with-current-buffer buf
                    (set-buffer-modified-p nil)
                    (set-visited-file-name new-file-name)
                    (lsp)))))
    (_ (let ((file-name (lsp--uri-to-path (lsp--ht-get edit "textDocument" "uri"))))
         (lsp-with-current-buffer (lsp--buffer-for-file file-name)
           (lsp-with-filename file-name
             (lsp--apply-text-edits (gethash "edits" edit))))))))

(defun lsp--position-compare (left right)
  "Compare position LEFT and RIGHT."
  (let ((left-line (gethash "line" left))
        (right-line (gethash "line" right)))
    (if (= left-line right-line)
        (let ((left-character (gethash "character" left))
              (right-character (gethash "character" right)))
          (> left-character right-character))
      (> left-line right-line))))

(defun lsp--position-equal (left right)
  "Return whether LEFT and RIGHT positions are equal."
  (and (= (gethash "line" left) (gethash "line" right))
       (= (gethash "character" left) (gethash "character" right))))

(defun lsp--text-edit-sort-predicate (e1 e2)
  (let ((start1 (gethash "start" (gethash "range" e1)))
        (start2 (gethash "start" (gethash "range" e2))))
    (if (lsp--position-equal start1 start2)
        (lsp--position-compare (gethash "end" (gethash "range" e1))
                               (gethash "end" (gethash "range" e2)))
      (lsp--position-compare  start1 start2))))

(defun lsp--apply-text-edit (text-edit)
  "Apply the edits described in the TextEdit object in TEXT-EDIT."
  ;; We sort text edits so as to apply edits that modify latter parts of the
  ;; document first. Furthermore, because the LSP spec dictates that:
  ;; "If multiple inserts have the same position, the order in the array
  ;; defines which edit to apply first."
  ;; We reverse the initial list and sort stably to make sure the order among
  ;; edits with the same position is preserved.
  (-let* (((&hash "newText" "range") text-edit)
          ((start . end) (lsp--range-to-region range)))
    (goto-char start)
    (delete-region start end)
    (insert newText)))


;; WORKAROUND: typescript-language might send -1 when applying code actions.
;; see https://github.com/emacs-lsp/lsp-mode/issues/1582
(defun lsp--fix-point (point)
  (-let [(&hash "character" "line") point]
    (ht ("line" (max 0 line))
        ("character" (max 0 character)))))

(defun lsp--apply-text-edit-replace-buffer-contents (text-edit)
  "Apply the edits described in the TextEdit object in TEXT-EDIT.
The method uses `replace-buffer-contents'."
  (-let* (((&hash "newText" "range" (&hash "start" "end")) text-edit)
          (source (current-buffer))
          ((beg . end) (lsp--range-to-region (ht ("start" (lsp--fix-point start))
                                                 ("end" (lsp--fix-point end))))))
    (with-temp-buffer
      (insert newText)
      (let ((temp (current-buffer)))
        (with-current-buffer source
          (save-excursion
            (save-restriction
              (narrow-to-region beg end)

              ;; On emacs versions < 26.2,
              ;; `replace-buffer-contents' is buggy - it calls
              ;; change functions with invalid arguments - so we
              ;; manually call the change functions here.
              ;;
              ;; See emacs bugs #32237, #32278:
              ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32237
              ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32278
              (let ((inhibit-modification-hooks t)
                    (length (- end beg)))
                (run-hook-with-args 'before-change-functions
                                    beg end)
                (when (fboundp 'replace-buffer-contents)
                  (with-no-warnings (replace-buffer-contents temp)))
                (run-hook-with-args 'after-change-functions
                                    beg (+ beg (length newText))
                                    length)))))))))

(defun lsp--apply-text-edits (edits)
  "Apply the edits described in the TextEdit[] object."
  (unless (seq-empty-p edits)
    (atomic-change-group
      (run-hooks 'lsp-before-apply-edits-hook)
      (let* ((change-group (when (functionp 'undo-amalgamate-change-group)
                             (prepare-change-group)))
             (howmany (length edits))
             (message (format "Applying %s edits to `%s' ..." howmany (current-buffer)))
             (_ (lsp--info message))
             (reporter (make-progress-reporter message 0 howmany))
             (done 0)
             (apply-edit (if (and (functionp 'replace-buffer-contents)
                                  (not lsp--virtual-buffer))
                             'lsp--apply-text-edit-replace-buffer-contents
                           'lsp--apply-text-edit)))
        (unwind-protect
            (->> edits
                 (mapc (lambda (edit)
                         (ht-set edit "newText"
                                 (s-replace "\r" "" (ht-get edit "newText" "")))))
                 (nreverse)
                 (seq-sort #'lsp--text-edit-sort-predicate)
                 (mapc (lambda (edit)
                         (progress-reporter-update reporter (cl-incf done))
                         (funcall apply-edit edit))))
          (when (fboundp 'undo-amalgamate-change-group)
            (with-no-warnings (undo-amalgamate-change-group change-group)))
          (progress-reporter-done reporter))))))

(defun lsp--capability (cap &optional capabilities)
  "Get the value of capability CAP.  If CAPABILITIES is non-nil, use them instead."
  (gethash cap (or capabilities
                   (lsp--server-capabilities)
                   lsp--empty-ht)))

(defun lsp--registered-capability (method)
  "Check whether there is workspace providing METHOD."
  (->> (lsp-workspaces)
       (--keep (seq-find (lambda (reg)
                           (equal (lsp--registered-capability-method reg) method))
                         (lsp--workspace-registered-server-capabilities it)))
       cl-first))

(defvar-local lsp--before-change-vals nil
  "Store the positions from the `lsp-before-change' function
  call, for validation and use in the `lsp-on-change' function.")

(defun lsp--text-document-content-change-event (start end length)
  "Make a TextDocumentContentChangeEvent body for START to END, of length LENGTH."
  ;; So (47 54 0) means add    7 chars starting at pos 47
  ;; must become
  ;;   {"range":{"start":{"line":5,"character":6}
  ;;             ,"end" :{"line":5,"character":6}}
  ;;             ,"rangeLength":0
  ;;             ,"text":"\nbb = 5"}
  ;;
  ;; And (47 47 7) means delete 7 chars starting at pos 47
  ;; must become
  ;;   {"range":{"start":{"line":6,"character":0}
  ;;            ,"end"  :{"line":7,"character":0}}
  ;;            ,"rangeLength":7
  ;;            ,"text":""}
  ;;
  ;; (208 221 3) means delete 3 chars starting at pos 208, and replace them with
  ;; 13 chars. So it must become
  ;;   {"range":{"start":{"line":5,"character":8}
  ;;             ,"end" :{"line":5,"character":11}}
  ;;             ,"rangeLength":3
  ;;             ,"text":"new-chars-xxx"}
  ;;

  ;; Adding text:
  ;;   lsp-before-change:(start,end)=(33,33)
  ;;   lsp-on-change:(start,end,length)=(33,34,0)
  ;;
  ;; Changing text:
  ;;   lsp-before-change:(start,end)=(208,211)
  ;;   lsp-on-change:(start,end,length)=(208,221,3)
  ;;
  ;; Deleting text:
  ;;   lsp-before-change:(start,end)=(19,27)
  ;;   lsp-on-change:(start,end,length)=(19,19,8)
  (if (zerop length)
      ;; Adding something only, work from start only
      `(:range ,(lsp--range (lsp--point-to-position start)
                            (lsp--point-to-position start))
               :rangeLength 0
               :text ,(buffer-substring-no-properties start end))

    (if (eq start end)
        ;; Deleting something only
        (if (lsp--bracketed-change-p start length)
            ;; The before-change value is bracketed, use it
            `(:range ,(lsp--range (lsp--point-to-position start)
                                  (plist-get lsp--before-change-vals :end-pos))
                     :rangeLength ,length
                     :text "")
          ;; If the change is not bracketed, send a full change event instead.
          (lsp--full-change-event))

      ;; Deleting some things, adding others
      (if (lsp--bracketed-change-p start length)
          ;; The before-change value is valid, use it
          `(:range ,(lsp--range (lsp--point-to-position start)
                                (plist-get lsp--before-change-vals :end-pos))
                   :rangeLength ,length
                   :text ,(buffer-substring-no-properties start end))
        (lsp--full-change-event)))))

(defun lsp--bracketed-change-p (start length)
  "If the before and after positions are the same, and the length
is the size of the start range, we are probably good."
  (-let [(&plist :end before-end :start before-start) lsp--before-change-vals]
    (and (eq start before-start)
         (eq length (- before-end before-start)))))

(defun lsp--full-change-event ()
  `(:text ,(lsp--buffer-content)))

(defun lsp-before-change (start end)
  "Executed before a file is changed.
Added to `before-change-functions'."
  ;; Note:
  ;;
  ;; This variable holds a list of functions to call when Emacs is about to
  ;; modify a buffer. Each function gets two arguments, the beginning and end of
  ;; the region that is about to change, represented as integers. The buffer
  ;; that is about to change is always the current buffer when the function is
  ;; called.
  ;;
  ;; WARNING:
  ;;
  ;; Do not expect the before-change hooks and the after-change hooks be called
  ;; in balanced pairs around each buffer change. Also don't expect the
  ;; before-change hooks to be called for every chunk of text Emacs is about to
  ;; delete. These hooks are provided on the assumption that Lisp programs will
  ;; use either before- or the after-change hooks, but not both, and the
  ;; boundaries of the region where the changes happen might include more than
  ;; just the actual changed text, or even lump together several changes done
  ;; piecemeal.
  (lsp-save-restriction-and-excursion
    (setq lsp--before-change-vals
          (list :start start
                :end end
                :end-pos (lsp--point-to-position end)))))

(defun lsp--flush-delayed-changes ()
  (let ((inhibit-quit t))
    (when lsp--delay-timer
      (cancel-timer lsp--delay-timer))
    (mapc (-lambda ((workspace buffer document change))
            (with-current-buffer buffer
              (with-lsp-workspace workspace
                (lsp-notify "textDocument/didChange"
                            (list :textDocument document
                                  :contentChanges (vector change))))))
          (prog1 (nreverse lsp--delayed-requests)
            (setq lsp--delayed-requests nil)))))

(defun lsp--workspace-sync-method (workspace)
  (let* ((sync (gethash "textDocumentSync" (lsp--workspace-server-capabilities workspace))))
    (if (hash-table-p sync) (gethash "change" sync) sync)))

(defun lsp-on-change (start end length &optional content-change-event-fn)
  "Executed when a file is changed.
Added to `after-change-functions'."
  ;; Note:
  ;;
  ;; Each function receives three arguments: the beginning and end of the region
  ;; just changed, and the length of the text that existed before the change.
  ;; All three arguments are integers. The buffer that has been changed is
  ;; always the current buffer when the function is called.
  ;;
  ;; The length of the old text is the difference between the buffer positions
  ;; before and after that text as it was before the change. As for the
  ;; changed text, its length is simply the difference between the first two
  ;; arguments.
  ;;
  ;; So (47 54 0) means add    7 chars starting at pos 47
  ;; So (47 47 7) means delete 7 chars starting at pos 47
  (save-match-data
    (let ((inhibit-quit t))
      ;; A (revert-buffer) call with the 'preserve-modes parameter (eg, as done
      ;; by auto-revert-mode) will cause this handler to get called with a nil
      ;; buffer-file-name. We need the buffer-file-name to send notifications;
      ;; so we skip handling revert-buffer-caused changes and instead handle
      ;; reverts separately in lsp-on-revert
      (when (not revert-buffer-in-progress-p)
        (cl-incf lsp--cur-version)
        (mapc
         (lambda (workspace)
           (pcase (or lsp-document-sync-method
                      (lsp--workspace-sync-method workspace))
             (1
              (if lsp-debounce-full-sync-notifications
                  (setq lsp--delayed-requests
                        (->> lsp--delayed-requests
                             (-remove (-lambda ((_ buffer))
                                        (equal (current-buffer) buffer)))
                             (cons (list workspace
                                         (current-buffer)
                                         (lsp--versioned-text-document-identifier)
                                         (lsp--full-change-event)))))
                (with-lsp-workspace workspace
                  (lsp-notify "textDocument/didChange"
                              (list :contentChanges (vector (lsp--full-change-event)))))))
             (2
              (with-lsp-workspace workspace
                (lsp-notify
                 "textDocument/didChange"
                 (list :textDocument (lsp--versioned-text-document-identifier)
                       :contentChanges (vector
                                        (if content-change-event-fn
                                            (funcall content-change-event-fn start end length)
                                          (lsp--text-document-content-change-event
                                           start end length)))))))))
         (lsp-workspaces))
        (when lsp--delay-timer (cancel-timer lsp--delay-timer))
        (setq lsp--delay-timer (run-with-idle-timer
                                lsp-debounce-full-sync-notifications-interval
                                nil
                                #'lsp--flush-delayed-changes))
        ;; force cleanup overlays after each change
        (lsp--remove-overlays 'lsp-highlight)
        (lsp--after-change  (current-buffer))
        (setq lsp--signature-last-index nil)
        (setq lsp--signature-last nil)))))



;; facilities for on change hooks. We do not want to make lsp calls on each
;; change event so we add debounce to avoid flooding the server with events.
;; Additionally, we want to have a mechanism for stopping the server calls in
;; particular cases like, e. g. when performing completion.

(defvar lsp-inhibit-lsp-hooks nil
  "Flag to control.")

(defcustom lsp-on-change-hook nil
  "Hooks to run when buffer has changed."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-idle-delay 0.500
  "Debounce interval for `after-change-functions'. "
  :type 'number
  :group 'lsp-mode)

(defcustom lsp-on-idle-hook nil
  "Hooks to run after `lsp-idle-delay'."
  :type 'hook
  :group 'lsp-mode)


(defun lsp--idle-reschedule (buffer)
  (setq lsp--on-idle-timer (run-with-idle-timer
                            lsp-idle-delay
                            nil
                            #'lsp--on-idle
                            buffer)))

(defun lsp--post-command ()
  (lsp--cleanup-highlights-if-needed)
  (lsp--idle-reschedule (current-buffer)))

(defun lsp--on-idle (buffer)
  "Start post command loop."
  (when (and (buffer-live-p buffer)
             (equal buffer (current-buffer))
             (not lsp-inhibit-lsp-hooks)
             lsp-managed-mode)
    (run-hooks 'lsp-on-idle-hook)))

(defun lsp--on-change-debounce (buffer)
  (when (and (buffer-live-p buffer)
             (equal buffer (current-buffer))
             (not lsp-inhibit-lsp-hooks)
             lsp-managed-mode)
    (run-hooks 'lsp-on-change-hook)))

(defun lsp--after-change (&rest _)
  (when lsp--on-change-timer
    (cancel-timer lsp--on-change-timer))

  (setq lsp--on-change-timer (run-with-idle-timer
                              lsp-idle-delay
                              nil
                              #'lsp--on-change-debounce
                              (current-buffer)))
  (lsp--idle-reschedule (current-buffer)))



(defun lsp--on-type-formatting (first-trigger-characters more-trigger-characters)
  "Self insert handling.
Applies on type formatting."
  (let ((ch last-command-event))
    (when (or (eq (string-to-char first-trigger-characters) ch)
              (cl-find ch more-trigger-characters :key #'string-to-char))
      (lsp-request-async "textDocument/onTypeFormatting"
                         (append (lsp--make-document-formatting-params)
                                 `(:ch ,(char-to-string ch) :position ,(lsp--cur-position)))
                         #'lsp--apply-text-edits
                         :mode 'tick))))


;; links
(defun lsp--document-links ()
  (lsp-request-async
   "textDocument/documentLink"
   `(:textDocument ,(lsp--text-document-identifier))
   (lambda (links)
     (lsp--remove-overlays 'lsp-link)
     (seq-do
      (-lambda ((link &as &hash "range"))
        (-doto (make-button (lsp--position-to-point (gethash "start" range))
                            (lsp--position-to-point (gethash "end" range))
                            'action (lsp--document-link-keymap link)
                            'keymap (let ((map (make-sparse-keymap)))
                                      (define-key map [M-return] 'push-button)
                                      (define-key map [mouse-2] 'push-button)
                                      map)
                            'help-echo "mouse-2, M-RET: Visit this link")
          (overlay-put 'lsp-link t)))
      links))
   :mode 'tick))

(defun lsp--document-link-handle-target (url)
  (let* ((parsed-url (url-generic-parse-url (url-unhex-string url)))
         (type (url-type parsed-url)))
    (pcase type
      ("file" (find-file (lsp--uri-to-path url)))
      ((or "http" "https") (browse-url url))
      (type (if-let ((handler (lsp--get-uri-handler type)))
                (funcall handler url)
              (signal 'lsp-file-scheme-not-supported (list url)))))))

(defun lsp--document-link-keymap (link)
  (-let (((&hash "target") link))
    (if target
        (lambda (_)
          (interactive)
          (lsp--document-link-handle-target target))
      (lambda (_)
        (interactive)
        (when (lsp--ht-get (lsp--capability "documentLinkProvider")
                           "resolveProvider")
          (lsp-request-async
           "documentLink/resolve"
           link
           (-lambda ((&hash "target"))
             (lsp--document-link-handle-target target))))))))



(defun lsp-buffer-language ()
  "Get language corresponding current buffer."
  (or (->> lsp-language-id-configuration
           (-first (-lambda ((mode-or-pattern . language))
                     (cond
                      ((and (stringp mode-or-pattern) (s-matches? mode-or-pattern buffer-file-name)) language)
                      ((eq mode-or-pattern major-mode) language))))
           cl-rest)
      (lsp-warn "Unable to calculate the languageId for current buffer. Take a look at lsp-language-id-configuration.")))

(defun lsp-workspace-root (&optional path)
  "Find the workspace root for the current file or PATH."
  (-when-let* ((file-name (or path (buffer-file-name)))
               (file-name (lsp-f-canonical file-name)))
    (->> (lsp-session)
         (lsp-session-folders)
         (--first (and (lsp--files-same-host it file-name)
                       (or (lsp-f-ancestor-of? it file-name)
                           (equal it file-name)))))))

(defun lsp-on-revert ()
  "Executed when a file is reverted.
Added to `after-revert-hook'."
  (let ((n (buffer-size))
        (revert-buffer-in-progress-p nil))
    (lsp-on-change 0 n n)))

(defun lsp--text-document-did-close (&optional keep-workspace-alive)
  "Executed when the file is closed, added to `kill-buffer-hook'.

If KEEP-WORKSPACE-ALIVE is non-nil, do not shutdown the workspace
if it's closing the last buffer in the workspace."
  (lsp-foreach-workspace
   (with-demoted-errors "Error on ‘lsp--text-document-did-close’: %S"
     (let ((old-buffers (lsp--workspace-buffers lsp--cur-workspace)))
       ;; remove buffer from the current workspace's list of buffers
       ;; do a sanity check first
       (when (memq (lsp-current-buffer) old-buffers)
         (setf (lsp--workspace-buffers lsp--cur-workspace)
               (delq (lsp-current-buffer) old-buffers))
         (with-demoted-errors "Error sending didClose notification in ‘lsp--text-document-did-close’: %S"
           (lsp-notify
            "textDocument/didClose"
            `(:textDocument ,(lsp--text-document-identifier))))
         (when (and (not lsp-keep-workspace-alive)
                    (not keep-workspace-alive)
                    (not (lsp--workspace-buffers lsp--cur-workspace)))
           (lsp--shutdown-workspace)))))))

(defun lsp--will-save-text-document-params (reason)
  (list :textDocument (lsp--text-document-identifier)
        :reason reason))

(defun lsp--before-save ()
  "Before save handler."
  (with-demoted-errors "Error in ‘lsp--before-save’: %S"
    (let ((params (lsp--will-save-text-document-params 1)))
      (when (lsp--send-will-save-p)
        (lsp-notify "textDocument/willSave" params))
      (when (and (lsp--send-will-save-wait-until-p) lsp-before-save-edits)
        (let ((lsp-response-timeout 0.1))
          (condition-case nil
              (lsp--apply-text-edits
               (lsp-request "textDocument/willSaveWaitUntil"
                            params))
            (error)))))))

(defun lsp--on-auto-save ()
  "Handler for auto-save."
  (when (lsp--send-will-save-p)
    (with-demoted-errors "Error in ‘lsp--on-auto-save’: %S"
      (lsp-notify "textDocument/willSave" (lsp--will-save-text-document-params 2)))))

(defun lsp--text-document-did-save ()
  "Executed when the file is closed, added to `after-save-hook''."
  (when (lsp--send-did-save-p)
    (with-demoted-errors "Error on ‘lsp--text-document-did-save: %S’"
      (lsp-notify "textDocument/didSave"
                  `(:textDocument ,(lsp--versioned-text-document-identifier)
                                  ,@(when (lsp--save-include-text-p)
                                      (list :text (lsp--buffer-content))))))))

(defun lsp--text-document-position-params (&optional identifier position)
  "Make TextDocumentPositionParams for the current point in the current document.
If IDENTIFIER and POSITION are non-nil, they will be used as the document identifier
and the position respectively."
  (list :textDocument (or identifier (lsp--text-document-identifier))
        :position (or position (lsp--cur-position))))

(defun lsp-cur-line-diagnostics ()
  "Return any diagnostics that apply to the current line."
  (-let [(&plist :start (&plist :line start) :end (&plist :line end)) (lsp--region-or-line)]
    (cl-coerce (-filter
                (-lambda ((&Diagnostic :range (&Range :start (&Position :line))))
                  (and (>= line start) (<= line end)))
                (lsp--get-buffer-diagnostics))
               'vector)))

(defalias 'lsp--cur-line-diagnotics 'lsp-cur-line-diagnostics)

(defconst lsp--completion-trigger-kinds
  `((invoked . 1)
    (character . 2)
    (incomplete . 3)))

(defun lsp--make-completion-item (item &rest plist)
  "Make completion item from lsp ITEM and PLIST."
  (-let (((&hash "label"
                 "insertText" insert-text
                 "sortText" sort-text
                 "_emacsStartPoint" start-point)
          item)
         ((&plist :markers :prefix) plist))
    (propertize (or label insert-text)
                'lsp-completion-item item
                'lsp-sort-text sort-text
                'lsp-completion-start-point start-point
                'lsp-completion-markers markers
                'lsp-completion-prefix prefix)))

(defun lsp--annotate (item)
  "Annotate ITEM detail."
  (-let (((&hash "detail" "kind") (plist-get (text-properties-at 0 item) 'lsp-completion-item)))
    (concat (when (and lsp-completion-show-detail detail)
              (concat " " (s-replace "\r" "" detail)))
            (when-let (kind-name (and kind (aref lsp--completion-item-kind kind)))
              (format " (%s)" kind-name)))))

(defun lsp--looking-back-trigger-characterp (trigger-characters)
  "Return trigger character if text before point matches any of the TRIGGER-CHARACTERS."
  (unless (= (point) (point-at-bol))
    (seq-some
     (lambda (trigger-char)
       (and (equal (buffer-substring-no-properties (- (point) (length trigger-char)) (point))
                   trigger-char)
            trigger-char))
     trigger-characters)))

(defvar lsp--capf-cache nil
  "Cached candidates for completion at point function.
In the form of list (prefix items :lsp-items ...).
When the completion is incomplete, cache contains value of `incomplete'.")

(defun lsp--capf-clear-cache (&rest _)
  "Clear completion caches."
  (-some-> (and (listp lsp--capf-cache) lsp--capf-cache)
    (cddr)
    (plist-get :markers)
    (cadr)
    (set-marker nil))
  (setq lsp--capf-cache nil))

(defun lsp--capf-guess-prefix (item &optional default)
  "Guess ITEM's prefix start point according to following heuristics:
- If `textEdit' exists, use insertion range start as prefix start point.
- Else, find the point before current point that's longest prefix match of
`insertText' or `label'.
When the heuristic fails to find the prefix start point, return DEFAULT value."
  (-let [(&hash "label"
                "insertText" insert-text
                "textEdit" text-edit)
         item]
    (or (cond
         (text-edit
          (car (-some->> text-edit
                 (gethash "range")
                 lsp--range-to-region)))
         ((or insert-text label)
          (let* ((text (or insert-text label))
                 (start (max 1 (- (point) (length text))))
                 start-point)
            (while (and (< start (point)) (not start-point))
              (when (s-prefix? (buffer-substring-no-properties start (point)) text)
                (setq start-point start))
              (cl-incf start))
            start-point)))
        default)))

(defun lsp--capf-cached-items (items)
  "Convert ITEMS into `lsp--capf-cache-items' form."
  (--> items
       (-map (-lambda ((item &as &hash
                             "label"
                             "filterText" filter-text
                             "_emacsStartPoint" start-point
                             "score"))
                 (propertize (or filter-text label)
                  'lsp-completion-item item
                  'lsp-completion-start-point start-point
                  'lsp-completion-score score))
             it)
       (-group-by (-partial #'get-text-property 0 'lsp-completion-start-point) it)
       (sort it (-on #'< (lambda (o) (or (car o) most-positive-fixnum))))))

(cl-defun lsp--capf-filter-candidates (items
                                       &rest plist
                                       &key lsp-items
                                         &allow-other-keys)
  "List all possible completions in cached ITEMS with their prefixes.
We can pass LSP-ITEMS, which will be used when there's no cache.
Also, additional data to attached to each candidate can be passed via PLIST."
  (let ((filtered-items
         (if items
             (->> items
                  (-map (-lambda ((start-point . candidates))
                          (let ((query (buffer-substring-no-properties start-point (point))))
                            (--> (lsp--regex-fuzzy query)
                                 (-keep (lambda (cand)
                                          (when (string-match it cand)
                                            (setq cand (copy-sequence cand))
                                            (put-text-property 0 1 'match-data (match-data) cand)
                                            cand))
                                  candidates)
                                 (-map (lambda (cand)
                                         (put-text-property
                                          0 1
                                          'completion-score (lsp--fuzzy-score query cand) cand)
                                         cand)
                                       it)))))
                  (-flatten-n 1)
                  (-sort (-on #'> (lambda (o)
                                    (or (get-text-property 0 'sort-score o)
                                        (let* ((score (* (or (get-text-property 0 'completion-score o)
                                                             0.001)
                                                         (or (get-text-property 0 'lsp-completion-score o)
                                                             0.001))))
                                          (put-text-property 0 1 'sort-score score o)
                                          score)))))
                  ;; TODO: pass additional function to sort the candidates
                  (-map (-partial #'get-text-property 0 'lsp-completion-item)))
           lsp-items)))
    (-map (apply #'-rpartial #'lsp--make-completion-item plist) filtered-items)))

(defun lsp--capf-company-match (candidate)
  "Return highlights of typed prefix inside CANDIDATE."
  (let* ((prefix (downcase
                  (buffer-substring-no-properties
                   (plist-get (text-properties-at 0 candidate) 'lsp-completion-start-point)
                   (point))))
         (prefix-len (length prefix))
         (prefix-pos 0)
         (label (downcase candidate))
         (label-len (length label))
         (label-pos 0)
         matches start)
    (while (and (not matches)
                (< prefix-pos prefix-len))
      (while (and (< prefix-pos prefix-len)
                  (< label-pos label-len))
        (if (equal (aref prefix prefix-pos) (aref label label-pos))
            (progn
              (unless start (setq start label-pos))
              (cl-incf prefix-pos))
          (when start
            (setq matches (nconc matches `((,start . ,label-pos))))
            (setq start nil)))
        (cl-incf label-pos))
      (when start (setq matches (nconc matches `((,start . ,label-pos)))))
      ;; Search again when the whole prefix is not matched
      (when (< prefix-pos prefix-len)
        (setq matches nil))
      ;; Start search from next offset of prefix to find a match with label
      (unless matches
        (cl-incf prefix-pos)
        (setq label-pos 0)))
    matches))

(defun lsp--capf-get-documentation (item)
  "Get doc comment for completion ITEM."
  (unless (get-text-property 0 'lsp-completion-resolved item)
    (let ((resolved-item
           (-some->> item
             (get-text-property 0 'lsp-completion-item)
             (lsp--resolve-completion)))
          (len (length item)))
      (put-text-property 0 len 'lsp-completion-item resolved-item item)
      (put-text-property 0 len 'lsp-completion-resolved t item)))
  (-some->> item
    (get-text-property 0 'lsp-completion-item)
    (gethash "documentation")
    (lsp--render-element)))

(defun lsp--capf-get-context (trigger-characters)
  "Get completion context with provided TRIGGER-CHARACTERS."
  (let* (trigger-char
         (trigger-kind (cond
                        ((setq trigger-char (lsp--looking-back-trigger-characterp
                                             trigger-characters))
                         'character)
                        ((equal lsp--capf-cache 'incomplete) 'incomplete)
                        (t 'invoked))))
    (ht<-alist
     (append
      `(("triggerKind" . ,(alist-get trigger-kind lsp--completion-trigger-kinds)))
      (when trigger-char `(("triggerCharacter" . ,trigger-char)))))))

(defun lsp-completion-at-point ()
  "Get lsp completions."
  (when (or (--some (lsp--client-completion-in-comments? (lsp--workspace-client it))
                    (lsp-workspaces))
            (not (nth 4 (syntax-ppss))))
    (let* ((trigger-chars (->> (lsp--server-capabilities)
                               (gethash "completionProvider")
                               (gethash "triggerCharacters")))
           (bounds-start (or (-some--> (car (bounds-of-thing-at-point 'symbol))
                               (save-excursion
                                 (ignore-errors
                                   (goto-char (+ it 1))
                                   (while (lsp--looking-back-trigger-characterp trigger-chars)
                                     (cl-incf it)
                                     (forward-char))
                                   it)))
                             (point)))
           result done?
           (all-completions
            (lambda ()
              (cond
               (done? result)
               ((and lsp--capf-cache
                     (listp lsp--capf-cache)
                     (s-prefix? (car lsp--capf-cache)
                                (buffer-substring-no-properties bounds-start (point)))
                     (< (caar (cadr lsp--capf-cache)) (point)))
                (apply #'lsp--capf-filter-candidates (cdr lsp--capf-cache)))
               (t
                (-let* ((resp (lsp-request-while-no-input
                               "textDocument/completion"
                               (plist-put (lsp--text-document-position-params)
                                          :context (lsp--capf-get-context trigger-chars))))
                        (completed (or (seqp resp)
                                       (not (gethash "isIncomplete" resp))))
                        (items (--> (cond
                                     ((seqp resp) resp)
                                     ((hash-table-p resp) (gethash "items" resp)))
                                    (if (or completed
                                            (seq-some (-rpartial #'lsp--ht-get "sortText") it))
                                        (lsp--sort-completions it)
                                      it)
                                    (-map (lambda (item)
                                            (puthash "_emacsStartPoint"
                                                     (lsp--capf-guess-prefix item bounds-start)
                                                     item)
                                            item)
                                          it)))
                        (markers (list bounds-start (copy-marker (point) t)))
                        (prefix (buffer-substring-no-properties bounds-start (point))))
                  (setf done? completed
                        lsp--capf-cache (cond
                                         ((and done? (not (seq-empty-p items)))
                                          (list (buffer-substring-no-properties bounds-start (point))
                                                (lsp--capf-cached-items items)
                                                :lsp-items nil
                                                :markers markers
                                                :prefix prefix))
                                         ((not done?) 'incomplete))
                        result (lsp--capf-filter-candidates (if done? (cadr lsp--capf-cache))
                                                            :lsp-items items
                                                            :markers markers
                                                            :prefix prefix))))))))
      (list
       bounds-start
       (point)
       (lambda (probe _pred action)
         (cond
          ;; metadata
          ((equal action 'metadata)
           `(metadata (category . lsp-capf)
                      (display-sort-function . identity)))
          ;; boundaries
          ((equal (car-safe action) 'boundaries) nil)
          ;; try-completion
          ((null action) (and (member probe (funcall all-completions)) t))
          ;; test-completion
          ((equal action 'lambda) (member probe (funcall all-completions)))
          ;; retrieve candidates
          (t (funcall all-completions))))
       :annotation-function #'lsp--annotate
       :company-require-match 'never
       :company-prefix-length
       (save-excursion
         (goto-char bounds-start)
         (and (lsp--looking-back-trigger-characterp trigger-chars) t))
       :company-match #'lsp--capf-company-match
       :company-doc-buffer (-compose #'company-doc-buffer
                                     #'lsp--capf-get-documentation)
       :exit-function
       (-rpartial #'lsp--capf-exit-fn trigger-chars)))))

(defun lsp--capf-exit-fn (candidate _status &optional trigger-chars)
  "Exit function of `completion-at-point'.
CANDIDATE is the selected completion item.
Others: TRIGGER-CHARS"
  (-let* (((&plist 'lsp-completion-item item
                   'lsp-completion-start-point start-point
                   'lsp-completion-markers markers
                   'lsp-completion-prefix prefix)
           (text-properties-at 0 candidate))
          ((&hash "label"
                  "insertText" insert-text
                  "textEdit" text-edit
                  "insertTextFormat" insert-text-format
                  "additionalTextEdits" additional-text-edits)
           (or item lsp--empty-ht)))
    (cond
     (text-edit
      (apply #'delete-region markers)
      (insert prefix)
      (lsp--apply-text-edit text-edit))
     ((or insert-text label)
      (apply #'delete-region markers)
      (insert prefix)
      (delete-region start-point (point))
      (insert (or insert-text label))))

    (when (eq insert-text-format 2)
      (let (yas-indent-line)
        (yas-expand-snippet
         (lsp--to-yasnippet-snippet (buffer-substring start-point (point)))
         start-point
         (point))))

    (when (and lsp-completion-enable-additional-text-edit additional-text-edits)
      (lsp--apply-text-edits additional-text-edits)))

  (lsp--capf-clear-cache)

  (when (and lsp-signature-auto-activate
             (lsp-feature? "textDocument/signatureHelp"))
    (lsp-signature-activate))

  (setq-local lsp-inhibit-lsp-hooks nil)

  (when (lsp--looking-back-trigger-characterp trigger-chars)
    (setq this-command 'self-insert-command)))

(advice-add #'completion-at-point :before #'lsp--capf-clear-cache)

(defun lsp--to-yasnippet-snippet (text)
  "Convert LSP snippet TEXT to yasnippet snippet."
  ;; LSP snippet doesn't escape "{", but yasnippet requires escaping it.
  (replace-regexp-in-string (rx (or bos (not (any "$" "\\"))) (group "{"))
                            (rx "\\" (backref 1))
                            text
                            nil nil 1))

(defun lsp--regex-fuzzy (str)
  "Build a regex sequence from STR. Insert .* between each char."
  (apply #'concat
         (cl-mapcar
          #'concat
          (cons "" (cdr (seq-map (lambda (c) (format "[^%c]*" c)) str)))
          (seq-map (lambda (c)
                     (format "\\(%s\\)" (regexp-quote (char-to-string c))))
                   str))))

(defvar lsp--fuzzy-score-case-sensitiveness 20
  "Case sensitiveness, can be in range of [0, inf).")

(defun lsp--fuzzy-score (query str)
  "Calculate fuzzy score for STR with query QUERY."
  (-when-let* ((md (cddr (or (get-text-property 0 'match-data str)
                             (let ((re (lsp--regex-fuzzy query)))
                               (when (string-match re str)
                                 (match-data))))))
               (start (pop md))
               (len (length str))
               ;; To understand how this works, consider these bad
               ;; ascii(tm) diagrams showing how the pattern "foo"
               ;; flex-matches "fabrobazo", "fbarbazoo" and
               ;; "barfoobaz":

               ;;      f abr o baz o
               ;;      + --- + --- +

               ;;      f barbaz oo
               ;;      + ------ ++

               ;;      bar foo baz
               ;;          +++

               ;; "+" indicates parts where the pattern matched.  A
               ;; "hole" in the middle of the string is indicated by
               ;; "-".  Note that there are no "holes" near the edges
               ;; of the string.  The completion score is a number
               ;; bound by ]0..1]: the higher the better and only a
               ;; perfect match (pattern equals string) will have
               ;; score 1.  The formula takes the form of a quotient.
               ;; For the numerator, we use the number of +, i.e. the
               ;; length of the pattern.  For the denominator, it
               ;; first computes
               ;;
               ;;     hole_i_contrib = 1 + (Li-1)^(1/tightness)
               ;;
               ;; , for each hole "i" of length "Li", where tightness
               ;; is given by `flex-score-match-tightness'.  The
               ;; final value for the denominator is then given by:
               ;;
               ;;    (SUM_across_i(hole_i_contrib) + 1) * len
               ;;
               ;; , where "len" is the string's length.
               (score-numerator 0)
               (score-denominator 0)
               (last-b 0)
               (q-ind 0)
               (update-score
                (lambda (a b)
                  "Update score variables given match range (A B)."
                  (setq score-numerator (+ score-numerator (- b a)))
                  (unless (= a len)
                    (setq score-denominator
                          (+ score-denominator
                             (if (= a last-b) 0
                               (+ 1
                                  (if (zerop last-b)
                                      (- 0 (expt 0.8 (- a last-b)))
                                    (expt (- a last-b 1)
                                          0.25))))
                             (if (equal (aref query q-ind) (aref str a))
                                 0
                               lsp--fuzzy-score-case-sensitiveness))))
                  (setq last-b b))))
    (funcall update-score start start)
    (while md
      (funcall update-score start (car md))
      (pop md)
      (setq start (pop md))
      (cl-incf q-ind))
    (funcall update-score len len)
    (unless (zerop len)
      (/ score-numerator (* len (1+ score-denominator)) 1.0))))

(defun lsp--sort-completions (completions)
  "Sort COMPLETIONS."
  (--sort (let ((left (gethash "sortText" it))
                (right (gethash "sortText" other)))
            (if (equal left right)
                (string-lessp (gethash "label" it) (gethash "label" other))
              (string-lessp left right)))
          completions))

(defun lsp--resolve-completion (item)
  "Resolve completion ITEM."
  (cl-assert item nil "Completion item must not be nil")
  (or (-first 'identity
              (condition-case _err
                  (lsp-foreach-workspace
                   (when (gethash "resolveProvider" (lsp--capability "completionProvider"))
                     (lsp-request "completionItem/resolve" item)))
                (error)))
      item))

(defun lsp--extract-line-from-buffer (pos)
  "Return the line pointed to by POS (a Position object) in the current buffer."
  (let* ((point (lsp--position-to-point pos))
         (inhibit-field-text-motion t))
    (save-excursion
      (goto-char point)
      (buffer-substring-no-properties (line-beginning-position)
                                      (line-end-position)))))

(defun lsp--xref-make-item (filename range)
  "Return a xref-item from a RANGE in FILENAME."
  (let* ((pos-start (gethash "start" range))
         (pos-end (gethash "end" range))
         (line (lsp--extract-line-from-buffer pos-start))
         (start (gethash "character" pos-start))
         (end (gethash "character" pos-end))
         (len (length line)))
    (add-face-text-property (max (min start len) 0)
                            (max (min end len) 0)
                            'highlight t line)
    ;; LINE is nil when FILENAME is not being current visited by any buffer.
    (xref-make (or line filename)
               (xref-make-file-location
                filename
                (lsp-translate-line (1+ (gethash "line" pos-start)))
                (lsp-translate-column (gethash "character" pos-start))))))

(defun lsp--locations-to-xref-items (locations)
  "Return a list of `xref-item' from Location[] or LocationLink[]."
  (setq locations (if (sequencep locations)
                      (append locations nil)
                    (list locations)))

  (cl-labels ((get-xrefs-in-file
               (file-locs location-link)
               (-let [(filename . matches) file-locs]
                 (condition-case err
                     (let ((visiting (lsp--buffer-for-file filename))
                           (fn (lambda (loc)
                                 (lsp-with-filename filename
                                   (lsp--xref-make-item filename
                                                        (if location-link
                                                            (or
                                                             (gethash "targetSelectionRange" loc)
                                                             (gethash "targetRange" loc))
                                                          (gethash "range" loc)))))))
                       (if visiting
                           (with-current-buffer visiting
                             (seq-map fn matches))
                         (when (file-readable-p filename)
                           (with-temp-buffer
                             (insert-file-contents-literally filename)
                             (seq-map fn matches)))))
                   (error (lsp-warn "Failed to process xref entry for filename '%s': %s"
                                    filename (error-message-string err)))
                   (file-error (lsp-warn "Failed to process xref entry, file-error, '%s': %s"
                                         filename (error-message-string err)))))))
    (apply #'append
           (if (-some->> locations (cl-first) (gethash "uri"))
               (->> locations
                    (seq-group-by (-compose #'lsp--uri-to-path
                                            (-partial #'gethash "uri")))
                    (seq-map (-rpartial #'get-xrefs-in-file nil)))
             (->> locations
                  (seq-group-by (-compose #'lsp--uri-to-path
                                          (-partial #'gethash "targetUri")))
                  (seq-map (-rpartial #'get-xrefs-in-file t)))))))

(defun lsp--make-reference-params (&optional td-position include-declaration)
  "Make a ReferenceParam object.
If TD-POSITION is non-nil, use it as TextDocumentPositionParams object instead.
If INCLUDE-DECLARATION is non-nil, request the server to include declarations."
  (let ((json-false :json-false))
    (plist-put (or td-position (lsp--text-document-position-params))
               :context `(:includeDeclaration ,(or include-declaration json-false)))))

(defun lsp--cancel-request (id)
  "Cancel request with ID in all workspaces."
  (lsp-foreach-workspace
   (->> lsp--cur-workspace lsp--workspace-client lsp--client-response-handlers (remhash id))
   (lsp-notify "$/cancelRequest" `(:id ,id))))

(defun lsp-eldoc-function ()
  "`lsp-mode' eldoc function."
  (run-hooks 'lsp-eldoc-hook)
  eldoc-last-message)

(eval-when-compile
  (defun dash-expand:&lsp-wks (key source)
    `(,(intern-soft (format "lsp--workspace-%s" (eval key) )) ,source))

  (defun dash-expand:&lsp-cln (key source)
    `(,(intern-soft (format "lsp--client-%s" (eval key) )) ,source)))

(defun lsp--point-on-highlight? ()
  (-some? (lambda (overlay)
            (overlay-get overlay 'lsp-highlight))
          (overlays-at (point))))

(defun lsp--cleanup-highlights-if-needed ()
  (when (and lsp-enable-symbol-highlighting
             lsp--have-document-highlights
             (not (lsp--point-on-highlight?)))
    (lsp--remove-overlays 'lsp-highlight)
    (setq lsp--have-document-highlights nil)
    (lsp-cancel-request-by-token :highlights)))

(defun lsp--document-highlight ()
  (unless (or (looking-at "[[:space:]\n]")
              (lsp--point-on-highlight?)
              (not lsp-enable-symbol-highlighting))
    (lsp-request-async "textDocument/documentHighlight"
                       (lsp--text-document-position-params)
                       #'lsp--document-highlight-callback
                       :mode 'tick
                       :cancel-token :highlights)))

(defun lsp-describe-thing-at-point ()
  "Display the type signature and documentation of the thing at
point."
  (interactive)
  (let ((contents (-some->> (lsp--text-document-position-params)
                    (lsp--make-request "textDocument/hover")
                    (lsp--send-request)
                    (gethash "contents"))))
    (if (and contents (not (equal contents "")))
        (let ((lsp-help-buf-name "*lsp-help*"))
          (with-current-buffer (get-buffer-create lsp-help-buf-name)
            (with-help-window lsp-help-buf-name
              (insert (string-trim-right (lsp--render-on-hover-content contents t))))))
      (lsp--info "No content at point."))))

(defun lsp--point-in-bounds-p (bounds)
  "Return whether the current point is within BOUNDS."
  (and (<= (car bounds) (point)) (< (point) (cdr bounds))))

(defun lsp-get-renderer (language)
  "Get renderer for LANGUAGE."
  (lambda (str)
    (lsp--render-string str language)))

(defun lsp--setup-markdown (mode)
  "Setup the ‘markdown-mode’ in the frame.
MODE is the mode used in the parent frame."
  (make-local-variable 'markdown-code-lang-modes)
  (dolist (mark (alist-get mode lsp-custom-markup-modes))
    (add-to-list 'markdown-code-lang-modes (cons mark mode)))
  (setq-local markdown-fontify-code-blocks-natively t)
  (setq-local markdown-fontify-code-block-default-mode mode)
  (setq-local markdown-hide-markup t)

  ;; Render some common HTML entities.
  ;; This should really happen in markdown-mode instead,
  ;; but it doesn't, so we do it here for now.
  (setq prettify-symbols-alist
        (cl-loop for i from 0 to 255
                 collect (cons (format "&#x%02X;" i) i)))
  (push '("&lt;" . ?<) prettify-symbols-alist)
  (push '("&gt;" . ?>) prettify-symbols-alist)
  (push '("&amp;" . ?&) prettify-symbols-alist)
  (push '("&nbsp;" . ? ) prettify-symbols-alist)
  (setq prettify-symbols-compose-predicate
        (lambda (_start _end _match) t))
  (prettify-symbols-mode 1))

(defun lsp--buffer-string-visible ()
  "Return visible buffer string.
Stolen from `org-copy-visible'."
  (let ((temp (generate-new-buffer " *temp*"))
        (beg (point-min))
        (end (point-max)))
    (while (/= beg end)
      (when (get-char-property beg 'invisible)
        (setq beg (next-single-char-property-change beg 'invisible nil end)))
      (let* ((next (next-single-char-property-change beg 'invisible nil end))
             (substring (buffer-substring beg next)))
        (with-current-buffer temp (insert substring))
        ;; (setq result (concat result substring))
        (setq beg next)))
    (setq deactivate-mark t)
    (prog1 (with-current-buffer temp
             (s-chop-suffix "\n" (buffer-string)))
      (kill-buffer temp))))

(defun lsp--render-markdown ()
  "Render markdown."

  (let((markdown-enable-math nil))
    ;; Temporary patch --- since the symbol is not rendered fine in lsp-ui
    ;; Anything that renders full-width disturbs the width calculation
    ;; of the resulting hover window.
    ;; See https://github.com/emacs-lsp/lsp-ui/issues/442
    (goto-char (point-min))
    (while (re-search-forward
            (rx (or
                 (seq bol (+ "-") eol)
                 (seq "\* \* \*" eol)))
            nil t)
      (replace-match ""))

    (goto-char (point-min))
    (while (re-search-forward
            (rx (and "\\" (group (or "\\" "`" "*" "_" ":" "/"
                                     "{" "}" "[" "]" "(" ")"
                                     "#" "+" "-" "." "!" "|"))))
            nil t)
      (replace-match (rx (backref 1))))

    ;; markdown-mode v2.3 does not yet provide gfm-view-mode
    (if (fboundp 'gfm-view-mode)
        (gfm-view-mode)
      (gfm-mode))

    (lsp--setup-markdown major-mode)))

(defvar lsp--display-inline-image-alist
  '((lsp--render-markdown
     (:regexp
      "!\\[.*?\\](data:image/[a-zA-Z]+;base64,\\([A-Za-z0-9+/\n]+?=*?\\)\\(|[^)]+\\)?)"
      :sexp
      (create-image
       (base64-decode-string
        (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
       nil t))))
  "Replaced string regexp and function returning image.
Each element should be:
(MODE . (PROPERTY-LIST...))
MODE (car) is function which is defined in `lsp-language-id-configuration'.
Cdr should be list of PROPERTY-LIST.

Each PROPERTY-LIST should have properties:
:regexp  Regexp which determines what string is relpaced to image.
         You should also get information of image, by parenthesis constructs.
         By default, all matched string is replaced to image, but you can
         change index of replaced string by keyword :replaced-index.

:sexp    Return image when evaluated. You can use information of regexp
         by using (match-beggining N), (match-end N) or (match-substring N).

In addition, each can have property:
:replaced-index  Determine index which is used to replace regexp to image.
                 The value means first argument of `match-beginning' and
                 `match-end'. If omitted, interpreted as index 0.")

(defcustom lsp-display-inline-image t
  "Showing inline image or not."
  :group 'lsp-mode
  :type 'boolean)

(defun lsp--display-inline-image (mode)
  "Add image property if available."
  (let ((plist-list (cdr (assq mode lsp--display-inline-image-alist))))
    (when (and (display-images-p) lsp-display-inline-image)
      (cl-loop
       for plist in plist-list
       with regexp with replaced-index
       do
       (setq regexp (plist-get plist :regexp))
       (setq replaced-index (or (plist-get plist :replaced-index) 0))

       (font-lock-remove-keywords nil (list regexp replaced-index))
       (let ((inhibit-read-only t))
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward regexp nil t)
             (set-text-properties
              (match-beginning replaced-index) (match-end replaced-index)
              nil)
             (add-text-properties
              (match-beginning replaced-index) (match-end replaced-index)
              `(display ,(eval (plist-get plist :sexp)))))))))))

(defun lsp--fontlock-with-mode (str mode)
  "Fontlock STR with MODE."
  (condition-case nil
      (with-temp-buffer
        (insert str)
        (delay-mode-hooks (funcall mode))
        (cl-flet ((window-body-width () lsp-window-body-width))
          (font-lock-ensure)
          (lsp--display-inline-image mode))
        (lsp--buffer-string-visible))
    (error str)))

(defun lsp--render-string (str language)
  "Render STR using `major-mode' corresponding to LANGUAGE.
When language is nil render as markup if `markdown-mode' is loaded."
  (setq str (or str ""))
  (if-let (mode (-some (-lambda ((mode . lang))
                         (when (and (equal lang language) (functionp mode))
                           mode))
                       lsp-language-id-configuration))
      (lsp--fontlock-with-mode str mode)
    (s-replace "\r" "" str)))

(defun lsp--render-element (content)
  "Render CONTENT element."
  (let ((inhibit-message t))
    (or
     (cond
      ;; MarkedString
      ((and (hash-table-p content)
            (gethash "language" content))
       (-let [(&hash "language" "value") content]
         (lsp--render-string value language)))

      ;; MarkupContent
      ((and (hash-table-p content)
            (gethash "kind" content))
       (-let [(&hash "value" "kind") content]
         (lsp--render-string value kind)))
      ;; plain string
      ((stringp content) (lsp--render-string content "markdown"))
      ((null content) "")
      (t (error "Failed to handle %s" content)))
     "")))

(defun lsp--select-action (actions)
  "Select an action to execute from ACTIONS."
  (cond
   ((seq-empty-p actions) (signal 'lsp-no-code-actions nil))
   ((and (eq (seq-length actions) 1) lsp-auto-execute-action)
    (lsp-seq-first actions))
   (t (let ((completion-ignore-case t))
        (lsp--completing-read "Select code action: "
                              (seq-into actions 'list)
                              (-lambda ((&hash "title" "command"))
                                (or title command))
                              nil t)))))

(defun lsp-join-region (beg end)
  "Apply join-line from BEG to END.
This function is useful when an indented function prototype needs
to be shown in a single line."
  (save-excursion
    (let ((end (copy-marker end)))
      (goto-char beg)
      (while (< (point) end)
        (join-line 1)))
    (s-trim (buffer-string))))

(defun lsp--workspace-server-id (workspace)
  "Return the server ID of WORKSPACE."
  (-> workspace lsp--workspace-client lsp--client-server-id))

(defun lsp--handle-rendered-for-echo-area (contents)
  "Return a single line from RENDERED, appropriate for display in the echo area."
  (pcase (lsp-workspaces)
    (`(,workspace)
     (lsp-clients-extract-signature-on-hover contents (lsp--workspace-server-id workspace)))
    ;; For projects with multiple active workspaces we also default to
    ;; render the first line.
    (_ (lsp-clients-extract-signature-on-hover contents nil))))

(cl-defgeneric lsp-clients-extract-signature-on-hover (contents _server-id)
  "Extract a representative line from CONTENTS, to show in the echo area."
  (car (s-lines (lsp--render-element contents))))

(defun lsp--render-on-hover-content (contents render-all)
  "Render the content received from 'document/onHover' request.
CONTENTS  - MarkedString | MarkedString[] | MarkupContent
RENDER-ALL - nil if only the signature should be rendered."
  (cond
   ((and (hash-table-p contents) (gethash "kind" contents))
    ;; MarkupContent.
    ;; It tends to be long and is not suitable to display fully in the echo area.
    ;; Just display the first line which is typically the signature.
    (if render-all
        (lsp--render-element contents)
      (lsp--handle-rendered-for-echo-area contents)))
   ((and (stringp contents) (not (string-match-p "\n" contents)))
    ;; If the contents is a single string containing a single line,
    ;; render it always.
    (lsp--render-element contents))
   (t
    ;; MarkedString -> MarkedString[]
    (when (or (hash-table-p contents) (stringp contents))
      (setq contents (list contents)))
    ;; Consider the signature consisting of the elements who have a renderable
    ;; "language" property. When render-all is nil, ignore other elements.
    (string-join
     (seq-map
      #'lsp--render-element
      (if render-all
          contents
        ;; Only render contents that have an available renderer.
        (seq-filter
         (-andfn 'hash-table-p
                 (-compose #'lsp-get-renderer (-partial 'gethash "language")))
         contents)))
     (if (bound-and-true-p page-break-lines-mode)
         "\n\n"
       "\n")))))



(defvar lsp-signature-mode-map
  (-doto (make-sparse-keymap)
    (define-key (kbd "M-n") #'lsp-signature-next)
    (define-key (kbd "M-p") #'lsp-signature-previous)
    (define-key (kbd "M-a") #'lsp-signature-toggle-full-docs)
    (define-key (kbd "C-c C-k") #'lsp-signature-stop))
  "Keymap for `lsp-signature-mode-map'")

(define-minor-mode lsp-signature-mode ""
  nil nil nil
  :keymap lsp-signature-mode-map
  :lighter ""
  :group 'lsp-mode)

(defun lsp-signature-stop ()
  "Stop showing current signature help."
  (interactive)
  (lsp-cancel-request-by-token :signature)
  (with-current-buffer (or lsp--signature-last-buffer (current-buffer))
    (remove-hook 'lsp-on-idle-hook #'lsp-signature t))
  (remove-hook 'post-command-hook #'lsp--signature-maybe-stop)
  (funcall lsp-signature-function nil)
  (lsp-signature-mode -1))

(defun lsp-lv-message (message)
  (if message
      (progn
        (setq lsp--signature-last-buffer (current-buffer))
        (let ((lv-force-update t))
          (lv-message message)))
    (lv-delete-window)))

(defun lsp--handle-signature-update (signature)
  (let ((message
         (if (string= 'cons (type-of signature))
             (mapconcat 'lsp--signature->message signature "\n")
           (lsp--signature->message signature))))
    (if (s-present? message)
        (funcall lsp-signature-function message)
      (lsp-signature-stop))))

(defun lsp--signature-maybe-stop ()
  (when (and lsp--signature-last-buffer
             (not (equal (current-buffer) lsp--signature-last-buffer)))
    (lsp-signature-stop)))

(defun lsp-signature-activate ()
  "Activate signature help.
It will show up only if current point has signature help."
  (interactive)
  (setq lsp--signature-last nil)
  (setq lsp--signature-last-index nil)
  (setq lsp--signature-last-buffer nil)
  (add-hook 'lsp-on-idle-hook #'lsp-signature nil t)
  (add-hook 'post-command-hook #'lsp--signature-maybe-stop)
  (lsp-signature-mode t))

(defun lsp-signature-next ()
  "Show next signature."
  (interactive)
  (when (and lsp--signature-last-index
             lsp--signature-last
             (< (1+ lsp--signature-last-index) (length (gethash "signatures" lsp--signature-last))))
    (setq lsp--signature-last-index (1+ lsp--signature-last-index))
    (funcall lsp-signature-function (lsp--signature->message lsp--signature-last))))

(defun lsp-signature-previous ()
  "Next signature."
  (interactive)
  (when (and lsp--signature-last-index
             lsp--signature-last
             (not (zerop lsp--signature-last-index)))
    (setq lsp--signature-last-index (1- lsp--signature-last-index))
    (funcall lsp-signature-function (lsp--signature->message lsp--signature-last))))

(defun lsp-signature-toggle-full-docs ()
  "Toggle full/partial signature documentation."
  (interactive)
  (let ((all? (not (numberp lsp-signature-doc-lines))))
    (setq lsp-signature-doc-lines (if all?
                                      (or (car-safe lsp-signature-doc-lines)
                                          20)
                                    (list lsp-signature-doc-lines))))
  (lsp-signature-activate))

(defun lsp--signature->message (signature-help)
  "Generate eldoc message from SIGNATURE-HELP response."
  (setq lsp--signature-last signature-help)

  (when (and signature-help (not (seq-empty-p (gethash "signatures" signature-help))))
    (-let* (((&hash "activeSignature" active-index
                    "activeParameter" active-parameter
                    "signatures")     signature-help)
            (active-index (or lsp--signature-last-index active-index 0))
            (_ (setq lsp--signature-last-index active-index))
            ((signature &as &hash? "label" "parameters") (seq-elt signatures active-index))
            (prefix (format "%s/%s%s"
                            (1+ active-index)
                            (length signatures)
                            (propertize "│ " 'face 'shadow)))
            (prefix-length (- (length prefix) 2))
            (method-docs (when
                             (and lsp-signature-render-documentation
                                  (or (not (numberp lsp-signature-doc-lines)) (< 0 lsp-signature-doc-lines)))
                           (let ((docs (lsp--render-element
                                        (gethash "documentation" signature))))
                             (when (s-present? docs)
                               (concat
                                (propertize (concat "\n"
                                                    (s-repeat prefix-length "─")
                                                    "┴─────────────────────────────────────────────────")
                                            'face 'shadow)
                                "\n"
                                (if (and (numberp lsp-signature-doc-lines)
                                         (> (length (s-lines docs)) lsp-signature-doc-lines))
                                    (concat (s-join "\n" (-take lsp-signature-doc-lines (s-lines docs)))
                                            (propertize "\nTruncated..." 'face 'highlight))
                                  docs)
                                (propertize (concat "\n"
                                                    (s-repeat prefix-length "─")
                                                    "──────────────────────────────────────────────────")
                                            'face 'shadow)))))))
      (when (and active-parameter (not (seq-empty-p parameters)))
        (-when-let* ((param (when (and (< -1 active-parameter (length parameters)))
                              (seq-elt parameters active-parameter)))
                     (selected-param-label (let ((label (gethash "label" param)))
                                             (if (stringp label) label (append label nil))))
                     (start (if (stringp selected-param-label)
                                (s-index-of selected-param-label label)
                              (cl-first selected-param-label)))
                     (end (if (stringp selected-param-label)
                              (+ start (length selected-param-label))
                            (cl-second selected-param-label))))
          (add-face-text-property start end 'eldoc-highlight-function-argument nil label)))
      (concat prefix label method-docs))))

(defun lsp-signature ()
  "Display signature info (based on `textDocument/signatureHelp')"
  (lsp-request-async "textDocument/signatureHelp"
                     (lsp--text-document-position-params)
                     #'lsp--handle-signature-update
                     :cancel-token :signature))


(defcustom lsp-overlay-document-color-char "■"
  "Display the char represent the document color in overlay"
  :type 'string
  :group 'lsp-mode)

;; color presentation
(defun lsp--color-create-interactive-command (color range)
  (lambda ()
    (interactive)
    (-let [(&hash? "textEdit" text-edit
                   "additionalTextEdits" additional-text-edits)
           (lsp--completing-read
            "Select color presentation: "
            (lsp-request
             "textDocument/colorPresentation"
             `(:textDocument ,(lsp--text-document-identifier)
                             :color ,color
                             :range ,range))
            (-lambda ((&hash "label")) label)
            nil
            t)]
      (lsp--apply-text-edit text-edit)
      (lsp--apply-text-edits additional-text-edits))))

(defun lsp--number->color (number)
  (let ((result (format "%x"
                        (round (* (or number 0) 255.0)))))
    (if (= 1 (length result))
        (concat "0" result)
      result)))

(defun lsp--document-color ()
  "Document color handler."
  (lsp-request-async
   "textDocument/documentColor"
   `(:textDocument ,(lsp--text-document-identifier))
   (lambda (result)
     (lsp--remove-overlays 'lsp-color)
     (seq-do
      (-lambda ((&hash "color" (color &as &hash "red" "green" "blue")
                       "range"))
        (-let* (((beg . end) (lsp--range-to-region range))
                (overlay (make-overlay beg end))
                (command (lsp--color-create-interactive-command color range)))
          (overlay-put overlay 'lsp-color t)
          (overlay-put overlay 'evaporate t)
          (overlay-put overlay
                       'before-string
                       (propertize
                        lsp-overlay-document-color-char
                        'face `((:foreground ,(format "#%s%s%s"
                                                      (lsp--number->color red)
                                                      (lsp--number->color green)
                                                      (lsp--number->color blue))))
                        'action command
                        'mouse-face 'lsp-lens-mouse-face
                        'local-map (-doto (make-sparse-keymap)
                                     (define-key [mouse-1] command))))))
      result))
   :mode 'tick
   :cancel-token :document-color-token))



;; hover

(defvar-local lsp--hover-saved-bounds nil)

(defun lsp-hover ()
  "Display hover info (based on `textDocument/signatureHelp')."
  (if (and lsp--hover-saved-bounds
           (lsp--point-in-bounds-p lsp--hover-saved-bounds))
      (lsp--eldoc-message lsp--eldoc-saved-message)
    (setq lsp--hover-saved-bounds nil
          lsp--eldoc-saved-message nil)
    (if (looking-at "[[:space:]\n]")
        (lsp--eldoc-message nil)
      (when (and lsp-eldoc-enable-hover (lsp--capability "hoverProvider"))
        (lsp-request-async
         "textDocument/hover"
         (lsp--text-document-position-params)
         (lambda (hover)
           (when hover
             (when-let (range (gethash "range" hover))
               (setq lsp--hover-saved-bounds (lsp--range-to-region range)))
             (-let* ((contents (gethash "contents" hover)))
               (lsp--eldoc-message (and contents
                                        (lsp--render-on-hover-content contents
                                                                      lsp-eldoc-render-all))))))
         :error-handler #'ignore
         :mode 'tick
         :cancel-token :eldoc-hover)))))



(defun lsp--find-action-handler (command)
  "Find action handler for particular COMMAND."
  (--some (-some->> it
            (lsp--workspace-client)
            (lsp--client-action-handlers)
            (gethash command))
          (lsp-workspaces)))

(defun lsp--text-document-code-action-params (&optional kind)
  "Code action params."
  (list :textDocument (lsp--text-document-identifier)
        :range (if (use-region-p)
                   (lsp--region-to-range (region-beginning) (region-end))
                 (lsp--region-to-range (point) (point)))
        :context `(:diagnostics ,(lsp-cur-line-diagnostics)
                                ,@(when kind (list :only (vector kind))))))

(defun lsp-code-actions-at-point (&optional kind)
  "Retrieve the code actions for the active region or the current line."
  (lsp-request "textDocument/codeAction" (lsp--text-document-code-action-params kind)))

(defun lsp-execute-code-action-by-kind (command-kind)
  "Execute code action by name."
  (if-let (action (->> (lsp-get-or-calculate-code-actions command-kind)
                       (-filter (-lambda ((&hash "kind"))
                                  (and kind (equal command-kind kind))))
                       lsp--select-action))
      (lsp-execute-code-action action)
    (signal 'lsp-no-code-actions '(command-kind))))

(defalias 'lsp-get-or-calculate-code-actions 'lsp-code-actions-at-point)

(defun lsp--execute-command (action)
  "Parse and execute a code ACTION represented as a Command LSP type."
  (-if-let* ((command (gethash "command" action))
             (action-handler (lsp--find-action-handler command)))
      (funcall action-handler action)
    (lsp--send-execute-command command (gethash "arguments" action))))

(lsp-defun lsp-execute-code-action ((action &as &CodeAction :command? :edit?))
  "Execute code action ACTION.
If ACTION is not set it will be selected from `lsp-code-actions'."
  (interactive (list (lsp--select-action (lsp-code-actions-at-point))))
  (when edit?
    (lsp--apply-workspace-edit edit?))

  (cond
   ((stringp command?) (lsp--execute-command action))
   ((hash-table-p command?) (lsp--execute-command command?))))

(defun lsp--make-document-formatting-params ()
  "Create document formatting params."
  `(:textDocument ,(lsp--text-document-identifier)
                  :options (:tabSize ,(if (bound-and-true-p c-buffer-is-cc-mode)
                                          c-basic-offset
                                        tab-width)
                                     :insertSpaces ,(if indent-tabs-mode :json-false t))))

(defun lsp-format-buffer ()
  "Ask the server to format this document."
  (interactive "*")
  (cond ((lsp-feature? "textDocument/formatting")
         (let ((edits (lsp-request "textDocument/formatting"
                                   (lsp--make-document-formatting-params))))
           (lsp--apply-text-edits edits)))
        ((lsp-feature? "textDocument/rangeFormatting")
         (save-restriction
           (widen)
           (lsp-format-region (point-min) (point-max))))
        (t (signal 'lsp-capability-not-supported (list "documentFormattingProvider")))))

(defun lsp-format-region (s e)
  "Ask the server to format the region, or if none is selected, the current line."
  (interactive "r")
  (lsp--apply-text-edits (lsp-request
                          "textDocument/rangeFormatting"
                          (lsp--make-document-range-formatting-params s e))))

(defun lsp-organize-imports ()
  "Perform the source.organizeImports code action, if available."
  (interactive)
  (condition-case nil
      (lsp-execute-code-action-by-kind "source.organizeImports")
    (lsp-no-code-actions
     (when (called-interactively-p 'any)
       (lsp--info "source.organizeImports action not available")))))

(defun lsp--make-document-range-formatting-params (start end)
  "Make DocumentRangeFormattingParams for selected region.
interface DocumentRangeFormattingParams {
    textDocument: TextDocumentIdentifier;
    range: Range;
    options: FormattingOptions;
}"
  (plist-put (lsp--make-document-formatting-params)
             :range (lsp--region-to-range start end)))

(defconst lsp--highlight-kind-face
  '((1 . lsp-face-highlight-textual)
    (2 . lsp-face-highlight-read)
    (3 . lsp-face-highlight-write)))

(defun lsp--remove-overlays (name)
  (save-restriction
    (widen)
    (remove-overlays (point-min) (point-max) name t)))

(defun lsp-document-highlight ()
  "Highlight all relevant references to the symbol under point."
  (interactive)
  (lsp--remove-overlays 'lsp-highlight) ;; clear any previous highlights
  (lsp--document-highlight))

(defun lsp--document-highlight-callback (highlights)
  "Create a callback to process the reply of a
'textDocument/documentHighlight' message for the buffer BUF.
A reference is highlighted only if it is visible in a window."
  (lsp--remove-overlays 'lsp-highlight)

  (let* ((wins-visible-pos (-map (lambda (win)
                                   (cons (1- (line-number-at-pos (window-start win)))
                                         (1+ (line-number-at-pos (window-end win)))))
                                 (get-buffer-window-list nil nil 'visible))))
    (setq lsp--have-document-highlights t)
    (-map
     (-lambda ((&hash "range" "kind"))
       (-let* (((start &as &hash "line" start-line) (gethash "start" range))
               ((end &as &hash "line" end-line) (gethash "end" range)))
         (-map
          (-lambda ((start-window . end-window))
            ;; Make the overlay only if the reference is visible
            (let ((start-point (lsp--position-to-point start))
                  (end-point (lsp--position-to-point end)))
              (when (and (> (1+ start-line) start-window)
                         (< (1+ end-line) end-window)
                         (not (and lsp-symbol-highlighting-skip-current
                                   (<= start-point (point) end-point))))
                (-doto (make-overlay start-point end-point)
                  (overlay-put 'face (cdr (assq (or kind 1) lsp--highlight-kind-face)))
                  (overlay-put 'lsp-highlight t)))))
          wins-visible-pos)))
     highlights)))

(defun lsp--build-face-map (identifiers faces category varname)
  (apply 'vector
         (mapcar (lambda (id)
                   (let ((maybe-face (cdr (assoc id faces))))
                     (when (and lsp-semantic-highlighting-warn-on-missing-face (not maybe-face))
                       (lsp-warn "No face has been associated to the %s '%s': consider adding a corresponding definition to %s"
                                 category id varname)) maybe-face)) identifiers)))

(defun lsp--semantic-tokens-initialize-workspace (workspace)
  (cl-assert workspace)
  (let* ((token-capabilities (gethash
                              "semanticTokensProvider"
                              (lsp--workspace-server-capabilities workspace)))
         (legend (gethash "legend" token-capabilities)))
    (setf (lsp--workspace-semantic-highlighting-faces workspace)
          (lsp--build-face-map (gethash "tokenTypes" legend)
                               lsp-semantic-token-faces
                               "semantic token"
                               "lsp-semantic-token-faces"))
    (setf (lsp--workspace-semantic-highlighting-modifier-faces workspace)
          (lsp--build-face-map (gethash "tokenModifiers" legend)
                               lsp-semantic-token-modifier-faces
                               "semantic token modifier"
                               "lsp-semantic-token-modifier-faces"))))

(defun lsp--semantic-tokens-request-update ()
  (lsp--semantic-tokens-request
   (when lsp--semantic-tokens-use-ranged-requests (cons (window-start) (window-end))) t))

(defun lsp--semantic-tokens-initialize-buffer (is-range-provider)
  (let* ((old-extend-region-functions font-lock-extend-region-functions)
         ;; make sure font-lock always fontifies entire lines (TODO: do we also have
         ;; to change some jit-lock-...-region functions/variables?)
         (new-extend-region-functions
          (if (memq 'font-lock-extend-region-wholelines old-extend-region-functions)
              old-extend-region-functions
            (cons 'font-lock-extend-region-wholelines old-extend-region-functions))))
    (setq lsp--semantic-tokens-use-ranged-requests is-range-provider)
    (setq font-lock-extend-region-functions new-extend-region-functions)
    (add-function :around (local 'font-lock-fontify-region-function) #'lsp--semantic-tokens-fontify)
    (add-hook 'lsp-on-change-hook #'lsp--semantic-tokens-request-update nil t)
    (lsp--semantic-tokens-request-update)
    (setq lsp--semantic-tokens-teardown
          (lambda ()
            (setq font-lock-extend-region-functions old-extend-region-functions)
            (remove-function (local 'font-lock-fontify-region-function)
                             #'lsp--semantic-tokens-fontify)
            (remove-hook 'lsp-on-change-hook #'lsp--semantic-tokens-request-update t)))))

(defun lsp--semantic-tokens-fontify (old-fontify-region beg end &optional loudly)
  ;; TODO: support multiple language servers per buffer?
  (let ((faces (seq-some #'lsp--workspace-semantic-highlighting-faces lsp--buffer-workspaces))
        (modifier-faces
         (when lsp-semantic-tokens-apply-modifiers
           (seq-some #'lsp--workspace-semantic-highlighting-modifier-faces lsp--buffer-workspaces))))
    (if (or (eq nil lsp--semantic-tokens-cache)
            (eq nil faces)
            ;; delay fontification until we have fresh tokens
            (not (= lsp--cur-version (gethash "documentVersion" lsp--semantic-tokens-cache))))
        '(jit-lock-bounds 0 . 0)
      (funcall old-fontify-region beg end loudly)
      (let* ((inhibit-field-text-motion t)
             (data (gethash "data" lsp--semantic-tokens-cache))
             (i0 0)
             (i-max (1- (length data)))
             (current-line 1)
             (line-delta)
             (column 0)
             (face)
             (line-start-pos)
             (line-min)
             (line-max-inclusive)
             (text-property-beg)
             (text-property-end))
        (save-mark-and-excursion
          (save-restriction
            (widen)
            (goto-char beg)
            (goto-char (line-beginning-position))
            (setq line-min (line-number-at-pos))
            (with-silent-modifications
              (goto-char end)
              (goto-char (line-end-position))
              (setq line-max-inclusive (line-number-at-pos))
              (forward-line (- line-min line-max-inclusive))
              (let ((skip-lines (- line-min current-line)))
                (while (and (<= i0 i-max) (< (aref data i0) skip-lines))
                  (setq skip-lines (- skip-lines (aref data i0)))
                  (setq i0 (+ i0 5)))
                (setq current-line (- line-min skip-lines)))
              (forward-line (- current-line line-min))
              (setq line-start-pos (point))
              (cl-loop
               for i from i0 to i-max by 5 do
               (setq line-delta (aref data i))
               (unless (= line-delta 0)
                 (forward-line line-delta)
                 (setq line-start-pos (point))
                 (setq column 0)
                 (setq current-line (+ current-line line-delta)))
               (setq column (+ column (aref data (1+ i))))
               (setq face (aref faces (aref data (+ i 3))))
               (setq text-property-beg (+ line-start-pos column))
               (setq text-property-end (+ text-property-beg (aref data (+ i 2))))
               (when face (put-text-property text-property-beg text-property-end 'face face))
               (cl-loop for i from 0 to (1- (length modifier-faces)) do
                        (when (and (aref modifier-faces i)
                                   (> 0 (logand (aref data (+ i 4)) (lsh 1 i))))
                          (add-face-text-property text-property-beg text-property-end
                                                  (aref modifier-faces i))))
               when (> current-line line-max-inclusive) return nil)))))
      (let ((token-region (gethash "region" lsp--semantic-tokens-cache)))
        (if token-region
            `(jit-lock-bounds ,(max beg (car token-region)) . ,(min end (cdr token-region)))
          `(jit-lock-bounds ,beg . ,end))))))

(defun lsp--semantic-tokens-request (region fontify-immediately)
  (let ((request-full-token-set
         (lambda (fontify-immediately)
           (when lsp--semantic-tokens-idle-timer
             (cancel-timer lsp--semantic-tokens-idle-timer))
           (setq lsp--semantic-tokens-idle-timer
                 (run-with-idle-timer
                  lsp-idle-delay
                  nil
                  (lambda () (lsp--semantic-tokens-request nil fontify-immediately)))))))
    (when lsp--semantic-tokens-idle-timer
      (cancel-timer lsp--semantic-tokens-idle-timer))
    (lsp-request-async
     (if region "textDocument/semanticTokens/range" "textDocument/semanticTokens")
     `(:textDocument ,(lsp--text-document-identifier)
       ,@(if region (list :range (lsp--region-to-range (car region) (cdr region))) '()))
     (lambda (response)
       (setq lsp--semantic-tokens-cache response)
       (puthash "documentVersion" lsp--cur-version lsp--semantic-tokens-cache)
       (puthash "region" region lsp--semantic-tokens-cache)
       (when fontify-immediately (font-lock-flush))
       ;; request full token set to improve fontification speed when scrolling
       (when region (funcall request-full-token-set nil)))
     :error-handler (lambda (&rest _) (funcall request-full-token-set t))
     :mode 'tick
     :cancel-token (format "semantic-tokens-%s" (lsp--buffer-uri)))))

(defconst lsp--symbol-kind
  '((1 . "File")
    (2 . "Module")
    (3 . "Namespace")
    (4 . "Package")
    (5 . "Class")
    (6 . "Method")
    (7 . "Property")
    (8 . "Field")
    (9 . "Constructor")
    (10 . "Enum")
    (11 . "Interface")
    (12 . "Function")
    (13 . "Variable")
    (14 . "Constant")
    (15 . "String")
    (16 . "Number")
    (17 . "Boolean")
    (18 . "Array")
    (19 . "Object")
    (20 . "Key")
    (21 . "Null")
    (22 . "Enum Member")
    (23 . "Struct")
    (24 . "Event")
    (25 . "Operator")
    (26 . "Type Parameter")))

(defun lsp--symbol-information-to-xref (symbol)
  "Return a `xref-item' from SYMBOL information."
  (let* ((location (gethash "location" symbol))
         (uri (gethash "uri" location))
         (range (gethash "range" location))
         (start (gethash "start" range)))
    (xref-make (format "[%s] %s"
                       (alist-get (gethash "kind" symbol) lsp--symbol-kind)
                       (gethash "name" symbol))
               (xref-make-file-location (lsp--uri-to-path uri)
                                        (1+ (gethash "line" start))
                                        (gethash "character" start)))))

(defun lsp--get-document-symbols ()
  "Get document symbols.

If the buffer has not been modified since symbols were last
retrieved, simply return the latest result.

Else, if the request was initiated by Imenu updating its menu-bar
entry, perform it asynchronously; i.e., give Imenu the latest
result and then force a refresh when a new one is available.

Else (e.g., due to interactive use of `imenu' or `xref'),
perform the request synchronously."
  (if (= (buffer-chars-modified-tick) lsp--document-symbols-tick)
      lsp--document-symbols
    (let ((method "textDocument/documentSymbol")
          (params `(:textDocument ,(lsp--text-document-identifier)))
          (tick (buffer-chars-modified-tick)))
      (if (not lsp--document-symbols-request-async)
          (prog1
              (setq lsp--document-symbols (lsp-request method params))
            (setq lsp--document-symbols-tick tick))
        (lsp-request-async method params
                           (lambda (document-symbols)
                             (setq lsp--document-symbols document-symbols
                                   lsp--document-symbols-tick tick)
                             (lsp--imenu-refresh))
                           :mode 'alive)
        lsp--document-symbols))))

(advice-add 'imenu-update-menubar :around
            (lambda (oldfun &rest r)
              (let ((lsp--document-symbols-request-async t))
                (apply oldfun r))))

(defun lsp--xref-backend () 'xref-lsp)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-lsp)))
  (propertize (or (thing-at-point 'symbol) "")
              'identifier-at-point t))

(defun lsp--xref-elements-index (symbols path)
  (-mapcat
   (-lambda ((&hash "name" "children" "selectionRange" (&hash? "start")
                    "location" (&hash? "range")))
     (cons (cons (concat path name)
                 (lsp--position-to-point (or start (gethash "start" range))))
           (lsp--xref-elements-index children (concat path name " / "))))
   symbols))

(defvar-local lsp--symbols-cache nil)

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-lsp)))
  (if (lsp--find-workspaces-for "textDocument/documentSymbol")
      (progn
        (setq lsp--symbols-cache (lsp--xref-elements-index
                                  (lsp--get-document-symbols) nil))
        lsp--symbols-cache)
    (list (propertize (or (thing-at-point 'symbol) "")
                      'identifier-at-point t))))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-lsp)) identifier)
  (save-excursion
    (unless (get-text-property 0 'identifier-at-point identifier)
      (goto-char (cl-rest (or (assoc identifier lsp--symbols-cache)
                              (user-error "Unable to find symbol %s in current document" identifier)))))
    (lsp--locations-to-xref-items (lsp-request "textDocument/definition"
                                               (lsp--text-document-position-params)))))

(cl-defmethod xref-backend-references ((_backend (eql xref-lsp)) identifier)
  (save-excursion
    (unless (get-text-property 0 'identifier-at-point identifier)
      (goto-char (cl-rest (or (assoc identifier lsp--symbols-cache)
                              (user-error "Unable to find symbol %s" identifier)))))
    (lsp--locations-to-xref-items (lsp-request "textDocument/references"
                                               (lsp--make-reference-params)))))

(cl-defmethod xref-backend-apropos ((_backend (eql xref-lsp)) pattern)
  (seq-map #'lsp--symbol-information-to-xref
           (lsp-request "workspace/symbol" `(:query ,pattern))))

(defun lsp--get-symbol-to-rename ()
  "Get symbol to rename and placeholder at point."
  (if (let ((rename-provider (or (lsp--capability "renameProvider")
                                 (-some-> (lsp--registered-capability "textDocument/rename")
                                   (lsp--registered-capability-options)))))
        (and (hash-table-p rename-provider)
             (gethash "prepareProvider" rename-provider)))
      (-when-let (response (lsp-request "textDocument/prepareRename"
                                        (lsp--text-document-position-params)))
        (-let* (((start . end) (lsp--range-to-region
                                (or (gethash "range" response) response)))
                (symbol (buffer-substring-no-properties start end))
                (placeholder (gethash "placeholder" response)))
          (cons symbol (or placeholder symbol))))
    (let ((symbol (thing-at-point 'symbol t)))
      (cons symbol symbol))))

(defun lsp-rename (newname)
  "Rename the symbol (and all references to it) under point to NEWNAME."
  (interactive (list (-when-let ((symbol . placeholder) (lsp--get-symbol-to-rename))
                       (read-string (format "Rename %s to: " symbol) placeholder nil symbol))))
  (unless newname
    (user-error "A rename is not valid at this position"))
  (let ((edits (lsp-request "textDocument/rename"
                            `(:textDocument ,(lsp--text-document-identifier)
                                            :position ,(lsp--cur-position)
                                            :newName ,newname))))
    (when edits
      (lsp--apply-workspace-edit edits))))

(defun lsp-show-xrefs (xrefs display-action references?)
  (unless (region-active-p) (push-mark nil t))
  (if (boundp 'xref-show-definitions-function)
      (with-no-warnings
        (xref-push-marker-stack)
        (funcall (if references? xref-show-xrefs-function xref-show-definitions-function)
                 (-const xrefs)
                 `((window . ,(selected-window))
                   (display-action . ,display-action))))
    (xref--show-xrefs xrefs display-action)))

(cl-defun lsp-find-locations (method &optional extra &key display-action references?)
  "Send request named METHOD and get cross references of the symbol under point.
EXTRA is a plist of extra parameters.
REFERENCES? t when METHOD returns references."
  (if-let ((loc (lsp-request method
                             (append (lsp--text-document-position-params) extra))))
      (lsp-show-xrefs (lsp--locations-to-xref-items loc) display-action references?)
    (message "Not found for: %s" (thing-at-point 'symbol t))))

(cl-defun lsp-find-declaration (&key display-action)
  "Find declarations of the symbol under point."
  (interactive)
  (lsp-find-locations "textDocument/declaration" nil :display-action display-action))

(cl-defun lsp-find-definition (&key display-action)
  "Find definitions of the symbol under point."
  (interactive)
  (lsp-find-locations "textDocument/definition" nil :display-action display-action))

(defun lsp-find-definition-mouse (click)
  "Click to start `lsp-find-definition' at clicked point."
  (interactive "e")
  (let* ((ec (event-start click))
         (p1 (posn-point ec))
         (w1 (posn-window ec)))
    (select-window w1)
    (goto-char p1)
    (lsp-find-definition)))

(cl-defun lsp-find-implementation (&key display-action)
  "Find implementations of the symbol under point."
  (interactive)
  (lsp-find-locations "textDocument/implementation" nil :display-action display-action))

(cl-defun lsp-find-references (&optional include-declaration &key display-action)
  "Find references of the symbol under point."
  (interactive)
  (lsp-find-locations "textDocument/references"
                      (list :context `(:includeDeclaration ,(or include-declaration json-false)))
                      :display-action display-action
                      :references? t))

(cl-defun lsp-find-type-definition (&key display-action)
  "Find type definitions of the symbol under point."
  (interactive)
  (lsp-find-locations "textDocument/typeDefinition" nil :display-action display-action))

(defalias 'lsp-find-custom #'lsp-find-locations)
(defalias 'lsp-goto-implementation #'lsp-find-implementation)
(defalias 'lsp-goto-type-definition #'lsp-find-type-definition)

(with-eval-after-load 'evil
  (evil-set-command-property 'lsp-find-definition :jump t)
  (evil-set-command-property 'lsp-find-implementation :jump t)
  (evil-set-command-property 'lsp-find-references :jump t)
  (evil-set-command-property 'lsp-find-type-definition :jump t))

(defun lsp--find-workspaces-for (msg-or-method)
  "Find all workspaces in the current that can handle MSG."
  (let ((method (if (stringp msg-or-method)
                    msg-or-method
                  (plist-get msg-or-method :method))))
    (-if-let (reqs (cdr (assoc method lsp-method-requirements)))
        (-let (((&plist :capability :check-command) reqs))
          (--filter
           (with-lsp-workspace it
             (or
              (when check-command (funcall check-command it))
              (when capability (lsp--capability capability))
              (lsp--registered-capability method)
              (and (not capability) (not check-command))))
           (lsp-workspaces)))
      (lsp-workspaces))))

(defalias 'lsp-feature? 'lsp--find-workspaces-for)

(cl-defmethod lsp-execute-command (_server command arguments)
  "Dispatch COMMAND execution."
  (lsp--execute-command (ht ("command" (symbol-name command))
                            ("arguments" arguments))))

(defun lsp--send-execute-command (command &optional args)
  "Create and send a 'workspace/executeCommand' message having command COMMAND and optional ARGS."
  (let ((params (if args
                    (list :command command :arguments args)
                  (list :command command))))
    (lsp-request "workspace/executeCommand" params)))

(defalias 'lsp-point-to-position #'lsp--point-to-position)
(defalias 'lsp-text-document-identifier #'lsp--text-document-identifier)
(defalias 'lsp-send-execute-command #'lsp--send-execute-command)
(defalias 'lsp-on-open #'lsp--text-document-did-open)
(defalias 'lsp-on-save #'lsp--text-document-did-save)

(defun lsp--set-configuration (settings)
  "Set the SETTINGS for the lsp server."
  (lsp-notify "workspace/didChangeConfiguration" `(:settings , settings)))

(defun lsp-current-buffer ()
  (or lsp--virtual-buffer
      (current-buffer)))

(defun lsp-buffer-live-p (buffer-id)
  (if-let (buffer-live (plist-get buffer-id :buffer-live?))
      (funcall buffer-live buffer-id)
    (buffer-live-p buffer-id)))

(defun lsp--on-set-visited-file-name (old-func &rest args)
  "Advice around function `set-visited-file-name'.

This advice sends textDocument/didClose for the old file and
textDocument/didOpen for the new file."
  (when lsp--cur-workspace
    (lsp--text-document-did-close t))
  (prog1 (apply old-func args)
    (when lsp--cur-workspace
      (lsp--text-document-did-open))))

(advice-add 'set-visited-file-name :around #'lsp--on-set-visited-file-name)

(defvar lsp--flushing-delayed-changes nil)

(defun lsp--send-no-wait (message proc)
  "Send MESSAGE to PROC without waiting for further output."

  (unless lsp--flushing-delayed-changes
    (let ((lsp--flushing-delayed-changes t))
      (lsp--flush-delayed-changes)))

  (condition-case err
      (process-send-string proc message)
    ('error (lsp--error "Sending to process failed with the following error: %s"
                        (error-message-string err)))))

(define-error 'lsp-parse-error
  "Error parsing message from language server" 'lsp-error)
(define-error 'lsp-unknown-message-type
  "Unknown message type" '(lsp-error lsp-parse-error))
(define-error 'lsp-unknown-json-rpc-version
  "Unknown JSON-RPC protocol version" '(lsp-error lsp-parse-error))
(define-error 'lsp-no-content-length
  "Content-Length header missing in message" '(lsp-error lsp-parse-error))
(define-error 'lsp-invalid-header-name
  "Invalid header name" '(lsp-error lsp-parse-error))

;;  id  method
;;   x    x     request
;;   x    .     response
;;   .    x     notification
(defun lsp--get-message-type (json-data)
  "Get the message type from JSON-DATA."
  (when (not (equal (gethash "jsonrpc" json-data "") "2.0"))
    (signal 'lsp-unknown-json-rpc-version (list (gethash "jsonrpc" json-data))))
  (if (gethash "id" json-data nil)
      (if (gethash "error" json-data nil)
          'response-error
        (if (gethash "method" json-data nil)
            'request
          'response))
    (if (gethash "method" json-data nil)
        'notification
      (signal 'lsp-unknown-message-type (list json-data)))))

(defconst lsp--default-notification-handlers
  (ht ("window/showMessage" #'lsp--window-show-message)
      ("window/logMessage" #'lsp--window-log-message)
      ("textDocument/publishDiagnostics" #'lsp--on-diagnostics)
      ("textDocument/diagnosticsEnd" #'ignore)
      ("textDocument/diagnosticsBegin" #'ignore)
      ("telemetry/event" #'ignore)
      ("$/progress" #'lsp--on-progress)))

(defun lsp--on-notification (workspace notification)
  "Call the appropriate handler for NOTIFICATION."
  (-let (((&hash "params" "method") notification)
         (client (lsp--workspace-client workspace)))
    (when lsp-print-io
      (lsp--log-entry-new (lsp--make-log-entry method nil params 'incoming-notif)
                          lsp--cur-workspace))
    (if-let (handler (or (gethash method (lsp--client-notification-handlers client))
                         (gethash method lsp--default-notification-handlers)))
        (funcall handler workspace params)
      (unless (string-prefix-p "$" method)
        (lsp-warn "Unknown notification: %s" method)))))

(lsp-defun lsp--build-workspace-configuration-response ((&ConfigurationParams :items))
  "Get section configuration.
PARAMS are the `workspace/configuration' request params"
  (->> items
       (-map (-lambda ((&ConfigurationItem :section?))
               (gethash section? (lsp-configuration-section section?))))
       (apply #'vector)))

(defun lsp--send-request-response (workspace recv-time request response)
  "Send the RESPONSE for REQUEST in WORKSPACE and log if needed."
  (-let* (((&RpcRequestResponse :params :method :id) request)
          (process (lsp--workspace-proc workspace))
          (response (lsp--make-response id response))
          (req-entry (and lsp-print-io
                          (lsp--make-log-entry method id params 'incoming-req)))
          (resp-entry (and lsp-print-io
                           (lsp--make-log-entry method id response 'outgoing-resp
                                                (/ (nth 2 (time-since recv-time)) 1000)))))
    ;; Send response to the server.
    (when lsp-print-io
      (lsp--log-entry-new req-entry workspace)
      (lsp--log-entry-new resp-entry workspace))
    (lsp--send-no-wait (lsp--make-message response) process)))

(defun lsp--on-request (workspace request)
  "Call the appropriate handler for REQUEST, and send the return value to the server.
WORKSPACE is the active workspace."
  (-let* ((recv-time (current-time))
          ((&hash "params" "method") request)
          (client (lsp--workspace-client workspace))
          handler
          (response (cond
                     ((setq handler (gethash method (lsp--client-request-handlers client) nil))
                      (funcall handler workspace params))
                     ((setq handler (gethash method (lsp--client-async-request-handlers client) nil))
                      (funcall handler workspace params
                               (-partial #'lsp--send-request-response
                                         workspace recv-time request))
                      'delay-response)
                     ((equal method "client/registerCapability")
                      (seq-doseq (reg (gethash "registrations" params))
                        (lsp--server-register-capability reg))
                      nil)
                     ((equal method "window/showMessageRequest")
                      (let ((choice (lsp--window-log-message-request params)))
                        `(:title ,choice)))
                     ((equal method "client/unregisterCapability")
                      (seq-doseq (unreg (gethash "unregisterations" params))
                        (lsp--server-unregister-capability unreg))
                      nil)
                     ((equal method "workspace/applyEdit")
                      (list :applied (condition-case err
                                         (prog1 t
                                           (lsp--apply-workspace-edit (gethash "edit" params)))
                                       (error
                                        (lsp--error "Failed to apply edits with message %s"
                                                    (error-message-string err))
                                        :json-false))))
                     ((equal method "workspace/configuration")
                      (with-lsp-workspace workspace (lsp--build-workspace-configuration-response params)))
                     ((equal method "workspace/workspaceFolders")
                      (let ((folders (or (-> workspace
                                             (lsp--workspace-client)
                                             (lsp--client-server-id)
                                             (gethash (lsp-session-server-id->folders (lsp-session))))
                                         (lsp-session-folders (lsp-session)))))
                        (->> folders
                             (-distinct)
                             (-map (lambda (folder)
                                     (list :uri (lsp--path-to-uri folder))))
                             (apply #'vector))))
                     ((equal method "window/workDoneProgress/create")
                      nil ;; no specific reply, no processing required
                      )
                     (t (lsp-warn "Unknown request method: %s" method) nil))))
    ;; Send response to the server.
    (unless (eq response 'delay-response)
      (lsp--send-request-response workspace recv-time request response))))

(defun lsp--error-string (err)
  "Format ERR as a user friendly string."
  (let ((code (gethash "code" err))
        (message (gethash "message" err)))
    (format "Error from the Language Server: %s (%s)"
            message
            (or (car (alist-get code lsp--errors)) "Unknown error"))))

(defun lsp--get-body-length (headers)
  (let ((content-length (cdr (assoc "Content-Length" headers))))
    (if content-length
        (string-to-number content-length)

      ;; This usually means either the server or our parser is
      ;; screwed up with a previous Content-Length
      (error "No Content-Length header"))))

(defun lsp--parse-header (s)
  "Parse string S as a LSP (KEY . VAL) header."
  (let ((pos (string-match "\:" s))
        key val)
    (unless pos
      (signal 'lsp-invalid-header-name (list s)))
    (setq key (substring s 0 pos)
          val (s-trim-left (substring s (+ 1 pos))))
    (when (equal key "Content-Length")
      (cl-assert (cl-loop for c across val
                          when (or (> c ?9) (< c ?0)) return nil
                          finally return t)
                 nil (format "Invalid Content-Length value: %s" val)))
    (cons key val)))

(defmacro lsp--read-json (str)
  "Read json string STR."
  (if (progn
        (require 'json)
        (fboundp 'json-parse-string))
      `(json-parse-string ,str
                          :object-type 'hash-table
                          :null-object nil
                          :false-object nil)
    `(let ((json-array-type 'vector)
           (json-object-type 'hash-table)
           (json-false nil))
       (json-read-from-string ,str))))

(defun lsp--log-request-time (server-id method id start-time before-send received-time after-parsed-time after-processed-time)
  (when lsp-print-performance
    (lsp-log "Perf> Request/Response
  ServerId: %s
  Request: %s (%s)
  Serialization took: %.06f
  ServerTime: %.06f
  Deserialization: %.06f
  CallbackTime: %s"
             server-id
             method
             id
             (float-time (time-subtract before-send start-time))
             (float-time (time-subtract received-time before-send))
             (float-time (time-subtract after-parsed-time received-time))
             (if after-processed-time
                 (format "%.06f" (float-time (time-subtract after-processed-time after-parsed-time)))
               "N/A"))))

(defun lsp--log-notification-performance (server-id json-data received-time after-parsed-time before-notification after-processed-time)
  (when lsp-print-performance
    (lsp-log "Perf> notification
  ServerId: %s
  Notification: %s
  Deserialization: %.06f
  Processing: %.06f "
             server-id
             (when json-data (gethash "method" json-data))
             (float-time (time-subtract after-parsed-time received-time))
             (float-time (time-subtract after-processed-time before-notification)))))

(defun lsp--parser-on-message (json-data workspace)
  "Called when the parser P read a complete MSG from the server."
  (with-demoted-errors "Error processing message %S."
    (with-lsp-workspace workspace
      (let* ((client (lsp--workspace-client workspace))
             (received-time (current-time))
             (server-id (lsp--client-server-id client))
             (after-parsed-time (current-time))
             (id (--when-let (gethash "id" json-data)
                   (if (stringp it) (string-to-number it) it)))
             (data (gethash "result" json-data)))
        (pcase (lsp--get-message-type json-data)
          ('response
           (cl-assert id)
           (-let [(callback _ method start-time before-send) (gethash id (lsp--client-response-handlers client))]
             (when lsp-print-io
               (lsp--log-entry-new
                (lsp--make-log-entry method id data 'incoming-resp
                                     (/ (nth 2 (time-since before-send)) 1000))
                workspace))
             (when callback
               (funcall callback (gethash "result" json-data))
               (remhash id (lsp--client-response-handlers client))
               (lsp--log-request-time server-id method id start-time before-send
                                      received-time after-parsed-time (current-time)))))
          ('response-error
           (cl-assert id)
           (-let [(_ callback method start-time before-send) (gethash id (lsp--client-response-handlers client))]
             (when lsp-print-io
               (lsp--log-entry-new
                (lsp--make-log-entry method id data 'incoming-resp
                                     (/ (nth 2 (time-since before-send)) 1000))
                workspace))
             (when callback
               (funcall callback (gethash "error" json-data))
               (remhash id (lsp--client-response-handlers client))
               (lsp--log-request-time server-id method id start-time before-send
                                      received-time after-parsed-time (current-time)))))
          ('notification
           (let ((before-notification (current-time)))
             (lsp--on-notification workspace json-data)
             (lsp--log-notification-performance
              server-id json-data received-time after-parsed-time before-notification (current-time))))
          ('request (lsp--on-request workspace json-data)))))))

(defun lsp--json-pretty-print (msg)
  "Convert json MSG string to pretty printed json string."
  (let ((json-encoding-pretty-print t))
    (json-encode (json-read-from-string msg))))

(defun lsp--create-filter-function (workspace)
  "Make filter for the workspace."
  (let ((body-received 0)
        leftovers body-length body chunk)
    (lambda (_proc input)
      (setf chunk (concat leftovers (encode-coding-string input 'utf-8 'nocopy)))
      (while (not (equal chunk ""))
        (if (not body-length)
            ;; Read headers
            (if-let (body-sep-pos (string-match-p "\r\n\r\n" chunk))
                ;; We've got all the headers, handle them all at once:
                (setf body-length (lsp--get-body-length
                                   (mapcar #'lsp--parse-header
                                           (split-string
                                            (substring chunk
                                                       (or (string-match-p "Content-Length" chunk)
                                                           (error "Unable to find Content-Length header."))
                                                       body-sep-pos)
                                            "\r\n")))
                      body-received 0
                      leftovers nil
                      chunk (substring chunk (+ body-sep-pos 4)))

              ;; Haven't found the end of the headers yet. Save everything
              ;; for when the next chunk arrives and await further input.
              (setf leftovers chunk
                    chunk ""))
          (let* ((chunk-length (string-bytes chunk))
                 (left-to-receive (- body-length body-received))
                 (this-body (if (< left-to-receive chunk-length)
                                (prog1 (substring chunk 0 left-to-receive)
                                  (setf chunk (substring chunk left-to-receive)))
                              (prog1 chunk
                                (setf chunk ""))))
                 (body-bytes (string-bytes this-body)))
            (push this-body body)
            (setf body-received (+ body-received body-bytes))
            (when (>= chunk-length left-to-receive)
              (lsp--parser-on-message
               (condition-case err
                   (let ((parsed-message (decode-coding-string
                                          (apply #'concat
                                                 (nreverse
                                                  (prog1 body
                                                    (setf leftovers nil
                                                          body-length nil
                                                          body-received nil
                                                          body nil)))) 'utf-8) ))
                     (lsp--read-json parsed-message))
                 (error
                  (lsp-warn "Failed to parse the following chunk:\n'''\n%s\n'''\nwith message %s"
                            (concat leftovers input)
                            err)))
               workspace))))))))

(defvar-local lsp--line-col-to-point-hash-table nil
  "Hash table with keys (line . col) and values that are either point positions or markers.")

(defun lsp--symbol-to-imenu-elem (sym)
  "Convert SYM to imenu element.

SYM is a SymbolInformation message.

Return a cons cell (full-name . start-point)."
  (let* ((start-point (ht-get lsp--line-col-to-point-hash-table
                              (lsp--get-line-and-col sym)))
         (name (gethash "name" sym))
         (container (gethash "containerName" sym)))
    (cons (if (and lsp-imenu-show-container-name container)
              (concat container lsp-imenu-container-name-separator name)
            name)
          start-point)))

(defun lsp--symbol-to-hierarchical-imenu-elem (sym)
  "Convert SYM to hierarchical imenu elements.

SYM is a DocumentSymbol message.

Return cons cell (\"symbol-name (symbol-kind)\" . start-point) if
SYM doesn't have any children. Otherwise return a cons cell with
an alist

  (\"symbol-name\" . ((\"(symbol-kind)\" . start-point)
                    cons-cells-from-children))"
  (let* ((start-point (ht-get lsp--line-col-to-point-hash-table
                              (lsp--get-line-and-col sym)))
         (name (gethash "name" sym)))
    (if (seq-empty-p (gethash "children" sym))
        (cons name start-point)
      (cons name
            (lsp--imenu-create-hierarchical-index (gethash "children" sym))))))

(defun lsp--symbol-filter (sym)
  "Determine if SYM is for the current document."
  ;; It's a SymbolInformation or DocumentSymbol, which is always in the current
  ;; buffer file.
  (-if-let ((&hash "location") sym)
      (not (eq (->> location
                    (gethash "uri")
                    (lsp--uri-to-path)
                    (lsp--buffer-for-file))
               (current-buffer)))))

(defun lsp--get-symbol-type (sym)
  "The string name of the kind of SYM."
  (-> (gethash "kind" sym)
      (assoc lsp--symbol-kind)
      (cdr)
      (or "Other")))

(defun lsp--get-line-and-col (sym)
  "Obtain the line and column corresponding to SYM."
  (-let* ((location (gethash "location" sym))
          (name-range (or (and location (gethash "range" location))
                          (gethash "selectionRange" sym)))
          (start (gethash "start" name-range))
          ((&hash "line" "character") start))
    (cons line character)))

(defun lsp--collect-lines-and-cols (symbols)
  "Return a sorted list ((line . col) ...) of the locations of SYMBOLS."
  (let ((stack (lsp--imenu-filter-symbols symbols))
        line-col-list)
    (while stack
      (let ((sym (pop stack)))
        (push (lsp--get-line-and-col sym) line-col-list)
        (unless (seq-empty-p (gethash "children" sym))
          (setf stack (nconc (lsp--imenu-filter-symbols (gethash "children" sym)) stack)))))
    (-sort #'lsp--line-col-comparator line-col-list)))

(defun lsp--convert-line-col-to-points-batch (line-col-list)
  "Convert a sorted list of positions from line-column
representation to point representation."
  (let ((line-col-to-point-map (ht-create))
        (inhibit-field-text-motion t)
        (curr-line 0))
    (lsp-save-restriction-and-excursion
      (goto-char (point-min))
      (cl-loop for (line . col) in line-col-list do
               (forward-line (- line curr-line))
               (setq curr-line line)
               (let ((line-end (line-end-position)))
                 (if (or (not col) (> col (- line-end (point))))
                     (goto-char line-end)
                   (forward-char col)))
               (ht-set! line-col-to-point-map (cons line col) (if imenu-use-markers
                                                                  (point-marker)
                                                                (point)))))
    line-col-to-point-map))

(cl-defun lsp--line-col-comparator ((l1 . c1) (l2 . c2))
  (or (< l1 l2)
      (and (= l1 l2)
           (cond ((and c1 c2)
                  (< c1 c2))
                 (c1 t)))))

(defun lsp--imenu-create-index ()
  "Create imenu index from document symbols."
  (let* ((filtered-symbols (lsp--imenu-filter-symbols (lsp--get-document-symbols)))
         (lsp--line-col-to-point-hash-table (-> filtered-symbols
                                                lsp--collect-lines-and-cols
                                                lsp--convert-line-col-to-points-batch)))
    (if (lsp--imenu-hierarchical-p filtered-symbols)
        (lsp--imenu-create-hierarchical-index filtered-symbols)
      (lsp--imenu-create-non-hierarchical-index filtered-symbols))))

(defun lsp--imenu-filter-symbols (symbols)
  "Filter out unsupported symbols from SYMBOLS."
  (seq-remove #'lsp--symbol-filter symbols))

(defun lsp--imenu-hierarchical-p (symbols)
  "Determine whether any element in SYMBOLS has children."
  (seq-some (-partial #'gethash "children") symbols))

(defun lsp--imenu-create-non-hierarchical-index (symbols)
  "Create imenu index for non-hierarchical SYMBOLS.

SYMBOLS are a list of DocumentSymbol messages.

Return a nested alist keyed by symbol names. e.g.

   ((\"SomeClass\" (\"(Class)\" . 10)
                 (\"someField (Field)\" . 20)
                 (\"someFunction (Function)\" . 25)
                 (\"SomeSubClass\" (\"(Class)\" . 30)
                                  (\"someSubField (Field)\" . 35))
    (\"someFunction (Function)\" . 40))"
  (seq-map (lambda (nested-alist)
             (cons (car nested-alist)
                   (seq-map #'lsp--symbol-to-imenu-elem (cdr nested-alist))))
           (seq-group-by #'lsp--get-symbol-type symbols)))

(defun lsp--imenu-create-hierarchical-index (symbols)
  "Create imenu index for hierarchical SYMBOLS.

SYMBOLS are a list of DocumentSymbol messages.

Return a nested alist keyed by symbol names. e.g.

   ((\"SomeClass\" (\"(Class)\" . 10)
                 (\"someField (Field)\" . 20)
                 (\"someFunction (Function)\" . 25)
                 (\"SomeSubClass\" (\"(Class)\" . 30)
                                  (\"someSubField (Field)\" . 35))
    (\"someFunction (Function)\" . 40))"
  (let ((symbols (lsp--imenu-filter-symbols symbols)))
    (seq-map #'lsp--symbol-to-hierarchical-imenu-elem
             (seq-sort #'lsp--imenu-symbol-lessp
                       (lsp--imenu-filter-symbols symbols)))))

(defun lsp--imenu-symbol-lessp (sym1 sym2)
  (let* ((compare-results (mapcar (lambda (method)
                                    (funcall (alist-get method lsp--imenu-compare-function-alist)
                                             sym1 sym2))
                                  lsp-imenu-sort-methods))
         (result (seq-find (lambda (result)
                             (not (= result 0)))
                           compare-results
                           0)))
    (and (numberp result) (< result 0))))

(defun lsp--imenu-compare-kind (sym1 sym2)
  "Compare SYM1 and SYM2 by kind."
  (let ((kind1 (gethash "kind" sym1))
        (kind2 (gethash "kind" sym2)))
    (- kind1 kind2)))

(defun lsp--imenu-compare-line-col (sym1 sym2)
  (if (lsp--line-col-comparator
       (lsp--get-line-and-col sym1)
       (lsp--get-line-and-col sym2))
      -1
    1))

(defun lsp--imenu-compare-name (sym1 sym2)
  "Compare SYM1 and SYM2 by name."
  (let* ((name1 (gethash "name" sym1))
         (name2 (gethash "name" sym2))
         (result (compare-strings name1 0 (length name1) name2 0 (length name2))))
    (if (numberp result) result 0)))

(defun lsp--imenu-refresh ()
  "Force Imenu to refresh itself."
  (imenu--menubar-select imenu--rescan-item))

(defun lsp-enable-imenu ()
  "Use lsp-imenu for the current buffer."
  (imenu--cleanup)
  (setq-local imenu-create-index-function #'lsp--imenu-create-index)
  (setq-local imenu-menubar-modified-tick -1)
  (setq-local imenu--index-alist nil)
  (when menu-bar-mode
    (lsp--imenu-refresh)))

(defun lsp-resolve-final-function (command)
  "Resolve final function COMMAND."
  (-let [command (if (functionp command) (funcall command) command)]
    (cl-etypecase command
      (list
       (cl-assert (seq-every-p (apply-partially #'stringp) command) nil
                  "Invalid command list")
       command)
      (string (list command)))))

(defun lsp-server-present? (final-command)
  "Check whether FINAL-COMMAND is present."
  ;; executable-find only gained support for remote checks after 27 release
  (or (and (cond
            ((not (file-remote-p default-directory))
             (executable-find (cl-first final-command)))
            ((version<= "27.0" emacs-version)
             (with-no-warnings (executable-find (cl-first final-command) (file-remote-p default-directory))))
            (t))
           (prog1 t
             (lsp-log "Command \"%s\" is present on the path." (s-join " " final-command))))
      (ignore (lsp-log "Command \"%s\" is not present on the path." (s-join " " final-command)))))

(defun lsp--value-to-string (value)
  "Convert VALUE to a string that can be set as value in an environment variable."
  (cond
   ((stringp value) value)
   ((booleanp value) (if value
                         "1"
                       "0"))
   ((and (sequencep value)
         (seq-every-p #'stringp value)) (string-join value ":"))
   (t (user-error "Only strings, booleans, and sequences of strings are supported as environment variables"))))

(defun lsp--compute-process-environment (environment-fn)
  "Append a list of KEY=VALUE from the alist ENVIRONMENT to `process-environment'.
Ignore non-boolean keys whose value is nil."
  (let ((environment (if environment-fn
                         (funcall environment-fn)
                       nil)))
    (-flatten (cons (cl-loop for (key . value) in environment
                             if (or (eval value)
                                    (eq (get value 'custom-type) 'boolean))
                             collect (concat key "=" (lsp--value-to-string
                                                      (eval value))))
                    process-environment))))

(defun lsp-stdio-connection (command &optional test-command)
  "Returns a connection property list using COMMAND.
COMMAND can be: A string, denoting the command to launch the
language server. A list of strings, denoting an executable with
its command line arguments. A function, that either returns a
string or a list of strings. In all cases, the launched language
server should send and receive messages on standard I/O.
TEST-COMMAND is a function with no arguments which returns
whether the command is present or not. When not specified
`lsp-mode' will check whether the first element of the list
returned by COMMAND is available via `executable-find'"
  (cl-check-type command (or string
                             function
                             (and list
                                  (satisfies (lambda (l)
                                               (seq-every-p (lambda (el)
                                                              (stringp el))
                                                            l))))))
  (list :connect (lambda (filter sentinel name environment-fn)
                   (let ((final-command (lsp-resolve-final-function command))
                         (process-name (generate-new-buffer-name name))
                         (process-environment
                          (lsp--compute-process-environment environment-fn)))
                     (let* ((stderr-buf (format "*%s::stderr*" process-name))
                            (proc (make-process
                                   :name process-name
                                   :connection-type 'pipe
                                   :buffer (format "*%s*" process-name)
                                   :coding 'no-conversion
                                   :command final-command
                                   :filter filter
                                   :sentinel sentinel
                                   :stderr stderr-buf
                                   :noquery t)))
                       (set-process-query-on-exit-flag proc nil)
                       (set-process-query-on-exit-flag (get-buffer-process stderr-buf) nil)
                       (with-current-buffer (get-buffer stderr-buf)
                         ;; Make the *NAME::stderr* buffer buffer-read-only, q to bury, etc.
                         (special-mode))
                       (cons proc proc))))
        :test? (or
                test-command
                (lambda () (-> command lsp-resolve-final-function lsp-server-present?)))))

(defun lsp--open-network-stream (host port name)
  "Open network stream to HOST:PORT.
  NAME will be passed to `open-network-stream'.
  RETRY-COUNT is the number of the retries.
  SLEEP-INTERVAL is the sleep interval between each retry."
  (let* ((retries 0)
         (sleep-interval 0.01)
         (number-of-retries (/ lsp-tcp-connection-timeout sleep-interval))
         connection)
    (while (and (not connection) (< retries number-of-retries))
      (condition-case err
          (setq connection (open-network-stream name nil host port :type 'plain))
        (file-error
         (let ((inhibit-message t))
           (lsp--warn "Failed to connect to %s:%s with error message %s"
                      host
                      port
                      (error-message-string err))
           (sleep-for sleep-interval)
           (cl-incf retries)))))
    (or connection (error "Port %s was never taken. Consider increasing `lsp-tcp-connection-timeout'." port))))

(defun lsp--find-available-port (host starting-port)
  "Find available port on HOST starting from STARTING-PORT."
  (let ((success nil)
        (port starting-port))
    (while (and (not success))
      (condition-case _err
          (progn
            (delete-process (open-network-stream "*connection-test*" nil host port :type 'plain))
            (cl-incf port))
        (file-error (setq success t))))
    port))

(defun lsp-tcp-connection (command-fn)
  "Returns a connection property list similar to `lsp-stdio-connection'.
COMMAND-FN can only be a function that takes a single argument, a
port number. It should return a command for launches a language server
process listening for TCP connections on the provided port."
  (cl-check-type command-fn function)
  (list
   :connect (lambda (filter sentinel name environment-fn)
              (let* ((host "localhost")
                     (port (lsp--find-available-port host (cl-incf lsp--tcp-port)))
                     (command (funcall command-fn port))
                     (final-command (if (consp command) command (list command)))
                     (_ (unless (executable-find (cl-first final-command))
                          (user-error (format "Couldn't find executable %s" (cl-first final-command)))))
                     (process-environment
                      (lsp--compute-process-environment environment-fn))
                     (proc (make-process :name name :connection-type 'pipe :coding 'no-conversion
                                         :command final-command :sentinel sentinel :stderr (format "*%s::stderr*" name) :noquery t))
                     (tcp-proc (lsp--open-network-stream host port (concat name "::tcp"))))

                ;; TODO: Same :noquery issue (see above)
                (set-process-query-on-exit-flag proc nil)
                (set-process-query-on-exit-flag tcp-proc nil)
                (set-process-filter tcp-proc filter)
                (cons tcp-proc proc)))
   :test? (lambda () (executable-find (cl-first (funcall command-fn 0))))))

(defalias 'lsp-tcp-server 'lsp-tcp-server-command)

(defun lsp-tcp-server-command (command-fn)
  "Create tcp server connection.
In this mode Emacs is TCP server and the language server connects
to it. COMMAND is function with one parameter(the port) and it
should return the command to start the LS server."
  (cl-check-type command-fn function)
  (list
   :connect (lambda (filter sentinel name environment-fn)
              (let* (tcp-client-connection
                     (tcp-server (make-network-process :name (format "*tcp-server-%s*" name)
                                                       :buffer (format "*tcp-server-%s*" name)
                                                       :family 'ipv4
                                                       :service 0
                                                       :sentinel (lambda (proc _string)
                                                                   (lsp-log "Language server %s is connected." name)
                                                                   (setf tcp-client-connection proc))
                                                       :server 't))
                     (port (process-contact tcp-server :service))
                     (final-command (funcall command-fn port))
                     (process-environment
                      (lsp--compute-process-environment environment-fn))
                     (cmd-proc (make-process :name name
                                             :connection-type 'pipe
                                             :coding 'no-conversion
                                             :command final-command
                                             :stderr (format "*tcp-server-%s*::stderr" name)
                                             :noquery t)))
                (let ((retries 0))
                  (while (and (not tcp-client-connection) (< retries 20))
                    (lsp--info "Waiting for connection for %s, retries: %s" name retries)
                    (sit-for 0.500)
                    (cl-incf retries)))

                (unless tcp-client-connection
                  (condition-case nil (delete-process tcp-server) (error))
                  (condition-case nil (delete-process cmd-proc) (error))
                  (error "Failed to create connection to %s on port %s" name port))
                (lsp--info "Successfully connected to %s" name)

                (set-process-query-on-exit-flag cmd-proc nil)
                (set-process-query-on-exit-flag tcp-client-connection  nil)
                (set-process-query-on-exit-flag tcp-server nil)

                (set-process-filter tcp-client-connection filter)
                (set-process-sentinel tcp-client-connection sentinel)
                (cons tcp-client-connection cmd-proc)))
   :test? (lambda () (executable-find (cl-first (funcall command-fn 0))))))

(defun lsp-tramp-connection (local-command &optional generate-error-file-fn)
  "Create LSP stdio connection named name.
LOCAL-COMMAND is either list of strings, string or function which
returns the command to execute."
  (list :connect (lambda (filter sentinel name environment-fn)
                   (let* ((final-command (lsp-resolve-final-function local-command))
                          ;; wrap with stty to disable converting \r to \n
                          (process-name (generate-new-buffer-name name))
                          (wrapped-command (append '("stty" "raw" ";")
                                                   final-command
                                                   (list
                                                    (concat "2>"
                                                            (or (when generate-error-file-fn
                                                                  (funcall generate-error-file-fn name))
                                                                (format "/tmp/%s-%s-stderr" name
                                                                        (cl-incf lsp--stderr-index)))))))
                          (process-environment
                           (lsp--compute-process-environment environment-fn)))
                     (let ((proc (apply 'start-file-process-shell-command process-name
                                        (format "*%s*" process-name) wrapped-command)))
                       (set-process-sentinel proc sentinel)
                       (set-process-filter proc filter)
                       (set-process-query-on-exit-flag proc nil)
                       (set-process-coding-system proc 'binary 'binary)
                       (cons proc proc))))
        :test? (lambda () (-> local-command lsp-resolve-final-function lsp-server-present?))))

(defun lsp--setup-company ()
  (add-hook 'company-completion-started-hook
            (lambda (&rest _)
              (setq-local lsp-inhibit-lsp-hooks t))
            nil
            t)
  (add-hook 'company-after-completion-hook
            (lambda (&rest _)
              (lsp--capf-clear-cache)
              (setq-local lsp-inhibit-lsp-hooks nil))
            nil
            t))

(defun lsp--auto-configure ()
  "Autoconfigure `company', `flycheck', `lsp-ui',  if they are installed."
  (when (functionp 'lsp-ui-mode)
    (lsp-ui-mode))

  (cond
   ((or
     (and (eq lsp-diagnostic-package :auto)
          (functionp 'flycheck-mode))
     (and (eq lsp-diagnostic-package :flycheck)
          (or (functionp 'flycheck-mode)
              (user-error
               "lsp-diagnostic-package is set to :flycheck but flycheck is not installed?")))
     ;; legacy
     (null lsp-diagnostic-package))
    (lsp-flycheck-enable))
   ((and (not (version< emacs-version "26.1"))
         (or (eq lsp-diagnostic-package :auto)
             (eq lsp-diagnostic-package :flymake)
             (eq lsp-diagnostic-package t)))
    (with-no-warnings
      (require 'flymake)
      (lsp--flymake-setup)))
   ((not (eq lsp-diagnostic-package :none))
    (lsp--warn "Unable to autoconfigure flycheck/flymake. The diagnostics won't be rendered.")))

  (cond
   ((and (functionp 'company-lsp)
         (not lsp-prefer-capf))
    (progn
      (company-mode 1)
      (add-to-list 'company-backends 'company-lsp)
      (setq-local company-backends (remove 'company-capf company-backends))))

   ((and (fboundp 'company-mode))
    (company-mode 1)
    (add-to-list 'company-backends 'company-capf)))

  ;; yas-snippet config
  (setq-local yas-inhibit-overlay-modification-protection t))

(defvar-local lsp--buffer-deferred nil
  "Whether buffer was loaded via `lsp-deferred'.")

(defun lsp--restart-if-needed (workspace)
  "Handler restart for WORKSPACE."
  (when (or (eq lsp-restart 'auto-restart)
            (eq (lsp--workspace-shutdown-action workspace) 'restart)
            (and (eq lsp-restart 'interactive)
                 (let ((query (format "Server %s exited with status %s. Do you want to restart it?"
                                      (lsp--workspace-print workspace)
                                      (process-status (lsp--workspace-proc workspace)))))
                   (y-or-n-p query))))
    (--each (lsp--workspace-buffers workspace)
      (when (lsp-buffer-live-p it)
        (lsp-with-current-buffer it
          (if lsp--buffer-deferred
              (lsp-deferred)
            (lsp--info "Restarting LSP in buffer %s" (buffer-name))
            (lsp)))))))

(defun lsp--update-key (table key fn)
  "Apply FN on value corresponding to KEY in TABLE."
  (let ((existing-value (gethash key table)))
    (if-let (new-value (funcall fn existing-value))
        (puthash key new-value table)
      (remhash key table))))



(defun lsp--process-sentinel (workspace process exit-str)
  "Create the sentinel for WORKSPACE."
  (unless (process-live-p process)
    (let* ((status (process-status process))
           (folder->workspaces (lsp-session-folder->servers (lsp-session)))
           (stderr (-> workspace lsp--workspace-proc process-name get-buffer)))

      (lsp--warn "%s has exited (%s)"
                 (process-name (lsp--workspace-proc workspace))
                 (string-trim-right exit-str))

      (with-lsp-workspace workspace
        ;; Clean workspace related data in each of the buffers
        ;; in the workspace.
        (--each (lsp--workspace-buffers workspace)
          (when (lsp-buffer-live-p it)
            (lsp-with-current-buffer it
              (setq lsp--buffer-workspaces (delete workspace lsp--buffer-workspaces))
              (lsp--uninitialize-workspace)
              (lsp--spinner-stop)
              (lsp--remove-overlays 'lsp-highlight))))

        ;; Cleanup session from references to the closed workspace.
        (--each (hash-table-keys folder->workspaces)
          (lsp--update-key folder->workspaces it (apply-partially 'delete workspace)))

        ;; Kill standard error buffer only if the process exited normally.
        ;; Leave it intact otherwise for debugging purposes.
        (when (and (eq status 'exit) (zerop (process-exit-status process)) (buffer-live-p stderr))
          (kill-buffer stderr)))

      (run-hook-with-args 'lsp-after-uninitialized-functions workspace)

      (if (eq (lsp--workspace-shutdown-action workspace) 'shutdown)
          (lsp--info "Workspace %s shutdown." (lsp--workspace-print workspace))
        (lsp--restart-if-needed workspace))
      (lsp--cleanup-hanging-watches))))

(defun lsp--start-workspace (session client-template root &optional initialization-options)
  "Create new workspace for CLIENT-TEMPLATE with project root ROOT.
INITIALIZATION-OPTIONS are passed to initialize function.
SESSION is the active session."
  (lsp--spinner-start)
  (-let* ((default-directory root)
          (client (copy-lsp--client client-template))
          (workspace (make-lsp--workspace
                      :root root
                      :client client
                      :status 'starting
                      :buffers (list (lsp-current-buffer))
                      :host-root (file-remote-p root)))
          ((&lsp-cln 'server-id 'environment-fn 'new-connection 'custom-capabilities
                     'multi-root 'initialized-fn) client)
          ((proc . cmd-proc) (funcall
                              (or (plist-get new-connection :connect)
                                  (user-error "Client %s is configured incorrectly" client))
                              (lsp--create-filter-function workspace)
                              (apply-partially #'lsp--process-sentinel workspace)
                              (format "%s" server-id)
                              environment-fn))
          (workspace-folders (gethash server-id (lsp-session-server-id->folders session))))
    (setf (lsp--workspace-proc workspace) proc
          (lsp--workspace-cmd-proc workspace) cmd-proc)

    ;; update (lsp-session-folder->servers) depending on whether we are starting
    ;; multi/single folder workspace
    (mapc (lambda (project-root)
            (->> session
                 (lsp-session-folder->servers)
                 (gethash project-root)
                 (cl-pushnew workspace)))
          (or workspace-folders (list root)))

    (with-lsp-workspace workspace
      (run-hooks 'lsp-before-initialize-hook)
      (lsp-request-async
       "initialize"
       (append
        (list :processId nil
              :rootPath (lsp-file-local-name (expand-file-name root))
              :clientInfo (list :name "emacs"
                                :version (emacs-version))
              :rootUri (lsp--path-to-uri root)
              :capabilities (lsp--client-capabilities custom-capabilities)
              :initializationOptions initialization-options
              :workDoneToken "1")
        (when lsp-server-trace
          (list :trace lsp-server-trace))
        (when multi-root
          (->> workspace-folders
               (-map (lambda (folder)
                       (list :uri (lsp--path-to-uri folder)
                             :name (f-filename folder))))
               (apply 'vector)
               (list :workspaceFolders))))
       (lambda (response)
         (unless response
           (lsp--spinner-stop)
           (signal 'lsp-empty-response-error (list "initialize")))

         (setf (lsp--workspace-server-capabilities workspace) (gethash "capabilities" response)
               (lsp--workspace-status workspace) 'initialized)

         (mapc #'lsp--semantic-tokens-initialize-workspace
               (lsp--find-workspaces-for "textDocument/semanticTokens"))

         (with-lsp-workspace workspace
           (lsp-notify "initialized" lsp--empty-ht))

         (when initialized-fn (funcall initialized-fn workspace))

         (->> workspace
              (lsp--workspace-buffers)
              (mapc (lambda (buffer)
                      (lsp-with-current-buffer buffer
                        (lsp--open-in-workspace workspace)))))

         (with-lsp-workspace workspace
           (run-hooks 'lsp-after-initialize-hook))
         (lsp--info "%s initialized successfully" (lsp--workspace-print workspace)))
       :mode 'detached))
    workspace))

(defun lsp--load-default-session ()
  "Load default session."
  (setq lsp--session (or (condition-case err
                             (lsp--read-from-file lsp-session-file)
                           (error (lsp--error "Failed to parse the session %s, starting with clean one."
                                              (error-message-string err))
                                  nil))
                         (make-lsp-session))))

(defun lsp-session ()
  "Get the session associated with the current buffer."
  (or lsp--session (setq lsp--session (lsp--load-default-session))))

(defun lsp--client-disabled-p (buffer-major-mode client)
  (seq-some
   (lambda (entry)
     (pcase entry
       ((pred symbolp) (eq entry client))
       (`(,mode . ,client-or-list)
        (and (eq mode buffer-major-mode)
             (if (listp client-or-list)
                 (memq client client-or-list)
               (eq client client-or-list))))))
   lsp-disabled-clients))


;; download server

(defcustom lsp-server-install-dir (expand-file-name
                                   (locate-user-emacs-file (f-join ".cache" "lsp")))
  "Directory in which the servers will be installed."
  :risky t
  :type 'directory
  :package-version '(lsp-mode . "6.3"))

(defvar lsp--dependencies (ht))

(defun lsp-dependency (name &rest definitions)
  "Used to specify a language server DEPENDENCY, the server
executable or other required file path. Typically, the
DEPENDENCY is found by locating it on the system path using
`executable-find'.

You can explicitly call lsp-dependency in your environment to
specify the absolute path to the DEPENDENCY. For example, the
typescript-language-server requires both the server and the
typescript compiler. If you've installed them in a team shared
read-only location, you can instruct lsp-mode to use them via

  (eval-after-load 'lsp-clients
    '(progn
       (lsp-dependency 'typescript-language-server `(:system ,tls-exe))
       (lsp-dependency 'typescript `(:system ,ts-js))))

where tls-exe is the absolute path to the typescript-language-server
executable and ts-js is the absolute path to the typescript compiler
JavaScript file, tsserver.js (the *.js is required for Windows)."
  (ht-set lsp--dependencies name definitions))

(defun lsp--server-binary-present? (client)
  (unless (equal (lsp--client-server-id client) 'lsp-pwsh)
    (condition-case ()
        (-some-> client lsp--client-new-connection (plist-get :test?) funcall)
      (error nil)
      (args-out-of-range nil))))

(defun lsp--download-status ()
  (-some--> #'lsp--client-download-in-progress?
    (lsp--filter-clients it)
    (-map (-compose #'symbol-name #'lsp--client-server-id) it)
    (format "%s" it)
    (propertize it 'face 'success)
    (format "Installing following servers: %s" it)
    (propertize it
                'local-map (make-mode-line-mouse-map
                            'mouse-1 (lambda ()
                                       (interactive)
                                       (switch-to-buffer (get-buffer-create  " *lsp-install*"))))
                'mouse-face 'highlight)))

(defun lsp--install-server-internal (client &optional update?)
  (setf (lsp--client-download-in-progress? client) t)
  (add-to-list 'global-mode-string '(t (:eval (lsp--download-status))))
  (cl-flet ((done
             (success? &optional error-message)
             (-let [(&lsp-cln 'server-id 'buffers) client]
               (setf (lsp--client-download-in-progress? client) nil
                     (lsp--client-buffers client) nil)
               (if success?
                   (lsp--info "Server %s downloaded, auto-starting in %s buffers." server-id
                              (length buffers))
                 (lsp--error "Server %s install process failed with the following error message: %s.
Check `*lsp-install*' and `*lsp-log*' buffer."
                             server-id
                             error-message))
               (seq-do
                (lambda (buffer)
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (setq global-mode-string (-remove-item '(t (:eval (lsp--download-status)))
                                                             global-mode-string))
                      (when success? (lsp)))))
                buffers)
               (unless (lsp--filter-clients #'lsp--client-download-in-progress?)
                 (setq global-mode-string (-remove-item '(t (:eval (lsp--download-status)))
                                                        global-mode-string))))))
    (lsp--info "Download %s started." (lsp--client-server-id client))
    (condition-case err
        (funcall
         (lsp--client-download-server-fn client)
         client
         (lambda () (done t))
         (lambda (msg) (done nil msg))
         update?)
      (error
       (done nil (error-message-string err))))))

(defun lsp-install-server (update?)
  "Interactively install server.
When prefix UPDATE? is t force installation even if the server is present."
  (interactive "P")
  (lsp--install-server-internal
   (lsp--completing-read
    "Select server to install: "
    (or (->> lsp-clients
             (ht-values)
             (-filter (-andfn
                       (-orfn (-not #'lsp--server-binary-present?)
                              (-const update?))
                       (-not #'lsp--client-download-in-progress?)
                       #'lsp--client-download-server-fn)))
        (user-error "There are no servers with automatic installation"))
    (-compose #'symbol-name #'lsp--client-server-id)
    nil
    t)
   update?))

(defun lsp-async-start-process (callback error-callback &rest command)
  "Start async process COMMAND with CALLBACK and ERROR-CALLBACK."
  (make-process
   :name (cl-first command)
   :command command
   :sentinel (lambda (proc _)
               (when (eq 'exit (process-status proc))
                 (if (zerop (process-exit-status proc))
                     (condition-case err
                         (funcall callback)
                       (error
                        (funcall error-callback (error-message-string err))))
                   (display-buffer " *lsp-install*")
                   (funcall error-callback
                            (format "Async process '%s' failed with exit code %d"
                                    (process-name proc) (process-exit-status proc))))))
   :stdout " *lsp-install*"
   :buffer " *lsp-install*"
   :noquery t))


(defvar lsp-deps-providers
  (list :npm (list :path #'lsp--npm-dependency-path
                   :install #'lsp--npm-dependency-download)
        :system (list :path #'lsp--system-path)))

(defun lsp--system-path (path)
  "If PATH is absolute and exists return it as is. Otherwise,
return the absolute path to the executable defined by PATH or
nil."
  ;; For node.js 'sub-packages' PATH may point to a *.js file. Consider the
  ;; typescript-language-server. When lsp invokes the server, lsp needs to
  ;; supply the path to the typescript compiler, tsserver.js, as an argument. To
  ;; make code platform independent, one must pass the absolute path to the
  ;; tsserver.js file (Windows requires a *.js file - see help on the JavaScript
  ;; child process spawn command that is invoked by the
  ;; typescript-language-server). This is why we check for existence and not
  ;; that the path is executable.
  (if (and (f-absolute? path)
           (f-exists? path))
      path
    (executable-find path)))

(defun lsp-package-path (dependency)
  "Path to the DEPENDENCY each of the registered providers."
  (let (path)
    (-first (-lambda ((provider . rest))
              (setq path (-some-> lsp-deps-providers
                           (plist-get provider)
                           (plist-get :path)
                           (apply rest))))
            (gethash dependency lsp--dependencies))
    path))

(defun lsp-package-ensure (dependency callback error-callback)
  "Asynchronously ensure a package."
  (or (-first (-lambda ((provider . rest))
                (-some-> lsp-deps-providers
                  (plist-get provider)
                  (plist-get :install)
                  (apply (cl-list* callback error-callback rest))))
              (gethash dependency lsp--dependencies))
      (funcall error-callback (format "Unable to find a way to install %s" dependency))))


;; npm handling

;; https://docs.npmjs.com/files/folders#executables
(cl-defun lsp--npm-dependency-path (&key package path &allow-other-keys)
  "Return npm dependency PATH for PACKAGE."
  (let ((path (executable-find
               (f-join lsp-server-install-dir "npm" package
                       (cond ((eq system-type 'windows-nt) "")
                             (t "bin"))
                       path))))
    (unless (and path (f-exists? path))
      (error "The package %s is not installed.  Unable to find %s" package path))
    path))

(cl-defun lsp--npm-dependency-download  (callback error-callback &key package &allow-other-keys)
  (if-let (npm-binary (executable-find "npm"))
      (lsp-async-start-process callback
                               error-callback
                               npm-binary
                               "-g"
                               "--prefix"
                               (f-join lsp-server-install-dir "npm" package)
                               "install"
                               package)
    (lsp-log "Unable to install %s via `npm' because it is not present" package)
    nil))


(defun lsp--matching-clients? (client)
  (and
   ;; both file and client remote or both local
   (eq (---truthy? (file-remote-p buffer-file-name))
       (---truthy? (lsp--client-remote? client)))

   ;; activation function or major-mode match.
   (if-let (activation-fn (lsp--client-activation-fn client))
       (funcall activation-fn (buffer-file-name) major-mode)
     (-contains? (lsp--client-major-modes client) major-mode))

   ;; check whether it is enabled if `lsp-enabled-clients' is not null
   (or (null lsp-enabled-clients)
       (or (member (lsp--client-server-id client) lsp-enabled-clients)
           (ignore (lsp--info "Client %s is not in lsp-enabled-clients"
                              (lsp--client-server-id client)))))

   ;; check whether it is not disabled.
   (not (lsp--client-disabled-p major-mode (lsp--client-server-id client)))))

(defun lsp--filter-clients (pred)
  (->> lsp-clients hash-table-values (-filter pred)))

(defun lsp--find-clients ()
  "Find clients which can handle BUFFER-MAJOR-MODE.
SESSION is the currently active session. The function will also
pick only remote enabled clients in case the FILE-NAME is on
remote machine and vice versa."
  (-when-let (matching-clients (lsp--filter-clients (-andfn #'lsp--matching-clients?
                                                            #'lsp--server-binary-present?)))
    (lsp-log "Found the following clients for %s: %s"
             buffer-file-name
             (s-join ", "
                     (-map (lambda (client)
                             (format "(server-id %s, priority %s)"
                                     (lsp--client-server-id client)
                                     (lsp--client-priority client)))
                           matching-clients)))
    (-let* (((add-on-clients main-clients) (-separate #'lsp--client-add-on? matching-clients))
            (selected-clients (if-let (main-client (and main-clients
                                                        (--max-by (> (lsp--client-priority it)
                                                                     (lsp--client-priority other))
                                                                  main-clients)))
                                  (cons main-client add-on-clients)
                                add-on-clients)))
      (lsp-log "The following clients were selected based on priority: %s"
               (s-join ", "
                       (-map (lambda (client)
                               (format "(server-id %s, priority %s)"
                                       (lsp--client-server-id client)
                                       (lsp--client-priority client)))
                             selected-clients)))
      selected-clients)))

(defun lsp-register-client (client)
  "Registers LSP client CLIENT."
  (cl-assert (symbolp (lsp--client-server-id client)) t)
  (cl-assert (or
              (functionp (lsp--client-activation-fn client))
              (and (listp (lsp--client-major-modes client))
                   (seq-every-p (apply-partially #'symbolp)
                                (lsp--client-major-modes client))))
             nil "Invalid activation-fn and/or major-modes.")
  (puthash (lsp--client-server-id client) client lsp-clients))

(defun lsp--create-initialization-options (_session client)
  "Create initialization-options from SESSION and CLIENT.
Add workspace folders depending on server being multiroot and
session workspace folder configuration for the server."
  (let* ((initialization-options-or-fn (lsp--client-initialization-options client)))
    (if (functionp initialization-options-or-fn)
        (funcall initialization-options-or-fn)
      initialization-options-or-fn)))

(defun lsp--plist-delete (prop plist)
  "Delete by side effect the property PROP from PLIST.
If PROP is the first property in PLIST, there is no way
to remove it by side-effect; therefore, write
\(setq foo (evil-plist-delete :prop foo)) to be sure of
changing the value of `foo'."
  (let ((tail plist) elt head)
    (while tail
      (setq elt (car tail))
      (cond
       ((eq elt prop)
        (setq tail (cdr (cdr tail)))
        (if head
            (setcdr (cdr head) tail)
          (setq plist tail)))
       (t
        (setq head tail
              tail (cdr (cdr tail))))))
    plist))

(defvar lsp-client-settings nil)

(defun lsp--compare-setting-path (a b)
  (equal (car a) (car b)))

(defun lsp-register-custom-settings (props)
  "Register PROPS.
PROPS is list of triple (path value boolean?) where PATH is the path to the
property; VALUE can be a literal value, symbol to be evaluated, or either a
function or lambda function to be called without arguments; BOOLEAN? is an
optional flag that should be non-nil for boolean settings, when it is nil the
property will be ignored if the VALUE is nil."
  (let ((-compare-fn #'lsp--compare-setting-path))
    (setq lsp-client-settings (-uniq (append props lsp-client-settings)))))

(defun lsp-region-text (region)
  "Get the text for REGION in current buffer."
  (-let (((start . end) (lsp--range-to-region region)))
    (buffer-substring-no-properties start end)))

(defun lsp-ht-set (tbl paths value)
  "Set nested hash table value.
TBL - a hash table, PATHS is the path to the nested VALUE."
  (pcase paths
    (`(,path) (ht-set! tbl path value))
    (`(,path . ,rst) (let ((nested-tbl (or (gethash path tbl)
                                           (let ((temp-tbl (ht)))
                                             (ht-set! tbl path temp-tbl)
                                             temp-tbl))))
                       (lsp-ht-set nested-tbl rst value)))))

(defun lsp-configuration-section (section)
  "Get settings for SECTION."
  (let ((ret (ht-create)))
    (mapc (-lambda ((path variable boolean?))
            (when (s-matches? (concat section "\\..*") path)
              (let* ((symbol-value (if (symbolp variable)
                                       (if (fboundp variable)
                                           (funcall variable)
                                         (symbol-value variable))
                                     (if (functionp variable)
                                         (funcall variable) variable)))
                     (value (if (and boolean? (not symbol-value))
                                :json-false
                              symbol-value)))
                (when (or boolean? value)
                  (lsp-ht-set ret (s-split "\\." path) value)))))
          lsp-client-settings)
    ret))

(defun lsp--start-connection (session client project-root)
  "Initiates connection created from CLIENT for PROJECT-ROOT.
SESSION is the active session."
  (when (lsp--client-multi-root client)
    (cl-pushnew project-root (gethash (lsp--client-server-id client)
                                      (lsp-session-server-id->folders session))))
  (run-hook-with-args 'lsp-workspace-folders-changed-functions (list project-root) nil)

  (unwind-protect
      (lsp--start-workspace session client project-root (lsp--create-initialization-options session client))
    (lsp--spinner-stop)))

;; lsp-log-io-mode

(defvar lsp-log-io-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'lsp-log-io-next)
    (define-key map (kbd "M-p") #'lsp-log-io-prev)
    (define-key map (kbd "k") #'lsp--erase-log-buffer)
    (define-key map (kbd "K") #'lsp--erase-session-log-buffers)
    map)
  "Keymap for lsp log buffer mode.")

(define-derived-mode lsp-log-io-mode special-mode "LspLogIo"
  "Special mode for viewing IO logs.")

(defun lsp-workspace-show-log (workspace)
  "Display the log buffer of WORKSPACE."
  (interactive
   (list (if lsp-print-io
             (if (eq (length (lsp-workspaces)) 1)
                 (cl-first (lsp-workspaces))
               (lsp--completing-read "Workspace: " (lsp-workspaces)
                                     #'lsp--workspace-print nil t))
           (user-error "IO logging is disabled"))))
  (pop-to-buffer (lsp--get-log-buffer-create workspace)))

(defalias 'lsp-switch-to-io-log-buffer 'lsp-workspace-show-log)

(defun lsp--get-log-buffer-create (workspace)
  "Return the lsp log buffer of WORKSPACE, creating a new one if needed."
  (let ((server-id (-> workspace lsp--workspace-client lsp--client-server-id symbol-name))
        (pid (format "%s" (process-id (lsp--workspace-cmd-proc workspace)))))
    (get-buffer-create (format "*lsp-log: %s:%s*" server-id pid))))

(defun lsp--erase-log-buffer (&optional all)
  "Delete contents of current lsp log buffer.
When ALL is t, erase all log buffers of the running session."
  (interactive)
  (let* ((workspaces (lsp--session-workspaces (lsp-session)))
         (current-log-buffer (current-buffer)))
    (dolist (w workspaces)
      (let ((b (lsp--get-log-buffer-create w)))
        (when (or all (eq b current-log-buffer))
          (with-current-buffer b
            (let ((inhibit-read-only t))
              (erase-buffer))))))))

(defun lsp--erase-session-log-buffers ()
  "Erase log buffers of the running session."
  (interactive)
  (lsp--erase-log-buffer t))

(defun lsp-log-io-next (arg)
  "Move to next log entry."
  (interactive "P")
  (ewoc-goto-next lsp--log-io-ewoc (or arg 1)))

(defun lsp-log-io-prev (arg)
  "Move to previous log entry."
  (interactive "P")
  (ewoc-goto-prev lsp--log-io-ewoc (or arg 1)))

(defun lsp--workspace-print (workspace)
  "Visual representation WORKSPACE."
  (let* ((proc (lsp--workspace-cmd-proc workspace))
         (status (lsp--workspace-status workspace))
         (server-id (-> workspace lsp--workspace-client lsp--client-server-id symbol-name (propertize 'face 'bold-italic)))
         (pid (propertize (format "%s" (process-id proc)) 'face 'italic)))

    (if (eq 'initialized status)
        (format "%s:%s" server-id pid)
      (format "%s:%s status:%s" server-id pid status))))

(defun lsp--map-tree-widget (m)
  "Build `tree-widget' from a hash-table M."
  (when (hash-table-p m)
    (let (nodes)
      (maphash (lambda (k v)
                 (push `(tree-widget
                         :tag ,(if (hash-table-p v)
                                   (format "%s:" k)
                                 (format "%s: %s" k
                                         (propertize (format "%s" v)
                                                     'face
                                                     'font-lock-string-face)))
                         :open t
                         ,@(lsp--map-tree-widget v))
                       nodes))
               m)
      nodes)))

(defun lsp-buffer-name (buffer-id)
  (if-let (buffer-name (plist-get buffer-id :buffer-name))
      (funcall buffer-name buffer-id)
    (buffer-name buffer-id)))

(defun lsp--render-workspace (workspace)
  "Tree node representation of WORKSPACE."
  `(tree-widget :tag ,(lsp--workspace-print workspace)
                :open t
                (tree-widget :tag ,(propertize "Buffers" 'face 'font-lock-function-name-face)
                             :open t
                             ,@(->> workspace
                                    (lsp--workspace-buffers)
                                    (--map `(tree-widget
                                             :tag ,(when (lsp-buffer-live-p it)
                                                     (let ((buffer-name (lsp-buffer-name it)))
                                                       (if (lsp-with-current-buffer it buffer-read-only)
                                                           (propertize buffer-name 'face 'font-lock-constant-face)
                                                         buffer-name)))))))
                (tree-widget :tag ,(propertize "Capabilities" 'face 'font-lock-function-name-face)
                             ,@(-> workspace lsp--workspace-server-capabilities lsp--map-tree-widget))))

(define-derived-mode lsp-browser-mode special-mode "LspBrowser"
  "Define mode for displaying lsp sessions."
  (setq-local display-buffer-base-action '(nil . ((inhibit-same-window . t)))))

(defun lsp-describe-session ()
  "Describes current `lsp-session'."
  (interactive)
  (let ((session (lsp-session))
        (buf (get-buffer-create "*lsp session*")))
    (with-current-buffer buf
      (lsp-browser-mode)
      (cursor-sensor-mode 1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (--each (lsp-session-folders session)
          (widget-create
           `(tree-widget
             :tag ,(propertize it 'face 'font-lock-keyword-face)
             :open t
             ,@(->> session
                    (lsp-session-folder->servers)
                    (gethash it)
                    (-map 'lsp--render-workspace)))))))
    (pop-to-buffer buf)))

(defun lsp--session-workspaces (session)
  "Get all workspaces that are part of the SESSION."
  (-> session lsp-session-folder->servers hash-table-values -flatten -uniq))

(defun lsp--find-multiroot-workspace (session client project-root)
  "Look for a multiroot connection in SESSION created from CLIENT for PROJECT-ROOT and BUFFER-MAJOR-MODE."
  (when (lsp--client-multi-root client)
    (-when-let (multi-root-workspace (->> session
                                          (lsp--session-workspaces)
                                          (--first (eq (-> it lsp--workspace-client lsp--client-server-id)
                                                       (lsp--client-server-id client)))))
      (with-lsp-workspace multi-root-workspace
        (lsp-notify "workspace/didChangeWorkspaceFolders"
                    `(:event (:added ,(vector (list :uri (lsp--path-to-uri project-root)))))))

      (->> session (lsp-session-folder->servers) (gethash project-root) (cl-pushnew multi-root-workspace))
      (->> session (lsp-session-server-id->folders) (gethash (lsp--client-server-id client)) (cl-pushnew project-root))

      (lsp--persist-session session)

      (lsp--info "Opened folder %s in workspace %s" project-root (lsp--workspace-print multi-root-workspace))

      multi-root-workspace)))

(defun lsp--ensure-lsp-servers (session clients project-root ignore-multi-folder)
  "Ensure that SESSION contain server CLIENTS created for PROJECT-ROOT.
IGNORE-MULTI-FOLDER to ignore multi folder server."
  (-map (lambda (client)
          (or
           (lsp--find-workspace session client project-root)
           (unless ignore-multi-folder
             (lsp--find-multiroot-workspace session client project-root))
           (lsp--start-connection session client project-root)))
        clients))

(defun lsp--spinner-stop ()
  "Stop the spinner in case all of the workspaces are started."
  (when (--all? (eq (lsp--workspace-status it) 'initialized)
                lsp--buffer-workspaces)
    (spinner-stop)))

(defun lsp--open-in-workspace (workspace)
  "Open in existing WORKSPACE."
  (if (eq 'initialized (lsp--workspace-status workspace))
      ;; when workspace is initialized just call document did open.
      (progn
        (with-lsp-workspace workspace
          (when-let (before-document-open-fn (-> workspace
                                                 lsp--workspace-client
                                                 lsp--client-before-file-open-fn))
            (funcall before-document-open-fn workspace))
          (lsp--text-document-did-open))
        (lsp--spinner-stop))
    ;; when it is not initialized
    (lsp--spinner-start)
    (cl-pushnew (lsp-current-buffer) (lsp--workspace-buffers workspace))))

(defun lsp--find-workspace (session client project-root)
  "Find server connection created with CLIENT in SESSION for PROJECT-ROOT."
  (when-let ((workspace (->> session
                             (lsp-session-folder->servers)
                             (gethash project-root)
                             (--first (eql (-> it lsp--workspace-client lsp--client-server-id)
                                           (lsp--client-server-id client))))))
    (lsp--open-in-workspace workspace)
    workspace))

(defun lsp--find-root-interactively (session)
  "Find project interactively.
Returns nil if the project should not be added to the current SESSION."
  (condition-case nil
      (let* ((project-root-suggestion (or (lsp--suggest-project-root) default-directory))
             (action (read-key (format
                                "%s is not part of any project. Select action:

%s==>Import project root %s.
%s==>Import project by selecting root directory interactively.
%s==>Do not ask again for the current project by adding %s to lsp-session-folders-blacklist.
%s==>Do not ask again for the current project by selecting ignore path interactively.
%s==>Do nothing: ask again when opening other files from the current project."
                                (propertize (buffer-name) 'face 'bold)
                                (propertize "i" 'face 'success)
                                (propertize project-root-suggestion 'face 'bold)
                                (propertize "I" 'face 'success)
                                (propertize "d" 'face 'warning)
                                (propertize project-root-suggestion 'face 'bold)
                                (propertize "D" 'face 'warning)
                                (propertize "n" 'face 'warning)))))
        (cl-case action
          (?i project-root-suggestion)
          (?\r project-root-suggestion)
          (?I (read-directory-name "Select workspace folder to add: "
                                   (or project-root-suggestion default-directory)
                                   nil
                                   t))
          (?d (push project-root-suggestion (lsp-session-folders-blacklist session))
              (lsp--persist-session session)
              nil)
          (?D (push (read-directory-name "Select folder to blacklist: "
                                         (or project-root-suggestion default-directory)
                                         nil
                                         t)
                    (lsp-session-folders-blacklist session))
              (lsp--persist-session session)
              nil)
          (t nil)))
    ('quit)))

(declare-function tramp-file-name-host "ext:tramp" (file) t)
(declare-function tramp-dissect-file-name "ext:tramp" (file &optional nodefault))

(defun lsp--files-same-host (f1 f2)
  "Predicate on whether or not two files are on the same host."
  (or (not (or (file-remote-p f1) (file-remote-p f2)))
      (and (file-remote-p f1)
           (file-remote-p f2)
           (progn (require 'tramp)
                  (equal (tramp-file-name-host (tramp-dissect-file-name f1))
                         (tramp-file-name-host (tramp-dissect-file-name f2)))))))

(defun lsp-find-session-folder (session file-name)
  "Look in the current SESSION for folder containing FILE-NAME."
  (let ((file-name-canonical (lsp-f-canonical file-name)))
    (->> session
         (lsp-session-folders)
         (--filter (and (lsp--files-same-host it file-name-canonical)
                        (or (lsp-f-same? it file-name-canonical)
                            (and (f-dir? it)
                                 (lsp-f-ancestor-of? it file-name-canonical)))))
         (--max-by (> (length it)
                      (length other))))))

(defun lsp-find-workspace (server-id &optional file-name)
  "Find workspace for SERVER-ID for FILE-NAME."
  (-when-let* ((session (lsp-session))
               (folder->servers (lsp-session-folder->servers session))
               (workspaces (if file-name
                               (gethash (lsp-find-session-folder session file-name) folder->servers)
                             (lsp--session-workspaces session))))

    (--first (eq (lsp--client-server-id (lsp--workspace-client it)) server-id) workspaces)))

(defun lsp--calculate-root (session file-name)
  "Calculate project root for FILE-NAME in SESSION."
  (and
   (->> session
        (lsp-session-folders-blacklist)
        (--first (and (lsp--files-same-host it file-name)
                      (lsp-f-ancestor-of? it file-name)
                      (prog1 t
                        (lsp--info "File %s is in blacklisted directory %s" file-name it))))
        not)
   (or
    (when lsp-auto-guess-root
      (lsp--suggest-project-root))
    (lsp-find-session-folder session file-name)
    (unless lsp-auto-guess-root
      (lsp--find-root-interactively session)))))

(defun lsp--try-open-in-library-workspace ()
  "Try opening current file as library file in any of the active workspace.
The library folders are defined by each client for each of the active workspace."

  (when-let (workspace (->> (lsp-session)
                            (lsp--session-workspaces)
                            ;; Sort the last active workspaces first as they are more likely to be
                            ;; the correct ones, especially when jumping to a definition.
                            (-sort (lambda (a _b)
                                     (-contains? lsp--last-active-workspaces a)))
                            (--first
                             (and (-contains? (-> it lsp--workspace-client lsp--client-major-modes)
                                              major-mode)
                                  (when-let (library-folders-fn
                                             (-> it lsp--workspace-client lsp--client-library-folders-fn))
                                    (-first (lambda (library-folder)
                                              (lsp-f-ancestor-of? library-folder (buffer-file-name)))
                                            (funcall library-folders-fn it)))))))
    (lsp--open-in-workspace workspace)
    (view-mode t)
    (lsp--info "Opening read-only library file %s." (buffer-file-name))
    (list workspace)))

(defun lsp--persist-session (session)
  "Persist SESSION to `lsp-session-file'."
  (lsp--persist lsp-session-file (make-lsp-session
                                  :folders (lsp-session-folders session)
                                  :folders-blacklist (lsp-session-folders-blacklist session)
                                  :server-id->folders (lsp-session-server-id->folders session))))

(defun lsp--try-project-root-workspaces (ask-for-client ignore-multi-folder)
  "Try create opening file as a project file.
When IGNORE-MULTI-FOLDER is t the lsp mode will start new
language server even if there is language server which can handle
current language. When IGNORE-MULTI-FOLDER is nil current file
will be opened in multi folder language server if there is
such."
  (-let ((session (lsp-session)))
    (-if-let (clients (if ask-for-client
                          (list (lsp--completing-read "Select server to start: "
                                                      (ht-values lsp-clients)
                                                      (-compose 'symbol-name 'lsp--client-server-id) nil t))
                        (lsp--find-clients)))
        (-if-let (project-root (-some-> session
                                 (lsp--calculate-root (buffer-file-name))
                                 (lsp-f-canonical)))
            (progn
              ;; update project roots if needed and persist the lsp session
              (unless (-contains? (lsp-session-folders session) project-root)
                (push project-root (lsp-session-folders session))
                (lsp--persist-session session))
              (lsp--ensure-lsp-servers session clients project-root ignore-multi-folder))
          (lsp--warn "%s not in project or it is blacklisted." (buffer-name))
          nil)
      (lsp--warn "No LSP server for %s(check *lsp-log*)." major-mode)
      nil)))

(defun lsp-shutdown-workspace ()
  "Shutdown language server."
  (interactive)
  (--when-let (pcase (lsp-workspaces)
                (`nil (user-error "There are no active servers in the current buffer"))
                (`(,workspace) (when (y-or-n-p (format "Are you sure you want to stop the server %s?"
                                                       (lsp--workspace-print workspace)))
                                 workspace))
                (workspaces (lsp--completing-read "Select server: "
                                                  workspaces
                                                  'lsp--workspace-print nil t)))
    (lsp-workspace-shutdown it)))

(make-obsolete 'lsp-shutdown-workspace 'lsp-workspace-shutdown "lsp-mode 6.1")

(defun lsp-workspace-shutdown (workspace)
  "Shut the workspace WORKSPACE and the language server associated with it"
  (interactive (list (lsp--completing-read "Select server: "
                                           (lsp-workspaces)
                                           'lsp--workspace-print nil t)))
  (lsp--warn "Stopping %s" (lsp--workspace-print workspace))
  (with-lsp-workspace workspace (lsp--shutdown-workspace)))

(defun lsp-disconnect ()
  "Disconnect the buffer from the language server."
  (interactive)
  (with-no-warnings (when (functionp 'lsp-ui-mode) (lsp-ui-mode -1)))
  (lsp--text-document-did-close t)
  (lsp-managed-mode -1)
  (lsp-mode -1)
  (setq lsp--buffer-workspaces nil)
  (lsp--info "Disconnected"))

(defun lsp-restart-workspace ()
  (interactive)
  (--when-let (pcase (lsp-workspaces)
                (`nil (user-error "There are no active servers in the current buffer"))
                (`(,workspace) workspace)
                (workspaces (lsp--completing-read "Select server: "
                                                  workspaces
                                                  'lsp--workspace-print nil t)))
    (lsp-workspace-restart it)))

(make-obsolete 'lsp-restart-workspace 'lsp-workspace-restart "lsp-mode 6.1")

(defun lsp-workspace-restart (workspace)
  "Restart the workspace WORKSPACE and the language server associated with it"
  (interactive (list (lsp--completing-read "Select workspace: "
                                           (lsp-workspaces)
                                           'lsp--workspace-print nil t)))
  (lsp--warn "Restarting %s" (lsp--workspace-print workspace))
  (with-lsp-workspace workspace (lsp--shutdown-workspace t)))

;;;###autoload
(defun lsp (&optional arg)
  "Entry point for the server startup.
When ARG is t the lsp mode will start new language server even if
there is language server which can handle current language. When
ARG is nil current file will be opened in multi folder language
server if there is such. When `lsp' is called with prefix
argument ask the user to select which language server to start. "
  (interactive "P")

  (when (and lsp-auto-configure)
    (seq-do (lambda (package)
              ;; loading client is slow and `lsp' can be called repeatedly
              (unless (featurep package) (require package nil t)))
            lsp-client-packages))

  (when (buffer-file-name)
    (let (clients
          (matching-clients (lsp--filter-clients
                             (-andfn #'lsp--matching-clients?
                                     #'lsp--server-binary-present?))))
      (cond
       (matching-clients
        (when (setq lsp--buffer-workspaces
                    (or (and
                         ;; Don't open as library file if file is part of a project.
                         (not (lsp-find-session-folder (lsp-session) (buffer-file-name)))
                         (lsp--try-open-in-library-workspace))
                        (lsp--try-project-root-workspaces (equal arg '(4))
                                                          (and arg (not (equal arg 1))))))
          (lsp-mode 1)
          (when lsp-auto-configure (lsp--auto-configure))
          (setq lsp-buffer-uri (lsp--buffer-uri))
          (lsp--info "Connected to %s."
                     (apply 'concat (--map (format "[%s]" (lsp--workspace-print it))
                                           lsp--buffer-workspaces)))))
       ;; look for servers which are currently being downloaded.
       ((setq clients (lsp--filter-clients (-andfn #'lsp--matching-clients?
                                                   #'lsp--client-download-in-progress?)))
        (lsp--info "There are language server(%s) installation in progress.
The server(s) will be started in the buffer when it has finished."
                   (-map #'lsp--client-server-id clients))
        (seq-do (lambda (client)
                  (cl-pushnew (current-buffer) (lsp--client-buffers client)))
                clients))
       ;; look for servers to install
       ((setq clients (lsp--filter-clients (-andfn #'lsp--matching-clients?
                                                   #'lsp--client-download-server-fn
                                                   (-not #'lsp--client-download-in-progress?))))
        (let ((client (lsp--completing-read
                       (concat "Unable to find installed server supporting this file. "
                               "The following servers could be installed automatically: ")
                       clients
                       (-compose #'symbol-name #'lsp--client-server-id)
                       nil
                       t)))
          (cl-pushnew (current-buffer) (lsp--client-buffers client))
          (lsp--install-server-internal client)))
       ;; no clients present
       ((setq clients (unless matching-clients
                        (lsp--filter-clients (-andfn #'lsp--matching-clients?
                                                     (-not #'lsp--server-binary-present?)))))
        (lsp--warn "The following servers support current file but do not have automatic installation configuration: %s
You may find the installation instructions at https://github.com/emacs-lsp/lsp-mode/#supported-languages.
(If you have already installed the server check *lsp-log*)."
                   (mapconcat (lambda (client)
                                (symbol-name (lsp--client-server-id client)))
                              clients
                              " ")))
       ;; no matches
       ((-> #'lsp--matching-clients? lsp--filter-clients not)
        (lsp--error "There are no language servers supporting current mode %s registered with `lsp-mode'."
                    major-mode))))))

(defun lsp--init-if-visible ()
  "Run `lsp' for the current buffer if the buffer is visible.
Returns `t' if `lsp' was run for the buffer."
  (when (or (buffer-modified-p) (get-buffer-window nil t))
    (remove-hook 'window-configuration-change-hook #'lsp--init-if-visible t)
    (lsp)
    t))

;;;###autoload
(defun lsp-deferred ()
  "Entry point that defers server startup until buffer is visible.
`lsp-deferred' will wait until the buffer is visible before invoking `lsp'.
This avoids overloading the server with many files when starting Emacs."
  ;; Workspace may not be initialized yet. Use a buffer local variable to
  ;; remember that we deferred loading of this buffer.
  (setq lsp--buffer-deferred t)
  (let ((buffer (current-buffer)))
    ;; Avoid false positives as desktop-mode restores buffers by deferring
    ;; visibility check until the stack clears.
    (run-with-timer 0 nil (lambda ()
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (unless (lsp--init-if-visible)
                                  (add-hook 'window-configuration-change-hook #'lsp--init-if-visible nil t))))))))



(defvar lsp-file-truename-cache (ht))

(defmacro lsp-with-cached-filetrue-name (&rest body)
  "Executes BODY caching the `file-truename' calls."
  `(let ((old-fn (symbol-function 'file-truename)))
     (unwind-protect
         (progn
           (fset 'file-truename
                 (lambda (file-name &optional counter prev-dirs)
                   (or (gethash file-name lsp-file-truename-cache)
                       (puthash file-name (apply old-fn (list file-name counter prev-dirs))
                                lsp-file-truename-cache))))
           ,@body)
       (fset 'file-truename old-fn))))

;; flycheck

(declare-function flycheck-define-generic-checker
                  "ext:flycheck" (symbol docstring &rest properties))
(declare-function flycheck-error-message "ext:flycheck" (err) t)
(declare-function flycheck-define-error-level "ext:flycheck" (level &rest properties))
(declare-function flycheck-mode "ext:flycheck")
(declare-function flycheck-checker-supports-major-mode-p "ext:flycheck")
(declare-function flycheck-error-new "ext:flycheck" t t)
(declare-function flycheck-buffer "ext:flycheck")
(declare-function flycheck-add-mode "ext:flycheck")

(defvar flycheck-check-syntax-automatically)
(defvar flycheck-checker)
(defvar flycheck-checkers)

(defcustom lsp-flycheck-default-level 'error
  "Error level to use when the server does not report back a diagnostic level."
  :type '(choice (const error)
                 (const warning)
                 (const info))
  :group 'lsp-mode)

(defun lsp--get-buffer-diagnostics ()
  (gethash (or
            (plist-get lsp--virtual-buffer :buffer-file-name)
            (lsp--fix-path-casing buffer-file-name)
            (lsp--fix-path-casing (file-truename buffer-file-name)))
           (lsp-diagnostics t)))

(defun lsp--flycheck-calculate-level (severity tags)
  (let ((level (pcase severity
                 (1 'error)
                 (2 'warning)
                 (3 'info)
                 (4 'info)
                 (_ lsp-flycheck-default-level)))
        ;; materialize only first tag.
        (tags (seq-map (lambda (tag)
                         (cond
                          ((= tag lsp/diagnostic-tag-unnecessary) 'unnecessary)
                          ((= tag lsp/diagnostic-tag-deprecated) 'deprecated)))
                       tags)))
    (if tags
        (lsp--flycheck-level level tags)
      level)))

(defun lsp-virtual-buffer-call (key &rest args)
  (when lsp--virtual-buffer
    (when-let (fn (plist-get lsp--virtual-buffer key))
      (apply fn args))))

(defun lsp-translate-column (column)
  "Translate COLUMN taking into account virtual buffers."
  (or (lsp-virtual-buffer-call :real->virtual-char column)
      column))

(defun lsp-translate-line (line)
  "Translate LINE taking into account virtual buffers."
  (or (lsp-virtual-buffer-call :real->virtual-line line)
      line))

;; (defun lsp-virtual-buffer-line (line)
;;   "Translate LINE taking into account virtual buffers."
;;   (or (lsp-virtual-buffer-call :real<-virtual-line line)
;;       line))

;; (defun lsp-buffer-column (column)
;;   "Translate COLUMN taking into account virtual buffers."
;;   (or (lsp-virtual-buffer-call :real<-virtual-char column)
;;       column))

(defun lsp--flycheck-start (checker callback)
  "Start an LSP syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck."

  (remove-hook 'lsp-on-idle-hook #'lsp--flycheck-buffer t)

  (->> (lsp--get-buffer-diagnostics)
       (-map (-lambda ((&Diagnostic :message :severity? :tags? :code?
                                    :range (&Range :start (&Position :line      start-line
                                                                     :character start-character)
                                                   :end   (&Position :line      end-line
                                                                     :character end-character))))
               (flycheck-error-new
                :buffer (current-buffer)
                :checker checker
                :filename buffer-file-name
                :message message
                :level (lsp--flycheck-calculate-level severity? tags?)
                :id code?
                :line (lsp-translate-line (1+ start-line))
                :column (1+ (lsp-translate-column start-character))
                :end-line (lsp-translate-line (1+ end-line))
                :end-column (1+ (lsp-translate-column end-character)))))
       (funcall callback 'finished)))

(defun lsp--flycheck-buffer ()
  (remove-hook 'lsp-on-idle-hook #'lsp--flycheck-buffer t)
  (flycheck-buffer))

(defun lsp--buffer-visible? ()
  (or (get-buffer-window (current-buffer))
      (eq (window-buffer (selected-window))
          (current-buffer))))

(defun lsp--flycheck-report ()
  "This callback is invoked when new diagnostics are received
from the language server."
  (when (and (memq 'idle-change flycheck-check-syntax-automatically)
             lsp--cur-workspace)
    ;; make sure diagnostics are published even if the diagnostics
    ;; have been received after idle-change has been triggered
    (-some->> lsp--cur-workspace
      (lsp--workspace-buffers)
      (mapc (lambda (buffer)
              (when (lsp-buffer-live-p buffer)
                (lsp-with-current-buffer buffer
                  (add-hook 'lsp-on-idle-hook #'lsp--flycheck-buffer nil t))))))))

(declare-function lsp-cpp-flycheck-clang-tidy-error-explainer "lsp-cpp")

(defvar lsp-diagnostics-attributes
  `((unnecessary :foreground "dim gray")
    (deprecated  :strike-through t) )
  "List containing (tag attributes) where tag is the LSP
  diagnostic tag and attributes is a `plist' containing face
  attributes which will be applied on top the flycheck face for
  that error level.")

(defun lsp--flycheck-level (flycheck-level tags)
  "Generate flycheck level from the original FLYCHECK-LEVEL (e.
g. `error', `warning') and list of LSP TAGS."
  (let ((name (format "lsp-flycheck-%s-%s"
                      flycheck-level
                      (mapconcat #'symbol-name tags "-"))))
    (or (intern-soft name)
        (let* ((face (--doto (intern (format "lsp-%s-face" name))
                       (copy-face (-> flycheck-level
                                      (get 'flycheck-overlay-category)
                                      (get 'face))
                                  it)
                       (mapc (lambda (tag)
                               (apply #'set-face-attribute it nil
                                      (cl-rest (assoc tag lsp-diagnostics-attributes))))
                             tags)))
               (category (--doto (intern (format "lsp-%s-category" name))
                           (setf (get it 'face) face
                                 (get it 'priority) 100)))
               (new-level (intern name))
               (bitmap (or (get flycheck-level 'flycheck-fringe-bitmaps)
                           (get flycheck-level 'flycheck-fringe-bitmap-double-arrow))))
          (flycheck-define-error-level new-level
            :severity (get flycheck-level 'flycheck-error-severity)
            :compilation-level (get flycheck-level 'flycheck-compilation-level)
            :overlay-category category
            :fringe-bitmap bitmap
            :fringe-face (get flycheck-level 'flycheck-fringe-face)
            :error-list-face face)
          new-level))))

(with-eval-after-load 'flycheck
  (flycheck-define-generic-checker 'lsp
    "A syntax checker using the Language Server Protocol (LSP)
provided by lsp-mode.
See https://github.com/emacs-lsp/lsp-mode."
    :start #'lsp--flycheck-start
    :modes '(python-mode)
    :predicate (lambda () lsp-mode)
    :error-explainer (lambda (e)
                       (cond ((string-prefix-p "clang-tidy" (flycheck-error-message e))
                              (lsp-cpp-flycheck-clang-tidy-error-explainer e))
                             (t (flycheck-error-message e))))))

(defun lsp-flycheck-add-mode (mode)
  "Register flycheck support for MODE."
  (unless (flycheck-checker-supports-major-mode-p 'lsp mode)
    (flycheck-add-mode 'lsp mode)))

(defun lsp-flycheck-enable (&rest _)
  "Enable flycheck integration for the current buffer."
  (flycheck-mode 1)
  (setq-local flycheck-checker 'lsp)
  (lsp-flycheck-add-mode major-mode)
  (add-to-list 'flycheck-checkers 'lsp)
  (add-hook 'lsp-after-diagnostics-hook #'lsp--flycheck-report nil t)
  (add-hook 'lsp-managed-mode-hook #'lsp--flycheck-report nil t))


;; avy integration

(declare-function avy-process "ext:avy" (candidates &optional overlay-fn cleanup-fn))
(declare-function avy--key-to-char "ext:avy" (c))
(defvar avy-action)

(defun lsp-avy-lens ()
  "Click lsp lens using `avy' package."
  (interactive)
  (if (not lsp-lens-mode)
      (user-error "`lsp-lens-mode' not active")
    (let* ((avy-action 'identity)
           (action (cl-third
                    (avy-process
                     (-mapcat
                      (lambda (overlay)
                        (-map-indexed
                         (lambda (index lens-token)
                           (list overlay index
                                 (get-text-property 0 'action lens-token)))
                         (overlay-get overlay 'lsp--metadata)))
                      lsp--lens-overlays)
                     (-lambda (path ((ov index) . _win))
                       (let* ((path (mapcar #'avy--key-to-char path))
                              (str (propertize (string (car (last path)))
                                               'face 'avy-lead-face))
                              (old-str (overlay-get ov 'before-string))
                              (old-str-tokens (s-split "\|" old-str))
                              (old-token (seq-elt old-str-tokens index))
                              (tokens `(,@(-take index old-str-tokens)
                                        ,(-if-let ((_ prefix suffix)
                                                   (s-match "\\(^[[:space:]]+\\)\\(.*\\)" old-token))
                                             (concat prefix str suffix)
                                           (concat str old-token))
                                        ,@(-drop (1+ index) old-str-tokens)))
                              (new-str (s-join (propertize "|" 'face 'lsp-lens-face) tokens))
                              (new-str (if (s-ends-with? "\n" new-str)
                                           new-str
                                         (concat new-str "\n"))))
                         (overlay-put ov 'before-string new-str)))
                     (lambda ()
                       (--map (overlay-put it 'before-string
                                           (overlay-get it 'lsp-original))
                              lsp--lens-overlays))))))
      (funcall-interactively action))))


;; lsp internal validation.

(defmacro lsp--validate (&rest checks)
  `(-let [buf (current-buffer)]
     (with-current-buffer (get-buffer-create "*lsp-performance*")
       (with-help-window (current-buffer)
         ,@(-map (-lambda ((msg form))
                   `(insert (format "%s: %s\n" ,msg
                                    (if (with-current-buffer buf
                                          ,form)
                                        (propertize "OK" 'face 'success)
                                      (propertize "ERROR" 'face 'error)))))
                 (-partition 2 checks))))))

(defun lsp-diagnose ()
  "Validate performance settings."
  (interactive)
  (lsp--validate
   "Checking for Native JSON support" (functionp 'json-serialize)
   "Checking emacs version has `read-process-output-max'" (boundp 'read-process-output-max)
   "Using company-capf: " (-contains? company-backends 'company-capf)
   "Check emacs supports `read-process-output-max'" (boundp 'read-process-output-max)
   "Check `read-process-output-max' default has been changed from 4k"
   (and (boundp 'read-process-output-max)
        (> read-process-output-max 4096))
   "Byte compiled against native json (recompile emacs if failing.)"
   (condition-case _err
       (progn (lsp--make-message  (list "a" "b"))
              nil)
     (error t))
   "`gc-cons-threshold' increased?" (> gc-cons-threshold 800000)))


;; org-mode/virtual-buffer

(declare-function org-babel-get-src-block-info "ext:ob-core")
(declare-function org-do-remove-indentation "ext:org-macs")
(declare-function org-src-get-lang-mode "ext:org-src")
(declare-function org-element-context "ext:org-element")

(defun lsp--virtual-buffer-update-position ()
  (-if-let (virtual-buffer (-first (-lambda ((&plist :in-range))
                                     (funcall in-range))
                                   lsp--virtual-buffer-connections))
      (unless (equal virtual-buffer lsp--virtual-buffer)
        (lsp-org))
    (when lsp-managed-mode
      (lsp-managed-mode -1)
      (lsp-mode -1)
      (setq-local lsp--buffer-workspaces nil)
      (setq-local lsp--virtual-buffer nil)
      (setq-local lsp-buffer-uri nil)

      ;; force refresh of diagnostics
      (run-hooks 'lsp-after-diagnostics-hook))))

(defun lsp-virtual-buffer-on-change (start end length)
  "Adjust on change event to be executed against the proper language server."
  (let ((max-point (max end
                        (or (plist-get lsp--before-change-vals :end) 0)
                        (+ start length))))
    (when-let (virtual-buffer (-first (lambda (vb)
                                        (let ((lsp--virtual-buffer vb))
                                          (and (lsp-virtual-buffer-call :in-range start)
                                               (lsp-virtual-buffer-call :in-range max-point))))
                                      lsp--virtual-buffer-connections))
      (lsp-with-current-buffer virtual-buffer
        (lsp-on-change start end length
                       (lambda (&rest _)
                         (list :range (lsp--range (list :character 0 :line 0)
                                                  lsp--virtual-buffer-point-max)
                               :text (lsp--buffer-content))))))))

(defun lsp-virtual-buffer-before-change (start _end)
  (when-let (virtual-buffer (-first (lambda (vb)
                                      (lsp-with-current-buffer vb
                                        (lsp-virtual-buffer-call :in-range start)))
                                    lsp--virtual-buffer-connections))
    (lsp-with-current-buffer virtual-buffer
      (setq lsp--virtual-buffer-point-max
            (lsp--point-to-position (lsp-virtual-buffer-call :last-point))))))

(defun lsp-patch-on-change-event ()
  (remove-hook 'after-change-functions #'lsp-on-change t)
  (add-hook 'after-change-functions #'lsp-virtual-buffer-on-change nil t)
  (add-hook 'before-change-functions #'lsp-virtual-buffer-before-change nil t))

(defun lsp-kill-virtual-buffers ()
  (mapc #'lsp-virtual-buffer-disconnect lsp--virtual-buffer-connections))

(defun lsp--move-point-in-indentation (point indentation)
  (save-excursion
    (goto-char point)
    (if (<= point (+ (point-at-bol) indentation))
        (point-at-bol)
      point)))

(defun lsp-org ()
  (interactive)
  (-if-let ((virtual-buffer &as &plist :workspaces) (-first (-lambda ((&plist :in-range))
                                                              (funcall in-range))
                                                            lsp--virtual-buffer-connections))
      (unless (equal lsp--virtual-buffer virtual-buffer)
        (setq-local lsp--buffer-workspaces workspaces)
        (setq-local lsp--virtual-buffer virtual-buffer)
        (setq-local lsp-buffer-uri nil)
        (lsp-mode 1)
        (lsp-managed-mode 1)
        (lsp-configure-buffer)
        (lsp-patch-on-change-event))

    (save-excursion
      (-let* (virtual-buffer
              (wcb (lambda (f)
                     (with-current-buffer (plist-get virtual-buffer :buffer)
                       (-let* (((&plist :major-mode :buffer-file-name
                                        :goto-buffer :workspaces) virtual-buffer)
                               (lsp--virtual-buffer virtual-buffer)
                               (lsp--buffer-workspaces workspaces))
                         (save-excursion
                           (funcall goto-buffer)
                           (funcall f))))))
              ((&plist :begin :end :post-blank :language) (cl-second (org-element-context)))
              ((&alist :tangle file-name) (cl-third (org-babel-get-src-block-info 'light)))

              (file-name (if file-name
                             (f-expand file-name)
                           (user-error "You should specify file name in the src block header.")))
              (begin-marker (progn
                              (goto-char begin)
                              (forward-line)
                              (set-marker (make-marker) (point))))
              (end-marker (progn
                            (goto-char end)
                            (forward-line (1- (- post-blank)))
                            (set-marker (make-marker) (1+ (point)))))
              (buf (current-buffer))
              (src-block (buffer-substring-no-properties begin-marker
                                                         (1- end-marker)))
              (indentation (with-temp-buffer
                             (insert src-block)

                             (goto-char (point-min))
                             (let ((indentation (current-indentation)))
                               (plist-put lsp--virtual-buffer :indentation indentation)
                               (org-do-remove-indentation)
                               (goto-char (point-min))
                               (- indentation (current-indentation))))))
        (add-hook 'post-command-hook #'lsp--virtual-buffer-update-position nil t)

        (when (fboundp 'flycheck-add-mode)
          (lsp-flycheck-add-mode 'org-mode))

        (setq lsp--virtual-buffer
              (list
               :in-range (lambda (&optional point)
                           (<= begin-marker (or point (point)) (1- end-marker)))
               :goto-buffer (lambda () (goto-char begin-marker))
               :buffer-string
               (lambda ()
                 (let ((src-block (buffer-substring-no-properties
                                   begin-marker
                                   (1- end-marker))))
                   (with-temp-buffer
                     (insert src-block)

                     (goto-char (point-min))
                     (while (not (eobp))
                       (delete-region (point) (if (> (+ (point) indentation) (point-at-eol))
                                                  (point-at-eol)
                                                (+ (point) indentation)))
                       (forward-line))
                     (buffer-substring-no-properties (point-min)
                                                     (point-max)))))
               :buffer buf
               :begin begin-marker
               :end end-marker
               :indentation indentation
               :last-point (lambda () (1- end-marker))
               :cur-position (lambda ()
                               (lsp-save-restriction-and-excursion
                                 (list :line (- (lsp--cur-line)
                                                (lsp--cur-line begin-marker))
                                       :character (let ((character (- (point)
                                                                      (line-beginning-position)
                                                                      indentation)))
                                                    (if (< character 0)
                                                        0
                                                      character)))))
               :line/character->point (-lambda (line character)
                                        (-let [inhibit-field-text-motion t]
                                          (+ indentation
                                             (lsp-save-restriction-and-excursion
                                               (goto-char begin-marker)
                                               (forward-line line)
                                               (-let [line-end (line-end-position)]
                                                 (if (> character (- line-end (point)))
                                                     line-end
                                                   (forward-char character)
                                                   (point)))))))
               :major-mode (org-src-get-lang-mode language)
               :buffer-file-name file-name
               :buffer-uri (lsp--path-to-uri file-name)
               :with-current-buffer wcb
               :buffer-live? (lambda (_) (buffer-live-p buf))
               :buffer-name (lambda (_)
                              (propertize (format "%s(%s:%s)%s"
                                                  (buffer-name buf)
                                                  begin-marker
                                                  end-marker
                                                  language)
                                          'face 'italic))
               :real->virtual-line (lambda (line)
                                     (+ line (line-number-at-pos begin-marker) -1))
               :real->virtual-char (lambda (char) (+ char indentation))
               :cleanup (lambda ()
                          (set-marker begin-marker nil)
                          (set-marker end-marker nil))))
        (setf virtual-buffer lsp--virtual-buffer)
        (puthash file-name virtual-buffer lsp--virtual-buffer-mappings)
        (push virtual-buffer lsp--virtual-buffer-connections)

        ;; TODO: tangle only connected sections
        (add-hook 'after-save-hook 'org-babel-tangle nil t)
        (add-hook 'lsp-after-open-hook #'lsp-patch-on-change-event nil t)
        (add-hook 'kill-buffer-hook #'lsp-kill-virtual-buffers nil t)

        (setq lsp--buffer-workspaces
              (lsp-with-current-buffer virtual-buffer
                (lsp)
                (plist-put virtual-buffer :workspaces (lsp-workspaces))
                (lsp-workspaces)))))))

(defun lsp-virtual-buffer-disconnect (virtual-buffer)
  (interactive (list (or
                      lsp--virtual-buffer
                      (when lsp--virtual-buffer-connections
                        (lsp--completing-read "Select virtual buffer to disconnect: "
                                              lsp--virtual-buffer-connections
                                              (-lambda ((&plist :buffer-file-name))
                                                buffer-file-name))))))
  (-if-let ((&plist :buffer-file-name file-name :cleanup) virtual-buffer)
      (progn
        (lsp-with-current-buffer virtual-buffer
          (lsp--text-document-did-close))
        (setq lsp--virtual-buffer-connections (-remove-item virtual-buffer lsp--virtual-buffer-connections))
        (when (eq virtual-buffer lsp--virtual-buffer)
          (setf lsp--virtual-buffer nil))
        (when cleanup (funcall cleanup))
        (remhash file-name lsp--virtual-buffer-mappings)

        (lsp--virtual-buffer-update-position)
        (lsp--info "Disconnected from buffer %s" file-name))
    (lsp--error "Nothing to disconnect from?")))



(provide 'lsp-mode)
;;; lsp-mode.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
