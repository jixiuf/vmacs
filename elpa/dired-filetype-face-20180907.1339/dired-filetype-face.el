;;; dired-filetype-face.el --- Set different faces for different filetypes in dired

;; Author: 纪秀峰 <jixiuf at gmail dot com>
;; Copyright (C) 2011~2015,纪秀峰 , all rights reserved.
;; Version: 1.0
;; Package-Version: 20180907.1339
;; URL:   https://github.com/jixiuf/dired-filetype-face
;; Keywords: dired filetype face
;;
;; This file is NOT part of GNU Emacs
;;
;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;  Set faces for different file types in dired.
;;  I use a dark background maybe the default face doesn't meet your request.
;;  You can:
;;    M-x customize-group dired-filetype-face  RET
;;  And maybe:
;;    M-x customize-group dired-faces  RET
;;  may do some help for you.

;;; Installation:
;;
;; Put `dired-filetype-face.el' in your load-path.
;; Your load-path might include the directory ~/elisp/, for example.
;; It's set in your ~/.emacs like this:
;;
;;   (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; Add the following to your ~/.emacs startup file.
;;
;;   (with-eval-after-load 'dired  (require 'dired-filetype-face))
;;
;; If you want to add a new face for new filetype(s):
;;
;;   (deffiletype-face "mytype" "Chartreuse")
;;
;; then either:
;;
;;   (deffiletype-face-regexp mytype
;;     :extensions '("foo" "bar") :type-for-docstring "my type")
;;
;; to match all files ending either ".foo" or ".bar", or equivalently:
;;
;;   (deffiletype-face-regexp mytype
;;     :regexp "^  -.*\\.\\(foo\\|bar\\)$" :type-for-docstring "my type")
;;
;; and finally:
;;
;;   (deffiletype-setup "mytype" "mytype")
;;
;; The :regexp form allows you to specify other things to match on each line of
;; the dired buffer than (only) file extensions, such as the permission bits,
;; the size and the modification times.
;;
;; No need more.
;;

;;; Code:

(require 'dired)
(require 'custom)

(defgroup dired-filetype-face nil
  "Set faces for different filetypes in dired."
  :prefix "dired-filetype-face-"
  :group 'dired-faces)

(defmacro dired-filetype-fmt (fmt sym)
  "Call `format' on FMT and SYM, then `downcase', then `intern'."
  `(intern (downcase (format ,fmt ,sym))))

(defmacro deffiletype-face (type color &optional type-for-symbol)
  "Declare a dired filetype face for displaying TYPE files in the given COLOR.

If TYPE-FOR-SYMBOL is nil, define a face named
  dired-filetype-TYPE

Otherwise, define a face named
  dired-filetype-TYPE-FOR-SYMBOL

COLOR may be a string or a list of face properties. If a string,
it is either a color name such as \"Chartreuse\" or a color
hexadecimal RGB number such as \"#xaaaaaa\"."
  `(defface ,(dired-filetype-fmt "dired-filetype-%s" (or type-for-symbol type))
     ,(if (stringp color)
          `(quote ((t (:foreground ,color))))
        color)
     ,(format "Face for displaying %s files in dired." type)
     :tag ,(format "Dired %s filetype face" type)
     :group 'dired-filetype-face))

(defmacro deffiletype-face-regexp (type-for-symbol &rest args)
  "Define a regexp option to colorize matching files in dired.

TYPE-FOR-SYMBOL is a symbol to splice into the defined option
symbol. The format string used in splicing is
\"dired-filetype-%s-regexp\", where %s will be replaced by
TYPE-FOR-SYMBOL.

The remaining arguments are keyword arguments accessed as ARGS.

Exactly one of the two mutually-exclusive keyword
arguments :regexp or :extensions is required.

Keyword argument :extensions must be a list of strings, each of
which is a literal filetype extension without a leading dot and
with no globbing or regexp syntax. This list will be used to
derive a regexp to match against each complete line in the dired
buffer.

Keyword argument :regexp must be a regexp string to match against
each complete line in the dired buffer. Use this to match file
names by something other than (only) the literal extension,
and/or by other attributes available in the dired buffer such as
modification timestamp and/or permission flags.

Optional keyword argument :type-for-docstring is either a symbol
or a string to splice into the user option docstring instead of
TYPE-FOR-SYMBOL. The format string used in splicing is \"Regexp
to match %s file-types in dired.\", where %s will be replaced by
keyword argument :type-for-docstring if given, or else by
TYPE-FOR-SYMBOL."
  (let*
      ((type-for-docstring
        (or
         (plist-get args :type-for-docstring)
         type-for-symbol))
       (regexp (plist-get args :regexp))
       (extensions (plist-get args :extensions)))
    (unless
        (or (and (null regexp) extensions) (and (null extensions) regexp))
      (error
       "Exactly one of keyword arguments :regexp and :extensions is required"))
    `(defcustom ,(dired-filetype-fmt "dired-filetype-%s-regexp" type-for-symbol)
       ,(or regexp extensions)
       ,(format
         "Either a list of file extensions or a regexp to match %s file-types in dired."
         type-for-docstring)
       :type
       '(choice
         (repeat
          :format "%t\n%v%h"
          :doc "List of file extensions (without a leading dot) to
group together for dired to fontify in the same face. Literal
file extensions only, no glob or regexp patterns."
          :tag "File extensions to match"
          string)
         (regexp
          :tag "Regular expression to match against whole dired line"
          :format "%t\n%v%h"
          :doc "Include two leading spaces, like this: \"^  \"."))
       :tag ,(format "Dired %s filetype pattern" type-for-docstring)
       :group 'dired-filetype-face)))

(defconst dired-filetype-face-font-lock-keywords
  '(("(\\(deffiletype\\(?:-\\(?:face\\|face-regexp\\|setup\\)\\)?\\)\\_>"
     (1 font-lock-keyword-face))))

(font-lock-add-keywords 'emacs-lisp-mode dired-filetype-face-font-lock-keywords)

(defvar dired-filetype-setup-hook nil)

(deffiletype-face "omit" "dark gray")

(deffiletype-face-regexp omit1
  :type-for-docstring unimportant
  :extensions
  '(
    "al"
    "bak"
    "cat"
    "class"
    "dat"
    "db"
    "DLL"
    "Dll"
    "dll"
    "elc"
    "fas"
    "fasl"
    "ix"
    "ko"
    "la"
    "o"
    "prf"
    "rdp"
    "sav"
    "so"
    "SYS"
    "sys"
    "td"
    "tlb"))

(deffiletype-face-regexp omit2
  :type-for-docstring "backup or cache"
  :regexp
  "^  -.*\\(\\.git\\|\\.svn\\|\\.bzr\\|\\.bazaar\\|~\\|#\\|%\\|\\.tmp\\|\\$DATA\\|:encryptable\\|\\.db_encryptable\\)$")

(deffiletype-face-regexp omit3
  :type-for-docstring hidden :regexp "^  .* \\.\\(.*$\\)")

(deffiletype-face "rich document" "DarkCyan" "document")

(deffiletype-face-regexp document
  :type-for-docstring "rich document"
  :extensions
  '(
    "csv"
    "pages"
    "numbers"
    "CHM"
    "chm"
    "doc"
    "docx"
    "kdh"
    "odp"
    "ods"
    "odt"
    "otp"
    "ott"
    "pdf"
    "ppt"
    "pptx"
    "rtf"
    "sdw"
    "sdx"
    "shx"
    "sxc"
    "tex"
    "xls"
    "xlsx"))

(deffiletype-face "plain text" "DarkSeaGreen1" "plain")

(deffiletype-face-regexp plain :type-for-docstring "plain text"
  :extensions
  '(
    "md"
    "yaml"
    "yml"
    "CFG"
    "cfg"
    "cnf"
    "conf"
    "config"
    "default"
    "diff"
    "ebuild"
    "example"
    "inf"
    "INI"
    "ini"
    "log"
    "lrc"
    "m4"
    "org"
    "patch"
    "plist"
    "properties"
    "sample"
    "TXT"
    "Txt"
    "txt"))

(deffiletype-face "common" "Peru")

(deffiletype-face-regexp common
  :regexp
  "^  -.*\\(\\.keystore\\|configure\\|INSTALL.*\\|Install.*\\|CONTRIBUTING.*\\|README.*\\|readme.*\\|todo\\|Todo.*\\|TODO.*\\|Cask\\|COPYING.*\\|CHANGES\\|CHANGELOG\\|Changes\\|LICENSE\\|ChangeLog\\|Makefile\\mk|\\|Makefile.in\\|MANIFEST.MF\\|NOTICE.txt\\|build.xml\\|Manifest\\|metadata.xml\\|install-sh\\|NEWS\\|HACKING\\|AUTHORS\\|TAGS\\|tag\\|GPATH\\|id_rsa\\|id_rsa.pub\\|id_dsa\\|id_dsa.pub\\|authorized_keys\\|known_hosts\\|CREDITS.*\\)$")

(deffiletype-face "XML" "Chocolate")

(deffiletype-face-regexp XML
  :extensions
  '(
    "vbs"
    "asp"
    "aspx"
    "dtd"
    "HTM"
    "htm"
    "HTML"
    "html"
    "jsp"
    "jspx"
    "mht"
    "rng"
    "xaml"
    "XML"
    "xml"
    "xsd"
    "xsl"))

(deffiletype-face "compressed" "Orchid" "compress")

(deffiletype-face-regexp compress
  :type-for-docstring compressed
  :extensions
  '(
    "7Z"
    "7z"
    "apk"
    "bz2"
    "bzip2"
    "cab"
    "deb"
    "ear"
    "gpg"
    "gz"
    "gzip"
    "img"
    "iso"
    "jar"
    "lzma"
    "pkg"
    "RAR"
    "rar"
    "rpm"
    "tar"
    "taz"
    "tbz2"
    "tgz"
    "txz"
    "war"
    "wim"
    "XAR"
    "xar"
    "XZ"
    "xz"
    "Z"
    "z"
    "ZIP"
    "zip"))

(deffiletype-face "source code" "SpringGreen" "source")

(deffiletype-face-regexp source
  :type-for-docstring "source code"
  :extensions
  '(
    "a"
    "ahk"
    "asm"
    "C"
    "c"
    "cc"
    "cpp"
    "cs"
    "css"
    "ddl"
    "el"
    "erl"
    "go"
    "h"
    "hrl"
    "JAVA"
    "java"
    "m"
    "mm"
    "lisp"
    "livecode"
    "lua"
    "p"
    "pas"
    "php"
    "pl"
    "py"
    "rb"
    "rev"
    "sch"
    "scheme"
    "scm"
    "sql"
    "st"))

(deffiletype-face "program" "green3")

(deffiletype-face-regexp program
  :regexp
  "^  -\\([r-][w-]-\\)\\{3\\}.*\\.\\(exe\\|EXE\\|cmd\\|bat\\|BAT\\|msi\\|MSI\\|\\(?:t?c\\|z\\)?sh\\|run\\|reg\\|REG\\|com\\|COM\\|vbx\\|VBX\\|bin\\|xpi\\|bundle\\|awk\\)$")

(deffiletype-face "executable" "green" "execute")

(deffiletype-face-regexp execute :type-for-docstring executable
  :regexp "^  -\\([r-][w-]-\\)\\{,2\\}[r-][w-]x")

(deffiletype-face "music" "SteelBlue")

(deffiletype-face-regexp music
  :extensions
  '(
    "AAC"
    "aac"
    "FLAC"
    "flac"
    "m3u"
    "M4A"
    "m4a"
    "MID"
    "mid"
    "MP3"
    "mp3"
    "OGG"
    "ogg"
    "pls"
    "WAV"
    "wav"
    "WMA"
    "wma"))

(deffiletype-face "video" "SandyBrown")

(deffiletype-face-regexp video
  :extensions
  '(
    "3gp"
    "AVI"
    "avi"
    "divx"
    "f4v"
    "FLV"
    "flv"
    "m4v"
    "mkv"
    "mov"
    "mp4"
    "mpeg"
    "MPG"
    "mpg"
    "ogm"
    "ogv"
    "RM"
    "rm"
    "RMVB"
    "rmvb"
    "swf"
    "webm"
    "WMV"
    "wmv"
    "xvid"))

(deffiletype-face "image" "IndianRed2")

(deffiletype-face-regexp image
  :extensions
  '(
    "BMP"
    "bmp"
    "eps"
    "epsf"
    "GIF"
    "gif"
    "icns"
    "ico"
    "icon"
    "JPEG"
    "jpeg"
    "JPG"
    "jpg"
    "odg"
    "pcx"
    "pic"
    "pict"
    "PNG"
    "png"
    "svg"
    "tga"
    "tif"
    "tiff"
    "xbm"
    "xpm"))

(deffiletype-face
  "link"
  '((((class color) (background dark)) :foreground "yellow" :background "forest green") (t ())))

(deffiletype-face-regexp link
  :regexp
  "^  l\\|^  -.*\\.\\(lnk\\|LNK\\|desktop\\|torrent\\|url\\|URL\\)$")

;;; Custom ends here.

(defcustom dired-filetype-disabled-diredp-faces
  t
  "Turn off filetype matching from package `dired+', if installed.

Without this setting, some files will be highlighted by one
package and some by the other. Does not disable any other
features of package dired+; only dired+ file-type highlighting is
affected. If you're wondering why only some of the filetype faces
you define here are taking effect, and you have dired+ installed,
try this."
  :type 'boolean
  :tag "Disable dired+ filetype matching"
  :group 'dired-filetype-face)

(defvar dired-filetype-old-diredp-faces nil
  "Backup of filetype faces from package `dired+'.")

(defun dired-filetype-disable-diredp-faces-maybe ()
  "Turn off filetype matching from package `dired+' if present."
  (when (featurep 'dired+)
    (if dired-filetype-disabled-diredp-faces
        (when (bound-and-true-p diredp-font-lock-keywords-1)
          ;; then backup and clear diredp faces
          (setq dired-filetype-old-diredp-faces diredp-font-lock-keywords-1)
          (setq diredp-font-lock-keywords-1 nil))
      ;; else restore diredp faces
      (when (and
             dired-filetype-old-diredp-faces
             (boundp 'diredp-font-lock-keywords-1)
             (null diredp-font-lock-keywords-1))
        (setq diredp-font-lock-keywords-1 dired-filetype-old-diredp-faces)))))

(add-hook 'dired-filetype-setup-hook #'dired-filetype-disable-diredp-faces-maybe)

(defmacro deffiletype-setup (type &optional type-for-docstring type-for-symbol type-for-face)
  "Declare a function to tell dired how to display TYPE files.

If not nil, use TYPE-FOR-DOCSTRING instead of TYPE for
documentation.

If not nil, use TYPE-FOR-SYMBOL instead of TYPE to derive the
function symbol.

If not nil, use TYPE-FOR-FACE instead of TYPE to derive the
symbol for the associated face."
  (let
      ((funcsym (dired-filetype-fmt "dired-filetype-set-%s-face" (or type-for-symbol type)))
       (optsym (dired-filetype-fmt "dired-filetype-%s-regexp" type)))
    `(progn
       (defun ,funcsym ()
         ,(format "Set dired-filetype-face for %s files." (or type-for-docstring type))
         (font-lock-add-keywords
          nil
          (list
           (cons
            (if (stringp ,optsym)
                ,optsym
              (format "^  -.*\\.%s$" (regexp-opt ,optsym 'grouped)))
            '((".+"
               (dired-move-to-filename)
               nil
               (0
                (quote
                 ,(dired-filetype-fmt "dired-filetype-%s" (or type-for-face type))))))))))
       (add-hook 'dired-filetype-setup-hook #',funcsym t))))

(deffiletype-setup "document" "rich document")

(deffiletype-setup "plain" "plain text")

(deffiletype-setup "common")

(deffiletype-setup "XML")

(deffiletype-setup "compress" "compressed")

(deffiletype-setup "source" "source code")

(deffiletype-setup "omit1" "unimportant" "omit" "omit")

(deffiletype-setup "omit2" "backup and cache" nil "omit")

(deffiletype-setup "omit3" "hidden" nil "omit")

(deffiletype-setup "program")

(deffiletype-setup "execute" "executable")

(deffiletype-setup "music" "audio")

(deffiletype-setup "video")

(deffiletype-setup "image")

(deffiletype-setup "link")


(deffiletype-face "js" "goldenrod")
(deffiletype-face-regexp js :extensions '("js" "json"))
(deffiletype-setup "js")

(defadvice dired-toggle-read-only (after  dired-filetype-face activate)
  "set different faces for different file type."
  (dired-filetype-setup))

(defadvice wdired-exit (after dired-filetype-face activate)
  "set different faces for different file type."
  (dired-filetype-setup))

(defadvice wdired-finish-edit (after dired-filetype-face activate)
  "set different faces for different file type."
  (dired-filetype-setup))

(defadvice wdired-abort-changes (after dired-filetype-face activate)
  "set different faces for different file type."
  (dired-filetype-setup))

(defun dired-filetype-setup()
  (run-hooks 'dired-filetype-setup-hook))

;; Append to mode hooks so ours are the last applied, overriding others.
(add-hook 'dired-mode-hook 'dired-filetype-setup 'append)
(add-hook 'wdired-mode-hook 'dired-filetype-setup 'append)

(provide 'dired-filetype-face)

;;; dired-filetype-face.el ends here
