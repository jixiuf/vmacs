;;; -*- lexical-binding: t; coding:utf-8 -*-
;;; faces
;;(set-background-color "#2e2d28")
;;(set-foreground-color "#a1aca7") "#f7f8c6"
;;(set-default-font "DejaVu Sans Mono:pixelsize=16")
;;几种不错的颜色 263111棕色 354022浅棕色 ;;48433d  41412e
;; (set-background-color "#263111")
;; (set-background-color "#2e2d28")

;; (set-mouse-color "GreenYellow")
;; (set-foreground-color "#f7f8c6")
;; (require 'server)

;; 如果配置好了， 下面20个汉字与40个英文字母应该等长
;; here are 20 hanzi and 40 english chars, see if they are the same width
;;
;; ;; 1l0oOLiI
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|
;; 你你你你你你你你你你你你你你你你你你你你|
;; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,|
;; 。。。。。。。。。。。。。。。。。。。。|
;; 1111111111111111111111111111111111111111|
;; 東東東東東東東東東東東東東東東東東東東東|
;; ここここここここここここここここここここ|
;; ｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺ|
;; 까까까까까까까까까까까까까까까까까까까까|

;; (create-frame-font-middle-mac)
;; (create-fontset-from-fontset-spec
;;    "-apple-Menlo-medium-normal-normal-*-12-*-*-*-m-0-fontset-mymac,
;;  ascii:-apple-Menlo-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1,
;; han:-*-PingFang SC-normal-normal-normal-*-14-*-*-*-p-0-iso10646-1,
;; cjk-misc:-*-PingFang SC-normal-normal-normal-*-14-*-*-*-p-0-iso10646-1,
;; kana:-*-PingFang SC-normal-normal-normal-*-14-*-*-*-p-0-iso10646-1,
;; hangul:-*-Apple SD Gothic Neo-normal-normal-normal-*-16-*-*-*-p-0-iso10646-1")
;; (add-to-list 'default-frame-alist '(font . "fontset-mymac"))
;; (set-frame-font "fontset-mymac" )


;; (create-fontset-from-fontset-spec
;;  (concat "-*-*-*-*-*--*-*-*-*-*-*-fontset-bigmac"
;;          ",han:PingFang SC:size=20"
;;          ",symbol:PingFang SC:size=20"
;;          ",cjk-misc:PingFang SC:size=20"
;;          ",bopomofo:PingFang SC:size=20"
;;          ",kana:Hiragino Sans:size=20"
;;          ",hangul:Apple SD Gothic Neo:size=23"
;;          ",latin:Menlo"))

;; ;;;###autoload
;; (defun create-frame-font-big-mac()          ;emacs 若直接启动 启动时调用此函数似乎无效
;;   (interactive)
;;   (set-face-attribute
;;    'default nil
;;    :font
;;    "Menlo 16"
;;    :fontset "fontset-bigmac"))

;; (create-fontset-from-fontset-spec
;;     (concat "-*-*-*-*-*--*-*-*-*-*-*-fontset-largemac"
;;             ",han:PingFang SC:size=24"
;;             ",symbol:PingFang SC:size=24"
;;             ",cjk-misc:PingFang SC:size=24"
;;             ",bopomofo:PingFang SC:size=24"
;;             ",kana:Hiragino Sans:size=24"
;;             ",hangul:Apple SD Gothic Neo:size=26"
;;             ",latin:Menlo"))

;; ;;;###autoload
;; (defun create-frame-font-large-mac()          ;emacs 若直接启动 启动时调用此函数似乎无效
;;   (interactive)
;;   (set-face-attribute
;;    'default nil
;;    :font "Menlo 20"
;;    :fontset "fontset-largemac")
;;   )

;; (create-fontset-from-fontset-spec
;;     (concat "-*-*-*-*-*--*-*-*-*-*-*-fontset-mac"
;;             ",han:PingFang SC:size=16"
;;             ",symbol:PingFang SC:size=16"
;;             ",cjk-misc:PingFang SC:size=16"
;;             ",bopomofo:PingFang SC:size=16"
;;             ",kana:Hiragino Sans:size=16"
;;             ",hangul:Apple SD Gothic Neo:size=18"
;;             ",latin:Menlo"))

;; ;;;###autoload
;; (defun create-frame-font-mac()
;;   (interactive)
;;   (set-face-attribute
;;    'default nil
;;    :font "Menlo 14"
;;    :fontset "fontset-mac"))

;; ;;;###autoload
;; (defun create-frame-font-w32()          ;emacs 若直接启动 启动时调用此函数似乎无效
;;   (set-face-attribute
;;    'default nil :font "Courier New 10")
;;   ;; Chinese Font
;;   (dolist (charset '( han symbol cjk-misc bopomofo)) ;script 可以通过C-uC-x=查看当前光标下的字的信息
;;     (set-fontset-font (frame-parameter nil 'font)
;;                       charset
;;                       (font-spec :family "新宋体" :size 16)))

;;   (set-fontset-font (frame-parameter nil 'font)
;;                     'kana                 ;script ｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺ
;;                     (font-spec :family "MS Mincho" :size 16))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     'hangul               ;script 까까까까까까까까까까까까까까까까까까까까
;;                     (font-spec :family "GulimChe" :size 16)))

;; (create-fontset-from-fontset-spec
;;     (concat "-*-*-*-*-*--*-*-*-*-*-*-fontset-middlemac"
;;             ",han:PingFang SC:size=18"
;;             ",symbol:PingFang SC:size=18"
;;             ",cjk-misc:PingFang SC:size=18"
;;             ",bopomofo:PingFang SC:size=18"
;;             ",kana:Hiragino Sans:size=16"
;;             ",hangul:Apple SD Gothic Neo:size=21"
;;             ",latin:Menlo"))

;; ;;;###autoload
;; (defun create-frame-font-middle-mac()
;;   (interactive)
;;   (set-face-attribute
;;    'default nil
;;    :font "Menlo 15"
;;    :fontset "fontset-middlemac"
;;    ))

;; (create-fontset-from-fontset-spec
;;     (concat "-*-*-*-*-*--*-*-*-*-*-*-fontset-smallmac"
;;             ",han:PingFang SC:size=16"
;;             ",symbol:PingFang SC:size=16"
;;             ",cjk-misc:PingFang SC:size=16"
;;             ",bopomofo:PingFang SC:size=16"
;;             ",kana:Hiragino Sans:size=16"
;;             ",hangul:Apple SD Gothic Neo:size=18"
;;             ",latin:Menlo"))
;; ;;;###autoload
;; (defun create-frame-font-small-mac()
;;   (interactive)
;;   (set-face-attribute
;;    'default nil
;;    :font "Menlo 13"
;;    :fontset "fontset-smallmac"
;;    ))

(provide 'lazy-font)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lazy-font.el ends here.
