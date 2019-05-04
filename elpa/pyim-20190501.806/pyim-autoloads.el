;;; pyim-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pyim" "pyim.el" (0 0 0 0))
;;; Generated autoloads from pyim.el

(autoload 'pyim-convert-string-at-point "pyim" "\
将光标前的用户输入的字符串转换为中文.

\(fn)" t nil)

(defvar pyim-isearch-mode nil "\
Non-nil if Pyim-Isearch mode is enabled.
See the `pyim-isearch-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pyim-isearch-mode'.")

(custom-autoload 'pyim-isearch-mode "pyim" nil)

(autoload 'pyim-isearch-mode "pyim" "\
这个 mode 为 isearch 添加拼音搜索功能.

\(fn &optional ARG)" t nil)

(autoload 'pyim-hanzi2pinyin "pyim" "\
将汉字字符串转换为对应的拼音字符串的工具.

如果 SHOU-ZI-MU 设置为 t, 转换仅得到拼音首字母字符串。当
RETURN-LIST 设置为 t 时，返回一个拼音列表，这个列表包含词条的一个
或者多个拼音（词条包含多音字时）；如果 IGNORE-DUO-YIN-ZI 设置为
t, 遇到多音字时，只使用第一个拼音，其它拼音忽略；当
ADJUST-DUO-YIN-Zi 设置为 t 时, `pyim-hanzi2pinyin' 会使用 pyim 已
安装的词库来校正多音字，但这个功能有一定的限制:

1. pyim 普通词库中不存在的词条不能较正
2. 多音字校正速度比较慢，实时转换会产生卡顿。

BUG: 当 STRING 中包含其它标点符号，并且设置 SEPERATER 时，结果会
包含多余的连接符：比如： '你=好' --> 'ni-=-hao'

\(fn STRING &optional SHOU-ZI-MU SEPARATOR RETURN-LIST IGNORE-DUO-YIN-ZI ADJUST-DUO-YIN-ZI)" nil nil)

(autoload 'pyim-hanzi2pinyin-simple "pyim" "\
简化版的 `pyim-hanzi2pinyin', 不处理多音字。

\(fn STRING &optional SHOU-ZI-MU SEPARATOR RETURN-LIST)" nil nil)

(autoload 'pyim-dicts-manager "pyim" "\
pyim 词库管理器。

使用这个词库管理器可以方便的执行下列命令：
1. 添加词库。
2. 删除词库。
3. 向上和向下移动词库。
4. 保存词库设置。
5. 重启输入法。

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pyim" '("pyim-")))

;;;***

;;;### (autoloads nil "pyim-devtools" "pyim-devtools.el" (0 0 0 0))
;;; Generated autoloads from pyim-devtools.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pyim-devtools" '("pyim-website-repository-directory")))

;;;***

;;;### (autoloads nil "pyim-probe" "pyim-probe.el" (0 0 0 0))
;;; Generated autoloads from pyim-probe.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pyim-probe" '("pyim-probe-")))

;;;***

;;;### (autoloads nil "pyim-pymap" "pyim-pymap.el" (0 0 0 0))
;;; Generated autoloads from pyim-pymap.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pyim-pymap" '("pyim-pymap")))

;;;***

;;;### (autoloads nil nil ("pyim-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pyim-autoloads.el ends here
