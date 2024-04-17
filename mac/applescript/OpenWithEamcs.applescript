-- -*- coding:utf-8 mode:applescript-*-

-- -*- coding:utf-8 -*-
-- http://superuser.com/questions/457484/how-to-open-emacs-from-macs-finder
-- https://gist.github.com/ambethia/304964#comment-799519
-- 在Finder 中右键使用emacsclient 打开文件
--  这个需要用automator.app 存成app  ,用applescript.app 存成的app 不行
on run {input}
	set filepath to quoted form of POSIX path of input
	-- if you donot  use ITerm  you can use Terminal instead 
	tell application "iTerm"
		do shell script "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient --alternate-editor=/Applications/Emacs.app/Contents/MacOS/Emacs -c -n " & filepath
	end tell
	
	-- bring the visible frame to the front
	tell application "Emacs" to activate
	
	return input
end run


