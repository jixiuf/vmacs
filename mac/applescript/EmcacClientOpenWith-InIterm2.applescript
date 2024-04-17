-- -*- coding:utf-8 mode:applescript-*-
-- http://superuser.com/questions/457484/how-to-open-emacs-from-macs-finder
-- https://gist.github.com/ambethia/304964#comment-799519
-- 在Finder 中右键使用emacsclient 打开文件
--  这个需要用automator.app 存成app  ,用applescript.app 存成的app 不行
-- em is my script wrapper for emacsclient

-- #!/bin/sh
-- if [ -z "$1" ]
-- then
--     emacsclient -t  
-- else
--     emacsclient -t   "$@"
-- fi


on run {input, parameters}
	tell application "iTerm"
		activate
		if (count of windows) = 0 then
			set w to (create window with default profile)
		else
			set w to current window
		end if
		
		tell w
			create tab with default profile
			
			tell current session of w
				set myPath to POSIX path of input
				-- set myPath to "/tmp/a.txt"
				set cmd to "~/.emacs.d/bin/em " & quote & myPath & quote & "&&exit"
				--exec command(cmd)
				write text (cmd)
				--close t
			end tell
			
		end tell
	end tell
end run
