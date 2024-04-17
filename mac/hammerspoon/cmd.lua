-- 在命令行下调用open -g "hammerspoon://openItermHereInFinder"
hs.urlevent.bind("openItermHereInFinder", function() openItermHereInFinder() end)
function openItermHereInFinder()
   scpt=[[tell application "Finder"
    try
        set frontWin to folder of front window as string
        set frontWinPath to (get POSIX path of frontWin)
        tell application "iTerm"
            activate
            if (count of windows) = 0 then
                set w to (create window with default profile)
            else
                set w to current window
            end if

            tell w
                set targetSession to null

                activate current session
                tell current session of w
                    if is at shell prompt then
                        set targetSession to current session of w
                    end if
                end tell
                if targetSession is null then
                    repeat with aTab in tabs
                        if targetSession is null then
                            tell aTab
                                select
                                repeat with aSession in sessions
                                    if targetSession is null then
                                        tell aSession
                                            select
                                            if is at shell prompt then
                                                set targetSession to aSession
                                            end if
                                        end tell
                                    end if
                                end repeat
                            end tell
                        end if
                    end repeat
                end if
                if targetSession is null then
                    create tab with default profile
                    tell current session of w
                        set targetSession to current session of w
                    end tell
                end if

                if targetSession is not null then
                    tell targetSession
                        select
                        -- set cmd to "cd " & quote & "/tmp/" & quote & ";clear"
                        set cmd to "cd " & quote & frontWinPath & quote & ";clear"
                        write text cmd
                    end tell

                end if
            end tell
        end tell
    on error error_message
        beep
        display dialog error_message buttons ¬
            {"OK"} default button 1
    end try
end tell
]]
   hs.osascript.applescript(scpt)
end


-- function openItermHereInFinder()
--    scpt=[[tell application "Finder"
--     try
--         set frontWin to folder of front window as string
--         set frontWinPath to (get POSIX path of frontWin)
--         tell application "iTerm"
--                 activate
--             if (count of windows) = 0 then
--                 set t to (create window with default profile)
--             else
--                 set t to current window
--             end if

--             tell t
--                 tell (create tab with default profile)
--                     tell the last session
--                         set cmd to "cd " & quote & frontWinPath & quote & ";clear"
--                         -- do script with command cmd
--                         write text cmd
--                         -- exec command (cmd)
--                     end tell
--                 end tell
--             end tell
--         end tell
--     on error error_message
--         beep
--         display dialog error_message buttons ¬
--             {"OK"} default button 1
--     end try
-- end tell
-- ]]
--    hs.osascript.applescript(scpt)
-- end

-- hs.urlevent.bind("toggleHiddenFile", function() toggleHiddenFile() end)

-- function toggleHiddenFile()
--    local finderStatus,_,_,_=hs.execute("defaults read com.apple.finder AppleShowAllFiles")
--    if finderStatus=="TRUE" then
--       hs.execute("defaults write com.apple.finder AppleShowAllFiles FALSE")
--    else
--       hs.execute("defaults write com.apple.finder AppleShowAllFiles TRUE")
--    end
--    local script=[[tell application "Finder"
--                  quit
--                  delay 0.1 -- without this there was a \"connection is invalid\" error
--                  launch -- without this Finder was not made frontmost
--                  activate -- make Finder frontmost
--                  reopen -- open a default window
--                  end tell]]
--    hs.osascript.applescript(script)
-- end

hs.urlevent.bind("openWithEmacsclientInFinder", function() openWithEmacsclientInFinder() end)

function openWithEmacsclientInFinder()
   local script=[[
tell application "Finder"
    try
        -- set frontWin to folder of front window as string
        -- set frontWinPath to (get POSIX path of frontWin)
        set theItems to selection
        repeat with itemRef in theItems
        --set myitem to POSIX path of (itemRef as string)
        set myitem to quoted form of  POSIX path of (itemRef as string)
        do shell script "~/.emacs.d/bin/ec --no-wait "  & myitem

        end repeat -- it will store the last filename in selection
    on error error_message
        beep
        display dialog error_message buttons ¬
            {"OK"} default button 1
    end try
end tell
]]
   hs.osascript.applescript(script)
end

hs.urlevent.bind("openWithEmacsclientInItermFromFinder", function() openWithEmacsclientInItermFromFinder() end)
function openWithEmacsclientInItermFromFinder()
   local script=[[
tell application "Finder"
    try
        -- set frontWin to folder of front window as string
        -- set frontWinPath to (get POSIX path of frontWin)
        set theItems to selection
        set cmd to "em "
        repeat with n from 1 to count of theItems
        --set myitem to POSIX path of (item n of theItems as string)
        set myitem to quoted form of  POSIX path of (item n of theItems as string)
        set cmd to cmd & myitem  & " "
        end repeat -- it will store the last filename in selection
        set cmd to cmd &  " && exit " -- after emacsclient ,call exist for close iterm tab
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
                    -- set cmd to "~/.emacs.d/bin/em " & quote & myitem & quote
                    write text (cmd)
                end tell

            end tell
        end tell

    on error error_message
        beep
        display dialog error_message buttons ¬
            {"OK"} default button 1
    end try
end tell

]]
   hs.osascript.applescript(script)
end


---------------------------------------------------------------
hs.urlevent.bind("openWithEmacs", function() openWithEmacs() end)

function openWithEmacs()
   local script=[[
tell application "Finder"
    try
        -- set frontWin to folder of front window as string
        -- set frontWinPath to (get POSIX path of frontWin)
        set theItems to selection
        repeat with itemRef in theItems
        --set myitem to POSIX path of (itemRef as string)
        set myitem to quoted form of  POSIX path of (itemRef as string)
        do shell script "open -a Emacs "  & myitem

        end repeat -- it will store the last filename in selection
    on error error_message
        beep
        display dialog error_message buttons ¬
            {"OK"} default button 1
    end try
end tell
]]
   hs.osascript.applescript(script)
end
---------------------------------------------------------------

hs.urlevent.bind("openExternalEditorInXcode", function() openExternalEditorInXcode() end)
function openExternalEditorInXcode()
   local menuName = {"File", "Open with External Editor"}
   local topApp =hs.application.frontmostApplication()
   if topApp==nil or topApp:title()~="Xcode" then
      return
   end
   topApp:selectMenuItem(menuName)
end

hs.urlevent.bind("open_with_external_editor_in_xcode", function() openExternalEditorInXcode() end)
---------------------------------------------------------------
hs.urlevent.bind("volumeUp", function() volumeUp() end)
function volumeUp()
   hs.audiodevice.defaultOutputDevice():setVolume(hs.audiodevice.current().volume + 3)
   hs.alert.closeAll()
   hs.alert.show("当前音量:" .. tostring(math.floor(hs.audiodevice.current().volume )))
end
hs.urlevent.bind("volumeDown", function() volumeDown() end)
function volumeDown()
   hs.audiodevice.defaultOutputDevice():setVolume(hs.audiodevice.current().volume - 3)
   hs.alert.closeAll()
   hs.alert.show("当前音量:" .. tostring(math.floor(hs.audiodevice.current().volume )))
   -- hs.notify.show("","","当前音量:" .. tostring(math.floor(hs.audiodevice.current().volume )))
end
