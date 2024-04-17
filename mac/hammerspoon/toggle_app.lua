require('hyper')
-------------------------------------------------------------
-- hs.hotkey.bind(hyper3, "t", function() hs.execute("/Library/Input\\ Methods/Squirrel.app/Contents/MacOS/squirrel_client -t ascii_mode")  end )
-- hs.hotkey.bind(hyper3, "w", function() toggleApp("com.wunderkinder.wunderlistdesktop") end )
-- hs.hotkey.bind(hyper, "/", function() toggleApp("com.apple.Notes") end )
-- hs.hotkey.bind(hyper3, "n", function() toggleApp("com.apple.Notes") end )
hs.hotkey.bind(hyper3, "q",function() appKill() end)
hs.hotkey.bind(hyper3, "w",function() appKill() end)
-- hs.hotkey.bind(hyper3, "d", function() toggleApp("com.emmac.mac") end )
-- hs.hotkey.bind(hyper3, "f", function() toggleApp("com.apple.Safari") end )
-- hs.hotkey.bind(hyper3, "f", function() toggleApp("org.mozilla.firefox") end )
-- hs.hotkey.bind(hyper, "f", function() toggleApp("com.google.Chrome") end )
-- hs.hotkey.bind(hyper, "t", function() toggleApp("com.googlecode.iterm2") end)
-- hs.hotkey.bind(hyper, "d", function() toggleApp("com.googlecode.iterm2") end)
-- hs.hotkey.bind(hyper, "j", function() toggleApp("com.tencent.qq") end)
-- hs.hotkey.bind(hyper, "3", function() toggleApp("com.tencent.WeWorkMac") end)
-- hs.hotkey.bind(hyper, "8", function() toggleApp("com.bitwarden.desktop") end)
-- hs.hotkey.bind(hyper, "5", function() toggleApp("cn.apifox.app-pdv") end)
-- hs.hotkey.bind(hyper, "3", function() toggleApp("com.electron.lark") end)
-- hs.hotkey.bind(hyper, "e", function() toggleEmacs() end )
hs.hotkey.bind(hyper, "g", function() toggleFinder() end )
-- hs.hotkey.bind(hyper, "b", function() toggleApp("com.tencent.xinWeChat") end)
-- hs.hotkey.bind(hyper3, "e", function() toggleEclpse() end)
-- hs.hotkey.bind(hyper3, "x", function() toggleApp("com.apple.dt.Xcode") end)
-- hs.hotkey.bind(hyper3, "m", function() toggleApp("com.apple.mail") end)
-- hs.hotkey.bind(hyper3, "s", function() toggleApp("com.sequelpro.SequelPro") end)
-- hs.hotkey.bind(hyper3, "c", function() toggleApp("com.mongodb.compass") end)
-- toggle App
function toggleApp(appBundleID)
   -- local win = hs.window.focusedWindow()
   -- local app = win:application()
   local app =hs.application.frontmostApplication()
   if app ~= nil and app:bundleID() == appBundleID    then
      app:hide()
      -- win:sendToBack()
   elseif app==nil then
      hs.application.launchOrFocusByBundleID(appBundleID)
   else
      -- app:activate()
      hs.application.launchOrFocusByBundleID(appBundleID)
      app=hs.application.get(appBundleID)
      if app==nil then
         return
      end
      local wins=app:visibleWindows()
      if #wins>0 then
         for k,win in pairs(wins) do
            if win:isMinimized() then
               win:unminimize()
            end
         end
      else
         hs.application.open(appBundleID)
         app:activate()
      end


      local win=app:mainWindow()
      if win ~= nil then
         win:application():activate(true)
         win:application():unhide()
         win:focus()
      end


   end
end

-- 在命令行下调用open -g "hammerspoon://toggleApp?bid=com.apple.Safari"
hs.urlevent.bind("toggleApp", function(eventName, params)
                    if params["bid"] then
                       toggleApp(params["bid"])
                    end
end)

hs.urlevent.bind("toggleSafari", function(eventName, params)  toggleApp("com.apple.Safari") end)

hs.urlevent.bind("toggleIterm2", function(eventName, params)  toggleApp("com.googlecode.iterm2") end)
hs.urlevent.bind("toggleEmacsTermMode", function(eventName, params)  toggleEmacsTermMode() end)

---------------------------------------------------------------
function hideEAF()
   local script=[[
    try
      tell application "System Events"
       --  if application process "python3" exists then
         if (exists of application process "python3") is true then
            click menu item "Hide eaf.py" of menu 1 of menu bar item "python3" of menu bar 1
         end if
      end tell
    on error error_message
        beep
    end try

]]
   hs.osascript.applescript(script)
end
function showEAF()
   local script=[[
try
      tell application "System Events"
         -- if application process "python3" exists then
         if (exists of application process "python3") is true then
         click menu item "Show all" of menu 1 of menu bar item "python3" of menu bar 1
         end if
      end tell
on error error_message
   beep
end try
]]
   hs.osascript.applescript(script)
end
hs.urlevent.bind("eaf", function(eventName, params)
                    if params["show"] then
                       showEAF()
                    else
                       hideEAF()
                    end
end)

function toggleEmacs()        --    toggle emacsclient if emacs daemon not started start it
   -- local win = hs.window.focusedWindow()
   -- local topApp = win:application()

   local topApp =hs.application.frontmostApplication()

   if topApp ~= nil and topApp:title():lower() == "emacs"  and #topApp:visibleWindows()>0 and not topApp:isHidden() then
      topApp:hide()
      -- hideEAF()
      -- local apps=hs.application.runningApplications()
      -- for k,app in pairs(apps) do
      --    -- hs.alert.show(app:name())
      --    if string.match(app:path() , "python3") then
      --    end
      -- end
      -- local wins=hs.window.allWindows()
      -- for k,win in pairs(wins) do
      --    hs.alert.show(win:application():path())
      --    if string.match(win:application():path() , "python3") then
      --       win:application():hide()
      --       win:sendToBack()
      --    end
      -- end
   else
      local emacsApp=hs.application.get("Emacs")
      if emacsApp==nil then
         -- ~/.emacs.d/bin/ecexec 是对emacsclient 的包装，你可以直接用emacsclient 来代替
         -- 这个脚本会检查emacs --daemon 是否已启动，未启动则启动之
         -- hs.execute("~/.emacs.d/bin/ecexec --no-wait -c") -- 创建一个窗口
         hs.task.new("~/.emacs.d/bin/en",nil):start()
         -- 这里可能需要等待一下，以确保窗口创建成功后再继续，否则可能窗口不前置
         emacsApp=hs.application.get("Emacs")
         if emacsApp ~=nil then
            emacsApp:activate()      -- 将刚创建的窗口前置
         end
         return
      end
      local wins=emacsApp:allWindows() -- 只在当前space 找，
      if #wins==0 then
         wins=hs.window.filter.new(false):setAppFilter("Emacs",{}):getWindows() -- 在所有space找，但是window.filter的bug多，不稳定
      end

      if #wins>0 then
         for _,win in pairs(wins) do

            if win:isMinimized() then
               win:unminimize()
            end

            win:application():activate(true)
            win:application():unhide()
            -- showEAF()
            -- win:focus() -- 不主动聚焦，有可能有miniframe
            -- hs.alert.show(win:title())
         end
      else
         -- ~/.emacs.d/bin/ecexec 是对emacsclient 的包装，你可以直接用emacsclient 来代替
         -- 这个脚本会检查emacs --daemon 是否已启动，未启动则启动之
         -- hs.execute("cd ~;~/.emacs.d/bin/ec") -- 创建一个窗口
         hs.task.new("~/.emacs.d/bin/en",nil):start()
         -- hs.execute("~/.emacs.d/bin/ecexec --no-wait -c") -- 创建一个窗口
         -- 这里可能需要等待一下，以确保窗口创建成功后再继续，否则可能窗口不前置
         emacsApp=hs.application.get("Emacs")
         if emacsApp ~=nil then
            emacsApp:activate()      -- 将刚创建的窗口前置
         end
      end
   end
end

hs.urlevent.bind("toggleEmacs", function(eventName, params) toggleEmacs() end)
-- open -g "hammerspoon://toggleEmacs"
---------------------------------------------------------------


---------------------------------------------------------------
function toggleEmacsTermMode()        --    toggle emacsclient if emacs daemon not started start it
   -- local win = hs.window.focusedWindow()
   -- local topApp = win:application()

   local topApp =hs.application.frontmostApplication()

   if topApp ~= nil and topApp:title() == "Emacs"  and #topApp:visibleWindows()>0 and not topApp:isHidden() and string.match(topApp:focusedWindow():title(),"*eshell*") then
      topApp:hide()
   else
      local emacsApp=hs.application.get("Emacs")
      if emacsApp==nil then
         -- ~/.emacs.d/bin/ecexec 是对emacsclient 的包装，你可以直接用emacsclient 来代替
         -- 这个脚本会检查emacs --daemon 是否已启动，未启动则启动之
         hs.execute("~/.emacs.d/bin/ec -e \"(toggle-eshell)\"") -- 创建一个窗口
         -- hs.execute("~/.emacs.d/bin/ecexec --no-wait -c") -- 创建一个窗口
         -- 这里可能需要等待一下，以确保窗口创建成功后再继续，否则可能窗口不前置
         emacsApp=hs.application.get("Emacs")
         if emacsApp ~=nil then
            emacsApp:activate()      -- 将刚创建的窗口前置
            hs.execute("~/.emacs.d/bin/ec -e \"(toggle-eshell)\"") -- 创建一个窗口
         end
         return
      end
      local wins=emacsApp:allWindows() -- 只在当前space 找，
      if #wins==0 then
         wins=hs.window.filter.new(false):setAppFilter("Emacs",{}):getWindows() -- 在所有space找，但是window.filter的bug多，不稳定
      end

      if #wins>0 then
         for _,win in pairs(wins) do

            if win:isMinimized() then
               win:unminimize()
            end

            win:application():activate(true)
            win:application():unhide()
            win:focus()
            hs.execute("~/.emacs.d/bin/ec -e \"(toggle-eshell)\"") -- 创建一个窗口
         end
      else
         -- ~/.emacs.d/bin/ecexec 是对emacsclient 的包装，你可以直接用emacsclient 来代替
         -- 这个脚本会检查emacs --daemon 是否已启动，未启动则启动之
         hs.execute("~/.emacs.d/bin/ec -e \"(toggle-eshell)\"") -- 创建一个窗口
         -- hs.execute("~/.emacs.d/bin/ecexec --no-wait -c") -- 创建一个窗口
         -- 这里可能需要等待一下，以确保窗口创建成功后再继续，否则可能窗口不前置
         emacsApp=hs.application.get("Emacs")
         if emacsApp ~=nil then
            emacsApp:activate()      -- 将刚创建的窗口前置
         end
      end
   end
end

---------------------------------------------------------------
function toggleFinder()
   local appBundleID="com.apple.finder"
   local topWin = hs.window.focusedWindow()
   -- if topWin==nil then
   --    return
   -- end
   -- local topApp = topWin:application()
   -- local topApp =hs.application.frontmostApplication()

   -- The desktop belongs to Finder.app: when Finder is the active application, you can focus the desktop by cycling through windows via cmd-`
   -- The desktop window has no id, a role of AXScrollArea and no subrole
   -- and #topApp:visibleWindows()>0
   if topWin~=nil and topWin:application() ~= nil and topWin:application():bundleID() == appBundleID   and topWin:role() ~= "AXScrollArea" then
      topWin:application():hide()
   else
      finderApp=hs.application.get(appBundleID)
      if finderApp==nil then
         hs.application.launchOrFocusByBundleID(appBundleID)
         return
      end
      local wins=finderApp:allWindows()
      local isWinExists=true
      if #wins==0  then
         isWinExists=false
      elseif  (wins[1]:role() =="AXScrollArea" and #wins==1 )  then
         isWinExists=false
      end

      -- local wins=app:visibleWindows()
      if not isWinExists then
         wins=hs.window.filter.new(false):setAppFilter("Finder",{}):getWindows()
      end


      if #wins==0 then
         hs.application.launchOrFocusByBundleID(appBundleID)
         for _,win in pairs(wins) do
            if win:isMinimized() then
               win:unminimize()
            end

            win:application():activate(true)
            win:application():unhide()
            win:focus()
         end
      else
         for _,win in pairs(wins) do
            if win:isMinimized() then
               win:unminimize()
            end

            win:application():activate(true)
            win:application():unhide()
            win:focus()
         end
      end
   end
end
-- open -g "hammerspoon://toggleFinder"
hs.urlevent.bind("toggleFinder", function(eventName, params) toggleFinder() end)

---------------------------------------------------------------
-- eclipse 比较特殊，
-- hs.application.launchOrFocusByBundleID(appBundleID) 会报错
-- 故用   hs.execute("open /Applications/Eclipse") -- 代替

function toggleEclpse()
   -- local win = hs.window.focusedWindow()
   -- local app = win:application()
   local app =hs.application.frontmostApplication()
   if app ~= nil and app:bundleID() == "org.eclipse.eclipse"    then
      app:hide()
      -- win:sendToBack()
   elseif app==nil then
      hs.execute("open /Applications/Eclipse") -- 创建一个窗口
      -- hs.application.launchOrFocusByBundleID(appBundleID)
   else
      hs.execute("open /Applications/Eclipse") -- 创建一个窗口
      app=hs.application.get("org.eclipse.eclipse")
      if app==nil then
         return
      end
      local wins=app:visibleWindows()
      if #wins>0 then
         for k,win in pairs(wins) do
            if win:isMinimized() then
               win:unminimize()
            end
         end
      else
         -- hs.application.open(appBundleID)
         hs.execute("open /Applications/Eclipse") -- 创建一个窗口

         app:activate()
      end


      local win=app:mainWindow()
      if win ~= nil then
         win:application():activate(true)
         win:application():unhide()
         win:focus()
      end


   end
end

function appKill()
   local app =hs.application.frontmostApplication()
   if app ~= nil then
      app:kill()
   end
end
-- Quick edit
local quick_edit_app = nil
hs.hotkey.bind(
    {"alt"},
    "`",
    function()
        local emacs = hs.application.find("emacs")
        local current_app = hs.window.focusedWindow()
        local topApp =hs.application.frontmostApplication()

        if topApp ~= nil and topApp:title():lower() == "emacs"  then
            if quick_edit_app == nil then
                hs.alert("🤔 No edit in progress")
                return
            end
            hs.eventtap.keyStroke({"alt", "shift"}, ";")
            hs.eventtap.keyStrokes("(meain/quick-edit-end)")
            hs.eventtap.keyStroke({}, "return")
            quick_edit_app:focus()
            os.execute("sleep " .. tonumber(0.01))
            hs.eventtap.keyStroke({"cmd"}, "a")
            hs.eventtap.keyStroke({"cmd"}, "v")
            quick_edit_app = nil
        else
            quick_edit_app = hs.window.focusedWindow()
            hs.eventtap.keyStroke({"cmd"}, "a")
            hs.eventtap.keyStroke({"cmd"}, "c")
            emacs:activate()
            hs.eventtap.keyStroke({"alt", "shift"}, ";")
            hs.eventtap.keyStrokes("(meain/quick-edit)")
            hs.eventtap.keyStroke({}, "return")
        end
    end
)
