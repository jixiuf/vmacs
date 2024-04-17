-- this file  implements hotkey for different app
-- 此文件废弃

keyCode = function(key,modifiers)
   modifiers = modifiers or {}
   -- Un-comment & reload config to log each keystroke that we're triggering
   -- log.d('Sending keystroke:', hs.inspect(modifiers), key)

   return function()
      --  .keyStrokes() now wait 200ms between the keydown and keyup events, to improve reliability
      -- keydown 与keyup 之间 200ms默认

      -- hs.eventtap.keyStroke(modifiers, key, 0)
      hs.eventtap.event.newKeyEvent(modifiers, string.lower(key), true):post() -- send keydown event
      hs.timer.usleep(1000)
      hs.eventtap.event.newKeyEvent(modifiers, string.lower(key), false):post() -- send keyup event
   end
end
function keyCodeIfUneditabled(key,modifiers) -- bind this in keydown function
   -- if uneditabled send key1 with modifiers1 ,else if focus is in text field or return nil
   modifiers = modifiers or {}
   return function()
      focusedElement=hs.uielement.focusedElement()
      if focusedElement ~=nil and (focusedElement:role()=="AXTextArea" or focusedElement:role()=="AXTextField"  or focusedElement:role()=="AXComboBox")then
         return nil
      end

      --  .keyStrokes() now wait 200ms between the keydown and keyup events, to improve reliability
      -- keydown 与keyup 之间 200ms默认
      -- hs.eventtap.keyStroke(modifiers, key, 0)
      hs.eventtap.event.newKeyEvent(modifiers, string.lower(key), true):post() -- send keydown event
      hs.timer.usleep(1000)
      hs.eventtap.event.newKeyEvent(modifiers, string.lower(key), false):post() -- send keyup event
   end

end
function keyCodeIfEdiabled(key) -- bind this in keyup function
   -- if focus is in text field or any editable element then  sending key if
   return function()
      focusedElement=hs.uielement.focusedElement()
      if focusedElement ~=nil and (focusedElement:role()=="AXTextArea" or focusedElement:role()=="AXComboBox"  or focusedElement:role()=="AXTextField"  )then
         hs.eventtap.keyStrokes(key) -- use keyStroke  doesnot work
         return
      end
   end
end

-- Subscribe to the necessary events on the given window filter such that the
-- given hotkey is enabled for windows that match the window filter and disabled
-- for windows that don't match the window filter.
--
-- windowFilter - An hs.window.filter object describing the windows for which
--                the hotkey should be enabled.
-- hotkey       - The hs.hotkey object to enable/disable.
--
-- Returns nothing.
enableHotkeyForWindowsMatchingFilter = function(windowFilter, hotkey)
   windowFilter:subscribe(hs.window.filter.windowFocused, function()
                             hotkey:enable()
   end)

   windowFilter:subscribe(hs.window.filter.windowUnfocused, function()
                             hotkey:disable()
   end)
end

-- hs.hotkey.new(mods, key, [message,] pressedfn, releasedfn, repeatfn) -> hs.hotkey object
---------------------------------------------------------------
-- local finderWindowFilter = hs.window.filter.new('Finder')
-- enableHotkeyForWindowsMatchingFilter(finderWindowFilter, hs.hotkey.new({"alt"},"C", function() openItermHereInFinder() end))
-- enableHotkeyForWindowsMatchingFilter(finderWindowFilter, hs.hotkey.new({"alt"},"O", function() toggleHiddenFile() end))
-- enableHotkeyForWindowsMatchingFilter(finderWindowFilter, hs.hotkey.new({"ctrl"},"Return", function() openWithEmacs() end)) -- openWithEmacsclientInFinder

-- enableHotkeyForWindowsMatchingFilter(finderWindowFilter, hs.hotkey.new({},"J", keyCodeIfUneditabled("Down"),keyCodeIfEdiabled("j") ,keyCodeIfUneditabled("Down")))
-- enableHotkeyForWindowsMatchingFilter(finderWindowFilter, hs.hotkey.new({},"K", keyCodeIfUneditabled("Up"),keyCodeIfEdiabled("k") ,keyCodeIfUneditabled("Up")))
-- enableHotkeyForWindowsMatchingFilter(finderWindowFilter, hs.hotkey.new({},"H", keyCodeIfUneditabled("Left"),keyCodeIfEdiabled("h") ,keyCodeIfUneditabled("Left")))
-- enableHotkeyForWindowsMatchingFilter(finderWindowFilter, hs.hotkey.new({},"L", keyCodeIfUneditabled("Right"),keyCodeIfEdiabled("l") ,keyCodeIfUneditabled("Right")))

-- enableHotkeyForWindowsMatchingFilter(finderWindowFilter, hs.hotkey.new({},"N", keyCodeIfUneditabled("Down"),keyCodeIfEdiabled("n") ,keyCodeIfUneditabled("Down")))
-- enableHotkeyForWindowsMatchingFilter(finderWindowFilter, hs.hotkey.new({},"P", keyCodeIfUneditabled("Up"),keyCodeIfEdiabled("p") ,keyCodeIfUneditabled("Up")))
-- enableHotkeyForWindowsMatchingFilter(finderWindowFilter, hs.hotkey.new({},"U", keyCodeIfUneditabled("Up",{"cmd"}),keyCodeIfEdiabled("u") ,keyCodeIfUneditabled("Up",{"cmd"})))

-- enableHotkeyForWindowsMatchingFilter(finderWindowFilter, hs.hotkey.new({"ctrl"},"U", keyCode("Up",{"cmd"}),nil ,keyCode("Up",{"cmd"})))



-- enableHotkeyForWindowsMatchingFilter(finderWindowFilter, hs.hotkey.new({"ctrl"},"H",  keyCode("Left"),nil,keyCode("Left")))
-- enableHotkeyForWindowsMatchingFilter(finderWindowFilter, hs.hotkey.new({"ctrl"},"L",  keyCode("Right"),nil,keyCode("Right")))

-- enableHotkeyForWindowsMatchingFilter(finderWindowFilter, hs.hotkey.new({},"TAB",  keyCode("M", {"ctrl"}) ))
-- enableHotkeyForWindowsMatchingFilter(finderWindowFilter, hs.hotkey.new({"ctrl"},",", keyCode("Up", {"alt"}) )) -- goto first  line
-- enableHotkeyForWindowsMatchingFilter(finderWindowFilter, hs.hotkey.new({"ctrl"},".", keyCode("Down",{"alt"}) )) -- goto last line
-- enableHotkeyForWindowsMatchingFilter(finderWindowFilter, hs.hotkey.new({"ctrl"},"y", keyCode("V",{"cmd","alt"}) )) -- paste by move

---------------------------------------------------------------
-- local safariWindowFilter = hs.window.filter.new('Safari')
-- function() hs.eventtap.scrollWheel({0 ,-3}, {}, "line") end -- offsets, modifiers, unit
-- enableHotkeyForWindowsMatchingFilter(safariWindowFilter, hs.hotkey.new({"ctrl"},"J", keyCode("Down"),nil,keyCode("Down")))
-- enableHotkeyForWindowsMatchingFilter(safariWindowFilter, hs.hotkey.new({"ctrl"},"K",keyCode("Up"),nil ,keyCode("Up")))
-- enableHotkeyForWindowsMatchingFilter(safariWindowFilter, hs.hotkey.new({"ctrl"},"G", keyCode("Escape")))
-- enableHotkeyForWindowsMatchingFilter(safariWindowFilter, hs.hotkey.new({"ctrl"},",", keyCode( "Home") ))
-- enableHotkeyForWindowsMatchingFilter(safariWindowFilter, hs.hotkey.new({"ctrl"},".", keyCode( "End") ))
-- enableHotkeyForWindowsMatchingFilter(safariWindowFilter, hs.hotkey.new({},"f4", keyCode( "L",{"cmd","shift"}) ))
---------------------------------------------------------------


---------------------------------------------------------------
-- local xcodeWindowFilter = hs.window.filter.new('Xcode')
-- enableHotkeyForWindowsMatchingFilter(xcodeWindowFilter, hs.hotkey.new({"ctrl"},"Return", function() openExternalEditorInXcode() end))

---------------------------------------------------------------
-- local eclipseWindowFilter = hs.window.filter.new('Eclipse')
-- enableHotkeyForWindowsMatchingFilter(eclipseWindowFilter, hs.hotkey.new({"ctrl"},"R",keyCode("f11",{"cmd","shift"})))

---------------------------------------------------------------
-- local mplayerx = hs.window.filter.new('MPlayerX')
-- enableHotkeyForWindowsMatchingFilter(mplayerx, hs.hotkey.new({},"L",keyCode("Right") ,nil,keyCode("Right") ))
-- enableHotkeyForWindowsMatchingFilter(mplayerx, hs.hotkey.new({},"H",  keyCode("Left"),nil,keyCode("Left") ))
-- enableHotkeyForWindowsMatchingFilter(mplayerx, hs.hotkey.new({},"J",  keyCode("Down"),nil,keyCode("Down") ))
-- enableHotkeyForWindowsMatchingFilter(mplayerx, hs.hotkey.new({},"K",  keyCode("Up")  ,nil,keyCode("Up") ))
-- enableHotkeyForWindowsMatchingFilter(mplayerx, hs.hotkey.new({},"U", keyCode("]") ,nil,keyCode("]") ))
-- enableHotkeyForWindowsMatchingFilter(mplayerx, hs.hotkey.new({},"D",  keyCode("[") ,nil,keyCode("[") ))
-- enableHotkeyForWindowsMatchingFilter(mplayerx, hs.hotkey.new({},",", volumeDown ,nil,volumeDown ))
-- enableHotkeyForWindowsMatchingFilter(mplayerx, hs.hotkey.new({},".",  volumeUp,nil,volumeUp ))
-- enableHotkeyForWindowsMatchingFilter(mplayerx, hs.hotkey.new({},"q", keyCode("w",{"cmd"}) ))
---------------------------------------------------------------

-- local quicktimePlayerWindowFilter = hs.window.filter.new('QuickTime Player')
-- enableHotkeyForWindowsMatchingFilter(quicktimePlayerWindowFilter,
--                                      hs.hotkey.new({},"L",function() hs.eventtap.scrollWheel({0 ,10}, {}, "pixel") end  ,nil,hs.eventtap.scrollWheel({0 ,10}, {}, "pixel") ))
-- enableHotkeyForWindowsMatchingFilter(quicktimePlayerWindowFilter,
--                                      hs.hotkey.new({},"H",function() hs.eventtap.scrollWheel({0 ,-10}, {}, "pixel") end  ,nil,hs.eventtap.scrollWheel({0 ,-10}, {}, "pixel") ))
-- function() hs.eventtap.scrollWheel({0 ,-3}, {}, "line") end -- offsets, modifiers, unit
---------------------------------------------------------------


-- local appKeyBindMap={
--    -- Safari={
--    --    -- hs.hotkey.new({"ctrl"}, "H", function() hs.eventtap.keyStroke({ "cmd"}, "[") end), -- go back
--    --    -- hs.hotkey.new({"ctrl"}, "L", function() hs.eventtap.keyStroke({ "cmd"}, "]") end), -- go forward
--    --    -- hs.hotkey.new({"cmd"}, "P", function() hs.eventtap.keyStroke({"cmd", "shift"}, "Left") end), -- pre tab
--    --    -- hs.hotkey.new({"cmd"}, "N", function() hs.eventtap.keyStroke({"cmd", "shift"}, "Right") end), -- next tab
--    --    -- hs.hotkey.new({"ctrl"}, ";", function() hs.eventtap.keyStroke({"cmd"}, "L") end), -- focus address bar
--    --    -- hs.hotkey.new({"ctrl"}, "W", function() hs.eventtap.keyStroke({"cmd"}, "W") end), -- close window
--    --    -- hs.hotkey.new({"ctrl"}, "S", function() hs.eventtap.keyStroke({"cmd"}, "F") end), -- search
--    --    -- hs.hotkey.new({}, "f4", function() hs.eventtap.keyStroke({"cmd" ,"shift"}, "L") end),
--    --    hs.hotkey.new({"ctrl"}, "J", function() hs.eventtap.keyStroke({}, "Down") end), -- scroll down
--    --    hs.hotkey.new({"ctrl"}, "K", function() hs.eventtap.keyStroke({}, "Up") end), -- scroll down
--    --    -- hs.hotkey.new({"ctrl"}, "R", function() hs.eventtap.keyStroke({"cmd"}, "R") end), -- reload
--    --    hs.hotkey.new({"ctrl"}, "G", function() hs.eventtap.keyStroke({}, "Escape") end), -- escape
--    --    -- Disables cmd-w entirely, which is so annoying on safari
--    --    -- hs.hotkey.new({"cmd"}, "w", function()  return end)
--    -- },
--    Xcode={
--       hs.hotkey.new({"ctrl"}, "Return", function()openExternalEditorInXcode()end),

--    },
--    Finder={
--       -- hs.hotkey.new({"ctrl"}, "H", function() hs.eventtap.keyStroke({ "cmd"}, "[") end),
--       -- hs.hotkey.new({"ctrl"}, "L", function() hs.eventtap.keyStroke({ "cmd"}, "]") end),

--       -- this doesnot work
--       -- hs.hotkey.new({"cmd"}, "G", function() hs.eventtap.keyStroke({ "cmd", "shift"}, "G") end),

--       -- hs.hotkey.new({"ctrl"}, ";", function() hs.eventtap.keyStroke({ "cmd", "shift"}, "G") end),
--       -- hs.hotkey.new({"ctrl"}, "D", function() hs.eventtap.keyStroke({ "cmd", }, "Delete") end),
--       -- hs.hotkey.new({"cmd"}, "X", function()
--       --       hs.eventtap.keyStroke( {"cmd"},"C")
--       --       hs.eventtap.keyStroke( {"cmd"},"Delete")
--       -- end),

--       -- disable cmd D
--       -- hs.hotkey.new({"cmd"}, "D", function() return end),
--    }
-- }
-- -- hs.hotkey 是全局性的，不能针对某个app 单独bind,彩一种折衷的办法，某个app激活时绑定特定的键，
-- local appLocalKeyBindWatcher = hs.application.watcher.new(function(name, eventType, app)
--       if eventType ~= hs.application.watcher.activated then return end

--       -- diable all keybind in  appKeyBindMap 避免其他app 中的bind干扰
--       for k, bind in pairs(appKeyBindMap) do
--          for i, keybind in ipairs(bind) do
--             keybind["disable"](keybind)
--          end
--       end

--       local bind=appKeyBindMap[name]
--       if bind==nil then
--          return
--       end

--       for i, keybind in ipairs(bind) do
--          keybind["enable"](keybind)
--       end
--       globalKeyBind()
-- end)
-- appLocalKeyBindWatcher:start()

