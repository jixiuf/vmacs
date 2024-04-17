require('hyper')
-- 希望默认是英文输入状态, 需要的时候才切换到输入法进行中文输入.
local function Chinese()
   hs.keycodes.currentSourceID("im.rime.inputmethod.Squirrel.Rime")
   -- hs.alert.show("中文")
end

local function English()
   hs.keycodes.currentSourceID("com.apple.keylayout.US")
   -- hs.alert.show("英文")
end

local function set_app_input_method(app_name, set_input_method_function, event)
   event = event or hs.window.filter.windowFocused

   hs.window.filter.new(app_name)
      :subscribe(event, function()
                    set_input_method_function()
                end)
end



set_app_input_method('Hammerspoon', English, hs.window.filter.windowCreated)
set_app_input_method('Spotlight', English, hs.window.filter.windowCreated)
set_app_input_method('Alfred', English, hs.window.filter.windowCreated)
set_app_input_method('Emacs', English)
set_app_input_method('iTerm2', English)
set_app_input_method('Google Chrome', English)
set_app_input_method('WeChat', Chinese)


-- hs.keycodes.inputSourceChanged(
--    function()
--       if string.find(hs.keycodes.currentSourceID(), "com.apple.keylayout.US") then
--          -- hs.alert.show("英文" .. hs.keycodes.currentSourceID())
--          hs.alert.show("英文")
--       else
--          hs.alert.show("中文")
--       end
--    end
-- )



-- -- -- 如果不知道当前的应用的名字和输入法SourceID, 可以用下面的函数查看:
-- hs.hotkey.bind(hyper2, ".", function()
-- 		  hs.alert.show("App path:        "
-- 				..hs.window.focusedWindow():application():path()
-- 				.."\n"
-- 				.."App name:      "
-- 				..hs.window.focusedWindow():application():name()
-- 				.."\n"
-- 				.."IM source id:  "
-- 				..hs.keycodes.currentSourceID())
-- end)
