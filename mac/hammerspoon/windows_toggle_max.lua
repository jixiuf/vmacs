-- require('hyper')
hs.hotkey.bind(hyper, "M", function ()toggleMaximized() end)
-- hs.hotkey.bind({"cmd"}, "M", function ()toggleMaximized() end)

local toggleMaximizedMap={}
-- hs.geometry.rect(0, -48, 400, 48)
function toggleMaximized()
   local win = hs.window.frontmostWindow()
   if win ==nil then
      return
   end
   local app = win:application()
   if app:bundleID() =="com.apple.finder" and win:role()== "AXScrollArea" then -- 如果是桌面
      return
   end
   -- if app:title()=="iTerm2" then
   --    toggleFullScreen()
   --    return
   -- end
   local bid = app:bundleID()
   local winKey= tostring(win:id())
   if bid ~=nil then
      winKey=app:bundleID() .. tostring(win:id())
   end
   local curFrame = win:frame()
   local originFrame=toggleMaximizedMap[winKey]
   local screen = win:screen()
   local max = screen:frame()
   -- hs.window.setFrameCorrectness=true

   if win:isFullScreen() then
      win:setFullScreen(false)
   end
   if win:isMinimized() then
      win:unminimize()
   end
   win:application():activate(true)
   win:application():unhide()
   win:focus()

   win:maximize(0)           -- 0s duration (无动画马上最大化)
   local maximizedFrame= win:frame() -- 最大化后的尺寸
   if math.abs(maximizedFrame.w-curFrame.w)<80 and math.abs(maximizedFrame.h-curFrame.h)<80 then -- 只要窗口大小跟全屏后的尺寸相差不大就认为是全屏状态
      if  originFrame then
         win:setFrame(originFrame,0) -- 恢复成初始窗口大小
      else                      -- 没有存窗口的初始大小，则随机将其调到一个位置
         win:moveToUnit(hs.geometry.rect(math.random()*0.1,math.random()*0.1, 0.718, 0.718),0)
      end
   else                         -- 当前是非最大化状态，
      if not (maximizedFrame.w-curFrame.w<0 or maximizedFrame.h-curFrame.h<0 )then -- 从 fullscreen 恢复回来的则不记录其窗口大小
         toggleMaximizedMap[winKey]=hs.geometry.copy(curFrame) -- 存储当前窗口大小
      end
   end
end
hs.urlevent.bind("toggleMaximized", function(eventName, params)  toggleMaximized() end)

---------------------------------------------------------------
function toggleFullScreen ()
   local win = hs.window.frontmostWindow()
   if win ==nil then
      return
   end
   local app = win:application()
   if app:title()=="Finder" and win:role()== "AXScrollArea" then -- 如果是桌面
      return
   end
   win:application():activate(true)
   win:application():unhide()
   win:focus()
   win:toggleFullScreen()

end
---------------------------------------------------------------
