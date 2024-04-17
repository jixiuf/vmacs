require('hyper')
--------------------------------------------------------------------------------
-- Define WindowLayout Mode
--
-- WindowLayout Mode allows you to manage window layout using keyboard shortcuts
-- that are on the home row, or very close to it. Use Control+s to turn
-- on WindowLayout mode. Then, use any shortcut below to perform a window layout
-- action. For example, to send the window left, press and release
-- Control+s, and then press h.
--
--   h/j/k/l => send window to the left/bottom/top/right half of the screen
--   8 => send window to the upper left quarter of the screen
--   9 => send window to the upper right quarter of the screen
--   i => send window to the lower left quarter of the screen
--   o => send window to the lower right quarter of the screen
--   return => quit
--   n => send window to the next monitor
--------------------------------------------------------------------------------

windowLayoutMode = hs.hotkey.modal.new(hyper, 'm')
local message = require('status-message')
windowLayoutMode.statusMessage = message.new('(主键 H-m esc 退出)(H-hjkl 上下左右) (hjkl) (H-89io 4 角) (调大小-+)')
windowLayoutMode.entered = function()
  windowLayoutMode.statusMessage:show()
end
windowLayoutMode.exited = function()
  windowLayoutMode.statusMessage:hide()
end

-- Bind the given key to call the given function and exit WindowLayout mode
function windowLayoutMode.bindWithAutomaticExit(mode,mod, key, fn)
  mode:bind(mod, key, function()
    mode:exit()
    fn()
  end)
end
windowLayoutMode:bind({},'escape', function()
      windowLayoutMode:exit()
end)
windowLayoutMode:bind({"ctrl"},'g', function()
      windowLayoutMode:exit()
end)
windowLayoutMode:bind({},'=', function() winIncrease() end)
windowLayoutMode:bind({},'-', function() winReduce() end)


windowLayoutMode:bind({},'h', function()
                         moveLeft(hs.window.focusedWindow())
end)
windowLayoutMode:bind({},'l', function()
                         moveRight(hs.window.focusedWindow())
end)

windowLayoutMode:bind({},'j', function()
                         moveDown(hs.window.focusedWindow())
end)
windowLayoutMode:bind({},'k', function()
                         moveUp(hs.window.focusedWindow())
end)


windowLayoutMode:bindWithAutomaticExit({},'m', function()
      toggleMaximized()
end)
windowLayoutMode:bindWithAutomaticExit(hyper,'m', function()
      toggleMaximized()
end)

windowLayoutMode:bindWithAutomaticExit({},'space', function()
      centerWithFullHeight(hs.window.focusedWindow())
end)

windowLayoutMode:bindWithAutomaticExit(hyper,'h', function()
      winleft(hs.window.focusedWindow())
end)

windowLayoutMode:bindWithAutomaticExit(hyper,'j', function()
      windown(hs.window.focusedWindow())
end)

windowLayoutMode:bindWithAutomaticExit(hyper,'k', function()
      winup(hs.window.focusedWindow())
end)

windowLayoutMode:bindWithAutomaticExit(hyper,'l', function()
      winright(hs.window.focusedWindow())
end)

windowLayoutMode:bindWithAutomaticExit(hyper,'8', function()
      upLeft(hs.window.focusedWindow())
end)

windowLayoutMode:bindWithAutomaticExit(hyper,'9', function()
      upRight(hs.window.focusedWindow())
end)

windowLayoutMode:bindWithAutomaticExit(hyper,'i', function()
      downLeft(hs.window.focusedWindow())
end)
windowLayoutMode:bindWithAutomaticExit(hyper,'o', function()
      downRight(hs.window.focusedWindow())
end)

-- windowLayoutMode:bindWithAutomaticExit({},'n', function()
--       nextScreen(hs.window.focusedWindow())
-- end)

-- Use Control+s to toggle WindowLayout Mode
-- hs.hotkey.bind(hyper, 't', function()
--   windowLayoutMode:enter()
-- end)
-- windowLayoutMode:bind(hyper, 't', function()
--   windowLayoutMode:exit()
-- end)

-- hs.hotkey.bind({"cmd"}, "LEFT", function()
--    hs.window.focusedWindow():left()
-- end)
-- open -g hammerspoon://moveWinLeft
-- karabiner 绑定 Fn+Left 键，因 hammerspoon 不支持 Fn 的绑定
-- hs.urlevent.bind("moveWinLeft", function(eventName, params) hs.window.focusedWindow():left() end)
-- hs.urlevent.bind("moveWinRight", function(eventName, params) hs.window.focusedWindow():right() end)
-- hs.urlevent.bind("moveWinUp", function(eventName, params) hs.window.focusedWindow():up() end)
-- hs.urlevent.bind("moveWinDown", function(eventName, params) hs.window.focusedWindow():down() end)
-- hs.urlevent.bind("winIncrease", function(eventName, params) winIncrease() end)
-- hs.urlevent.bind("winReduce", function(eventName, params) winReduce() end)
-- hs.hotkey.bind({"cmd","shift"}, "R", function() winReduce() end)


hs.window.animationDuration = 0

-- +-----------------+
-- |        |        |
-- |  HERE  |        |
-- |        |        |
-- +-----------------+
-- hs.window.focusedWindow():left()
function winleft(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end

-- +-----------------+
-- |        |        |
-- |        |  HERE  |
-- |        |        |
-- +-----------------+
function winright(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w / 2)
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end

-- +-----------------+
-- |      HERE       |
-- +-----------------+
-- |                 |
-- +-----------------+
function winup(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.w = max.w
  f.y = max.y
  f.h = max.h / 2
  win:setFrame(f)
end

-- +-----------------+
-- |                 |
-- +-----------------+
-- |      HERE       |
-- +-----------------+
function windown(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.w = max.w
  f.y = max.y + (max.h / 2)
  f.h = max.h / 2
  win:setFrame(f)
end

-- +-----------------+
-- |  HERE  |        |
-- +--------+        |
-- |                 |
-- +-----------------+
function upLeft(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:fullFrame()

  f.x = 0
  f.y = 0
  f.w = max.w/2
  f.h = max.h/2
  win:setFrame(f)
end

-- +-----------------+
-- |                 |
-- +--------+        |
-- |  HERE  |        |
-- +-----------------+
function downLeft(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:fullFrame()

  f.x = 0
  f.y = max.h/2
  f.w = max.w/2
  f.h = max.h/2
  win:setFrame(f)
end

-- +-----------------+
-- |                 |
-- |        +--------|
-- |        |  HERE  |
-- +-----------------+
function downRight(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:fullFrame()

  f.x = max.w/2
  f.y = max.h/2
  f.w = max.w/2
  f.h = max.h/2

  win:setFrame(f)
end

-- +-----------------+
-- |        |  HERE  |
-- |        +--------|
-- |                 |
-- +-----------------+
function upRight(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:fullFrame()

  f.x = max.w/2
  f.y = 0
  f.w = max.w/2
  f.h = max.h/2
  win:setFrame(f)
end

-- +--------------+
-- |  |        |  |
-- |  |  HERE  |  |
-- |  |        |  |
-- +---------------+
function centerWithFullHeight(win)
  local f = win:frame()
  local screen = win:screen()
  local max = screen:fullFrame()

  f.x = max.w * 1/5
  f.w = max.w * 3/5
  f.y = max.y
  f.h = max.h
  win:setFrame(f)
end

function moveLeft(win)
  local f = win:frame()

  f.x = f.x-80
  win:setFrame(f)
end
function moveRight(win)
  local f = win:frame()

  f.x = f.x+80
  win:setFrame(f)
end
function moveUp(win)
  local f = win:frame()

  f.y = f.y-60
  win:setFrame(f)
end
function moveDown(win)
  local f = win:frame()

  f.y = f.y+60
  win:setFrame(f)
end
function nextScreen(win)
  local currentScreen = win:screen()
  local allScreens = hs.screen.allScreens()
  currentScreenIndex = hs.fnutils.indexOf(allScreens, currentScreen)
  nextScreenIndex = currentScreenIndex + 1

  if allScreens[nextScreenIndex] then
    win:moveToScreen(allScreens[nextScreenIndex])
  else
    win:moveToScreen(allScreens[1])
  end
end

function winIncrease()
   local win = hs.window.focusedWindow()
   if win==nil then
      return
   end
   local curFrame = win:frame()
   local screen = win:screen()
   if screen==nil then
      return
   end
   local max = screen:frame()
   local inscW =120
   if (max.w-curFrame.w)==0 then
      win:setFrame(max)
      return
   end
   local inscH =inscW*(max.h-curFrame.h)/(max.w-curFrame.w)


   if max.w-curFrame.h<inscW and max.h-curFrame.h<inscW then
      win.setFrame(max)
   else
      curFrame.w=curFrame.w +inscW
      local a = (curFrame.x-max.x) -- 左边空白的宽度
      local b =((max.x+max.w)-(curFrame.w+curFrame.x)) -- 右边空白的宽度
      if b<0 then
         curFrame.w=max.w
         curFrame.x=max.x
         -- elseif b-a==0 then
         --    curFrame.x=max.x
      else
         -- a*(inscW-m)=b*m -->a*inscW-a*m=b*m
         if b+a==0 then
            return
         end
         local m =inscW*a/(b+a)                         -- 左边应变化的尺寸
         curFrame.x=curFrame.x-m                          -- 变化后左边的坐标
         if curFrame.x<max.x then
            curFrame.x=max.x
         end
      end

      curFrame.h=curFrame.h +inscH
      local a = (curFrame.y-max.y) -- 左边空白的宽度
      local b =((max.y+max.h)-(curFrame.h+curFrame.y)) -- 右边空白的宽度
      if b<0 then
         curFrame.h=max.h
         curFrame.y=max.y
         -- elseif b-a==0 then
         --    curFrame.y=max.y
      else
         -- a*(inscH-m)=b*m -->a*inscH-a*m=b*m
         if b+a==0 then
            win:setFrame(max)
            return
         end
         local m =inscH*a/(b+a)                         -- 左边应变化的尺寸
         curFrame.y=curFrame.y-m                          -- 变化后左边的坐标
         if curFrame.y<max.y then
            curFrame.y=max.y
         end
      end


      win:setFrame(curFrame)
   end
end

function winReduce()
   local win = hs.window.focusedWindow()
   if win==nil then
      return
   end
   local curFrame = win:frame()
   local screen = win:screen()
   if screen==nil then
      return
   end
   local max = screen:frame()
   local inscW =100
   if curFrame.w==0 then
      return
   end
   local inscH =inscW*(curFrame.h)/(curFrame.w)


   -- hs.alert.show(tostring((max.w-curFrame.w)))
   curFrame.w =curFrame.w-inscW
   curFrame.x =curFrame.x+inscW/2


   -- hs.alert.show(tostring((max.h-curFrame.h)))
   curFrame.h =curFrame.h-inscH
   curFrame.y =curFrame.y+inscH/2
   win:setFrame(curFrame)
end
