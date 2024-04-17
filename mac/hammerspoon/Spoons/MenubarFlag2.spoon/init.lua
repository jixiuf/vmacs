--- === MenubarFlag2 ===
---
--- Color the menubar according to the current keyboard layout
---
--- Download: [https://github.com/Hammerspoon/Spoons/raw/master/Spoons/MenubarFlag2.spoon.zip](https://github.com/Hammerspoon/Spoons/raw/master/Spoons/MenubarFlag2.spoon.zip)
---
--- Functionality inspired by [ShowyEdge](https://pqrs.org/osx/ShowyEdge/index.html.en)

local obj2={}
obj2.__index = obj2

local draw = require "hs.drawing"
local col2 = draw.color.x11

-- Metadata
obj2.name = "MenubarFlag2"
obj2.version = "0.1"
obj2.author = "Diego Zamboni <diego@zzamboni.org>"
obj2.homepage = "https://github.com/Hammerspoon/Spoons"
obj2.license = "MIT - https://opensource.org/licenses/MIT"

--- MenubarFlag2.allScreens
--- Variable
--- Boolean to specify whether the indicators should be shown on all monitors or just the current one. Defaults to `true`
obj2.allScreens = true

--- MenubarFlag2.indicatorHeight
--- Variable
--- Number to specify the height of the indicator. Specify 0.0-1.0 to specify a percentage of the height of the menu bar, larger values indicate a fixed height in pixels. Defaults to 1.0
obj2.indicatorHeight = 1.0

--- MenubarFlag2.indicatorAlpha
--- Variable
--- Number to specify the indicator transparency (0.0 - invisible; 1.0 - fully opaque). Defaults to 0.3
obj2.indicatorAlpha = 0.3

--- MenubarFlag2.indicatorInAllSpaces
--- Variable
--- Boolean to specify whether the indicator should be shown in all spaces (this includes full-screen mode). Defaults to `true`
obj2.indicatorInAllSpaces = true

--- MenubarFlag2.colors
--- Variable
--- Table that contains the configuration of indicator colors
---
--- The table below indicates the colors to use for a given keyboard
--- layout. The index is the name of the layout as it appears in the
--- input source menu. The value of each indicator is a table made of
--- an arbitrary number of segments, which will be distributed evenly
--- across the width of the screen. Each segment must be a valid
--- `hs.drawing.color` specification (most commonly, you should just
--- use the named colors from within the tables). If a layout is not
--- found, then the indicators are removed when that layout is active.
---
--- Indicator specs can be static flag-like:
--- ```
---   Spanish = {col2.green, col2.white, col2.red},
---   German = {col2.black, col2.red, col2.yellow},
--- ```
--- or complex, programmatically-generated:
--- ```
--- ["U.S."] = (
---    function() res={}
---       for i = 0,10,1 do
---          table.insert(res, col2.blue)
---          table.insert(res, col2.white)
---          table.insert(res, col2.red)
---       end
---       return res
---    end)()
--- ```
--- or solid colors:
--- ```
---   Spanish = {col2.red},
---   German = {col2.yellow},
--- ```
--- Contributions of indicator specs are welcome!
obj2.colors = {
   ["U.S."] = { }, -- empty list or no table entry means "no indicator"
   Spanish = {col2.red, col2.yellow, col2.red},
   German = {col2.black, col2.red, col2.yellow},
}

--- MenubarFlag2.timerFreq
--- Variable
--- Number to indicate how frequently (in seconds) should the menubar indicator be updated. Defaults to 1.0.
---
--- Sometimes Hammerspoon misses the callback when the keyboard layout
--- changes. As a workaround, MenuBarFlag can automatically update the
--- indicator at a fixed frequency. The timer can be disabled by
--- setting this parameter to 0.
obj2.timerFreq = 1.0

obj2.logger = hs.logger.new('MenubarFlag2')
obj2.timer = nil
----------------------------------------------------------------------

-- Internal variables
local prevlayout2 = nil
local ind2 = nil

-- Initialize the empty indicator table
function initIndicators2()
   if ind2 ~= nil then
      delIndicators2()
   end
   ind2 = {}
end

-- Delete existing indicator objects
function delIndicators2()
   if ind2 ~= nil then
      for i,v in ipairs(ind2) do
         if v ~= nil then
            v:delete()
         end
      end
      ind2 = nil
   end
end

--- MenubarFlag2:drawIndicators(src)
--- Method
--- Draw the indicators corresponding to the given layout name
---
--- Parameters:
---  * src - name of the layout to draw. If the given element exists in `MenubarFlag2.colors`, it will be drawn. If it does not exist, then the indicators will be removed from the screen.
---
--- Returns:
---  * The MenubarFlag2 object
function obj2:drawIndicators(src)
   --   hs.alert.show("in drawindicators src=" .. src .. "  prevlayout2=" .. (prevlayout2 or "nil"))

   if src ~= prevlayout2 then
      initIndicators2()

      def = self.colors[src]
      self.logger.df("Indicator definition for %s: %s", src, hs.inspect(def))
      if def ~= nil then
         if self.allScreens then
            screens = hs.screen.allScreens()
         else
            screens = { hs.screen.mainScreen() }
         end
         for i,screen in ipairs(screens) do
            local screeng = screen:fullFrame()
            local width = screeng.w / #def
            for i,v in ipairs(def) do
               if self.indicatorHeight >= 0.0 and self.indicatorHeight <= 1.0 then
                  height = self.indicatorHeight*(screen:frame().y - screeng.y)
               else
                  height = self.indicatorHeight
               end
               c = draw.rectangle(hs.geometry.rect(screeng.x+(width*(i-1)), screeng.y,
                                                   width, height))
               c:setFillColor(v)
               c:setFill(true)
               c:setAlpha(self.indicatorAlpha)
               c:setLevel(draw.windowLevels.overlay)
               c:setStroke(false)
               if self.indicatorInAllSpaces then
                  c:setBehavior(draw.windowBehaviors.canJoinAllSpaces)
               end
               c:show()
               table.insert(ind2, c)
            end
         end
      else
         self.logger.df("Removing indicators for %s because there is no color definitions for it.", src)
         delIndicators2()
      end
   end

   prevlayout2 = src

   return self
end

--- MenubarFlag2:getLayoutAndDrawindicators
--- Method
--- Draw indicators for the current keyboard method or layout
---
--- Parameters:
---  * None
---
--- Returns:
---  * The MenubarFlag2 object
function obj2:getLayoutAndDrawIndicators()
   return self:drawIndicators(hs.keycodes.currentMethod() or hs.keycodes.currentLayout())
end

--- MenubarFlag2:start()
--- Method
--- Start the keyboard layout watcher to draw the menubar indicators.
function obj2:start()
   initIndicators2()
   self:getLayoutAndDrawIndicators()
   hs.keycodes.inputSourceChanged(function()
         self:getLayoutAndDrawIndicators()
   end)
   -- This solves the problem that the callback would not be called until the second layout change after a restart
   hs.focus()
   if (self.timerFreq > 0.0) then
     self.timer = hs.timer.new(self.timerFreq, function() self:getLayoutAndDrawIndicators() end):start()
   end
   return self
end

--- MenubarFlag2:stop()
--- Method
--- Remove indicators and stop the keyboard layout watcher
function obj2:stop()
  delIndicators2()
  if self.timer ~= nil then
    self.timer:stop()
    self.timer = nil
  end
  hs.keycodes.inputSourceChanged(nil)
  return self
end

return obj2
