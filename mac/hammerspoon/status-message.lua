local drawing = require 'hs.drawing'
local geometry = require 'hs.geometry'
local screen = require 'hs.screen'
local styledtext = require 'hs.styledtext'

local statusmessage = {}
statusmessage.new = function(messageText)
  local buildParts = function(messageText)
    local frame = screen.primaryScreen():frame()

    local styledTextAttributes = {
      font = { name = 'Monaco', size = 24 },
    }

    local styledText = styledtext.new('🔨 ' .. messageText, styledTextAttributes)

    local styledTextSize = drawing.getTextDrawingSize(styledText)
    local textRect = {
      x = 200,
      y = 50,
      w = styledTextSize.w + 40,
      h = styledTextSize.h + 40,
    }
    local text = drawing.text(textRect, styledText):setAlpha(1)
    text:setTextColor({ red = 1, green = 0, blue = 0, alpha=1 })

    local background = drawing.rectangle(
      {
        x = 200,
        y = 50,
        w = styledTextSize.w + 15,
        h = styledTextSize.h + 6
      }
    )
    background:setRoundedRectRadii(10, 10)
    background:setFillColor({ red = 0, green = 1, blue = 0, alpha=1 })

    return background, text
  end

  return {
    _buildParts = buildParts,
    show = function(self)
      self:hide()

      self.background, self.text = self._buildParts(messageText)
      self.background:show()
      self.text:show()
    end,
    hide = function(self)
      if self.background then
        self.background:delete()
        self.background = nil
      end
      if self.text then
        self.text:delete()
        self.text = nil
      end
    end
  }
end

return statusmessage
