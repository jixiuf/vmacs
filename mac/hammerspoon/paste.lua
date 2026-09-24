require("hyper")
--[[
   From https://github.com/victorso/.hammerspoon/blob/master/tools/clipboard.lua
   Converted to plugin by Diego Zamboni

   This is my attempt to implement a jumpcut replacement in Lua/Hammerspoon.
   It monitors the clipboard/pasteboard for changes, and stores the strings you copy to the transfer area.
   You can access this history on the menu (Unicode scissors icon).
   Clicking on any item will add it to your transfer area.
   If you open the menu while pressing option/alt, you will enter the Direct Paste Mode. This means that the selected item will be
   "typed" instead of copied to the active clipboard.
   The clipboard persists across launches.
   -> Ng irc suggestion: hs.settings.set("jumpCutReplacementHistory", clipboard_history)
]]--

local mod={}

-- Feel free to change these settings
mod.config = {
   -- Key binding
   clipboard_menu_key = {hyper, "y" },
   -- Speed in seconds to check for clipboard changes. If you check
   -- too frequently, you will loose performance, if you check
   -- sparsely you will loose copies
   frequency = 0.8,
   -- How many items to keep on history
   hist_size = 300,
   -- How wide (in characters) the dropdown menu should be. Copies
   -- larger than this will have their label truncated and end with
   -- "…" (unicode for elipsis ...)
   label_length = 70,
   --asmagill request. If any application clears the pasteboard, we
   --also remove it from the history
   --https://groups.google.com/d/msg/hammerspoon/skEeypZHOmM/Tg8QnEj_N68J
   honor_clearcontent = false,
   -- Auto-type on click
   paste_on_select = true,
   -- Show item count in the menu item
   show_menu_counter = false,
   -- Show menu in the menubar
   show_in_menubar = true,
   -- Title to show on the menubar, if enabled above
   menubar_title = "\u{1F4CB}",
   -- Use hs.menubar or hs.chooser?
   -- use_chooser = false
   use_chooser = (hs.chooser ~= nil),
}

-- Chooser/menu object
mod.menubar = nil
mod.chooser = nil
-- Cache for focused window to work around the current window losing focus after the chooser comes up
mod.prevFocusedWindow = nil

-- Don't change anything bellow this line
local pasteboard = require("hs.pasteboard") -- http://www.hammerspoon.org/docs/hs.pasteboard.html
local settings = require("hs.settings") -- http://www.hammerspoon.org/docs/hs.settings.html
local last_change = pasteboard.changeCount() -- displays how many times the pasteboard owner has changed // Indicates a new copy has been made

--Array to store the clipboard history
local clipboard_history = settings.get("so.victor.hs.jumpcut.jixiuf") or {} --If no history is saved on the system, create an empty history
local clipboard_history_persist = settings.get("so.victor.hs.jumpcut.persist") or {} --If no history is saved on the system, create an empty history

-- Debounced persistence: settings.set serializes the whole history to
-- NSUserDefaults on every call, which lags badly when entries are large.
-- Instead, schedule a single write 2s after the last change.
local saveTimer = nil
local function saveHistory()
   settings.set("so.victor.hs.jumpcut.jixiuf", clipboard_history)
   settings.set("so.victor.hs.jumpcut.persist", clipboard_history_persist)
end
local function scheduleSave()
   if saveTimer ~= nil then saveTimer:stop() end
   saveTimer = hs.timer.doAfter(2, function()
      saveTimer = nil
      saveHistory()
   end)
end

-- Build a short single-line preview for chooser/menu rows. Rendering huge or
-- multiline strings in hs.chooser rows is slow; full content stays in history.
local function previewText(v)
   if type(v) ~= "string" then return v end
   local s = v:gsub("[\r\n]+", " ")
   if #s > mod.config.label_length then
      s = string.sub(s, 1, mod.config.label_length) .. "…"
   end
   return s
end

-- Chooser state. Rows shown in the UI carry only a short preview plus a
-- numeric id; the full contents and pre-lowered search text live in these
-- Lua-side tables. Keeping large strings out of the row tables is essential:
-- hs.chooser converts every row table to NSArray/NSDictionary on each query
-- refresh, so full text must never cross the Lua/ObjC bridge per keystroke.
local chooserAllRows = {}      -- all rows, rebuilt when the chooser opens
local chooserFilteredRows = {} -- rows matching the current query
local chooserFullContents = {} -- id -> full content
local chooserSearchIndex = {}  -- id -> lowercased full text (for fast find)
local chooserRowId = 0

-- Append a history counter to the menu
function setTitle()
   if ((#clipboard_history == 0) or (mod.config.show_menu_counter == false)) then
      -- Clipboard Emoji: http://emojipedia.org/clipboard/
      mod.menubar:setTitle(mod.config.menubar_title) -- Unicode magic
   else
      mod.menubar:setTitle(mod.config.menubar_title .. " (".. (#clipboard_history + #clipboard_history_persist) ..")") -- updates the menu counter
   end
end

function putOnPaste(value,key)
   if mod.prevFocusedWindow ~= nil then
      mod.prevFocusedWindow:focus()
   end
   if value == nil then return end
   if type(value) == "table" then
      if value.valid == false then return end -- placeholder row (e.g. empty history)
      if value.id ~= nil then
         value = chooserFullContents[value.id] -- resolve preview row to full content
         if value == nil then return end
      elseif value.fullContent ~= nil then
         value = value.fullContent
      end
   end
   if type(value) == "string" then
      pasteboard.setContents(value)
   else if type(value) == "table" then
         if value.image ~= nil then
            pasteboard.setImageContents(value.image)
         else
            pasteboard.setContents(value.text)
         end
        else
           if pasteboard.setImageContents ~= nil then
              pasteboard.setImageContents(value)
           end
        end
   end
   last_change = pasteboard.changeCount()

   if (mod.config.paste_on_select) then
      hs.eventtap.keyStroke({"cmd"}, "v")
   else
      -- if (key.alt == true) then -- If the option/alt key is active when clicking on the menu, perform a "direct paste", without changing the clipboard
      --    hs.eventtap.keyStroke({"cmd"}, "v") -- Defeating paste blocking http://www.hammerspoon.org/go/#pasteblock
      -- end
   end
end

-- Clears the clipboard and history
function clearAll()
   pasteboard.clearContents()
   clipboard_history = {}
   scheduleSave()
   now = pasteboard.changeCount()
   setTitle()
end
function clearAllPersist()
   pasteboard.clearContents()
   clipboard_history_persist = {}
   scheduleSave()
   now = pasteboard.changeCount()
   setTitle()
end

-- Clears the last added to the history
function clearLastItem()
   table.remove(clipboard_history,#clipboard_history)
   scheduleSave()
   now = pasteboard.changeCount()
   setTitle()
end

function pasteboardToClipboard(item)
   if clipboard_history~=nil and  #clipboard_history~=0 then
      -- 去重
      for k,v in pairs(clipboard_history) do
         if v==item then
            return
         end
      end -- end for
      for k,v in pairs(clipboard_history_persist) do
         if v==item then
            return
         end
      end -- end for
   end


   -- Loop to enforce limit on qty of elements in history. Removes the oldest items
   while (#clipboard_history >= mod.config.hist_size) do
      table.remove(clipboard_history,1)
   end
   table.insert(clipboard_history, item)
   scheduleSave() -- updates the saved history (debounced)
   setTitle() -- updates the menu counter
end


function persistLastItem()
   if clipboard_history==nil or #clipboard_history==0 then
      return
   end

   -- Loop to enforce limit on qty of elements in history. Removes the oldest items
   table.insert(clipboard_history_persist, clipboard_history[#clipboard_history])
   scheduleSave() -- updates the saved history (debounced)
   clearLastItem()
end
-- Dynamic menu by cmsj https://github.com/Hammerspoon/hammerspoon/issues/61#issuecomment-64826257
populateMenubar = function(key)
   setTitle() -- Update the counter every time the menu is refreshed
   menuData = {}
   table.insert(menuData, {title="清除所有 临时内容", fn = function() clearAll() end })
   table.insert(menuData, {title="清除所有 永久保存的内容", fn = function() clearAllPersist() end })

   table.insert(menuData, {title="永久保存 当前剪切板内容", fn = function() persistLastItem() end })
   if (key.shift == true or mod.config.paste_on_select) then
      table.insert(menuData, {title="Direct Paste Mode ✍", disabled=true})
   end
   table.insert(menuData, {title="-"})
   local staticMenuCount= #menuData+1

   if (#clipboard_history == 0) then
      table.insert(menuData, {title="None", disabled = true}) -- If the history is empty, display "None"
   else
      for k,v in pairs(clipboard_history) do
         if (type(v) == "string" and string.len(v) > mod.config.label_length) then
            table.insert(menuData,staticMenuCount, {title=string.sub(v,0,mod.config.label_length).."…", fn = function() putOnPaste(v,key) end }) -- Truncate long strings
         else
            if type(v) == "userdata" then
               table.insert(menuData,staticMenuCount, {title="(image)", fn = function() putOnPaste(v,key) end })
            else
               table.insert(menuData,staticMenuCount, {title=v, fn = function() putOnPaste(v,key) end })
            end
         end -- end if else
      end-- end for

      for k,v in pairs(clipboard_history_persist) do
         if (type(v) == "string" and string.len(v) > mod.config.label_length) then
            table.insert(menuData,staticMenuCount, {title=string.sub(v,0,mod.config.label_length).."…", fn = function() putOnPaste(v,key) end }) -- Truncate long strings
         else
            if type(v) == "userdata" then
               table.insert(menuData,staticMenuCount, {title="(image)", fn = function() putOnPaste(v,key) end })
            else
               table.insert(menuData,staticMenuCount, {title=v, fn = function() putOnPaste(v,key) end })
            end
         end -- end if else
      end-- end for
   end-- end if else
   -- footer
   return menuData
end

-- When a queryChangedCallback is set, HSChooser skips its own filtering
-- entirely, so search cost and result order are fully under our control here.
-- Filtering uses a plain C-level string.find over pre-lowered text.
local function addChooserRow(menuData, v)
   chooserRowId = chooserRowId + 1
   local id = chooserRowId
   chooserFullContents[id] = v
   chooserSearchIndex[id] = (type(v) == "string") and string.lower(v) or ""
   if type(v) == "userdata" then
      table.insert(menuData, {text="(image)", image=v, id=id})
   else
      table.insert(menuData, {text=previewText(v), id=id})
   end
end

local function buildChooserRows()
   chooserRowId = 0
   chooserFullContents = {}
   chooserSearchIndex = {}
   local rows = {}
   if (#clipboard_history == 0 and #clipboard_history_persist == 0) then
      table.insert(rows, {text="Clipboard history is empty", valid=false})
   else
      for i = #clipboard_history, 1, -1 do -- newest first
         addChooserRow(rows, clipboard_history[i])
      end
      for i = #clipboard_history_persist, 1, -1 do
         addChooserRow(rows, clipboard_history_persist[i])
      end
   end
   chooserAllRows = rows
   chooserFilteredRows = rows
end

local function chooserQueryChanged(query)
   query = string.lower(query or "")
   if query == "" then
      chooserFilteredRows = chooserAllRows
   else
      local filtered = {}
      for _, row in ipairs(chooserAllRows) do
         local hay = chooserSearchIndex[row.id]
         if hay and string.find(hay, query, 1, true) then
            table.insert(filtered, row)
         end
      end
      chooserFilteredRows = filtered
   end
   mod.chooser:refreshChoicesCallback()
end

-- If the pasteboard owner has changed, we add the current item to our history and update the counter.
function storeCopy()
   now = pasteboard.changeCount()
   if (now > last_change) then
      current_clipboard = pasteboard.getContents()
      if current_clipboard == nil then
         return
      end
      if (current_clipboard == nil) and (pasteboard.getImageContents ~= nil) then
         pcall(function() current_clipboard = pasteboard.getImageContents() end)
      end
      -- asmagill requested this feature. It prevents the history from keeping items removed by password managers
      if (current_clipboard == nil and mod.config.honor_clearcontent) then
         clearLastItem()
      else
         pasteboardToClipboard(current_clipboard)
      end
      last_change = now
   end
end

function mod.init()
   mod.menubar= hs.menubar.new(mod.config.show_in_menubar)
   mod.menubar:setTooltip("Clipboard history")
   mod.menubar:setMenu(populateMenubar)

   if mod.config.use_chooser then
      mod.chooser = hs.chooser.new(putOnPaste)
      mod.chooser:choices(function() return chooserFilteredRows end)
      mod.chooser:queryChangedCallback(chooserQueryChanged)
      hs.hotkey.bind(mod.config.clipboard_menu_key[1],
                     mod.config.clipboard_menu_key[2],
                     function()
                        buildChooserRows()
                        mod.prevFocusedWindow = hs.window.focusedWindow()
                        mod.chooser:show()
      end)
   else
      hs.hotkey.bind(mod.config.clipboard_menu_key[1],
                     mod.config.clipboard_menu_key[2],
                     function() mod.menubar:popupMenu(hs.mouse.getAbsolutePosition()) end)
   end

   --Checks for changes on the pasteboard. Is it possible to replace with eventtap?
   timer = hs.timer.new(mod.config.frequency, storeCopy)
   timer:start()

   setTitle() --Avoid wrong title if the user already has something on his saved history

end

mod.init()
-- return mod
