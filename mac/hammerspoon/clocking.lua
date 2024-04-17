local clockingLog = hs.logger.new("clocking")
local clockingMenu = hs.menubar.new()
local currentTask = nil

local function trim(s)
   return (s:gsub("^%s*(.-)%s*$", "%1"))
end

local function eval_elisp(sexp, callback)
   hs.task.new(
      "/usr/local/bin/emacsclient",
      function(exitCode, stdOut, stdErr)
         if exitCode == 0 then
            callback(trim(stdOut))
         end
      end,
      { "--eval", sexp }
   ):start()
end


local function updateClockingMenu()
   eval_elisp(
      "(org-clock-is-active)",
      function(value)
         if value == "nil" then
            clockingMenu:setTitle("No Task")
         else
            eval_elisp(
               "(substring-no-properties(org-clock-get-clock-string))",
                function(value)
                   hs.alert.show(string.match(value, '"(.+)"'))
                   clockingMenu:setTitle(string.match(value, '"(.+)"'))
                end
            )
         end
      end
   )
end

local function startUpdatingClockingMenu()
   hs.timer.doEvery(10, updateClockingMenu)
end

local mod = {}

function mod.init()
    updateClockingMenu()
    startUpdatingClockingMenu()
end

return mod