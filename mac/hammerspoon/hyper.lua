-- other lua require this
hyper={"cmd","ctrl"}            -- space
hyper2={"cmd","ctrl","alt","shift"} -- ,
hyper3={"cmd","alt"}               -- caplock

local function paste_dwim()
   hs.eventtap.keyStrokes(hs.pasteboard.getContents())
   -- local app =hs.application.frontmostApplication()
   -- if app ~= nil and app:bundleID() == "com.cisco.Cisco-AnyConnect-Secure-Mobility-Client"    then
   -- else
   --    hs.eventtap.keyStrokes(hs.pasteboard.getContents())
   -- end
end

hs.hotkey.bind(hyper2, "v", paste_dwim)
hs.urlevent.bind("fnv_paste", paste_dwim)


hs.hotkey.bind(hyper, "f12", function() hs.caffeinate.lockScreen() end)
