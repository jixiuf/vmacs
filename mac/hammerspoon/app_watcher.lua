local events = hs.uielement.watcher

watchers = {}

function init()
  appsWatcher = hs.application.watcher.new(handleGlobalAppEvent)
  appsWatcher:start()

  -- Watch any apps that already exist
  local apps = hs.application.runningApplications()
  for i = 1, #apps do
    if apps[i]:title() ~= "Hammerspoon" then
      watchApp(apps[i], true)
    end
  end
end

function handleGlobalAppEvent(name, event, app)
  if     event == hs.application.watcher.launched then
    watchApp(app)
  elseif event == hs.application.watcher.terminated then
    -- Clean up
    local appWatcher = watchers[app:pid()]
    if appWatcher then
      appWatcher.watcher:stop()
      for id, watcher in pairs(appWatcher.windows) do
        watcher:stop()
      end
      watchers[app:pid()] = nil
    end
  end
end

function watchApp(app, initializing)
  if watchers[app:pid()] then return end

  local watcher = app:newWatcher(handleAppEvent)
  watchers[app:pid()] = {watcher = watcher, windows = {}}

  -- http://www.hammerspoon.org/docs/hs.uielement.watcher.html
  -- 设置设置监控哪些事件
  watcher:start({events.windowCreated, events.focusedWindowChanged ,events.focusedElementChanged})

  -- Watch any windows that already exist
  for i, window in pairs(app:allWindows()) do
    watchWindow(window, initializing)
  end
end

function handleAppEvent(element, event)
  if event == events.windowCreated then
    watchWindow(element)
  elseif event == events.focusedElementChanged then
     handleFocusedElementChanged(element,event)
  -- elseif event == events.focusedWindowChanged then
    -- Handle window change


  end
end

function watchWindow(win, initializing)
  local appWindows = watchers[win:application():pid()].windows
  if win:isStandard() and not appWindows[win:id()] then
    local watcher = win:newWatcher(handleWindowEvent, {pid=win:pid(), id=win:id()})
    appWindows[win:id()] = watcher

    watcher:start({events.elementDestroyed, events.windowResized, events.windowMoved})

    -- if not initializing then
    --   hs.alert.show('window created: '..win:id()..' with title: '..win:title())
    -- end
  end
end

function handleWindowEvent(win, event, watcher, info)
  if event == events.elementDestroyed then
    watcher:stop()
    watchers[info.pid].windows[info.id] = nil
    handleWindowDestroy(win,event,watcher,info)
  else
    -- Handle other events...
  end
  -- hs.alert.show('window event '..event..' on '..info.id)
end
function handleWindowDestroy(win, event, watcher, info)
   -- auto kill mplayer after window closed
   if win:application():title() == "MPlayerX" then
      win:application():kill()
   end
end

function handleFocusedElementChanged(element,event)
   -- 处理窗口内焦点变化事件
   -- hs.alert.show("handleAppEvent.focusedElementChanged.uielement.role: " .. element:role())
   -- print("handleAppEvent.focusedElementChanged.uielement.role: " .. element:role())
end

init()
