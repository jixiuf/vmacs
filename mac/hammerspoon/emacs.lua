

---------------------------------------------------------------
-- 主要用来实现 当输入法开始进入中文模式时，使用emacs 自动进入evil-insert-state(相当于vim中进入insert mode)
-- 在当在emacs中编辑文件时，模拟按下f18键，以使emacs 进入evil-insert-state状态
function emacs_evil_insert_state()
   local win = hs.window.frontmostWindow()
   if win ==nil then
      return
   end
   local app = win:application()
   if app:bundleID()=="org.gnu.Emacs" then
      -- emacs 中f18 绑定为进入evil-insert-state
      -- 此处模拟在emacs 中按f18键

      -- (global-set-key (kbd "<f18>") 'evil-insert-state) ;mac karabiner用来控制输入法
      -- (define-key isearch-mode-map (kbd "<f18>") 'evil-insert-state) ;详见isearch-pre-command-hook

      hs.eventtap.keyStroke({}, "f18")
   elseif app:bundleID()=="com.googlecode.iterm2" then -- 如果是在终端下，通过终端的窗口标题来区分当前是否在用emacs编辑文件
      -- 如果是在iterm2终端中，也模拟按下f18键，
      -- hs.alert.show(win:title())
      if string.match(win:title(), " em ") then -- em is an emacsclient wrapper
         hs.eventtap.keyStroke({}, "f18")
      elseif string.match(win:title(), " emacs ") then
         hs.eventtap.keyStroke({}, "f18")
      elseif string.match(win:title(), " emacsclient ") then
         hs.eventtap.keyStroke({}, "f18")
      elseif string.match(win:title(), " vim ") then
         -- do something when you edit file using vim in iterm
      end
   end
end
hs.urlevent.bind("emacs_evil_insert_state", function() emacs_evil_insert_state() end)
-- open -g "hammerspoon://hello"
-- hs.urlevent.bind("hello", function() hs.eventtap.keyStroke({"shift"}, "capslock") end)





---------------------------------------------------------------
-- 主要用来实现 当输入法开始进入中文模式时，使用emacs 自动进入evil-insert-state(相当于vim中进入insert mode)
-- 在当在emacs中编辑文件时，模拟按下f18键，以使emacs 进入evil-insert-state状态
function emacs_evil_normal_state()
   local win = hs.window.frontmostWindow()
   if win ==nil then
      return
   end
   local app = win:application()
   if app:bundleID()=="org.gnu.Emacs" then
      -- emacs 中f18 绑定为进入evil-insert-state
      -- 此处模拟在emacs 中按f18键

      -- (global-set-key (kbd "<f18>") 'evil-insert-state) ;mac karabiner用来控制输入法
      -- (define-key isearch-mode-map (kbd "<f18>") 'evil-insert-state) ;详见isearch-pre-command-hook

      hs.eventtap.keyStroke({}, "f17")
   elseif app:bundleID()=="com.googlecode.iterm2" then -- 如果是在终端下，通过终端的窗口标题来区分当前是否在用emacs编辑文件
      -- 如果是在iterm2终端中，也模拟按下f18键，
      -- hs.alert.show(win:title())
      if string.match(win:title(), " em ") then -- em is an emacsclient wrapper
         hs.eventtap.keyStroke({}, "f17")
      elseif string.match(win:title(), " emacs ") then
         hs.eventtap.keyStroke({}, "f17")
      elseif string.match(win:title(), " emacsclient ") then
         hs.eventtap.keyStroke({}, "f17")
      elseif string.match(win:title(), " vim ") then
         -- do something when you edit file using vim in iterm
      end
   end
end

hs.urlevent.bind("emacs_evil_normal_state", function() emacs_evil_normal_state() end)
