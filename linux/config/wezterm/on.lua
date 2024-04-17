local wezterm = require 'wezterm'
local act = wezterm.action

local function display_copy_mode(window, pane)
	local name = window:active_key_table()
	if name then
		name = "Mode: " .. name
	end
	return { { Attribute = { Italic = false } }, { Text = name or "" } }
end
local function update_window_background(window, pane)
	local overrides = window:get_config_overrides() or {}
	-- If there's no foreground process, assume that we are "wezterm connect" or "wezterm ssh"
	-- and use a different background color
	local name = window:active_key_table()
    if name == "copy_mode" then
       -- overrides.window_background_image = '/home/jixiuf/.config/wallpaper/wallpaper.png'
       -- overrides.color_scheme = "OneHalfDark"
       -- overrides.colors = { background = "#222222" }
       -- overrides.window_background_opacity=1
       -- overrides.text_background_opacity=1.0 --
       -- overrides.macos_window_background_blur = 20
    else
       -- overrides.color_scheme = 'foot'
       -- overrides.window_background_image = nil
       -- overrides.window_background_opacity=0.9
       -- overrides.text_background_opacity=1.0
       -- overrides.cursor_fg=wezterm.color.get_default_colors().cursor_fg
       -- overrides.color_scheme = "OneHalfDark"
       -- overrides.colors = { background = wezterm.color.get_default_colors().background}
    end
    -- wezterm.log_info('2222opening: ' .. pane:get_foreground_process_name() )
	-- if pane:get_foreground_process_name() == nil then
	-- --  overrides.color_scheme = "Red Alert"
	-- end

	-- if overrides.color_scheme == nil then
	--  return
	-- end
	-- if pane:get_user_vars().production == "1" then
	--  overrides.color_scheme = "OneHalfDark"
	-- end
    -- overrides.window_background_image = '/home/jixiuf/.config/wallpaper/blured.jpg'
	window:set_config_overrides(overrides)
end
-- wezterm.on("update-right-status", function(window, pane)
--  -- local tmux = update_tmux_style_tab(window, pane)
--  local copy_mode = display_copy_mode(window, pane)
-- update_window_background(window, pane)
--  -- local status = utils.merge_lists(ssh, copy_mode)
--  window:set_right_status(wezterm.format(copy_mode))
-- end)

-- 使用emacsclient 打开scrollback
-- table.insert(keys,  {key = 'F', mods = 'CTRL|SHIFT', action = act.EmitEvent 'edit-scrollback'})
wezterm.on('edit-scrollback', function(window, pane)
  -- Retrieve the current viewport's text.
  --
  -- Note: You could also pass an optional number of lines (eg: 2000) to
  -- retrieve that number of lines starting from the bottom of the viewport.
  -- local viewport_text = pane:get_lines_as_text(20000)         -- 有换行
  local viewport_text = pane:get_logical_lines_as_text(20000) -- 无换行


  -- Create a temporary file to pass to emacsclient
  local name = os.tmpname()
  local f = io.open(name, 'w+')
  f:write(viewport_text)
  f:flush()
  f:close()

  -- Open a new tab running vim and tell it to open the file
  window:perform_action(
    act.SpawnCommandInNewTab {
       -- 加一个非常大的行号，让emacsclient 跳转到文件末
      args = { 'open-with', name..":100000"},
      -- args = { 'emacsclient',"-t","+10000", name},
    },
    pane
  )

  -- Wait "enough" time for vim to read the file before we remove it.
  -- The window creation and process spawn are asynchronous wrt. running
  -- this script and are not awaitable, so we just pick a number.
  --
  -- Note: We don't strictly need to remove this file, but it is nice
  -- to avoid cluttering up the temporary directory.
  wezterm.sleep_ms(1000)
  os.remove(name)
end)
