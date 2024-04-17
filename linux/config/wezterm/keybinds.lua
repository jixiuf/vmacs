local config = {}
local util = require 'util'
local wezterm = require 'wezterm'
local act = wezterm.action


function starts_with(str, prefix)
   return string.sub(str, 1, string.len(prefix)) == prefix
end
local search_mode = {
   { key = 'g', mods = 'CTRL', action =  wezterm.action.Multiple {
        act.ClearSelection,
        act.CopyMode 'EditPattern',
        act.CopyMode 'ClearPattern',
        act.CopyMode 'AcceptPattern',
        act.CopyMode  'ClearSelectionMode' }},
   { key = 'Enter', mods = 'NONE', action = wezterm.action.Multiple {
        act.CopyMode 'AcceptPattern',
        act.CopyMode  'ClearSelectionMode',
        act.ClearSelection}},

}

local copy_mode = {
   { key = 'g', mods = 'CTRL',
     action =  wezterm.action.Multiple {
        act.ClearSelection,
        act.CopyMode 'ClearPattern',
        act.CopyMode  'ClearSelectionMode',
        act.ClearSelection,
        act.CopyMode("Close"),}},
   { key = 'Escape', mods = 'NONE',
     action =  wezterm.action.Multiple {
        act.ClearSelection,
        act.CopyMode 'ClearPattern',
        act.CopyMode  'ClearSelectionMode',
        act.ClearSelection,
        act.CopyMode("Close"),}},
   { key = 'a', mods = 'NONE',
     action =  wezterm.action.Multiple {
        act.ClearSelection,
        act.CopyMode 'ClearPattern',
        act.CopyMode  'ClearSelectionMode',
        act.ClearSelection,
        act.CopyMode("Close"),}},
   { key = 'q', mods = 'NONE',
     action =  wezterm.action.Multiple {
        act.ClearSelection,
        act.CopyMode 'ClearPattern',
        act.CopyMode  'ClearSelectionMode',
        act.ClearSelection,
        act.CopyMode("Close"),
   }},
   -- table.insert(copy_mode, { key = 'e', mods = 'ALT', action = act.CopyMode 'EditPattern' })
   { key = '/', mods = 'NONE', action = act.Search 'CurrentSelectionOrEmptyString' },
   { key = "p", mods = "NONE",
     action = act.Multiple({
           act.CopyMode("PriorMatch"),
           act.CopyMode("ClearSelectionMode"),
           act.ClearSelection,
     }),
   },
   { key = "n", mods = "SHIFT",
     action = act.Multiple({
           act.CopyMode("PriorMatch"),
           act.CopyMode("ClearSelectionMode"),
           act.ClearSelection,
     }),
   },
   { key = "n", mods = "NONE",
     action = act.Multiple({
           act.CopyMode("NextMatch"),
           act.CopyMode("ClearSelectionMode"),
           act.ClearSelection,
     }),
   },
   { key = "e", mods = "NONE", action = act.CopyMode("MoveForwardWord"),},
   { key = "v", mods = "NONE", action = act.CopyMode("MoveBackwardWord"),},
   { key = 's', mods = 'NONE', action = act.CopyMode{ SetSelectionMode =  'Cell' } },
   { key = "y", mods = "NONE",
     action = act({
           Multiple = {
              act({ CopyTo = "ClipboardAndPrimarySelection" }),
              act.ClearSelection,
              act.CopyMode("Close"),
           },
     }),
   },
   -- wezterm show-keys --lua --key-table copy_mode
   { key = 'v', mods = 'ALT', action = act.CopyMode 'PageUp' },
   { key = 'v', mods = 'CTRL', action = act.CopyMode 'PageDown' },
   { key = 'e', mods = 'CTRL',  action = act.CopyMode 'MoveToEndOfLineContent'},
   { key = 'a', mods = 'CTRL',  action = act.CopyMode 'MoveToStartOfLineContent'},
   { key = '2', mods = 'CTRL', action = act.CopyMode{ SetSelectionMode =  'Cell' } },
   { key = 'x', mods = 'NONE',  action = act.CopyMode 'MoveToSelectionOtherEndHoriz'},
}
local keys = {
   { key = 'p', mods = 'CTRL|ALT', action = act.ScrollToPrompt(-1) },
   { key = 'n', mods = 'CTRL|ALT', action = act.ScrollToPrompt(1) },
   { key = '.', mods = 'CTRL', action =  wezterm.action.ActivateTabRelative(1) },
   { key = ',', mods = 'CTRL', action =  wezterm.action.ActivateTabRelative(-1) },
   { key = 'c', mods = 'CTRL|CMD', action = wezterm.action.CopyTo 'Clipboard' },
   { key = "v", mods = "CMD|CTRL", action = wezterm.action{PasteFrom="Clipboard"}},
   -- PrimarySelection default: C-S-v
   { key = "v", mods = "SHIFT|CTRL", action = wezterm.action{PasteFrom="PrimarySelection"}},
   { key = 'v', mods = 'ALT', action =  wezterm.action.ActivateCopyMode },

   { key = 't', mods = 'CMD|CTRL', action =  wezterm.action.SpawnTab 'CurrentPaneDomain' },
   -- table.insert(keys, { key = 'w', mods = 'CMD|CTRL', action =  wezterm.action.CloseCurrentTab{ confirm = false } })
   { key = 'w', mods = 'CMD|CTRL', action =  wezterm.action.CloseCurrentPane{ confirm = false } },
   -- { key = 'v', mods = 'ALT', action =  wezterm.action.ActivateCopyMode },
   -- { key = 'j', mods = 'CMD', action =  wezterm.action.ActivatePaneDirection 'Down', },
   -- { key = 'k', mods = 'CMD', action =  wezterm.action.ActivatePaneDirection 'Up', },
   -- { key = 'R', mods = 'SHIFT|CTRL', action =  wezterm.action.ReloadConfiguration },
   { key = 'Enter', mods = 'CMD|CTRL', action =  wezterm.action.SpawnWindow },
   { key = '2', mods = 'CTRL', action =  wezterm.action.Multiple
     {
        act.Search { CaseSensitiveString = '' },
        act.ClearSelection,
        -- act.SendKey { key = '/'  },
        -- act.SendKey { key = 'u',mods = 'CTRL'  },
        -- act.SendKey { key = 'Enter'},
        act.CopyMode  'ClearSelectionMode' ,
        wezterm.action.ActivateCopyMode ,
     }
   },
   {key = 'F', mods = 'CTRL|SHIFT', action = act.EmitEvent 'edit-scrollback'},

   -- ctrl_shift_m: 快速打开指定文件 ，http， filename:linenum
   -- also see https://github.com/wez/wezterm/discussions/529
   {
      key = 'M',
      mods = 'CTRL|SHIFT',
      action = wezterm.action.QuickSelectArgs {
         label = 'open url',
         patterns = {
            'https?://\\S+',
            "([\\/\\.\\-_a-zA-Z0-9]+:[0-9]+)",
            "([\\/\\.\\-_a-zA-Z0-9]+)",
         },
         action = wezterm.action_callback(function(window, pane)
               local url = window:get_selection_text_for_pane(pane)
               local cwd_uri = pane:get_current_working_dir()
               local cwd = ''
               local hostname = ''
               if cwd_uri then
                  if type(cwd_uri) == 'userdata' then
                     -- Running on a newer version of wezterm and we have
                     -- a URL object here, making this simple!

                     cwd = cwd_uri.file_path
                     hostname = cwd_uri.host or wezterm.hostname()
                  else
                     -- an older version of wezterm, 20230712-072601-f4abf8fd or earlier,
                     -- which doesn't have the Url object
                     cwd_uri = cwd_uri:sub(8)
                     local slash = cwd_uri:find '/'
                     if slash then
                        hostname = cwd_uri:sub(1, slash - 1)
                        -- and extract the cwd from the uri, decoding %-encoding
                        cwd = cwd_uri:sub(slash):gsub('%%(%x%x)', function(hex)
                                                         return string.char(tonumber(hex, 16))
                                                     end)
                     end
                  end
               end
               if url:sub(1, 1) == "/" then
                  wezterm.open_with(url,'open-with')
                  -- ctrl_shift_l see log
                  -- wezterm.log_info('2222opening: ' .. cwd .. "|" .. hostname .. "|" .. url)
               elseif starts_with(url,"http") then
                  wezterm.open_with(url)
               else
                  wezterm.open_with(cwd .. "/" .. url,'open-with')
               end
         end),
      },
   },

}
if wezterm.gui then
   -- https://wezfurlong.org/wezterm/config/lua/wezterm.gui/default_key_tables.html
   search_mode = util.merge_lists(wezterm.gui.default_key_tables().search_mode, search_mode)
   -- wezterm show-keys --lua
   config.keys = util.merge_lists(wezterm.gui.default_keys(), keys)
   copy_mode = util.merge_lists(wezterm.gui.default_key_tables().copy_mode, copy_mode)
   -- 使用emacsclient 打开scrollback
   config.mouse_bindings = {
      -- Change the default click behavior so that it only selects
      -- text and doesn't open hyperlinks
      -- {
      --   event = { Up = { streak = 1, button = 'Left' } },
      --   mods = 'NONE',
      --   action = act.CompleteSelection 'ClipboardAndPrimarySelection',
      -- },

      -- -- and make CTRL-Click open hyperlinks
      -- {
      --   event = { Up = { streak = 1, button = 'Left' } },
      --   mods = 'CTRL',
      --   action = act.OpenLinkAtMouseCursor,
      -- },

      -- Scrolling up while holding CTRL increases the font size
      {
         event = { Down = { streak = 1, button = { WheelUp = 1 } } },
         mods = 'CTRL',
         action = act.IncreaseFontSize,
      },

      -- Scrolling down while holding CTRL decreases the font size
      {
         event = { Down = { streak = 1, button = { WheelDown = 1 } } },
         mods = 'CTRL',
         action = act.DecreaseFontSize,
      },
      {
         -- 左键3连点，选中命令的output 内容 需要osc 133 的支持
         -- ohmyzsh 用户可以使用 iterm2的 shell_integration 来实现
         -- source $ZSH/plugins/iterm2/iterm2_shell_integration.zsh
         event = { Down = { streak = 3, button = 'Left' } },
         action = wezterm.action.SelectTextAtMouseCursor 'SemanticZone',
         mods = 'NONE',
      },
   }

   -- { key = 'l', mods = 'CTRL|CMD', action =  wezterm.action.Multiple
   --   {
   --      wezterm.action.ClearScrollback 'ScrollbackAndViewport',
   --      wezterm.action.SendKey { key = 'L', mods = 'CTRL' },
   --   }
   -- },

   config.key_tables = {copy_mode = copy_mode,search_mode=search_mode}
end

return config