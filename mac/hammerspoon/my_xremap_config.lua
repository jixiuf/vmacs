-- ~/.hammerspoon/my_xremap_config.lua
-- Your Xremap-style window-conditional key remaps
--
-- Key name reference:
--   "return" "space" "tab" "delete" (backspace) "escape"
--   "left" "right" "up" "down" "home" "end" "pageup" "pagedown"
--   "f1".."f20"   "`" "-" "=" "[" "]" "\\" ";" "'" "," "." "/"
--   "0".."9"  "a".."z"
--   Modifiers: "cmd" "ctrl" "alt" "shift"
--
-- Output key format (in to-sequence):
--   - "key"           → key with no modifiers
--   - {"mod","key"}   → single modifier + key
--   - {"mod1","mod2","key"} → multiple modifiers + key (last is key)

local config = {

    ------------------------------------------------------------------
    -- 0) Generic terminal (no window title filter)
    --     Matching: Alacritty, dterm, foot, APMSSH, sshemacs
    ------------------------------------------------------------------
    -- {
    --     name = "Terminal",
    --     app_only = {
    --         "alacritty",
    --         "^dterm$",
    --         "^foot",
    --         "^APMSSH$",
    --         "^sshemacs$",
    --     },
    --     -- no win_only → matches all windows of these apps
    --     -- remap = {
    --     --     -- super-C-w → super-C-q
    --     --     { {"cmd","ctrl"}, "w",    {"cmd","q"} },

    --     --     -- C-w leader key (submap / modal)
    --     --     {
    --     --         leader = {"ctrl", "w"},
    --     --         submap = {
    --     --             { {"ctrl"}, "w",    {"ctrl","w"} },          -- C-w → C-w
    --     --             { {"ctrl"}, "d",    {"ctrl","x"}, {"ctrl","e"} },  -- C-d → C-x C-e
    --     --             { {"ctrl"}, "f",    {"ctrl","shift","o"} },  -- C-f → C-S-o (hints)
    --     --             { {"ctrl"}, "j",    cmd = "open-with" },     -- C-j → launch
    --     --             { {"ctrl"}, "k",    cmd = "open-with" },     -- C-k → launch
    --     --         },
    --     --     },
    --     -- }
    --     -- ,
    -- },

    ------------------------------------------------------------------
    -- 1) tmux inside any terminal
    --    super-C-\        →  C-c Space        (tmux prefix + next-layout)
    --    super-C-backspace →  C-c M-2         (tmux prefix + win-2)
    --    super-M-v        →  C-w C-y          (tmux paste)
    ------------------------------------------------------------------

    ------------------------------------------------------------------
    -- 2) emacs -nw inside Alacritty (no tmux layer)
    --    Matches window titles like "emacs@host" or " em filename"
    ------------------------------------------------------------------
    {
        name = "alacritty emacs",
        app_only = { "alacritty" },
        win_only = {
            "GNU/Emacs",
        },
        remap = {
            -- TODO: add your emacs-specific remaps here
            -- { {"cmd","ctrl"}, "w",   {"cmd","ctrl","w"}  },
            { {}, "escape",    {}, "f12" },
            -- { {"ctrl"}, ",",    {"ctrl","x"}, "left" },
            -- { {"ctrl"}, "p",    {"ctrl","x"}, "left" },
            -- { {"ctrl"}, ",",         {"ctrl","x"}, "left" } ,    -- keycode for ','

        },
    },
    {
        name = "terminal tmux",
        app_only = {
            "iTerm2",
            "alacritty",
            "kitty",
            "^Terminal$",       -- Apple Terminal
        },
        win_unless = {
            "GNU/Emacs",
        },
        remap = {
            { {"cmd","ctrl"}, "w",    {"ctrl","c"},{"k"} },
            -- { fromMods,   fromKey,       toSeq... }
            { {"cmd","ctrl"}, "\\",     {"ctrl","c"}, "space"         },
            { {"cmd","ctrl"}, "delete", {"ctrl","c"}, {"alt","2"}     },
            { {"cmd","alt"},  "v",      {"ctrl","w"}, {"ctrl","y"}    },
            { {"ctrl"}, ",",         {"ctrl","c"}, "p" } ,
            { {"ctrl"}, ".",         {"ctrl","c"}, "n" } ,
        },
    },
}

return config
