-- xremap_style.lua — Xremap-like window-title-based key remapping for macOS
-- Usage: require('xremap_style').setup({...})  in your init.lua
--
-- =========================================================================
-- CONFIG FORMAT
-- =========================================================================
-- Each rule group:
-- {
--   name       = "my group",
--   app_only   = {"Pattern1", ...},  -- Lua patterns matching app name
--   app_unless = {"Pattern1", ...},  -- exclude apps matching these
--   win_only   = {"Pattern1", ...},  -- Lua patterns matching window title
--   win_unless = {"Pattern1", ...},  -- exclude windows matching these
--   remap      = { ... }
-- }
--
-- REMAP ENTRY FORMATS:
--
-- 1. Simple key→key(s):
--    { fromMods, fromKey, toStep1, toStep2, ... }
--    Each toStep: "key" (no mods) or {"mod","key"} or {"mod1","mod2","key"}
--    { {"cmd","ctrl"}, "w",    {"cmd","ctrl","q"} }
--
-- 2. Leader key (submap / modal):
--    { leader = {mods, key}, submap = { ... } }
--    Submap entries: { mods, key, toStep... }  or  { mods, key, cmd = "..." }
--
-- 3. Shell command (key → command):
--    { fromMods, fromKey, cmd = "shell command" }
--
-- =========================================================================
-- KEY NAMES
-- =========================================================================
-- "return" "space" "tab" "delete" (backspace) "escape"
-- "left" "right" "up" "down" "home" "end" "pageup" "pagedown"
-- "f1".."f20"   "`" "-" "=" "[" "]" "\\" ";" "'" "," "." "/"
-- "0".."9"  "a".."z"
-- Modifiers: "cmd" "ctrl" "alt" "shift"

local M = {}

-- ── state ───────────────────────────────────────────────────────────────

local _rules       = {}   -- all rule groups
local _ruleStates  = {}   -- per-rule: { hotkeys, modal, leaderHk }
local _globalFilter = nil -- single window filter for all rules
local _pollTimer   = nil  -- fallback polling timer
local _lastMatch   = nil  -- last matching state (for change logging)
local _POLL_INTERVAL = 2  -- seconds

-- ── helpers ─────────────────────────────────────────────────────────────

local function _sendKey(mods, key)
    hs.eventtap.keyStroke(mods, key)
end

--- Play an output sequence from entry[start] onward.
--- Before sending, releases any physically-held modifiers that are NOT
--- needed by the output — this prevents modifier leakage (e.g. if you
--- press Cmd+Ctrl+W and remap to Cmd+W, the physical Ctrl is released).
local function _playOutput(entry, start)
    -- 1. Collect all modifiers needed by the output sequence
    local needed = {}
    for i = start, #entry do
        local a = entry[i]
        if type(a) == "table" then
            for j = 1, #a - 1 do
                needed[a[j]] = true
            end
        end
    end

    -- 2. Release any held modifiers that are NOT needed
    local held = hs.eventtap.checkKeyboardModifiers()
    local allMods = {"cmd", "ctrl", "alt", "shift"}
    local released = {}
    for _, m in ipairs(allMods) do
        if held[m] and not needed[m] then
            hs.eventtap.event.newKeyEvent({}, m, false):post()
            released[m] = true
        end
    end
    if next(released) then
        hs.timer.usleep(15000)  -- let modifier release settle
    end

    -- 3. Send output keys
    for i = start, #entry do
        local a = entry[i]
        if type(a) == "table" then
            local n = #a
            if n >= 1 then
                local mods = {}
                for j = 1, n - 1 do mods[j] = a[j] end
                _sendKey(mods, a[n])
            end
        else
            _sendKey({}, a)
        end
    end

    -- 4. Restore released modifiers
    for m, _ in pairs(released) do
        hs.eventtap.event.newKeyEvent({}, m, true):post()
    end
end

local function _runCmd(cmd)
    if type(cmd) == "table" then
        local t = {}
        for _, v in ipairs(cmd) do table.insert(t, v) end
        local prog = table.remove(t, 1)
        hs.task.new(prog, nil, t):start()
    else
        hs.execute(cmd)
    end
end

-- ── rule matching ───────────────────────────────────────────────────────

--- Check whether a window+app match a rule's filters.
--- Matching is case-INsensitive.
local function _ruleMatches(rule, win, app)
    if not win or not app then return false end

    if rule.app_only and #rule.app_only > 0 then
        local name = app:name() or ""
        local ok = false
        for _, pat in ipairs(rule.app_only) do
            if name:lower():match(pat:lower()) then ok = true; break end
        end
        if not ok then return false end
    end

    if rule.app_unless and #rule.app_unless > 0 then
        local name = app:name() or ""
        for _, pat in ipairs(rule.app_unless) do
            if name:lower():match(pat:lower()) then return false end
        end
    end

    if rule.win_only and #rule.win_only > 0 then
        local title = win:title() or ""
        local ok = false
        for _, pat in ipairs(rule.win_only) do
            if title:lower():match(pat:lower()) then ok = true; break end
        end
        if not ok then return false end
    end

    if rule.win_unless and #rule.win_unless > 0 then
        local title = win:title() or ""
        for _, pat in ipairs(rule.win_unless) do
            if title:lower():match(pat:lower()) then return false end
        end
    end

    return true
end

-- ── hotkey builder ──────────────────────────────────────────────────────

--- Build all hotkeys for a single remap entry.
--- Returns a table: { hotkeys = {...}, modal = modal_or_nil, leaderHk = hk_or_nil }
local function _buildEntry(entry)
    local result = { hotkeys = {} }

    if entry.leader then
        -- Leader key with submap
        local m = hs.hotkey.modal.new()
        m:exit()

        for _, sub in ipairs(entry.submap or {}) do
            if sub.cmd then
                m:bind(sub[1], sub[2], function()
                    m:exit()
                    _runCmd(sub.cmd)
                end)
            else
                m:bind(sub[1], sub[2], function()
                    m:exit()
                    _playOutput(sub, 3)
                end)
            end
        end
        m:bind({}, "escape", function() m:exit() end)

        local leaderHk = hs.hotkey.new(entry.leader[1], entry.leader[2], function()
            m:enter()
        end)
        leaderHk:disable()

        table.insert(result.hotkeys, leaderHk)
        result.leaderHk = leaderHk
        result.modal = m

    elseif entry.cmd then
        -- Shell command
        local hk = hs.hotkey.new(entry[1], entry[2], function()
            _runCmd(entry.cmd)
        end)
        hk:disable()
        table.insert(result.hotkeys, hk)

    else
        -- Simple key → key(s) — extra modifiers auto-released by _playOutput
        local hk = hs.hotkey.new(entry[1], entry[2], function()
            _playOutput(entry, 3)
        end)
        hk:disable()
        table.insert(result.hotkeys, hk)
    end

    return result
end

-- ── global state updater ────────────────────────────────────────────────

local function _updateAll()
    local win = hs.window.frontmostWindow()
    local app = win and win:application()
    local appName = app and app:name() or "-"
    local winTitle = win and win:title() or "-"

    local anyMatch = false
    for i, rule in ipairs(_rules) do
        local state = _ruleStates[i]
        if state then
            local match = _ruleMatches(rule, win, app)
            if match then anyMatch = true end
            for _, hk in ipairs(state.hotkeys) do
                if match then hk:enable() else hk:disable() end
            end
            if not match and state.modal then
                state.modal:exit()
            end
        end
    end
    -- Log when matching state changes (only when there are rules to match)
    if #_rules > 0 and _lastMatch ~= anyMatch then
        _lastMatch = anyMatch
        if anyMatch then
            print("[xremap] ✓ matched: app=" .. appName .. "  title=" .. winTitle)
        else
            print("[xremap] ✗ no match: app=" .. appName .. "  title=" .. winTitle)
        end
    end
end

-- ── public API ──────────────────────────────────────────────────────────

--- Start window-conditional key remapping.
--- Safe to call multiple times (e.g. on Hammerspoon reload) — cleans up previous state.
function M.setup(rules)
    -- Teardown previous state
    M.teardown()

    _rules = rules
    _ruleStates = {}

    -- Build hotkeys for each rule group
    for _, rule in ipairs(rules) do
        local state = { hotkeys = {} }
        for _, entry in ipairs(rule.remap or {}) do
            local res = _buildEntry(entry)
            for _, hk in ipairs(res.hotkeys) do
                table.insert(state.hotkeys, hk)
            end
            if res.modal then
                state.modal = res.modal
                state.leaderHk = res.leaderHk
            end
        end
        table.insert(_ruleStates, state)
    end

    -- Initial sync
    _updateAll()

    -- Single global window filter: fires on ANY window focus change
    _globalFilter = hs.window.filter.new(true)  -- true = match all windows
    _globalFilter:subscribe(
        hs.window.filter.windowFocused,
        _updateAll
    )
    -- Also update when window titles change (e.g. tmux renames a pane)
    _globalFilter:subscribe(
        hs.window.filter.windowTitleChanged,
        _updateAll
    )

    -- Fallback polling timer — catches any missed events
    _pollTimer = hs.timer.new(_POLL_INTERVAL, _updateAll)
    _pollTimer:start()

    print("[xremap_style] loaded " .. #rules .. " rule(s), poll=" .. _POLL_INTERVAL .. "s")
end

--- Stop all hotkeys and cleanup.
function M.teardown()
    if _globalFilter then
        _globalFilter:delete()
        _globalFilter = nil
    end
    if _pollTimer then
        _pollTimer:stop()
        _pollTimer = nil
    end
    for _, state in ipairs(_ruleStates or {}) do
        for _, hk in ipairs(state.hotkeys or {}) do
            hk:delete()
        end
    end
    _ruleStates = {}
    _rules = {}
end

return M
