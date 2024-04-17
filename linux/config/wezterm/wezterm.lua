-- Pull in the wezterm API
local wezterm = require 'wezterm'
local io = require 'io'
local os = require 'os'
local act = wezterm.action
local theme = require("theme")
local keybinds = require("keybinds")
local util=require("util")
require("on")


-- This table will hold the configuration.
local config = {}


-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end


config.hyperlink_rules = wezterm.default_hyperlink_rules()

config.unix_domains = { {name = 'unix'} }
config.default_gui_startup_args = { 'connect', 'unix' }
-- and finally, return the configuration to wezterm
util.merge_tables(config,theme)
util.merge_tables(config,keybinds)

return config
