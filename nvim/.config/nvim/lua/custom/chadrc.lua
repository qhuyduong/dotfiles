---@type ChadrcConfig
local M = {}

-- Path to overriding theme and highlights files
local highlights = require("custom.highlights")

M.ui = {
	theme = "catppuccin",
	theme_toggle = { "catppuccin", "one_light" },

	hl_override = highlights.override,
	hl_add = highlights.add,
	statusline = {
		theme = "vscode_colored", -- default/vscode/vscode_colored/minimal
	},
}

M.plugins = "custom.plugins"

-- check core.mappings for table structure
M.mappings = require("custom.mappings")

return M
