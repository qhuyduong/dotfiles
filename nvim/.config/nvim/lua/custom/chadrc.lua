local M = {}

local override = require "custom.plugins.override"
local userPlugins = require "custom.plugins"

M.plugins = {
   options = {
      lspconfig = {
         setup_lspconf = "custom.plugins.lspconfig",
      },

      statusline = {
         separator_style = "round",
      },
   },

   override = {
      ["kyazdani42/nvim-tree.lua"] = override.nvimtree,
      ["nvim-treesitter/nvim-treesitter"] = override.treesitter,
      ["nvim-telescope/telescope.nvim"] = override.telescope,
   },

   user = userPlugins,
}

M.ui = {
   theme = "tokyonight",
}

M.options = {
   user = function()
      require "custom.options"
   end,
}

M.mappings = require "custom.mappings"

return M
