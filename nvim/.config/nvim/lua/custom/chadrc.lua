local M = {}

local override = require "custom.override"

M.plugins = {
   override = {
      ["kyazdani42/nvim-tree.lua"] = override.nvimtree,
      ["nvim-treesitter/nvim-treesitter"] = override.treesitter,
      ["nvim-telescope/telescope.nvim"] = override.telescope,
      ["williamboman/mason.nvim"] = override.mason,
   },

   user = require "custom.plugins",
}

M.ui = {
   theme = "tokyonight",
}

M.mappings = require "custom.mappings"

return M
