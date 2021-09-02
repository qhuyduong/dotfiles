-- plugins - Doom nvim custom plugins
--
-- This file contains all the custom plugins that are not in Doom nvim but that
-- the user requires. All the available fields can be found here
-- https://github.com/wbthomason/packer.nvim#specifying-plugins
--
-- By example, for including a plugin with a dependency on telescope:
-- return {
--     {
--         'user/repository',
--         requires = { 'nvim-lua/telescope.nvim' },
--     },
-- }

return {
  { "folke/tokyonight.nvim" },
  { "christoomey/vim-tmux-navigator" },
  { "vim-test/vim-test" },
  { "preservim/vimux" },
  { "tpope/vim-projectionist" },
  { "matze/vim-move" },
  {
    "blackCauldron7/surround.nvim",
    config = function()
      vim.g.surround_load_autogroups = false
      vim.g.surround_mappings_style = "surround"
      vim.g.surround_load_keymaps = true
      require("surround").setup({})
    end,
  },
}
