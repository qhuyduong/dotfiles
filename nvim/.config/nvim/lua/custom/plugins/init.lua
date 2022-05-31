return {
   ["windwp/nvim-ts-autotag"] = {
      ft = { "html", "javascriptreact" },
      after = "nvim-treesitter",
      config = function()
         require("nvim-ts-autotag").setup()
      end,
   },

   ["jose-elias-alvarez/null-ls.nvim"] = {
      after = "nvim-lspconfig",
      config = function()
         require "custom.plugins.null-ls"
      end,
   },

   ["nvim-telescope/telescope-media-files.nvim"] = {
      after = "telescope.nvim",
      config = function()
         require("telescope").load_extension "media_files"
      end,
   },

   ["aserowy/tmux.nvim"] = {
      config = function()
         require "custom.plugins.tmux"
      end,
   },

   ["vim-test/vim-test"] = { requires = "preservim/vimux" },

   ["tpope/vim-surround"] = {},

   ["tpope/vim-projectionist"] = {},

   ["windwp/nvim-autopairs"] = {
      after = "nvim-cmp",
      config = function()
         require "custom.plugins.autopairs"
      end,
   },

   ["qhuyduong/lazygit.nvim"] = {
      requires = "plenary.nvim",
      cmd = { "LazyGit", "LazyGitConfig" },
   },

   ["tpope/vim-fugitive"] = {
      cmd = {
         "Gclog",
         "Gdiff",
         "Gdiffsplit",
         "Git",
         "Gvdiffsplit",
         "Gw",
         "Gwrite",
      },
   },

   ["francoiscabrol/ranger.vim"] = {
      requires = "rbgrouleff/bclose.vim",
   },
}
