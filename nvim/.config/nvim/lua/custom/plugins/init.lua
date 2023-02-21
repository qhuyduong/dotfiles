local overrides = require "custom.plugins.overrides"

return {
  ["nvim-treesitter/nvim-treesitter"] = {
    override_options = overrides.treesitter,
  },

  ["williamboman/mason.nvim"] = {
    override_options = overrides.mason,
  },

  ["nvim-telescope/telescope.nvim"] = {
    override_options = overrides.telescope,
  },

  ["NvChad/ui"] = {
    override_options = {
      statusline = {
        overriden_modules = function()
          return require "custom.plugins.ui"
        end,
      },
    },
  },

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

  ["neovim/nvim-lspconfig"] = {
    config = function()
      require "plugins.configs.lspconfig"
      require "custom.plugins.lspconfig"
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

  ["phaazon/hop.nvim"] = {
    branch = "v2", -- optional but strongly recommended
    config = function()
      require("hop").setup()
    end,
  },

  ["kdheepak/lazygit.nvim"] = {},

  ["tversteeg/registers.nvim"] = {
    config = function()
      require("registers").setup()
    end,
  },
}
