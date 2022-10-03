-- overriding default plugin configs!

local M = {}

M.treesitter = {
   ensure_installed = {
      "vim",
      "lua",
      "json",
      "markdown",
      "ruby",
      "html",
      "css",
      "javascript",
      "typescript",
      "tsx",
   },
}

M.nvimtree = {
   git = {
      enable = true,
   },
}

M.mason = {
   ensure_installed = {
      -- lua stuff
      "lua-language-server",
      "stylua",

      -- web dev
      "css-lsp",
      "html-lsp",
      "typescript-language-server",
      "emmet-ls",
      "json-lsp",
   },
}

return M
